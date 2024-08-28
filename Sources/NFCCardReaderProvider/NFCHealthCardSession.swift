#if os(iOS)

import CardReaderProviderApi
import Combine
import CoreNFC
import Foundation
import HealthCardAccess
import HealthCardControl
import OSLog

public protocol NFCOperation {
    associatedtype Output
    func execute(with handle: NFCHealthCardSessionHandle) async throws -> Output
}

public class AnyNFCOperationBase {
    func execute(with handle: NFCHealthCardSessionHandle) async throws -> Any {
        fatalError("This method should be overridden")
    }
}

public class AnyNFCOperation<Output>: AnyNFCOperationBase {
    private let _execute: (NFCHealthCardSessionHandle) async throws -> Output

    public init<O: NFCOperation>(_ operation: O) where O.Output == Output {
        self._execute = operation.execute
    }

    public override func execute(with handle: NFCHealthCardSessionHandle) async throws -> Any {
        return try await _execute(handle)
    }
}

public class NFCHealthCardSession: NSObject, NFCTagReaderSessionDelegate {
    private typealias OperationCheckedContinuation<Output> = CheckedContinuation<Output, Error>
    private var operationContinuation: Any?

    private let messages: Messages
    private let can: String

    private var session: NFCTagReaderSession?

    var currentOperation: AnyNFCOperationBase?

    public init?(
        pollingOption: NFCTagReaderSession.PollingOption = .iso14443,
        on queue: DispatchQueue = .global(qos: .userInitiated),
        messages: Messages,
        can: String
    ) {
        self.messages = messages
        self.can = can
        super.init()

        guard let mNFCReaderSession = NFCTagReaderSession(
            pollingOption: pollingOption,
            delegate: self,
            queue: queue
        )
        else {
            Logger.nfcCardReaderProvider
                .debug("Could not start discovery for NFCCardReader: refused to init a NFCTagReaderSession")
            return nil
        }

        session = mNFCReaderSession
    }

    public func executeOperation<O: NFCOperation>(_ operation: O) async throws -> O.Output {
        guard let session = self.session else {
            throw NFCHealthCardSessionError.couldNotInitializeSession
        }
        session.alertMessage = messages.discoveryMessage
        Logger.nfcCardReaderProvider.debug("Starting session: \(String(describing: self.session))")
        session.begin()

        currentOperation = AnyNFCOperation(operation)

        return try await withCheckedThrowingContinuation { (continuation: OperationCheckedContinuation<O.Output>) in
            self.operationContinuation = continuation
        }
    }

    deinit {
        Logger.nfcCardReaderProvider.debug("Deinit MyNFCSession")
        session?.invalidate()
    }

    public func invalidateSession(with error: String?) {
        if let error = error {
            session?.invalidate(errorMessage: error)
        } else {
            session?.invalidate()
        }
    }

    // MARK: - NFCTagReaderSessionDelegate

    public func tagReaderSessionDidBecomeActive(_: NFCTagReaderSession) {
        Logger.nfcCardReaderProvider.debug("NFC reader session became active")
    }

    public func tagReaderSession(_: NFCTagReaderSession, didInvalidateWithError error: Swift.Error) {
        Logger.nfcCardReaderProvider.debug("NFC reader session was invalidated: \(error)")
        let coreNFCError = error.asCoreNFCError()
        if let continuation = operationContinuation as? OperationCheckedContinuation<Any> {
            continuation.resume(throwing: NFCHealthCardSessionError.coreNFC(coreNFCError))
        }
        operationContinuation = nil
    }

    public func tagReaderSession(_ session: NFCTagReaderSession, didDetect tags: [NFCTag]) {
        Logger.nfcCardReaderProvider.debug("tagReaderSession:didDetect - [\(tags)]")
        if tags.count > 1 {
            session.alertMessage = messages.multipleCardsMessage
            DispatchQueue.global().asyncAfter(deadline: .now() + .milliseconds(500)) {
                session.invalidate(errorMessage: self.messages.multipleCardsMessage)
            }
            return
        }

        guard let tag = tags.first else {
            session.alertMessage = messages.noCardMessage
            DispatchQueue.global().asyncAfter(deadline: .now() + .milliseconds(500)) {
                session.invalidate(errorMessage: self.messages.noCardMessage)
            }
            return
        }
        guard case let .iso7816(iso7816NfcTag) = tag else {
            session.invalidate(errorMessage: messages.unsupportedCardMessage)
            if let continuation = operationContinuation as? OperationCheckedContinuation<Any> {
                continuation.resume(throwing: NFCHealthCardSessionError.unsupportedTag)
            }
            operationContinuation = nil
            return
        }

        session.alertMessage = messages.connectMessage

        Task {
            do {
                try await session.connect(to: tag)
            } catch {
                if let continuation = operationContinuation as? OperationCheckedContinuation<Any> {
                    continuation.resume(throwing: NFCHealthCardSessionError.coreNFC(error.asCoreNFCError()))
                }
                operationContinuation = nil
                return
            }

            session.alertMessage = messages.secureChannelMessage
            let card = NFCCard(isoTag: iso7816NfcTag)

            let secureHealthCard: HealthCardType
            do {
                secureHealthCard = try await card.openSecureSessionAsync(can: can)
            } catch let error as CoreNFCError {
                if let continuation = operationContinuation as? OperationCheckedContinuation<Any> {
                    continuation.resume(throwing: NFCHealthCardSessionError.coreNFC(error))
                }
                operationContinuation = nil
                return
            } catch HealthCardControl.KeyAgreement.Error.macPcdVerificationFailedOnCard {
                if let continuation = operationContinuation as? OperationCheckedContinuation<Any> {
                    continuation.resume(throwing: NFCHealthCardSessionError.wrongCAN)
                }
                operationContinuation = nil
                return
            } catch {
                if let continuation = operationContinuation as? OperationCheckedContinuation<Any> {
                    continuation.resume(throwing: NFCHealthCardSessionError.establishingSecureChannel(error))
                }
                operationContinuation = nil
                return
            }

            let myNFCCardSession = DefaultNFCHealthCardSessionHandle(
                card: secureHealthCard,
                session: session
            )

            do {
                guard let currentOperation = self.currentOperation else {
                    throw NFCHealthCardSessionError.operationNotSet
                }
                let outcome = try await currentOperation.execute(with: myNFCCardSession)
                if let continuation = operationContinuation as? OperationCheckedContinuation<Any> {
                    continuation.resume(returning: outcome)
                }
                operationContinuation = nil
            } catch let error as CoreNFCError {
                if let continuation = operationContinuation as? OperationCheckedContinuation<Any> {
                    continuation.resume(throwing: NFCHealthCardSessionError.coreNFC(error))
                }
                operationContinuation = nil
                session.invalidate()
                return
            } catch {
                if let continuation = operationContinuation as? OperationCheckedContinuation<Any> {
                    continuation.resume(throwing: NFCHealthCardSessionError.operation(error))
                }
                operationContinuation = nil
                session.invalidate()
                return
            }
        }
    }
}

/// The (only) error type that is thrown by `.executeOperation().
public enum NFCHealthCardSessionError: Swift.Error {
    /// Indicates that the NFC session could not be initialized.
    case couldNotInitializeSession

    /// Represents an error when the detected tag is not supported, e.g. that not a Health Card.
    case unsupportedTag

    /// Encapsulates errors originating from the CoreNFC framework. This includes, but is not limited to,
    /// communication errors, user cancellation, or configuration issues.
    /// `CoreNFCError` is a bridge from `NFCReaderError`.
    case coreNFC(CoreNFCError)

    /// Signifies that the provided CAN (Card Access Number) is incorrect or failed verification, preventing
    /// establishment of a secure channel. It's a common sub case of the `establishingSecureChannel` error.
    case wrongCAN

    /// Occurs when establishing a secure channel with the health card fails. This includes errors during key agreement,
    /// authentication, or other security protocol failures.
    case establishingSecureChannel(Swift.Error)

    /// Generic error for failures during operation execution. This can include APDU de-/serialization errors, and
    /// errors thrown by the operation's instructions itself.
    case operation(Swift.Error)

    /// Error indicating that the operation was not set.
    case operationNotSet

    /// Error indicating that the operation result type is invalid.
    case invalidOperationResult
}

/// Abstraction to the NFCTagReaderSession to update the alertMessage that is being displayed to the user.
/// And to close/invalidate the session
public protocol NFCHealthCardSessionHandle {
    /// Update the NFC Dialog message
    func updateAlert(message: String)

    /// End session
    ///
    /// - Parameter error: when set the session will end erroneously
    func invalidateSession(with error: String?)

    /// The underlying Card for the emitted NFCCardSession
    ///  The secure card channel has already been established initially
    var card: HealthCardType { get }
}

private struct DefaultNFCHealthCardSessionHandle: NFCHealthCardSessionHandle {
    let card: HealthCardType
    let session: NFCTagReaderSession

    func updateAlert(message: String) {
        Task { @MainActor in self.session.alertMessage = message }
    }

    func invalidateSession(with error: String?) {
        Task { @MainActor in
            if let error = error {
                session.invalidate(errorMessage: error)
            } else {
                session.invalidate()
            }
        }
    }
}

extension NFCHealthCardSession {
    /// NFCTagReaderSession messages
    public struct Messages {
        /// The message that is being displayed when polling for a NFC Card
        public let discoveryMessage: String
        /// The message when the card is being initialized for downstream usage
        public let connectMessage: String
        /// The message during establishing a secure card channel after the connect
        public let secureChannelMessage: String
        /// The message when 'something else' as a card is found, but not a card
        public let noCardMessage: String
        /// The message to display when multiple NFC Cards were detected
        public let multipleCardsMessage: String
        /// The message when the card type is unsupported
        public let unsupportedCardMessage: String
        /// The generic error message
        public let connectionErrorMessage: String

        /// Messages constructor
        ///
        /// - Parameters:
        ///   - discoveryMessage: The message that is being displayed when polling for a NFC Card
        ///   - connectMessage: The message when the card is being initialized for downstream usage
        ///   - secureChannelMessage: The message during establishing a secure card channel after the connect
        ///   - noCardMessage: The message when 'something else' as a card is found, but not a card
        ///   - multipleCardsMessage: The message to display when multiple NFC Cards were detected
        ///   - unsupportedCardMessage:  The message when the card type is unsupported
        ///   - connectionErrorMessage: The generic (communication) error message
        public init(
            discoveryMessage: String,
            connectMessage: String,
            secureChannelMessage: String,
            noCardMessage: String,
            multipleCardsMessage: String,
            unsupportedCardMessage: String,
            connectionErrorMessage: String
        ) {
            self.discoveryMessage = discoveryMessage
            self.connectMessage = connectMessage
            self.secureChannelMessage = secureChannelMessage
            self.noCardMessage = noCardMessage
            self.multipleCardsMessage = multipleCardsMessage
            self.unsupportedCardMessage = unsupportedCardMessage
            self.connectionErrorMessage = connectionErrorMessage
        }
    }
}

#endif
