import type { Operation, OperationAndRevision } from "./operation";

export enum ClientEntryType {
  USER_EDIT_IMMEDIATELY_SENT_TO_SERVER = "USER_EDIT_IMMEDIATELY_SENT_TO_SERVER",
  USER_EDIT_STORED_AS_BUFFER = "USER_EDIT_STORED_AS_BUFFER",
  USER_EDIT_ADDED_TO_BUFFER = "USER_EDIT_ADDED_TO_BUFFER",
  RECEIVED_OWN_OPERATION = "RECEIVED_OWN_OPERATION",
  RECEIVED_OWN_OPERATION_AND_SENT_BUFFER = "RECEIVED_OWN_OPERATION_AND_SENT_BUFFER",
  RECEIVED_SERVER_OPERATION_WHILE_SYNCHRONIZED = "RECEIVED_SERVER_OPERATION_WHILE_SYNCHRONIZED",
  RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION = "RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION",
  RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION_WITH_BUFFER = "RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION_WITH_BUFFER",
}

export type ClientLogEntry<OpT> =
  | UserEditImmediatelySentToServer<OpT>
  | UserEditStoredAsBuffer<OpT>
  | UserEditAddedToBuffer<OpT>
  | ReceivedOwnOperation<OpT>
  | ReceivedOwnOperationAndSentBuffer<OpT>
  | ReceivedServerOperationWhileSynchronized<OpT>
  | ReceivedServerOperationWhileAwaitingOperation<OpT>
  | ReceivedServerOperationWhileAwaitingOperationWithBuffer<OpT>;

export interface UserEditImmediatelySentToServer<OpT> {
  type: ClientEntryType.USER_EDIT_IMMEDIATELY_SENT_TO_SERVER;
  operation: OperationAndRevision<OpT>;
}

export interface UserEditStoredAsBuffer<OpT> {
  type: ClientEntryType.USER_EDIT_STORED_AS_BUFFER;
  operation: OperationAndRevision<OpT>;
}

export interface UserEditAddedToBuffer<OpT> {
  type: ClientEntryType.USER_EDIT_ADDED_TO_BUFFER;
  base: OpT;
}

export interface ReceivedOwnOperation<OpT> {
  type: ClientEntryType.RECEIVED_OWN_OPERATION;
  acknowledgedOperation: OperationAndRevision<OpT>;
}

export interface ReceivedOwnOperationAndSentBuffer<OpT> {
  type: ClientEntryType.RECEIVED_OWN_OPERATION_AND_SENT_BUFFER;
  acknowledgedOperation: OperationAndRevision<OpT>;
  sentBuffer: OperationAndRevision<OpT>;
}

export interface ReceivedServerOperationWhileSynchronized<OpT> {
  type: ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_SYNCHRONIZED;
  receivedOperation: OperationAndRevision<OpT>;
}

export interface ReceivedServerOperationWhileAwaitingOperation<OpT> {
  type: ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION;
  receivedOperation: OperationAndRevision<OpT>;
  transformedReceivedOperation: Operation<OpT>;
  awaitedOperation: OperationAndRevision<OpT>;
  transformedAwaitedOperation: OperationAndRevision<OpT>;
}

export interface ReceivedServerOperationWhileAwaitingOperationWithBuffer<OpT> {
  type: ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION_WITH_BUFFER;
  receivedOperation: OperationAndRevision<OpT>;
  onceTransformedReceivedOperation: Operation<OpT>;
  twiceTransformedReceivedOperation: Operation<OpT>;
  awaitedOperation: OperationAndRevision<OpT>;
  transformedAwaitedOperation: OperationAndRevision<OpT>;
  bufferOperation: Operation<OpT>;
  transformedBufferOperation: Operation<OpT>;
}
