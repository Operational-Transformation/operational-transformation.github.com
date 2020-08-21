import { Operation, OperationAndRevision } from "./operation";
import { TextOperation } from "ot";

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

export type ClientLogEntry =
  | UserEditImmediatelySentToServer
  | UserEditStoredAsBuffer
  | UserEditAddedToBuffer
  | ReceivedOwnOperation
  | ReceivedOwnOperationAndSentBuffer
  | ReceivedServerOperationWhileSynchronized
  | ReceivedServerOperationWhileAwaitingOperation
  | ReceivedServerOperationWhileAwaitingOperationWithBuffer;

export type ClientLog = ClientLogEntry[];

export interface UserEditImmediatelySentToServer {
  type: ClientEntryType.USER_EDIT_IMMEDIATELY_SENT_TO_SERVER;
  operation: OperationAndRevision;
}

export interface UserEditStoredAsBuffer {
  type: ClientEntryType.USER_EDIT_STORED_AS_BUFFER;
  operation: OperationAndRevision;
}

export interface UserEditAddedToBuffer {
  type: ClientEntryType.USER_EDIT_ADDED_TO_BUFFER;
  textOperation: TextOperation;
}

export interface ReceivedOwnOperation {
  type: ClientEntryType.RECEIVED_OWN_OPERATION;
  acknowledgedOperation: OperationAndRevision;
}

export interface ReceivedOwnOperationAndSentBuffer {
  type: ClientEntryType.RECEIVED_OWN_OPERATION_AND_SENT_BUFFER;
  acknowledgedOperation: OperationAndRevision;
  sentBuffer: OperationAndRevision;
}

export interface ReceivedServerOperationWhileSynchronized {
  type: ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_SYNCHRONIZED;
  receivedOperation: OperationAndRevision;
}

export interface ReceivedServerOperationWhileAwaitingOperation {
  type: ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION;
  receivedOperation: OperationAndRevision;
  transformedReceivedOperation: Operation;
  awaitedOperation: OperationAndRevision;
  transformedAwaitedOperation: OperationAndRevision;
}

export interface ReceivedServerOperationWhileAwaitingOperationWithBuffer {
  type: ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION_WITH_BUFFER;
}
