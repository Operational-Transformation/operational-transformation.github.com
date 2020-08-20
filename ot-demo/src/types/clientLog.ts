import { OperationAndRevision } from "./operation";
import { TextOperation } from "ot";

export enum ClientEntryType {
  UserEditImmediatelySentToServer = "UserEditImmediatelySentToServer",
  UserEditStoredAsBuffer = "UserEditStoredAsBuffer",
  UserEditAddedToBuffer = "UserEditAddedToBuffer",
}

export type ClientLogEntry =
  | UserEditImmediatelySentToServer
  | UserEditStoredAsBuffer
  | UserEditAddedToBuffer;

export type ClientLog = ClientLogEntry[];

export interface UserEditImmediatelySentToServer {
  type: ClientEntryType.UserEditImmediatelySentToServer;
  operation: OperationAndRevision;
}

export interface UserEditStoredAsBuffer {
  type: ClientEntryType.UserEditStoredAsBuffer;
  operation: OperationAndRevision;
}

export interface UserEditAddedToBuffer {
  type: ClientEntryType.UserEditAddedToBuffer;
  textOperation: TextOperation;
}
