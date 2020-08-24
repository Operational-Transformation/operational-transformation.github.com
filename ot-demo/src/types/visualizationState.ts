import { Operation, OperationAndRevision } from "./operation";
import { ClientLogEntry } from "./clientLog";

export type Queue<A> = A[];

export enum ClientName {
  Alice = "Alice",
  Bob = "Bob",
}

export interface ServerVisualizationState {
  operations: Operation[];
  text: string;
}

export enum SynchronizationStateStatus {
  SYNCHRONIZED = "SYNCHRONIZED",
  AWAITING_OPERATION = "AWAITING_OPERATION",
  AWAITING_OPERATION_WITH_BUFFER = "AWAITING_OPERATION_WITH_BUFFER",
}

interface SynchronizationStateSynchronized {
  status: SynchronizationStateStatus.SYNCHRONIZED;
  serverRevision: number; // non-negative integer
}

interface SynchronizationStateAwaitingAck {
  status: SynchronizationStateStatus.AWAITING_OPERATION;
  awaitedOperation: OperationAndRevision;
}

interface SynchronizationStateAwaitingAckWithOperation {
  status: SynchronizationStateStatus.AWAITING_OPERATION_WITH_BUFFER;
  awaitedOperation: OperationAndRevision;
  buffer: OperationAndRevision;
}

export type SynchronizationState =
  | SynchronizationStateSynchronized
  | SynchronizationStateAwaitingAck
  | SynchronizationStateAwaitingAckWithOperation;

export interface ClientLogItem {
  entry: ClientLogEntry;
  newState: SynchronizationState;
}

export type ClientLog = ClientLogItem[];

export interface ClientAndSocketsVisualizationState {
  toServer: Queue<OperationAndRevision>;
  fromServer: Queue<OperationAndRevision>;
  initialSynchronizationState: SynchronizationState;
  clientLog: ClientLog;
  text: string;
}

export interface VisualizationState {
  server: ServerVisualizationState;
  alice: ClientAndSocketsVisualizationState;
  bob: ClientAndSocketsVisualizationState;
}
