import { Operation, OperationAndRevision } from "./operation";
import { ClientLog } from "./clientLog";

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
  expectedOperation: OperationAndRevision;
}

interface SynchronizationStateAwaitingAckWithOperation {
  status: SynchronizationStateStatus.AWAITING_OPERATION_WITH_BUFFER;
  expectedOperation: OperationAndRevision;
  buffer: OperationAndRevision;
}

export type SynchronizationState =
  | SynchronizationStateSynchronized
  | SynchronizationStateAwaitingAck
  | SynchronizationStateAwaitingAckWithOperation;

export interface ClientAndSocketsVisualizationState {
  toServer: Queue<OperationAndRevision>;
  fromServer: Queue<OperationAndRevision>;
  synchronizationState: SynchronizationState;
  clientLog: ClientLog;
  text: string;
}

export interface VisualizationState {
  server: ServerVisualizationState;
  alice: ClientAndSocketsVisualizationState;
  bob: ClientAndSocketsVisualizationState;
}
