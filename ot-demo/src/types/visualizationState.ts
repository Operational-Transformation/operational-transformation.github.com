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
  AWAITING_ACK = "AWAITING_ACK",
  AWAITING_ACK_WITH_OPERATION = "AWAITING_ACK_WITH_OPERATION",
}

interface SynchronizationStateSynchronized {
  status: SynchronizationStateStatus.SYNCHRONIZED;
  serverRevision: number; // non-negative integer
}

interface SynchronizationStateAwaitingAck {
  status: SynchronizationStateStatus.AWAITING_ACK;
  expectedOperation: OperationAndRevision;
}

interface SynchronizationStateAwaitingAckWithOperation {
  status: SynchronizationStateStatus.AWAITING_ACK_WITH_OPERATION;
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
