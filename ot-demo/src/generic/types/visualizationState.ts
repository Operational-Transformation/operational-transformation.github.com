import { Operation, OperationAndRevision } from "./operation";
import { ClientLogEntry } from "./clientLog";

export type Queue<A> = A[];

export enum ClientName {
  Alice = "Alice",
  Bob = "Bob",
}

export interface ServerVisualizationState<SnapshotT, OpT> {
  operations: Operation<OpT>[];
  snapshot: SnapshotT;
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

interface SynchronizationStateAwaitingAck<OpT> {
  status: SynchronizationStateStatus.AWAITING_OPERATION;
  awaitedOperation: OperationAndRevision<OpT>;
}

interface SynchronizationStateAwaitingAckWithOperation<OpT> {
  status: SynchronizationStateStatus.AWAITING_OPERATION_WITH_BUFFER;
  awaitedOperation: OperationAndRevision<OpT>;
  buffer: OperationAndRevision<OpT>;
}

export type SynchronizationState<OpT> =
  | SynchronizationStateSynchronized
  | SynchronizationStateAwaitingAck<OpT>
  | SynchronizationStateAwaitingAckWithOperation<OpT>;

export interface ClientLogItem<OpT> {
  entry: ClientLogEntry<OpT>;
  newState: SynchronizationState<OpT>;
}

export type ClientLog<OpT> = ClientLogItem<OpT>[];

export interface ClientAndSocketsVisualizationState<SnapshotT, OpT> {
  toServer: Queue<OperationAndRevision<OpT>>;
  fromServer: Queue<OperationAndRevision<OpT>>;
  initialSynchronizationState: SynchronizationState<OpT>;
  clientLog: ClientLog<OpT>;
  snapshot: SnapshotT;
}

export interface VisualizationState<SnapshotT, OpT> {
  server: ServerVisualizationState<SnapshotT, OpT>;
  alice: ClientAndSocketsVisualizationState<SnapshotT, OpT>;
  bob: ClientAndSocketsVisualizationState<SnapshotT, OpT>;
}
