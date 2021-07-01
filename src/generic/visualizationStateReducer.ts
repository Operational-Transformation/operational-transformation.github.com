import {
  ClientName,
  composeOperation,
  createNewOperation,
  Operation,
  OperationAndRevision,
  transformOperation,
} from "./types/operation";
import {
  ClientAndSocketsVisualizationState,
  ServerVisualizationState,
  SynchronizationState,
  SynchronizationStateStatus,
  VisualizationState,
} from "./types/visualizationState";
import { ClientEntryType, ClientLogEntry } from "./types/clientLog";
import type {
  ApplicationSpecificFunctions,
  CompositionFunction,
  TransformationFunction,
} from "./types/applicationSpecific";

function receiveOperationFromClient<SnapshotT, OpT>(
  functions: Pick<ApplicationSpecificFunctions<SnapshotT, OpT>, "transform" | "apply">,
  server: ServerVisualizationState<SnapshotT, OpT>,
  operation: OperationAndRevision<OpT>,
): {
  newServerState: ServerVisualizationState<SnapshotT, OpT>;
  operationToBroadcast: OperationAndRevision<OpT>;
} {
  const concurrentOperations = server.operations.slice(operation.revision);

  const transformedOperation = concurrentOperations.reduce<OperationAndRevision<OpT>>(
    (op, concurrentOp) => transformOperation(functions.transform, concurrentOp, op)[1],
    operation,
  );

  const newServerState: ServerVisualizationState<SnapshotT, OpT> = {
    operations: [...server.operations, transformedOperation],
    snapshot: functions.apply(transformedOperation.base, server.snapshot),
  };
  return { operationToBroadcast: transformedOperation, newServerState };
}

export interface Lens<S, A> {
  get: (s: S) => A;
  set: (s: S, a: A) => S;
}

export const makeAliceLens = <SnapshotT, OpT>(): Lens<
  VisualizationState<SnapshotT, OpT>,
  ClientAndSocketsVisualizationState<SnapshotT, OpT>
> => ({
  get: (globalState) => globalState.alice,
  set: (globalState, aliceState) => ({ ...globalState, alice: aliceState }),
});

export const makeBobLens = <SnapshotT, OpT>(): Lens<
  VisualizationState<SnapshotT, OpT>,
  ClientAndSocketsVisualizationState<SnapshotT, OpT>
> => ({
  get: (globalState) => globalState.bob,
  set: (globalState, bobState) => ({ ...globalState, bob: bobState }),
});

function processClientUserOperation<OpT>(
  compositionFunction: CompositionFunction<OpT>,
  synchronizationState: SynchronizationState<OpT>,
  baseOperation: OpT,
  clientName: ClientName,
): {
  newSynchronizationState: SynchronizationState<OpT>;
  operationsToSendToServer: OperationAndRevision<OpT>[];
  newClientLogEntry: ClientLogEntry<OpT>;
} {
  switch (synchronizationState.status) {
    case SynchronizationStateStatus.SYNCHRONIZED: {
      const revision = synchronizationState.serverRevision;
      const operationToSendToServer: OperationAndRevision<OpT> = {
        ...createNewOperation(baseOperation, clientName),
        revision,
      };
      return {
        newSynchronizationState: {
          status: SynchronizationStateStatus.AWAITING_OPERATION,
          awaitedOperation: operationToSendToServer,
        },
        operationsToSendToServer: [operationToSendToServer],
        newClientLogEntry: {
          type: ClientEntryType.USER_EDIT_IMMEDIATELY_SENT_TO_SERVER,
          operation: operationToSendToServer,
        },
      };
    }
    case SynchronizationStateStatus.AWAITING_OPERATION: {
      const revision = synchronizationState.awaitedOperation.revision + 1;
      let buffer: OperationAndRevision<OpT> = {
        ...createNewOperation(baseOperation, clientName),
        revision,
      };
      return {
        newSynchronizationState: {
          status: SynchronizationStateStatus.AWAITING_OPERATION_WITH_BUFFER,
          awaitedOperation: synchronizationState.awaitedOperation,
          buffer,
        },
        operationsToSendToServer: [],
        newClientLogEntry: {
          type: ClientEntryType.USER_EDIT_STORED_AS_BUFFER,
          operation: buffer,
        },
      };
    }
    case SynchronizationStateStatus.AWAITING_OPERATION_WITH_BUFFER: {
      return {
        newSynchronizationState: {
          status: SynchronizationStateStatus.AWAITING_OPERATION_WITH_BUFFER,
          awaitedOperation: synchronizationState.awaitedOperation,
          buffer: composeOperation(compositionFunction, synchronizationState.buffer, baseOperation),
        },
        operationsToSendToServer: [],
        newClientLogEntry: {
          type: ClientEntryType.USER_EDIT_ADDED_TO_BUFFER,
          base: baseOperation,
        },
      };
    }
  }
}

function getLatestSynchronizationState<SnapshotT, OpT>({
  clientLog,
  initialSynchronizationState,
}: ClientAndSocketsVisualizationState<SnapshotT, OpT>) {
  return clientLog.length > 0 ? clientLog[0].newState : initialSynchronizationState;
}

function clientUserOperation<SnapshotT, OpT>(
  functions: Pick<ApplicationSpecificFunctions<SnapshotT, OpT>, "compose" | "apply">,
  clientState: ClientAndSocketsVisualizationState<SnapshotT, OpT>,
  baseOperation: OpT,
  clientName: ClientName,
): ClientAndSocketsVisualizationState<SnapshotT, OpT> {
  const { initialSynchronizationState, clientLog, toServer, fromServer, snapshot } = clientState;
  const { newSynchronizationState, operationsToSendToServer, newClientLogEntry } =
    processClientUserOperation(
      functions.compose,
      getLatestSynchronizationState(clientState),
      baseOperation,
      clientName,
    );

  return {
    clientLog: [{ entry: newClientLogEntry, newState: newSynchronizationState }, ...clientLog],
    toServer: [...toServer, ...operationsToSendToServer],
    fromServer,
    snapshot: functions.apply(baseOperation, snapshot),
    initialSynchronizationState,
  };
}

export function onClientOperation<SnapshotT, OpT>(
  functions: Pick<ApplicationSpecificFunctions<SnapshotT, OpT>, "compose" | "apply">,
  visualizationState: VisualizationState<SnapshotT, OpT>,
  clientLens: Lens<
    VisualizationState<SnapshotT, OpT>,
    ClientAndSocketsVisualizationState<SnapshotT, OpT>
  >,
  clientName: ClientName,
  baseOperation: OpT,
): VisualizationState<SnapshotT, OpT> {
  const newClientState = clientUserOperation(
    functions,
    clientLens.get(visualizationState),
    baseOperation,
    clientName,
  );
  return clientLens.set(visualizationState, newClientState);
}

function sendOperationToClient<SnapshotT, OpT>(
  client: ClientAndSocketsVisualizationState<SnapshotT, OpT>,
  operation: OperationAndRevision<OpT>,
): ClientAndSocketsVisualizationState<SnapshotT, OpT> {
  return {
    ...client,
    fromServer: [...client.fromServer, operation],
  };
}

export function onServerReceive<SnapshotT, OpT>(
  functions: Pick<ApplicationSpecificFunctions<SnapshotT, OpT>, "transform" | "apply">,
  visualizationState: VisualizationState<SnapshotT, OpT>,
  clientLens: Lens<
    VisualizationState<SnapshotT, OpT>,
    ClientAndSocketsVisualizationState<SnapshotT, OpT>
  >,
): VisualizationState<SnapshotT, OpT> {
  const clientState = clientLens.get(visualizationState);
  const [operation, ...remainingOperations] = clientState.toServer;
  const newClientState = { ...clientState, toServer: remainingOperations };
  const nextVisualizationState = clientLens.set(visualizationState, newClientState);
  const { newServerState, operationToBroadcast } = receiveOperationFromClient(
    functions,
    nextVisualizationState.server,
    operation,
  );
  return {
    server: newServerState,
    alice: sendOperationToClient(nextVisualizationState.alice, operationToBroadcast),
    bob: sendOperationToClient(nextVisualizationState.bob, operationToBroadcast),
  };
}

function processOperationFromServer<OpT>(
  transformationFunction: TransformationFunction<OpT>,
  synchronizationState: SynchronizationState<OpT>,
  receivedOperation: OperationAndRevision<OpT>,
): {
  newSynchronizationState: SynchronizationState<OpT>;
  operationToSendToServer: OperationAndRevision<OpT> | undefined;
  transformedReceivedOperationToApply: Operation<OpT> | undefined;
  newClientLogEntry: ClientLogEntry<OpT>;
} {
  switch (synchronizationState.status) {
    case SynchronizationStateStatus.SYNCHRONIZED: {
      const newSynchronizationState: SynchronizationState<OpT> = {
        status: SynchronizationStateStatus.SYNCHRONIZED,
        serverRevision: synchronizationState.serverRevision + 1,
      };
      return {
        newSynchronizationState,
        operationToSendToServer: undefined,
        transformedReceivedOperationToApply: receivedOperation,
        newClientLogEntry: {
          type: ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_SYNCHRONIZED,
          receivedOperation,
        },
      };
    }
    case SynchronizationStateStatus.AWAITING_OPERATION: {
      const { awaitedOperation } = synchronizationState;
      if (receivedOperation.meta.id === awaitedOperation.meta.id) {
        const newSynchronizationState: SynchronizationState<OpT> = {
          status: SynchronizationStateStatus.SYNCHRONIZED,
          serverRevision: awaitedOperation.revision + 1,
        };
        return {
          newSynchronizationState,
          operationToSendToServer: undefined,
          transformedReceivedOperationToApply: undefined,
          newClientLogEntry: {
            type: ClientEntryType.RECEIVED_OWN_OPERATION,
            acknowledgedOperation: receivedOperation,
          },
        };
      } else {
        const [transformedReceivedOperation, transformedAwaitedOperation] = transformOperation(
          transformationFunction,
          receivedOperation,
          awaitedOperation,
        );
        const newSynchronizationState: SynchronizationState<OpT> = {
          status: SynchronizationStateStatus.AWAITING_OPERATION,
          awaitedOperation: transformedAwaitedOperation,
        };
        return {
          newSynchronizationState,
          operationToSendToServer: undefined,
          transformedReceivedOperationToApply: transformedReceivedOperation,
          newClientLogEntry: {
            type: ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION,
            receivedOperation,
            transformedReceivedOperation: transformedReceivedOperation,
            awaitedOperation: awaitedOperation,
            transformedAwaitedOperation: transformedAwaitedOperation,
          },
        };
      }
    }
    case SynchronizationStateStatus.AWAITING_OPERATION_WITH_BUFFER: {
      const { awaitedOperation, buffer } = synchronizationState;
      if (receivedOperation.meta.id === awaitedOperation.meta.id) {
        const newSynchronizationState: SynchronizationState<OpT> = {
          status: SynchronizationStateStatus.AWAITING_OPERATION,
          awaitedOperation: synchronizationState.buffer,
        };
        return {
          newSynchronizationState,
          operationToSendToServer: synchronizationState.buffer,
          transformedReceivedOperationToApply: undefined,
          newClientLogEntry: {
            type: ClientEntryType.RECEIVED_OWN_OPERATION_AND_SENT_BUFFER,
            acknowledgedOperation: receivedOperation,
            sentBuffer: synchronizationState.buffer,
          },
        };
      } else {
        const [onceTransformedReceivedOperation, transformedAwaitedOperation] = transformOperation(
          transformationFunction,
          receivedOperation,
          awaitedOperation,
        );
        const [twiceTransformedReceivedOperation, transformedBuffer] = transformOperation(
          transformationFunction,
          onceTransformedReceivedOperation,
          buffer,
        );
        const newSynchronizationState: SynchronizationState<OpT> = {
          status: SynchronizationStateStatus.AWAITING_OPERATION_WITH_BUFFER,
          awaitedOperation: transformedAwaitedOperation,
          buffer: transformedBuffer,
        };
        return {
          newSynchronizationState,
          operationToSendToServer: undefined,
          transformedReceivedOperationToApply: twiceTransformedReceivedOperation,
          newClientLogEntry: {
            type: ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION_WITH_BUFFER,
            receivedOperation,
            onceTransformedReceivedOperation,
            twiceTransformedReceivedOperation,
            awaitedOperation,
            transformedAwaitedOperation,
            bufferOperation: buffer,
            transformedBufferOperation: transformedBuffer,
          },
        };
      }
    }
  }
}

function clientReceiveOperation<SnapshotT, OpT>(
  functions: Pick<ApplicationSpecificFunctions<SnapshotT, OpT>, "transform" | "apply">,
  clientState: ClientAndSocketsVisualizationState<SnapshotT, OpT>,
): {
  newClientState: ClientAndSocketsVisualizationState<SnapshotT, OpT>;
  transformedReceivedOperationToApply: Operation<OpT> | undefined;
} {
  const { initialSynchronizationState, clientLog, fromServer, toServer, snapshot } = clientState;
  const [operation, ...remainingOperations] = fromServer;
  const {
    newSynchronizationState,
    operationToSendToServer,
    transformedReceivedOperationToApply,
    newClientLogEntry,
  } = processOperationFromServer(
    functions.transform,
    getLatestSynchronizationState(clientState),
    operation,
  );
  const newClientState: ClientAndSocketsVisualizationState<SnapshotT, OpT> = {
    snapshot:
      transformedReceivedOperationToApply === undefined
        ? snapshot
        : functions.apply(transformedReceivedOperationToApply.base, snapshot),
    toServer:
      operationToSendToServer === undefined ? toServer : [...toServer, operationToSendToServer],
    fromServer: remainingOperations,
    clientLog: [{ entry: newClientLogEntry, newState: newSynchronizationState }, ...clientLog],
    initialSynchronizationState,
  };
  return { newClientState, transformedReceivedOperationToApply };
}

export function onClientReceive<SnapshotT, OpT>(
  functions: Pick<ApplicationSpecificFunctions<SnapshotT, OpT>, "transform" | "apply">,
  visualizationState: VisualizationState<SnapshotT, OpT>,
  clientLens: Lens<
    VisualizationState<SnapshotT, OpT>,
    ClientAndSocketsVisualizationState<SnapshotT, OpT>
  >,
): {
  newState: VisualizationState<SnapshotT, OpT>;
  transformedReceivedOperationToApply: Operation<OpT> | undefined;
} {
  const { newClientState, transformedReceivedOperationToApply } = clientReceiveOperation(
    functions,
    clientLens.get(visualizationState),
  );
  const newState = clientLens.set(visualizationState, newClientState);
  return { newState, transformedReceivedOperationToApply };
}
