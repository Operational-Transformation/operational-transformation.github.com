import { TextOperation } from "ot";
import { Operation, OperationAndRevision } from "./types/operation";
import {
  ClientAndSocketsVisualizationState,
  ClientName,
  ServerVisualizationState,
  SynchronizationState,
  SynchronizationStateStatus,
  VisualizationState,
} from "./types/visualizationState";
import { ClientEntryType, ClientLogEntry } from "./types/clientLog";

const transformTextOperation = (
  a: TextOperation,
  b: TextOperation,
): [TextOperation, TextOperation] =>
  (TextOperation.transform(a, b) as unknown) as [TextOperation, TextOperation]; // because type definition is wrong

const transformOperation = (
  a: Operation, // server operation
  b: OperationAndRevision, // client operation
): [Operation, OperationAndRevision] => {
  const [aTextPrime, bTextPrime] = transformTextOperation(a.textOperation, b.textOperation);
  const aPrime: Operation = { textOperation: aTextPrime, meta: a.meta };
  const bPrime: OperationAndRevision = {
    textOperation: bTextPrime,
    meta: b.meta,
    revision: b.revision + 1,
  };
  return [aPrime, bPrime];
};

function receiveOperationFromClient(
  server: ServerVisualizationState,
  operation: OperationAndRevision,
): {
  newServerState: ServerVisualizationState;
  operationToBroadcast: OperationAndRevision;
} {
  const concurrentOperations = server.operations.slice(operation.revision);

  const transformedOperation = concurrentOperations.reduce<OperationAndRevision>(
    (op, concurrentOp) => transformOperation(concurrentOp, op)[1],
    operation,
  );

  const newServerState: ServerVisualizationState = {
    operations: [...server.operations, transformedOperation],
    text: transformedOperation.textOperation.apply(server.text),
  };
  return { operationToBroadcast: transformedOperation, newServerState };
}

export interface Lens<S, A> {
  get: (s: S) => A;
  set: (s: S, a: A) => S;
}

export const aliceLens: Lens<VisualizationState, ClientAndSocketsVisualizationState> = {
  get: (globalState) => globalState.alice,
  set: (globalState, aliceState) => ({ ...globalState, alice: aliceState }),
};

export const bobLens: Lens<VisualizationState, ClientAndSocketsVisualizationState> = {
  get: (globalState) => globalState.bob,
  set: (globalState, bobState) => ({ ...globalState, bob: bobState }),
};

function processClientUserOperation(
  synchronizationState: SynchronizationState,
  textOperation: TextOperation,
  clientName: ClientName,
): {
  newSynchronizationState: SynchronizationState;
  operationsToSendToServer: OperationAndRevision[];
  newClientLogEntry: ClientLogEntry;
} {
  switch (synchronizationState.status) {
    case SynchronizationStateStatus.SYNCHRONIZED: {
      const revision = synchronizationState.serverRevision;
      const meta = { key: `${clientName}-${revision}`, author: clientName };
      const operationToSendToServer = { meta, textOperation, revision };
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
      const meta = { key: `${clientName}-${revision}`, author: clientName };
      let buffer: OperationAndRevision = { textOperation, meta, revision };
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
          buffer: {
            meta: synchronizationState.buffer.meta,
            revision: synchronizationState.buffer.revision,
            textOperation: synchronizationState.buffer.textOperation.compose(textOperation),
          },
        },
        operationsToSendToServer: [],
        newClientLogEntry: {
          type: ClientEntryType.USER_EDIT_ADDED_TO_BUFFER,
          textOperation,
        },
      };
    }
  }
}

function clientUserOperation(
  client: ClientAndSocketsVisualizationState,
  operation: TextOperation,
  clientName: ClientName,
): ClientAndSocketsVisualizationState {
  const {
    newSynchronizationState,
    operationsToSendToServer,
    newClientLogEntry,
  } = processClientUserOperation(client.synchronizationState, operation, clientName);

  return {
    synchronizationState: newSynchronizationState,
    clientLog: [newClientLogEntry, ...client.clientLog],
    toServer: [...client.toServer, ...operationsToSendToServer],
    fromServer: client.fromServer,
    text: operation.apply(client.text),
  };
}

export function onClientOperation(
  visualizationState: VisualizationState,
  clientLens: Lens<VisualizationState, ClientAndSocketsVisualizationState>,
  clientName: ClientName,
  operation: TextOperation,
): VisualizationState {
  const newClientState = clientUserOperation(
    clientLens.get(visualizationState),
    operation,
    clientName,
  );
  return clientLens.set(visualizationState, newClientState);
}

function sendOperationToClient(
  client: ClientAndSocketsVisualizationState,
  operation: OperationAndRevision,
): ClientAndSocketsVisualizationState {
  return {
    ...client,
    fromServer: [...client.fromServer, operation],
  };
}

export function onServerReceive(
  visualizationState: VisualizationState,
  clientLens: Lens<VisualizationState, ClientAndSocketsVisualizationState>,
): VisualizationState {
  const clientState = clientLens.get(visualizationState);
  const [operation, ...remainingOperations] = clientState.toServer;
  const newClientState = { ...clientState, toServer: remainingOperations };
  const nextVisualizationState = clientLens.set(visualizationState, newClientState);
  const { newServerState, operationToBroadcast } = receiveOperationFromClient(
    nextVisualizationState.server,
    operation,
  );
  return {
    server: newServerState,
    alice: sendOperationToClient(nextVisualizationState.alice, operationToBroadcast),
    bob: sendOperationToClient(nextVisualizationState.bob, operationToBroadcast),
  };
}

function processOperationFromServer(
  synchronizationState: SynchronizationState,
  receivedOperation: OperationAndRevision,
): {
  newSynchronizationState: SynchronizationState;
  operationToSendToServer: OperationAndRevision | undefined;
  transformedReceivedOperationToApply: Operation | undefined;
  newClientLogEntry: ClientLogEntry;
} {
  switch (synchronizationState.status) {
    case SynchronizationStateStatus.SYNCHRONIZED: {
      const newSynchronizationState: SynchronizationState = {
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
      if (receivedOperation.meta.key === awaitedOperation.meta.key) {
        const newSynchronizationState: SynchronizationState = {
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
          receivedOperation,
          awaitedOperation,
        );
        const newSynchronizationState: SynchronizationState = {
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
      if (receivedOperation.meta.key === awaitedOperation.meta.key) {
        const newSynchronizationState: SynchronizationState = {
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
          receivedOperation,
          awaitedOperation,
        );
        const [twiceTransformedReceivedOperation, transformedBuffer] = transformOperation(
          onceTransformedReceivedOperation,
          buffer,
        );
        const newSynchronizationState: SynchronizationState = {
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

function clientReceiveOperation({
  synchronizationState,
  clientLog,
  fromServer,
  toServer,
  text,
}: ClientAndSocketsVisualizationState): {
  newClientState: ClientAndSocketsVisualizationState;
  transformedReceivedOperationToApply: Operation | undefined;
} {
  const [operation, ...remainingOperations] = fromServer;
  const {
    newSynchronizationState,
    operationToSendToServer,
    transformedReceivedOperationToApply,
    newClientLogEntry,
  } = processOperationFromServer(synchronizationState, operation);
  const newClientState: ClientAndSocketsVisualizationState = {
    synchronizationState: newSynchronizationState,
    text:
      transformedReceivedOperationToApply === undefined
        ? text
        : transformedReceivedOperationToApply.textOperation.apply(text),
    toServer:
      operationToSendToServer === undefined ? toServer : [...toServer, operationToSendToServer],
    fromServer: remainingOperations,
    clientLog: [newClientLogEntry, ...clientLog],
  };
  return { newClientState, transformedReceivedOperationToApply };
}

export function onClientReceive(
  visualizationState: VisualizationState,
  clientLens: Lens<VisualizationState, ClientAndSocketsVisualizationState>,
): {
  newState: VisualizationState;
  transformedReceivedOperationToApply: Operation | undefined;
} {
  const { newClientState, transformedReceivedOperationToApply } = clientReceiveOperation(
    clientLens.get(visualizationState),
  );
  const newState = clientLens.set(visualizationState, newClientState);
  return { newState, transformedReceivedOperationToApply };
}
