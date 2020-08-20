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
  a: TextOperation, // server operation
  b: OperationAndRevision, // client operation
): [TextOperation, OperationAndRevision] => {
  const [aPrime, bTextPrime] = transformTextOperation(a, b.textOperation);
  return [aPrime, { textOperation: bTextPrime, meta: b.meta, revision: b.revision + 1 }];
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
    (op, concurrentOp) => transformOperation(concurrentOp.textOperation, op)[1],
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
          status: SynchronizationStateStatus.AWAITING_ACK,
          expectedOperation: operationToSendToServer,
        },
        operationsToSendToServer: [operationToSendToServer],
        newClientLogEntry: {
          type: ClientEntryType.UserEditImmediatelySentToServer,
          operation: operationToSendToServer,
        },
      };
    }
    case SynchronizationStateStatus.AWAITING_ACK: {
      const revision = synchronizationState.expectedOperation.revision + 1;
      const meta = { key: `${clientName}-${revision}`, author: clientName };
      let buffer: OperationAndRevision = { textOperation, meta, revision };
      return {
        newSynchronizationState: {
          status: SynchronizationStateStatus.AWAITING_ACK_WITH_OPERATION,
          expectedOperation: synchronizationState.expectedOperation,
          buffer,
        },
        operationsToSendToServer: [],
        newClientLogEntry: {
          type: ClientEntryType.UserEditStoredAsBuffer,
          operation: buffer,
        },
      };
    }
    case SynchronizationStateStatus.AWAITING_ACK_WITH_OPERATION: {
      return {
        newSynchronizationState: {
          status: SynchronizationStateStatus.AWAITING_ACK_WITH_OPERATION,
          expectedOperation: synchronizationState.expectedOperation,
          buffer: {
            meta: synchronizationState.buffer.meta,
            revision: synchronizationState.buffer.revision,
            textOperation: synchronizationState.buffer.textOperation.compose(textOperation),
          },
        },
        operationsToSendToServer: [],
        newClientLogEntry: {
          type: ClientEntryType.UserEditAddedToBuffer,
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
  receivedOperation: Operation,
): {
  newSynchronizationState: SynchronizationState;
  operationToSendToServer: OperationAndRevision | undefined;
  transformedReceivedOperationToApply: TextOperation | undefined;
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
        transformedReceivedOperationToApply: receivedOperation.textOperation,
      };
    }
    case SynchronizationStateStatus.AWAITING_ACK: {
      const { expectedOperation } = synchronizationState;
      if (receivedOperation.meta.key === expectedOperation.meta.key) {
        const newSynchronizationState: SynchronizationState = {
          status: SynchronizationStateStatus.SYNCHRONIZED,
          serverRevision: expectedOperation.revision + 1,
        };
        return {
          newSynchronizationState,
          operationToSendToServer: undefined,
          transformedReceivedOperationToApply: undefined,
        };
      } else {
        const [transformedReceivedOperation, transformedExpectedOperation] = transformOperation(
          receivedOperation.textOperation,
          expectedOperation,
        );
        const newSynchronizationState: SynchronizationState = {
          status: SynchronizationStateStatus.AWAITING_ACK,
          expectedOperation: transformedExpectedOperation,
        };
        return {
          newSynchronizationState,
          operationToSendToServer: undefined,
          transformedReceivedOperationToApply: transformedReceivedOperation,
        };
      }
    }
    case SynchronizationStateStatus.AWAITING_ACK_WITH_OPERATION: {
      const { expectedOperation, buffer } = synchronizationState;
      if (receivedOperation.meta.key === expectedOperation.meta.key) {
        const newSynchronizationState: SynchronizationState = {
          status: SynchronizationStateStatus.AWAITING_ACK,
          expectedOperation: synchronizationState.buffer,
        };
        return {
          newSynchronizationState,
          operationToSendToServer: synchronizationState.buffer,
          transformedReceivedOperationToApply: undefined,
        };
      } else {
        const [
          onceTransformedReceivedTextOperation,
          transformedExpectedOperation,
        ] = transformOperation(receivedOperation.textOperation, expectedOperation);
        const [transformedReceivedOperation, transformedBuffer] = transformOperation(
          onceTransformedReceivedTextOperation,
          buffer,
        );
        const newSynchronizationState: SynchronizationState = {
          status: SynchronizationStateStatus.AWAITING_ACK_WITH_OPERATION,
          expectedOperation: transformedExpectedOperation,
          buffer: transformedBuffer,
        };
        return {
          newSynchronizationState,
          operationToSendToServer: undefined,
          transformedReceivedOperationToApply: transformedReceivedOperation,
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
  transformedReceivedOperationToApply: TextOperation | undefined;
} {
  const [operation, ...remainingOperations] = fromServer;
  const {
    newSynchronizationState,
    operationToSendToServer,
    transformedReceivedOperationToApply,
  } = processOperationFromServer(synchronizationState, operation);
  const newClientState: ClientAndSocketsVisualizationState = {
    synchronizationState: newSynchronizationState,
    text:
      transformedReceivedOperationToApply === undefined
        ? text
        : transformedReceivedOperationToApply.apply(text),
    toServer:
      operationToSendToServer === undefined ? toServer : [...toServer, operationToSendToServer],
    fromServer: remainingOperations,
    clientLog, // TODO
  };
  return { newClientState, transformedReceivedOperationToApply };
}

export function onClientReceive(
  visualizationState: VisualizationState,
  clientLens: Lens<VisualizationState, ClientAndSocketsVisualizationState>,
): {
  newState: VisualizationState;
  transformedReceivedOperationToApply: TextOperation | undefined;
} {
  const { newClientState, transformedReceivedOperationToApply } = clientReceiveOperation(
    clientLens.get(visualizationState),
  );
  const newState = clientLens.set(visualizationState, newClientState);
  return { newState, transformedReceivedOperationToApply };
}
