import React, { useState, FunctionComponent, useEffect, useCallback, CSSProperties } from "react";
import {createUseStyles} from 'react-jss';
import clsx from 'clsx';
import { TextOperation } from "ot";
import { Editor, EditorChangeLinkedList, EditorConfiguration } from "codemirror";
import { UnControlled as CodeMirror } from 'react-codemirror2';
import { CodeMirrorAdapter } from "./codemirror-adapter";

const useStyles = createUseStyles({
  container: {
    position: "relative",
  },
  site: {
    background: "#eee",
    padding: "20px",
    "& h2": {
      margin: "0 0 16px",
    },
  },
  server: {
    width: "940px",
    height: "130px",
    position: "absolute",
    left: "0px",
    top: "0px",
  },
  client: {
    width: "460px",
    height: "230px",
    display: "flex",
    flexDirection: "column",
  },
  alice: {
    position: "absolute",
    left: "0px",
    top: "130px",
  },
  bob: {
    position: "absolute",
    right: "0px",
    top: "130px",
  },
  sockets: {
    position: "relative",
    height: "150px",
  },
  codeMirrorContainer: {
    border: "1px solid #ccc",
    flex: "1",
    "& .CodeMirror": {
      height: "100%",
    },
  },
});

type Queue<A> = A[];

interface Operation {
  textOperation: TextOperation;
  key: string;
}

interface OperationAndRevision extends Operation {
  revision: number; // non-negative integer
}

interface ServerVisualizationState {
  operations: Operation[];
  text: string;
}

enum ClientStateStatus {
  SYNCHRONIZED = "SYNCHRONIZED",
  AWAITING_ACK = "AWAITING_ACK",
  AWAITING_ACK_WITH_OPERATION = "AWAITING_ACK_WITH_OPERATION",
}

interface BaseSynchronizationState {
  serverRevision: number; // non-negative integer
}

interface SynchronizationStateSynchronized extends BaseSynchronizationState {
  status: ClientStateStatus.SYNCHRONIZED,
}

interface SynchronizationStateAwaitingAck extends BaseSynchronizationState {
  status: ClientStateStatus.AWAITING_ACK,
  expectedOperation: TextOperation;
}

interface SynchronizationStateAwaitingAckWithOperation extends BaseSynchronizationState {
  status: ClientStateStatus.AWAITING_ACK_WITH_OPERATION,
  expectedOperation: TextOperation;
  buffer: TextOperation;
}

type SynchronizationState = SynchronizationStateSynchronized | SynchronizationStateAwaitingAck | SynchronizationStateAwaitingAckWithOperation;

interface ClientAndSocketsVisualizationState {
  toServer: Queue<OperationAndRevision>;
  fromServer: Queue<Operation>;
  synchronizationState: SynchronizationState;
  text: string;
}

interface VisualizationState {
  server: ServerVisualizationState;
  alice: ClientAndSocketsVisualizationState;
  bob: ClientAndSocketsVisualizationState;
}

function receiveOperationFromClient(server: ServerVisualizationState, operation: OperationAndRevision): ({
  newServer: ServerVisualizationState,
  operationToBroadcast: Operation,
}) {
  const concurrentOperations = server.operations.slice(operation.revision);

  const transformedTextOperation = concurrentOperations.reduce((op, concurrentOp) => TextOperation.transform(op, concurrentOp.textOperation), operation.textOperation);

  const operationToBroadcast: Operation = {
    key: operation.key,
    textOperation: transformedTextOperation,
  }
  const newServer: ServerVisualizationState = {
    operations: [...server.operations, operationToBroadcast],
    text: transformedTextOperation.apply(server.text),
  }
  return { operationToBroadcast, newServer };
}

function processClientUserOperation(synchronizationState: SynchronizationState, textOperation: TextOperation, clientName: string): ({
  newSynchronizationState: SynchronizationState,
  operationsToSendToServer: OperationAndRevision[],
}) {
  switch (synchronizationState.status) {
    case ClientStateStatus.SYNCHRONIZED:
      const revision = synchronizationState.serverRevision + 1;
      return {
        newSynchronizationState: {
          status: ClientStateStatus.AWAITING_ACK,
          serverRevision: synchronizationState.serverRevision,
          expectedOperation: textOperation,
        },
        operationsToSendToServer: [{ revision, textOperation, key: `${clientName}-${revision}` }],
      };
    case ClientStateStatus.AWAITING_ACK:
      return {
        newSynchronizationState: {
          status: ClientStateStatus.AWAITING_ACK_WITH_OPERATION,
          serverRevision: synchronizationState.serverRevision,
          expectedOperation: synchronizationState.expectedOperation,
          buffer: textOperation,
        },
        operationsToSendToServer: [],
      };
    case ClientStateStatus.AWAITING_ACK_WITH_OPERATION:
      return {
        newSynchronizationState: {
          status: ClientStateStatus.AWAITING_ACK_WITH_OPERATION,
          serverRevision: synchronizationState.serverRevision,
          expectedOperation: synchronizationState.expectedOperation,
          buffer: synchronizationState.buffer.compose(textOperation),
        },
        operationsToSendToServer: [],
      };
  }
}

function clientUserOperation(client: ClientAndSocketsVisualizationState, operation: TextOperation, clientName: string): ClientAndSocketsVisualizationState {
  const { newSynchronizationState, operationsToSendToServer } = processClientUserOperation(client.synchronizationState, operation, clientName);

  return {
    synchronizationState: newSynchronizationState,
    toServer: [...client.toServer, ...operationsToSendToServer],
    fromServer: client.fromServer,
    text: operation.apply(client.text),
  };
}

const initialText = "Lorem ipsum";
const initialRevision = 0;

const initialClientAndSocketsVisualizationState: ClientAndSocketsVisualizationState = {
  toServer: [],
  fromServer: [],
  synchronizationState: {
    status: ClientStateStatus.SYNCHRONIZED,
    serverRevision: initialRevision,
  },
  text: initialText,
};

const initialVisualizationState: VisualizationState = {
  alice: initialClientAndSocketsVisualizationState,
  bob: initialClientAndSocketsVisualizationState,
  server: {
    operations: [],
    text: initialText,
  },
};

interface Lens<S,A> {
  get: (s: S) => A;
  set: (s: S, a: A) => S;
}

const aliceLens: Lens<VisualizationState, ClientAndSocketsVisualizationState> = {
  get: (globalState) => globalState.alice,
  set: (globalState, aliceState) => ({ ...globalState, alice: aliceState }),
};

const bobLens: Lens<VisualizationState, ClientAndSocketsVisualizationState> = {
  get: (globalState) => globalState.bob,
  set: (globalState, bobState) => ({ ...globalState, bob: bobState }),
};

export const Visualization = () => {
  const classes = useStyles();

  const [visualizationState, setVisualizationState] = useState<VisualizationState>(initialVisualizationState);

  const makeClientProps = (
    clientLens: Lens<VisualizationState, ClientAndSocketsVisualizationState>,
    clientName: string
  ): Pick<ClientAndSocketsVisualizationProps, "state" | "onClientOperation"> => ({
    state: clientLens.get(visualizationState),
    onClientOperation: operation => {
      const newClientState = clientUserOperation(clientLens.get(visualizationState), operation, clientName);
      setVisualizationState(clientLens.set(visualizationState, newClientState));
    },
  });

  return (
    <div className={classes.container}>
      <ServerVisualization state={visualizationState.server} />
      <ClientAndSocketsVisualization clientName="Alice" className={classes.alice} {...makeClientProps(aliceLens, "alice")} />
      <ClientAndSocketsVisualization clientName="Bob" className={classes.bob} {...makeClientProps(bobLens, "bob")} />
    </div>
  );
};

interface ServerVisualizationProps {
  state: ServerVisualizationState;
}

const ServerVisualization: FunctionComponent<ServerVisualizationProps> = (props) => {
  const classes = useStyles();

  return (
    <div className={clsx(classes.site, classes.server)}>
      <h2>Server</h2>
      <p>Doc: {props.state.text.replace(/\n/g, "\\n")}</p>
    </div>
  );
};

const useOperationStyles = createUseStyles({
  operation: {
    position: "absolute",
    zIndex: "-1",
    width: "20px",
    height: "20px",
    borderRadius: "10px",
    transform: "translate(-10px, -10px)",
    background: "#888",
    transitionProperty: "top",
    transitionDuration: "0.5s",
  },
  operationAtBottom: {
    top: "calc(100% + 20px)",
  },
});

interface OperationInSocketProps {
  operation: OperationAndRevision;
  positionTop?: string,
}

const OperationInSocket: FunctionComponent<OperationInSocketProps> = (props) => {
  const classes = useOperationStyles();

  const [initialRender, setInitialRender] = useState<boolean>(true);

  useEffect(() => {
    const timeout = setTimeout(() => {
      setInitialRender(false);
    }, 10);
    return () => {
      clearTimeout(timeout);
    };
  }, []);

  return (
    <div
      className={clsx(classes.operation, { [classes.operationAtBottom]: initialRender })}
      style={!initialRender && props.positionTop !== undefined ? { top: props.positionTop } : undefined}
    />
  );
};

const useSocketStyles = createUseStyles({
  socket: {
    height: "100%",
  },
  line: {
    position: "absolute",
    left: "-1px",
    height: "100%",
    borderLeft: "2px dashed #eee",
    zIndex: "-1",
  },
});

interface ToServerSocketProps {
  className: string;
  queue: Queue<OperationAndRevision>;
}

const ToServerSocket: FunctionComponent<ToServerSocketProps> = (props) => {
  const socketClasses = useSocketStyles();

  return (
    <div className={clsx(socketClasses.socket, props.className)}>
      <div className={socketClasses.line}></div>
      {props.queue.map((operationWithRevision, i) => (
        <OperationInSocket
          key={operationWithRevision.key}
          operation={operationWithRevision}
          positionTop={`calc(100% / ${props.queue.length + 1} * ${i + 1})`}
        />
      ))}
    </div>
  );
};

const useClientStyles = createUseStyles({
  toServerSocket: {
    position: "absolute",
    left: "100px",
    height: "100%",
  },
});

interface ClientAndSocketsVisualizationProps {
  clientName: string;
  className: string;
  state: ClientAndSocketsVisualizationState;
  onClientOperation: (operation: TextOperation) => void;
}

const editorConfiguration: EditorConfiguration = {
  lineNumbers: true,
};

const ClientAndSocketsVisualization: FunctionComponent<ClientAndSocketsVisualizationProps> = (props) => {
  const { onClientOperation } = props;
  const clientClasses = useClientStyles();
  const classes = useStyles();

  const [editor, setEditor] = useState<Editor | undefined>(undefined);

  const onChanges = useCallback((editor: Editor, changes: EditorChangeLinkedList[]) => {
    console.log("onChanges called with ", editor, changes); // TODO: remove
    const [operation, inverse] = CodeMirrorAdapter.operationFromCodeMirrorChanges(changes, editor);
    console.log("operation=", operation); // TODO
    console.log("inverse=", inverse); // TODO
    onClientOperation(operation);
  }, [onClientOperation]);

  useEffect(() => {
    if (editor !== undefined) {
      editor.on("changes", onChanges);
      return () => {
        editor.off("changes", onChanges);
      };
    }
  }, [editor, onChanges]);

  return (
    <div className={props.className}>
      <div className={classes.sockets}>
        <ToServerSocket className={clientClasses.toServerSocket} queue={props.state.toServer} />
      </div>
      <div className={clsx(classes.site, classes.client)}>
        <h2>{props.clientName}</h2>
        <CodeMirror className={classes.codeMirrorContainer} options={editorConfiguration} value={initialText} editorDidMount={setEditor} />
      </div>
    </div>
  );
};