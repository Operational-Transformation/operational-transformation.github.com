import React, { useState, FunctionComponent } from "react";
import {createUseStyles} from 'react-jss';
import clsx from 'clsx';
import { TextOperation } from "ot";
import { Editor, EditorChange, EditorConfiguration } from "codemirror";
import { Controlled as CodeMirror } from 'react-codemirror2';

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
    top: "280px",
  },
  bob: {
    position: "absolute",
    right: "0px",
    top: "280px",
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
}

enum ClientStateStatus {
  SYNCHRONIZED = "SYNCHRONIZED",
  AWAITING_ACK = "AWAITING_ACK",
  AWAITING_ACK_WITH_OPERATION = "AWAITING_ACK_WITH_OPERATION",
}

interface SynchronizationStateSynchronized {
  status: ClientStateStatus.SYNCHRONIZED,
}

interface SynchronizationStateAwaitingAck {
  status: ClientStateStatus.AWAITING_ACK,
}

interface SynchronizationStateAwaitingAckWithOperation {
  status: ClientStateStatus.AWAITING_ACK_WITH_OPERATION,
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
  }
  return { operationToBroadcast, newServer };
}

const initialText = "Lorem ipsum";

const initialClientAndSocketsVisualizationState: ClientAndSocketsVisualizationState = {
  toServer: [],
  fromServer: [],
  synchronizationState: {
    status: ClientStateStatus.SYNCHRONIZED,
  },
  text: initialText,
};

const initialVisualizationState: VisualizationState = {
  alice: initialClientAndSocketsVisualizationState,
  bob: initialClientAndSocketsVisualizationState,
  server: {
    operations: [],
  },
};

export const Visualization = () => {
  const classes = useStyles();

  const [visualizationState, setVisualizationState] = useState<VisualizationState>(initialVisualizationState);

  return (
    <div className={classes.container}>
      <ServerVisualization state={visualizationState.server} />
      <ClientAndSocketsVisualization state={visualizationState.alice} clientName="Alice" className={classes.alice} />
      <ClientAndSocketsVisualization state={visualizationState.bob} clientName="Bob" className={classes.bob} />
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
    </div>
  );
};

interface ClientAndSocketsVisualizationProps {
  clientName: string;
  className: string;
  state: ClientAndSocketsVisualizationState;
}

const editorConfiguration: EditorConfiguration = {
  lineNumbers: true,
};

const ClientAndSocketsVisualization: FunctionComponent<ClientAndSocketsVisualizationProps> = (props) => {
  const classes = useStyles();

  const onBeforeChange = (editor: Editor, data: EditorChange, value: string) => {
    console.log("onBeforeChange called with ", editor, data, value); // TODO: remove
  };
  const onChange = (editor: Editor, data: EditorChange, value: string) => {
    console.log("onChange called with ", editor, data, value); // TODO: remove
  };

  return (
    <div className={clsx(classes.site, classes.client, props.className)}>
      <h2>{props.clientName}</h2>
      <CodeMirror className={classes.codeMirrorContainer} options={editorConfiguration} value={props.state.text} onBeforeChange={onBeforeChange} onChange={onChange} />
    </div>
  );
};