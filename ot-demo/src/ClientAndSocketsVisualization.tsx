import {
  ClientAndSocketsVisualizationState,
  OperationAndRevision,
  Queue,
  SynchronizationState,
  SynchronizationStateStatus,
} from "./visualizationState";
import { TextOperation } from "ot";
import { createUseStyles } from "react-jss";
import React, { FunctionComponent, useCallback, useEffect, useState } from "react";
import { Editor, EditorChangeLinkedList, EditorConfiguration } from "codemirror";
import { CodeMirrorAdapter } from "./codemirror-adapter";
import clsx from "clsx";
import { UnControlled as CodeMirror } from "react-codemirror2";
import { useSharedStyles } from "./sharedStyles";
import IconButton from "@material-ui/core/IconButton";
import Tooltip from "@material-ui/core/Tooltip";
import ArrowUpward from "@material-ui/icons/ArrowUpward";
import ArrowDownward from "@material-ui/icons/ArrowDownward";
import Computer from "@material-ui/icons/Computer";
import { OperationVisualization } from "./OperationVisualization";

const useSocketOperationStyles = createUseStyles({
  operationInSocket: {
    position: "absolute",
    transform: "translate(-50%, -50%)",
    zIndex: "-1",
    transitionProperty: "top",
    transitionDuration: "0.5s",
  },
});

interface OperationInSocketProps {
  operation: OperationAndRevision;
  initialPositionTop: string;
  positionTop?: string;
}

const OperationInSocket: FunctionComponent<OperationInSocketProps> = (props) => {
  const classes = useSocketOperationStyles();

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
    <OperationVisualization
      operation={props.operation}
      className={classes.operationInSocket}
      style={
        initialRender
          ? { top: props.initialPositionTop }
          : props.positionTop !== undefined
          ? { top: props.positionTop }
          : undefined
      }
    />
  );
};

const useSocketStyles = createUseStyles({
  socket: {
    position: "relative",
    margin: "0 40px",
    height: "100%",
  },
  line: {
    position: "absolute",
    left: "-1px",
    height: "100%",
    borderLeft: "2px dashed #eee",
    zIndex: "-1",
  },
  receiveButton: {
    // specificity hack
    "&$receiveButton": {
      backgroundColor: "#7FDBFF",
      position: "absolute",
      padding: "2px",
      transform: "translate(-50%, -50%)",
      "&:hover": {
        backgroundColor: "#7faaff",
      },
      "&[class*=Mui-disabled]": {
        backgroundColor: "#ddd",
      },
    },
  },
});

enum SocketDirection {
  UP,
  DOWN,
}

interface SocketProps {
  direction: SocketDirection;
  tooltip: string;
  queue: Queue<OperationAndRevision>;
  onReceiveClick: () => void;
}

const Socket: FunctionComponent<SocketProps> = (props) => {
  const socketClasses = useSocketStyles();

  const queueEmpty = props.queue.length === 0;

  const positionInverter = props.direction === SocketDirection.DOWN ? "100% -" : "";

  const receiveButton = (
    <IconButton
      className={socketClasses.receiveButton}
      onClick={props.onReceiveClick}
      disabled={queueEmpty}
      style={{ top: `calc(${positionInverter} 0%)` }}
    >
      {props.direction === SocketDirection.UP ? <ArrowUpward /> : <ArrowDownward />}
    </IconButton>
  );

  return (
    <div className={socketClasses.socket}>
      {queueEmpty ? receiveButton : <Tooltip title={props.tooltip}>{receiveButton}</Tooltip>}
      <div className={socketClasses.line} />
      {props.queue.map((operationWithRevision, i) => (
        <OperationInSocket
          key={operationWithRevision.key}
          operation={operationWithRevision}
          positionTop={`calc(${positionInverter} 100% / ${props.queue.length + 1} * ${i + 1})`}
          initialPositionTop={`calc(${positionInverter} (100% + 20px))`}
        />
      ))}
    </div>
  );
};

const useClientStyles = createUseStyles({
  client: {
    width: "460px",
    height: "230px",
    display: "flex",
    flexDirection: "column",
  },
  sockets: {
    display: "flex",
    flexDirection: "row",
    justifyContent: "center",
    position: "relative",
    height: "150px",
  },
  synchronizationState: {
    lineHeight: "24px",
    margin: "0 0 12px",
  },
  synchronizationStateOperation: {
    margin: "0 2px",
    verticalAlign: "middle",
  },
  codeMirrorContainer: {
    border: "1px solid #ccc",
    flex: "1",
    "& .CodeMirror": {
      height: "100%",
    },
  },
});

const SynchronizationStateVisualization: FunctionComponent<{
  synchronizationState: SynchronizationState;
}> = ({ synchronizationState }) => {
  const clientClasses = useClientStyles();

  switch (synchronizationState.status) {
    case SynchronizationStateStatus.SYNCHRONIZED:
      return <p className={clientClasses.synchronizationState}>State: Synchronized</p>;
    case SynchronizationStateStatus.AWAITING_ACK:
      return (
        <p className={clientClasses.synchronizationState}>
          State: Awaiting operation{" "}
          <OperationVisualization
            operation={synchronizationState.expectedOperation}
            className={clientClasses.synchronizationStateOperation}
          />
        </p>
      );
    case SynchronizationStateStatus.AWAITING_ACK_WITH_OPERATION:
      return (
        <p className={clientClasses.synchronizationState}>
          State: Awaiting operation{" "}
          <OperationVisualization
            operation={synchronizationState.expectedOperation}
            className={clientClasses.synchronizationStateOperation}
          />{" "}
          with buffer{" "}
          <OperationVisualization
            operation={synchronizationState.buffer}
            className={clientClasses.synchronizationStateOperation}
          />
        </p>
      );
  }
};

export interface ClientAndSocketsVisualizationProps {
  clientName: string;
  className: string;
  state: ClientAndSocketsVisualizationState;
  onClientOperation: (operation: TextOperation) => void;
  onServerReceiveClick: () => void;
}

const editorConfiguration: EditorConfiguration = {
  lineNumbers: true,
};

export const ClientAndSocketsVisualization: FunctionComponent<ClientAndSocketsVisualizationProps> = (
  props,
) => {
  const { onClientOperation } = props;
  const clientClasses = useClientStyles();
  const sharedClasses = useSharedStyles();

  const [initialText] = useState(() => props.state.text);

  const [editor, setEditor] = useState<Editor | undefined>(undefined);

  const onChanges = useCallback(
    (editor: Editor, changes: EditorChangeLinkedList[]) => {
      console.log("onChanges called with ", editor, changes); // TODO: remove
      const [operation, inverse] = CodeMirrorAdapter.operationFromCodeMirrorChanges(
        changes,
        editor,
      );
      console.log("operation=", operation); // TODO
      console.log("inverse=", inverse); // TODO
      onClientOperation(operation);
    },
    [onClientOperation],
  );

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
      <div className={clientClasses.sockets}>
        <Socket
          direction={SocketDirection.UP}
          tooltip="Receive next operation from client"
          queue={props.state.toServer}
          onReceiveClick={props.onServerReceiveClick}
        />
        <Socket
          direction={SocketDirection.DOWN}
          tooltip="Receive next operation from server"
          queue={props.state.fromServer}
          onReceiveClick={() => {
            alert("To Client Socket Receive Click");
            /* TODO */
          }}
        />
      </div>
      <div className={clsx(sharedClasses.site, clientClasses.client)}>
        <h2>
          <Computer />
          {props.clientName}
        </h2>
        <SynchronizationStateVisualization
          synchronizationState={props.state.synchronizationState}
        />
        <CodeMirror
          className={clientClasses.codeMirrorContainer}
          options={editorConfiguration}
          value={initialText}
          editorDidMount={setEditor}
        />
      </div>
    </div>
  );
};
