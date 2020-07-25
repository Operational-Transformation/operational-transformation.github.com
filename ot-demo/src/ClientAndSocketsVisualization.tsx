import {
  ClientAndSocketsVisualizationState,
  OperationAndRevision,
  Queue,
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
import Computer from "@material-ui/icons/Computer";

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
  positionTop?: string;
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
      className={clsx(classes.operation, {
        [classes.operationAtBottom]: initialRender,
      })}
      style={
        !initialRender && props.positionTop !== undefined ? { top: props.positionTop } : undefined
      }
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

interface ToServerSocketProps {
  className: string;
  queue: Queue<OperationAndRevision>;
  onReceiveClick: () => void;
}

const ToServerSocket: FunctionComponent<ToServerSocketProps> = (props) => {
  const socketClasses = useSocketStyles();

  const queueEmpty = props.queue.length === 0;

  const receiveButton = (
    <IconButton
      className={socketClasses.receiveButton}
      onClick={props.onReceiveClick}
      disabled={queueEmpty}
    >
      <ArrowUpward />
    </IconButton>
  );

  return (
    <div className={clsx(socketClasses.socket, props.className)}>
      {queueEmpty ? (
        receiveButton
      ) : (
        <Tooltip title="Receive next operation from client">{receiveButton}</Tooltip>
      )}
      <div className={socketClasses.line} />
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
  client: {
    width: "460px",
    height: "230px",
    display: "flex",
    flexDirection: "column",
  },
  toServerSocket: {
    position: "absolute",
    left: "100px",
    height: "100%",
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
        <ToServerSocket
          className={clientClasses.toServerSocket}
          queue={props.state.toServer}
          onReceiveClick={props.onServerReceiveClick}
        />
      </div>
      <div className={clsx(sharedClasses.site, clientClasses.client)}>
        <h2>
          <Computer />
          {props.clientName}
        </h2>
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
