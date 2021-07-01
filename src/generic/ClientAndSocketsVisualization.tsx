import type { ClientAndSocketsVisualizationState, Queue } from "./types/visualizationState";
// @ts-ignore
import React from "react";
import { createUseStyles } from "react-jss";
import { CSSProperties, FunctionComponent, useCallback, useEffect, useRef, useState } from "react";
import clsx from "clsx";
import { getClientColor, useSharedStyles } from "./sharedStyles";
import { IconButton, Tooltip } from "@material-ui/core";
import ArrowUpward from "@material-ui/icons/ArrowUpward";
import ArrowDownward from "@material-ui/icons/ArrowDownward";
import Computer from "@material-ui/icons/Computer";
import Tablet from "@material-ui/icons/Tablet";
import { ClientName, Operation, OperationAndRevision } from "./types/operation";
import { useIsInitialRender } from "./hooks/useIsInitialRender";
import { makeClientLogVisualization } from "./ClientLogVisualization";
import type { ApplicationSpecificComponents, EditorHandle } from "./types/applicationSpecific";
import { makeOperationVisualization, OperationVisualizationComp } from "./OperationVisualization";

const useSocketOperationStyles = createUseStyles({
  operationInSocket: {
    position: "absolute",
    transform: "translate(0, -50%)",
    transitionProperty: "top",
    transitionDuration: "0.5s",
  },
});

interface OperationInSocketProps<OpT> {
  operation: OperationAndRevision<OpT>;
  initialPositionTop?: string;
  positionTop?: string;
  disableHover?: boolean;
  onTransitionEnd?: () => void;
}

const makeOperationInSocket =
  <OpT extends unknown>(
    OperationVisualization: OperationVisualizationComp<OpT>,
  ): FunctionComponent<OperationInSocketProps<OpT>> =>
  (props) => {
    const classes = useSocketOperationStyles();

    const isInitialRender = useIsInitialRender();

    const positionStyle: CSSProperties =
      isInitialRender && props.initialPositionTop !== undefined
        ? { top: props.initialPositionTop }
        : props.positionTop !== undefined
        ? { top: props.positionTop }
        : {};

    const hoverStyle: CSSProperties = props.disableHover ? { pointerEvents: "none" } : {};

    return (
      <OperationVisualization
        operation={props.operation}
        className={classes.operationInSocket}
        style={{ ...positionStyle, ...hoverStyle }}
        onTransitionEnd={props.onTransitionEnd}
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
  operations: {
    position: "relative",
    height: "100%",
    width: "20px",
    left: "-10px",
    overflowY: "hidden",
    overflowX: "visible",
  },
  receiveButton: {
    // specificity hack
    "&$receiveButton": {
      backgroundColor: "#222",
      color: "#FFDC00",
      position: "absolute",
      padding: "2px",
      transform: "translate(-50%, -50%)",
      zIndex: "20",
      "&:hover": {
        backgroundColor: "#444",
      },
      "&[class*=Mui-disabled]": {
        backgroundColor: "#ccc",
        color: "#eee",
      },
    },
  },
});

enum SocketDirection {
  UP,
  DOWN,
}

interface SocketProps<OpT> {
  direction: SocketDirection;
  tooltip: string;
  queue: Queue<OperationAndRevision<OpT>>;
  onReceiveClick: () => void;
}

const makeSocketVisualization = <OpT extends unknown>(
  OperationVisualization: OperationVisualizationComp<OpT>,
): FunctionComponent<SocketProps<OpT>> => {
  const OperationInSocket = makeOperationInSocket(OperationVisualization);

  return ({ queue, onReceiveClick, direction, tooltip }) => {
    const socketClasses = useSocketStyles();

    const queueEmpty = queue.length === 0;

    const [delayedQueue, setDelayedQueue] = useState<typeof queue>([]);

    useEffect(() => {
      setDelayedQueue((delayedQueue) => [
        ...delayedQueue.filter((operation) => !queue.includes(operation)),
        ...queue,
      ]);
    }, [queue]);

    const positionInverter = direction === SocketDirection.DOWN ? "100% -" : "";

    const receiveButton = (
      <IconButton
        className={socketClasses.receiveButton}
        onClick={onReceiveClick}
        disabled={queueEmpty}
        style={{ top: `calc(${positionInverter} 0%)` }}
      >
        {direction === SocketDirection.UP ? <ArrowUpward /> : <ArrowDownward />}
      </IconButton>
    );

    const leavingOps = delayedQueue.filter((operation) => !queue.includes(operation));

    return (
      <div className={socketClasses.socket}>
        <Tooltip title={queueEmpty ? "" : tooltip}>{receiveButton}</Tooltip>
        <div className={socketClasses.line} />
        <div className={socketClasses.operations}>
          {[
            ...leavingOps.map((operation) => (
              <OperationInSocket
                key={operation.meta.id}
                operation={operation}
                positionTop={`calc(${positionInverter} 0px)`}
                disableHover={
                  true /* prevent accidentally triggering tooltip when operation moves */
                }
                onTransitionEnd={() =>
                  setDelayedQueue((delayedQueue) => delayedQueue.filter((o) => o !== operation))
                }
              />
            )),
            ...queue.map((operation, i) => (
              <OperationInSocket
                key={operation.meta.id}
                operation={operation}
                positionTop={`calc(${positionInverter} 100% / ${queue.length + 1} * ${i + 1})`}
                initialPositionTop={`calc(${positionInverter} (100% + 20px))`}
              />
            )),
          ]}
        </div>
      </div>
    );
  };
};

const useClientStyles = createUseStyles({
  client: {
    width: "460px",
    display: "flex",
    flexDirection: "column",
    position: "relative",
    zIndex: "10",
    paddingBottom: "12px",
    borderBottom: "2px solid #fff",
  },
  sockets: {
    display: "flex",
    flexDirection: "row",
    justifyContent: "center",
    position: "relative",
    height: "150px",
  },
});

export interface ClientAndSocketsVisualizationProps<SnapshotT, OpT> {
  clientName: ClientName;
  className: string;
  state: ClientAndSocketsVisualizationState<SnapshotT, OpT>;
  onClientOperation: (operation: OpT) => void;
  onServerReceiveClick: () => void;
  onClientReceiveClick: () => Operation<OpT> | undefined;
}

export const getClientIcon = (clientName: ClientName): JSX.Element => {
  switch (clientName) {
    case ClientName.Alice:
      return <Computer />;
    case ClientName.Bob:
      return <Tablet />;
  }
};

export const makeClientAndSocketsVisualization = <SnapshotT extends unknown, OpT extends unknown>(
  applicationSpecific: ApplicationSpecificComponents<SnapshotT, OpT>,
): FunctionComponent<ClientAndSocketsVisualizationProps<SnapshotT, OpT>> => {
  const OperationVisualization = makeOperationVisualization(applicationSpecific);
  const SocketVisualization = makeSocketVisualization(OperationVisualization);
  const ClientLogVisualization = makeClientLogVisualization(applicationSpecific);
  const { EditorComponent } = applicationSpecific;

  return ({
    onClientOperation,
    onClientReceiveClick,
    onServerReceiveClick,
    state,
    clientName,
    className,
  }) => {
    const clientClasses = useClientStyles();
    const sharedClasses = useSharedStyles();

    const editorHandleRef = useRef<EditorHandle<OpT>>(null);

    const onClientReceive = useCallback(() => {
      const operationToApply = onClientReceiveClick();
      if (operationToApply !== undefined) {
        editorHandleRef.current?.applyOperation(operationToApply.base);
      }
    }, [onClientReceiveClick]);

    return (
      <div className={className}>
        <div className={clientClasses.sockets}>
          <SocketVisualization
            direction={SocketDirection.UP}
            tooltip="Receive next operation from client"
            queue={state.toServer}
            onReceiveClick={onServerReceiveClick}
          />
          <SocketVisualization
            direction={SocketDirection.DOWN}
            tooltip="Receive next operation from server"
            queue={state.fromServer}
            onReceiveClick={onClientReceive}
          />
        </div>
        <div className={clsx(sharedClasses.site, clientClasses.client)}>
          <h2 style={{ color: getClientColor(clientName) }}>
            {getClientIcon(clientName)}
            {clientName}
          </h2>
          <EditorComponent
            snapshot={state.snapshot}
            onUserChange={onClientOperation}
            ref={editorHandleRef}
          />
        </div>
        <ClientLogVisualization
          clientLog={state.clientLog}
          initialSynchronizationState={state.initialSynchronizationState}
        />
      </div>
    );
  };
};
