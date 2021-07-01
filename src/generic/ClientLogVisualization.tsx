import React, { FunctionComponent, useCallback, useState } from "react";
import {
  ClientEntryType,
  ClientLogEntry,
  ReceivedOwnOperation,
  ReceivedOwnOperationAndSentBuffer,
  ReceivedServerOperationWhileAwaitingOperation,
  ReceivedServerOperationWhileAwaitingOperationWithBuffer,
  ReceivedServerOperationWhileSynchronized,
  UserEditAddedToBuffer,
  UserEditImmediatelySentToServer,
  UserEditStoredAsBuffer,
} from "./types/clientLog";
import { createUseStyles } from "react-jss";
import { ArrowDiagramArrowProps, makeArrowDiagram, SvgArrow } from "./ArrowDiagram";
import type { ClientLog, ClientLogItem, SynchronizationState } from "./types/visualizationState";
import { makeSynchronizationStateVisualization } from "./SynchronizationStateVisualization";
import type { ApplicationSpecificOperationComponents } from "./types/applicationSpecific";
import { makeOperationVisualization } from "./OperationVisualization";

const useStyles = createUseStyles({
  clientLog: {
    margin: "0 15px 20px",
    lineHeight: "24px",
  },
  clientLogItem: {
    transition: "margin 0.5s ease, height 0.5s ease",
    overflow: "hidden",
    position: "relative",
    "& > div": {
      position: "absolute",
      left: "0",
      right: "0",
      bottom: "0",
    },
  },
  clientLogEntry: {
    padding: "12px 20px 12px 40px",
  },
  inlineOperation: {
    margin: "0 2px",
    verticalAlign: "-4px",
  },
});

const makeClientLogItemVisualization = <OpT extends unknown>(
  applicationSpecific: ApplicationSpecificOperationComponents<OpT>,
): FunctionComponent<ClientLogItem<OpT>> => {
  const OperationVisualization = makeOperationVisualization(applicationSpecific);
  const SynchronizationStateVisualization =
    makeSynchronizationStateVisualization(OperationVisualization);
  const ArrowDiagram = makeArrowDiagram(OperationVisualization);

  const UserEditImmediatelySentToServerVisualization: FunctionComponent<
    UserEditImmediatelySentToServer<OpT>
  > = (logEntry) => {
    const classes = useStyles();

    return (
      <p>
        User edit{" "}
        <OperationVisualization
          className={classes.inlineOperation}
          operation={logEntry.operation}
        />{" "}
        was sent to the server.
      </p>
    );
  };

  const UserEditStoredAsBufferVisualization: FunctionComponent<UserEditStoredAsBuffer<OpT>> = (
    logEntry,
  ) => {
    const classes = useStyles();

    return (
      <p>
        User edit{" "}
        <OperationVisualization
          className={classes.inlineOperation}
          operation={logEntry.operation}
        />{" "}
        was stored in buffer.
      </p>
    );
  };

  const UserEditAddedToBufferVisualization: FunctionComponent<UserEditAddedToBuffer<OpT>> = (
    logEntry,
  ) => {
    // TODO: render user edit
    return <p>Added user edit to buffer.</p>;
  };

  const ReceivedOwnOperationVisualization: FunctionComponent<ReceivedOwnOperation<OpT>> = (
    logEntry,
  ) => {
    const classes = useStyles();

    return (
      <p>
        Received acknowledgment of own operation{" "}
        <OperationVisualization
          className={classes.inlineOperation}
          operation={logEntry.acknowledgedOperation}
        />
        .
      </p>
    );
  };

  const ReceivedOwnOperationAndSentBufferVisualization: FunctionComponent<
    ReceivedOwnOperationAndSentBuffer<OpT>
  > = (logEntry) => {
    const classes = useStyles();

    return (
      <p>
        Received acknowledgment of own operation{" "}
        <OperationVisualization
          className={classes.inlineOperation}
          operation={logEntry.acknowledgedOperation}
        />{" "}
        and sent buffer{" "}
        <OperationVisualization
          className={classes.inlineOperation}
          operation={logEntry.sentBuffer}
        />
        .
      </p>
    );
  };

  const ReceivedServerOperationWhileSynchronizedVisualization: FunctionComponent<
    ReceivedServerOperationWhileSynchronized<OpT>
  > = (logEntry) => {
    const classes = useStyles();

    return (
      <p>
        Received operation{" "}
        <OperationVisualization
          className={classes.inlineOperation}
          operation={logEntry.receivedOperation}
        />{" "}
        from the server and immediately applied it to the document.
      </p>
    );
  };

  const ReceivedServerOperationWhileAwaitingOperationVisualization: FunctionComponent<
    ReceivedServerOperationWhileAwaitingOperation<OpT>
  > = (logEntry) => {
    const {
      receivedOperation,
      transformedReceivedOperation,
      awaitedOperation,
      transformedAwaitedOperation,
    } = logEntry;
    const classes = useStyles();

    const topLeft = { x: 20, y: 15 };
    const topRight = { x: 125, y: 20 };
    const bottomLeft = { x: 15, y: 120 };
    const bottomRight = { x: 120, y: 125 };

    const arrows: ArrowDiagramArrowProps<OpT>[] = [
      {
        operation: awaitedOperation,
        start: topLeft,
        end: topRight,
        tooltipPlacement: "top",
      },
      {
        operation: transformedAwaitedOperation,
        start: bottomLeft,
        end: bottomRight,
        tooltipPlacement: "bottom",
      },
      {
        operation: receivedOperation,
        start: topLeft,
        end: bottomLeft,
        tooltipPlacement: "left",
      },
      {
        operation: transformedReceivedOperation,
        start: topRight,
        end: bottomRight,
        tooltipPlacement: "right",
      },
    ];

    return (
      <>
        <ArrowDiagram width={140} height={140} arrows={arrows} />
        <p style={{ marginTop: "4px" }}>
          Transformed received operation{" "}
          <OperationVisualization
            className={classes.inlineOperation}
            operation={receivedOperation}
          />{" "}
          against the awaited transformation{" "}
          <OperationVisualization
            className={classes.inlineOperation}
            operation={awaitedOperation}
          />{" "}
          resulting in{" "}
          <OperationVisualization
            className={classes.inlineOperation}
            operation={transformedReceivedOperation}
          />{" "}
          (applied to the editor) and a new awaited operation{" "}
          <OperationVisualization
            className={classes.inlineOperation}
            operation={transformedAwaitedOperation}
          />
          .
        </p>
      </>
    );
  };

  const ReceivedServerOperationWhileAwaitingOperationWithBufferVisualization: FunctionComponent<
    ReceivedServerOperationWhileAwaitingOperationWithBuffer<OpT>
  > = (logEntry) => {
    const {
      receivedOperation,
      onceTransformedReceivedOperation,
      twiceTransformedReceivedOperation,
      awaitedOperation,
      transformedAwaitedOperation,
      bufferOperation,
      transformedBufferOperation,
    } = logEntry;
    const classes = useStyles();

    const topLeft = { x: 20, y: 15 };
    const topCenter = { x: 125, y: 20 };
    const topRight = { x: 230, y: 25 };
    const bottomLeft = { x: 15, y: 120 };
    const bottomCenter = { x: 120, y: 125 };
    const bottomRight = { x: 225, y: 130 };

    const arrows: ArrowDiagramArrowProps<OpT>[] = [
      { operation: awaitedOperation, start: topLeft, end: topCenter, tooltipPlacement: "top" },
      {
        operation: transformedAwaitedOperation,
        start: bottomLeft,
        end: bottomCenter,
        tooltipPlacement: "bottom",
      },
      { operation: bufferOperation, start: topCenter, end: topRight, tooltipPlacement: "top" },
      {
        operation: transformedBufferOperation,
        start: bottomCenter,
        end: bottomRight,
        tooltipPlacement: "bottom",
      },
      {
        operation: receivedOperation,
        start: topLeft,
        end: bottomLeft,
        tooltipPlacement: "left",
      },
      {
        operation: onceTransformedReceivedOperation,
        start: topCenter,
        end: bottomCenter,
        tooltipPlacement: "bottom",
      },
      {
        operation: twiceTransformedReceivedOperation,
        start: topRight,
        end: bottomRight,
        tooltipPlacement: "right",
      },
    ];

    return (
      <>
        <ArrowDiagram width={245} height={145} arrows={arrows} />
        <p style={{ marginTop: "4px" }}>
          Transformed received operation{" "}
          <OperationVisualization
            className={classes.inlineOperation}
            operation={receivedOperation}
          />{" "}
          first against the awaited transformation{" "}
          <OperationVisualization
            className={classes.inlineOperation}
            operation={awaitedOperation}
          />{" "}
          and then against the buffer{" "}
          <OperationVisualization className={classes.inlineOperation} operation={bufferOperation} />{" "}
          resulting in{" "}
          <OperationVisualization
            className={classes.inlineOperation}
            operation={twiceTransformedReceivedOperation}
          />{" "}
          (applied to the editor), a new awaited operation{" "}
          <OperationVisualization
            className={classes.inlineOperation}
            operation={transformedAwaitedOperation}
          />{" "}
          and a new buffer{" "}
          <OperationVisualization
            className={classes.inlineOperation}
            operation={transformedBufferOperation}
          />
          .
        </p>
      </>
    );
  };

  const renderClientLogEntry = (
    clientLogEntry: ClientLogEntry<OpT>,
  ): NonNullable<React.ReactNode> => {
    switch (clientLogEntry.type) {
      case ClientEntryType.USER_EDIT_ADDED_TO_BUFFER:
        return <UserEditAddedToBufferVisualization {...clientLogEntry} />;
      case ClientEntryType.USER_EDIT_IMMEDIATELY_SENT_TO_SERVER:
        return <UserEditImmediatelySentToServerVisualization {...clientLogEntry} />;
      case ClientEntryType.USER_EDIT_STORED_AS_BUFFER:
        return <UserEditStoredAsBufferVisualization {...clientLogEntry} />;
      case ClientEntryType.RECEIVED_OWN_OPERATION:
        return <ReceivedOwnOperationVisualization {...clientLogEntry} />;
      case ClientEntryType.RECEIVED_OWN_OPERATION_AND_SENT_BUFFER:
        return <ReceivedOwnOperationAndSentBufferVisualization {...clientLogEntry} />;
      case ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_SYNCHRONIZED:
        return <ReceivedServerOperationWhileSynchronizedVisualization {...clientLogEntry} />;
      case ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION:
        return <ReceivedServerOperationWhileAwaitingOperationVisualization {...clientLogEntry} />;
      case ClientEntryType.RECEIVED_SERVER_OPERATION_WHILE_AWAITING_OPERATION_WITH_BUFFER:
        return (
          <ReceivedServerOperationWhileAwaitingOperationWithBufferVisualization
            {...clientLogEntry}
          />
        );
    }
  };

  return ({ entry, newState }) => {
    const classes = useStyles();

    const [measuredHeight, setMeasuredHeight] = useState<number | undefined>(undefined);

    const setInnerDiv = useCallback((innerDiv: HTMLDivElement | null) => {
      if (innerDiv !== null) {
        const rect = innerDiv.getBoundingClientRect();
        setMeasuredHeight(rect.height);
      }
    }, []);

    return (
      <div
        className={classes.clientLogItem}
        style={
          measuredHeight === undefined
            ? { height: "1px", margin: "-100px 0 100px" }
            : { height: `${measuredHeight}px`, margin: "0" }
        }
      >
        <div ref={setInnerDiv}>
          <svg
            xmlns="http://www.w3.org/2000/svg"
            width="40"
            height={measuredHeight ?? "0"}
            style={{ position: "absolute", zIndex: -1 }}
          >
            <SvgArrow
              start={{ x: 20, y: (measuredHeight ?? 0) - 2 }}
              end={{ x: 20, y: 50 }}
              shaftWidth={8}
              tipLength={28}
              tipWidth={20}
              color="#eee"
            />
          </svg>
          <SynchronizationStateVisualization synchronizationState={newState} />
          <div className={classes.clientLogEntry}>{renderClientLogEntry(entry)}</div>
        </div>
      </div>
    );
  };
};

interface ClientLogVisualizationProps<OpT> {
  clientLog: ClientLog<OpT>;
  initialSynchronizationState: SynchronizationState<OpT>;
}

export const makeClientLogVisualization = <OpT extends unknown>(
  applicationSpecific: ApplicationSpecificOperationComponents<OpT>,
): FunctionComponent<ClientLogVisualizationProps<OpT>> => {
  const OperationVisualization = makeOperationVisualization(applicationSpecific);
  const SynchronizationStateVisualization =
    makeSynchronizationStateVisualization(OperationVisualization);
  const ClientLogItemVisualization = makeClientLogItemVisualization(applicationSpecific);

  return ({ clientLog, initialSynchronizationState }) => {
    const classes = useStyles();

    return (
      <div className={classes.clientLog}>
        {clientLog.map(({ entry, newState }, i) => (
          <ClientLogItemVisualization
            key={`log-entry-${clientLog.length - i}`}
            entry={entry}
            newState={newState}
          />
        ))}
        <SynchronizationStateVisualization synchronizationState={initialSynchronizationState} />
      </div>
    );
  };
};
