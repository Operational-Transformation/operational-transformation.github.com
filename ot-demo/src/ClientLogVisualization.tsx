import React, { CSSProperties, FunctionComponent, useCallback, useState } from "react";
import {
  ClientEntryType,
  ClientLog,
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
import { OperationVisualization } from "./OperationVisualization";
import { createUseStyles } from "react-jss";
import { ArrowDiagram } from "./ArrowDiagram";

const useStyles = createUseStyles({
  clientLog: {
    margin: "20px 15px",
    lineHeight: "24px",
  },
  clientLogEntry: {
    background: "#f7f7f7",
    padding: "10px 20px",
  },
  inlineOperation: {
    margin: "0 2px",
    verticalAlign: "-4px",
  },
});

const UserEditImmediatelySentToServerVisualization: FunctionComponent<UserEditImmediatelySentToServer> = (
  logEntry,
) => {
  const classes = useStyles();

  return (
    <p>
      User edit{" "}
      <OperationVisualization className={classes.inlineOperation} operation={logEntry.operation} />{" "}
      was sent to the server.
    </p>
  );
};

const UserEditStoredAsBufferVisualization: FunctionComponent<UserEditStoredAsBuffer> = (
  logEntry,
) => {
  const classes = useStyles();

  return (
    <p>
      User edit{" "}
      <OperationVisualization className={classes.inlineOperation} operation={logEntry.operation} />{" "}
      was stored in buffer.
    </p>
  );
};

const UserEditAddedToBufferVisualization: FunctionComponent<UserEditAddedToBuffer> = (logEntry) => {
  // TODO: render user edit
  return <p>Added user edit to buffer.</p>;
};

const ReceivedOwnOperationVisualization: FunctionComponent<ReceivedOwnOperation> = (logEntry) => {
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

const ReceivedOwnOperationAndSentBufferVisualization: FunctionComponent<ReceivedOwnOperationAndSentBuffer> = (
  logEntry,
) => {
  const classes = useStyles();

  return (
    <p>
      Received acknowledgment of own operation{" "}
      <OperationVisualization
        className={classes.inlineOperation}
        operation={logEntry.acknowledgedOperation}
      />{" "}
      and sent buffer{" "}
      <OperationVisualization className={classes.inlineOperation} operation={logEntry.sentBuffer} />
      .
    </p>
  );
};

const ReceivedServerOperationWhileSynchronizedVisualization: FunctionComponent<ReceivedServerOperationWhileSynchronized> = (
  logEntry,
) => {
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

const ReceivedServerOperationWhileAwaitingOperationVisualization: FunctionComponent<ReceivedServerOperationWhileAwaitingOperation> = (
  logEntry,
) => {
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

  const arrows = [
    { operation: awaitedOperation, start: topLeft, end: topRight },
    { operation: transformedAwaitedOperation, start: bottomLeft, end: bottomRight },
    { operation: receivedOperation, start: topLeft, end: bottomLeft },
    { operation: transformedReceivedOperation, start: topRight, end: bottomRight },
  ];

  return (
    <>
      <ArrowDiagram width={140} height={140} arrows={arrows} />
      <p style={{ marginTop: "4px" }}>
        Transformed received operation{" "}
        <OperationVisualization className={classes.inlineOperation} operation={receivedOperation} />{" "}
        against the awaited transformation{" "}
        <OperationVisualization className={classes.inlineOperation} operation={awaitedOperation} />{" "}
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

const ReceivedServerOperationWhileAwaitingOperationWithBufferVisualization: FunctionComponent<ReceivedServerOperationWhileAwaitingOperationWithBuffer> = (
  logEntry,
) => {
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

  const arrows = [
    { operation: awaitedOperation, start: topLeft, end: topCenter },
    { operation: transformedAwaitedOperation, start: bottomLeft, end: bottomCenter },
    { operation: bufferOperation, start: topCenter, end: topRight },
    { operation: transformedBufferOperation, start: bottomCenter, end: bottomRight },
    { operation: receivedOperation, start: topLeft, end: bottomLeft },
    { operation: onceTransformedReceivedOperation, start: topCenter, end: bottomCenter },
    { operation: twiceTransformedReceivedOperation, start: topRight, end: bottomRight },
  ];

  return (
    <>
      <ArrowDiagram width={245} height={145} arrows={arrows} />
      <p style={{ marginTop: "4px" }}>
        Transformed received operation{" "}
        <OperationVisualization className={classes.inlineOperation} operation={receivedOperation} />{" "}
        first against the awaited transformation{" "}
        <OperationVisualization className={classes.inlineOperation} operation={awaitedOperation} />{" "}
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

const renderClientLogEntry = (clientLogEntry: ClientLogEntry): NonNullable<React.ReactNode> => {
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
        <ReceivedServerOperationWhileAwaitingOperationWithBufferVisualization {...clientLogEntry} />
      );
  }
};

const ClientLogEntryVisualization: FunctionComponent<{ clientLogEntry: ClientLogEntry }> = ({
  clientLogEntry,
}) => {
  const classes = useStyles();

  const [measuredHeight, setMeasuredHeight] = useState<number | undefined>(undefined);

  const setInnerDiv = useCallback((innerDiv: HTMLDivElement) => {
    const rect = innerDiv.getBoundingClientRect();
    setMeasuredHeight(rect.height);
  }, []);

  const outerDivStyles: CSSProperties = {
    transition: "margin 0.5s ease, height 0.5s ease",
    overflow: "hidden",
  };

  return (
    <div
      style={
        measuredHeight === undefined
          ? { height: "1px", margin: "-100px 0 100px", ...outerDivStyles }
          : { height: `${measuredHeight}px`, margin: "0 0 10px", ...outerDivStyles }
      }
    >
      <div ref={setInnerDiv} className={classes.clientLogEntry}>
        {renderClientLogEntry(clientLogEntry)}
      </div>
    </div>
  );
};

interface ClientLogVisualizationProps {
  clientLog: ClientLog;
}

export const ClientLogVisualization: FunctionComponent<ClientLogVisualizationProps> = ({
  clientLog,
}) => {
  const classes = useStyles();

  return (
    <div className={classes.clientLog}>
      {clientLog.map((clientLogEntry, i) => (
        <ClientLogEntryVisualization
          key={`log-entry-${clientLog.length - i}`}
          clientLogEntry={clientLogEntry}
        />
      ))}
    </div>
  );
};
