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
import { start } from "repl";
import { getClientColor } from "./sharedStyles";

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

const SvgArrow: FunctionComponent<{
  start: { x: number; y: number };
  end: { x: number; y: number };
  shaftWidth: number;
  tipLength: number;
  tipWidth: number;
  color: string;
}> = ({ start, end, shaftWidth, tipLength, tipWidth, color }) => {
  const diffX = end.x - start.x;
  const diffY = end.y - start.y;
  const length = Math.sqrt(diffX * diffX + diffY * diffY);
  const tipToShaftLength = (tipWidth - shaftWidth) / 2;
  const shaftLength = length - tipLength;

  return (
    <path
      fill={color}
      transform={`matrix(${diffX / length}, ${diffY / length}, ${-diffY / length}, ${
        diffX / length
      }, ${start.x}, ${start.y})`}
      d={`m ${length},0 -${tipLength},-${
        tipWidth / 2
      } 0,${tipToShaftLength} -${shaftLength},0 0,${shaftWidth} ${shaftLength},0 0,${tipToShaftLength} z`}
    />
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
  const arrowStyle = {
    shaftWidth: 10,
    tipLength: 24,
    tipWidth: 20,
    // color: "#ddd",
  };

  const opStyle = (x: number, y: number): CSSProperties => ({
    position: "absolute",
    left: `${x - 10}px`,
    top: `${y - 10}px`,
  });

  const receivedOperationColor = getClientColor(receivedOperation.meta.author);
  const awaitedOperationColor = getClientColor(awaitedOperation.meta.author);

  return (
    <>
      <div style={{ position: "relative", width: "140px", height: "140px", margin: "0 auto 4px" }}>
        <svg
          xmlns="http://www.w3.org/2000/svg"
          width={140}
          height={140}
          style={{ position: "absolute", opacity: "0.2" }}
        >
          <SvgArrow
            start={{ x: 30, y: 20 }}
            end={{ x: 115, y: 20 }}
            {...arrowStyle}
            color={receivedOperationColor}
          />
          <SvgArrow
            start={{ x: 20, y: 30 }}
            end={{ x: 20, y: 115 }}
            {...arrowStyle}
            color={awaitedOperationColor}
          />
          <SvgArrow
            start={{ x: 30, y: 120 }}
            end={{ x: 115, y: 120 }}
            {...arrowStyle}
            color={receivedOperationColor}
          />
          <SvgArrow
            start={{ x: 120, y: 30 }}
            end={{ x: 120, y: 115 }}
            {...arrowStyle}
            color={awaitedOperationColor}
          />
        </svg>
        <OperationVisualization operation={receivedOperation} style={opStyle(68, 20)} />
        <OperationVisualization operation={transformedReceivedOperation} style={opStyle(68, 120)} />
        <OperationVisualization operation={awaitedOperation} style={opStyle(20, 68)} />
        <OperationVisualization operation={transformedAwaitedOperation} style={opStyle(120, 68)} />
      </div>
      <p>
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
  const classes = useStyles();

  return <p>TODO: ReceivedServerOperationWhileAwaitingOperationWithBufferVisualization</p>;
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
  console.log(clientLog.length);

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
