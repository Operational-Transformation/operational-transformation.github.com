import React, { CSSProperties, FunctionComponent, useCallback, useState } from "react";
import {
  ClientEntryType,
  ClientLog,
  ClientLogEntry,
  UserEditAddedToBuffer,
  UserEditImmediatelySentToServer,
  UserEditStoredAsBuffer,
} from "./types/clientLog";
import { OperationVisualization } from "./OperationVisualization";
import { createUseStyles } from "react-jss";

const useStyles = createUseStyles({
  clientLog: {
    margin: "20px 15px",
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

const renderClientLogEntry = (clientLogEntry: ClientLogEntry): NonNullable<React.ReactNode> => {
  switch (clientLogEntry.type) {
    case ClientEntryType.UserEditAddedToBuffer:
      return <UserEditAddedToBufferVisualization {...clientLogEntry} />;
    case ClientEntryType.UserEditImmediatelySentToServer:
      return <UserEditImmediatelySentToServerVisualization {...clientLogEntry} />;
    case ClientEntryType.UserEditStoredAsBuffer:
      return <UserEditStoredAsBufferVisualization {...clientLogEntry} />;
  }
};

const ClientLogEntryVisualization: FunctionComponent<{ clientLogEntry: ClientLogEntry }> = ({
  clientLogEntry,
}) => {
  const classes = useStyles();

  const [measuredHeight, setMeasuredHeight] = useState<number | undefined>(undefined);

  const setInnerDiv = useCallback((innerDiv: HTMLDivElement) => {
    const rect = innerDiv.getBoundingClientRect();
    console.log("rect: ", rect);
    setMeasuredHeight(rect.height);
  }, []);

  const outerDivStyles: CSSProperties = {
    transition: "margin 0.5s ease, height 0.5s ease",
  };

  return (
    <div
      style={
        measuredHeight === undefined
          ? { height: "1px", margin: "-100px 0 100px", overflow: "hidden", ...outerDivStyles }
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
