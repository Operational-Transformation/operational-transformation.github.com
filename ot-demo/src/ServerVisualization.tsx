import React, { FunctionComponent } from "react";
import { replaceInvisibleCharacters, useSharedStyles } from "./sharedStyles";
import clsx from "clsx";
import StorageIcon from "@material-ui/icons/Storage";
import { createUseStyles } from "react-jss";
import { ServerVisualizationState } from "./types/visualizationState";
import { OperationVisualization } from "./OperationVisualization";

export const useServerStyles = createUseStyles({
  server: {
    width: "940px",
  },
  document: {
    whiteSpace: "pre",
    backgroundColor: "white",
    fontFamily: "monospace",
  },
  operationInLog: {
    verticalAlign: "-4px",
    marginRight: "4px",
  },
  stateTable: {
    lineHeight: "32px",
    "& th": {
      textAlign: "left",
      fontWeight: "normal",
      color: "#666",
      paddingRight: "8px",
    },
  },
});

interface ServerVisualizationProps {
  state: ServerVisualizationState;
}

export const ServerVisualization: FunctionComponent<ServerVisualizationProps> = ({ state }) => {
  const sharedClasses = useSharedStyles();
  const classes = useServerStyles();

  return (
    <div className={clsx(sharedClasses.site, classes.server)}>
      <h2>
        <StorageIcon />
        Central Server
      </h2>
      <table className={classes.stateTable}>
        <tr>
          <th>Document:</th>
          <td>
            <span className={classes.document}>{replaceInvisibleCharacters(state.text)}</span>
          </td>
        </tr>
        <tr>
          <th>Operations:</th>
          <td>
            {state.operations.length === 0 ? <>none yet</> : <></>}
            {state.operations.map((operation) => (
              <OperationVisualization
                key={operation.meta.key}
                operation={operation}
                className={classes.operationInLog}
              />
            ))}
          </td>
        </tr>
      </table>
    </div>
  );
};
