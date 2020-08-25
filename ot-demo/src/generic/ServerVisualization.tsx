import React, { FunctionComponent } from "react";
import { useSharedStyles } from "./sharedStyles";
import clsx from "clsx";
import StorageIcon from "@material-ui/icons/Storage";
import { createUseStyles } from "react-jss";
import { ServerVisualizationState } from "./types/visualizationState";
import { ApplicationSpecificComponents } from "./types/applicationSpecific";
import { makeOperationVisualization } from "./OperationVisualization";

export const useServerStyles = createUseStyles({
  server: {
    width: "940px",
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

interface ServerVisualizationProps<SnapshotT, OpT> {
  state: ServerVisualizationState<SnapshotT, OpT>;
}

export const makeServerVisualization = <SnapshotT extends unknown, OpT extends unknown>(
  applicationSpecificComponents: ApplicationSpecificComponents<SnapshotT, OpT>,
): FunctionComponent<ServerVisualizationProps<SnapshotT, OpT>> => {
  const OperationVisualization = makeOperationVisualization<OpT>(applicationSpecificComponents);

  return ({ state }) => {
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
            <td>{applicationSpecificComponents.renderSnapshot(state.snapshot)}</td>
          </tr>
          <tr>
            <th>Operations:</th>
            <td>
              {state.operations.length === 0 ? <>none yet</> : <></>}
              {state.operations.map((operation) => (
                <OperationVisualization
                  key={operation.meta.id}
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
};
