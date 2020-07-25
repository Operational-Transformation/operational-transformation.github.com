import React, { FunctionComponent } from "react";
import { useSharedStyles } from "./sharedStyles";
import clsx from "clsx";
import StorageIcon from "@material-ui/icons/Storage";
import { ServerVisualizationState } from "./visualizationState";
import { createUseStyles } from "react-jss";

export const useServerStyles = createUseStyles({
  server: {
    width: "940px",
    height: "130px",
  },
  document: {
    whiteSpace: "pre",
    backgroundColor: "white",
  },
});

interface ServerVisualizationProps {
  state: ServerVisualizationState;
}

export const ServerVisualization: FunctionComponent<ServerVisualizationProps> = (props) => {
  const sharedClasses = useSharedStyles();
  const classes = useServerStyles();

  return (
    <div className={clsx(sharedClasses.site, classes.server)}>
      <h2>
        <StorageIcon />
        Central Server
      </h2>
      <p>
        Document: <span className={classes.document}>{props.state.text.replace(/\n/g, "↲")}</span>
      </p>
    </div>
  );
};
