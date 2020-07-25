import React, { FunctionComponent, useState } from "react";
import { createUseStyles } from "react-jss";
import clsx from "clsx";
import {
  aliceLens,
  bobLens,
  ClientAndSocketsVisualizationState,
  ClientStateStatus,
  clientUserOperation,
  Lens,
  ServerVisualizationState,
  VisualizationState,
} from "./visualizationState";
import {
  ClientAndSocketsVisualization,
  ClientAndSocketsVisualizationProps,
} from "./ClientAndSocketsVisualization";
import { useSharedStyles } from "./sharedStyles";

const useStyles = createUseStyles({
  container: {
    position: "relative",
  },
  server: {
    width: "940px",
    height: "130px",
    position: "absolute",
    left: "0px",
    top: "0px",
  },
  alice: {
    position: "absolute",
    left: "0px",
    top: "130px",
  },
  bob: {
    position: "absolute",
    right: "0px",
    top: "130px",
  },
});

const initialText = "Lorem ipsum";
const initialRevision = 0;

const initialClientAndSocketsVisualizationState: ClientAndSocketsVisualizationState = {
  toServer: [],
  fromServer: [],
  synchronizationState: {
    status: ClientStateStatus.SYNCHRONIZED,
    serverRevision: initialRevision,
  },
  text: initialText,
};

const initialVisualizationState: VisualizationState = {
  alice: initialClientAndSocketsVisualizationState,
  bob: initialClientAndSocketsVisualizationState,
  server: {
    operations: [],
    text: initialText,
  },
};

export const Visualization = () => {
  const classes = useStyles();

  const [visualizationState, setVisualizationState] = useState<
    VisualizationState
  >(initialVisualizationState);

  const makeClientProps = (
    clientLens: Lens<VisualizationState, ClientAndSocketsVisualizationState>,
    clientName: string,
  ): Pick<
    ClientAndSocketsVisualizationProps,
    "state" | "onClientOperation"
  > => ({
    state: clientLens.get(visualizationState),
    onClientOperation: (operation) => {
      const newClientState = clientUserOperation(
        clientLens.get(visualizationState),
        operation,
        clientName,
      );
      setVisualizationState(clientLens.set(visualizationState, newClientState));
    },
  });

  return (
    <div className={classes.container}>
      <ServerVisualization state={visualizationState.server} />
      <ClientAndSocketsVisualization
        clientName="Alice"
        className={classes.alice}
        {...makeClientProps(aliceLens, "alice")}
      />
      <ClientAndSocketsVisualization
        clientName="Bob"
        className={classes.bob}
        {...makeClientProps(bobLens, "bob")}
      />
    </div>
  );
};

interface ServerVisualizationProps {
  state: ServerVisualizationState;
}

const ServerVisualization: FunctionComponent<ServerVisualizationProps> = (
  props,
) => {
  const classes = useStyles();
  const sharedClasses = useSharedStyles();

  return (
    <div className={clsx(sharedClasses.site, classes.server)}>
      <h2>Server</h2>
      <p>Doc: {props.state.text.replace(/\n/g, "\\n")}</p>
    </div>
  );
};
