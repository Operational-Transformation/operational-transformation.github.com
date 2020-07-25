import React, { useState } from "react";
import { createUseStyles } from "react-jss";
import {
  aliceLens,
  bobLens,
  ClientAndSocketsVisualizationState,
  ClientStateStatus,
  Lens,
  onClientOperation,
  onServerReceive,
  VisualizationState,
} from "./visualizationState";
import {
  ClientAndSocketsVisualization,
  ClientAndSocketsVisualizationProps,
} from "./ClientAndSocketsVisualization";
import { ServerVisualization } from "./ServerVisualization";

const useStyles = createUseStyles({
  container: {
    position: "relative",
  },
  clients: {
    display: "flex",
    flexDirection: "row",
  },
  alice: {},
  bob: {},
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

  const [visualizationState, setVisualizationState] = useState<VisualizationState>(
    initialVisualizationState,
  );

  const makeClientProps = (
    clientLens: Lens<VisualizationState, ClientAndSocketsVisualizationState>,
    clientName: string,
  ): Pick<
    ClientAndSocketsVisualizationProps,
    "state" | "onClientOperation" | "onServerReceiveClick"
  > => ({
    state: clientLens.get(visualizationState),
    onClientOperation: (operation) => {
      setVisualizationState((visualizationState) =>
        onClientOperation(visualizationState, clientLens, clientName, operation),
      );
    },
    onServerReceiveClick: () => {
      setVisualizationState((visualizationState) =>
        onServerReceive(visualizationState, clientLens),
      );
    },
  });

  return (
    <div className={classes.container}>
      <ServerVisualization state={visualizationState.server} />
      <div className={classes.clients}>
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
    </div>
  );
};
