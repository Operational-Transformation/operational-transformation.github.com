import React, { useState } from "react";
import { createUseStyles } from "react-jss";
import {
  aliceLens,
  bobLens,
  ClientAndSocketsVisualizationState,
  SynchronizationStateStatus,
  Lens,
  onClientOperation,
  onServerReceive,
  VisualizationState,
  onClientReceive,
} from "./visualizationState";
import {
  ClientAndSocketsVisualization,
  ClientAndSocketsVisualizationProps,
} from "./ClientAndSocketsVisualization";
import { ServerVisualization } from "./ServerVisualization";
import { TextOperation } from "ot";

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
    status: SynchronizationStateStatus.SYNCHRONIZED,
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
    "state" | "onClientOperation" | "onServerReceiveClick" | "onClientReceiveClick"
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
    onClientReceiveClick: () => {
      let transformedReceivedOperation: TextOperation | undefined = undefined;
      setVisualizationState((visualizationState) => {
        const { newState, transformedReceivedOperationToApply } = onClientReceive(
          visualizationState,
          clientLens,
        );
        transformedReceivedOperation = transformedReceivedOperationToApply;
        return newState;
      });
      return transformedReceivedOperation;
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
