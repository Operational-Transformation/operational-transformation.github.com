import React, { FunctionComponent, useState } from "react";
import { createUseStyles } from "react-jss";
import {
  Lens,
  makeAliceLens,
  makeBobLens,
  onClientOperation,
  onClientReceive,
  onServerReceive,
} from "./visualizationStateReducer";
import {
  ClientAndSocketsVisualizationProps,
  makeClientAndSocketsVisualization,
} from "./ClientAndSocketsVisualization";
import { makeServerVisualization } from "./ServerVisualization";
import {
  ClientAndSocketsVisualizationState,
  SynchronizationStateStatus,
  VisualizationState,
} from "./types/visualizationState";
import { ClientName, Operation } from "./types/operation";
import type {
  ApplicationSpecificComponents,
  ApplicationSpecificFunctions,
} from "./types/applicationSpecific";

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

const makeInitialVisualizationState = <SnapshotT extends unknown, OpT extends unknown>(
  initialSnapshot: SnapshotT,
): VisualizationState<SnapshotT, OpT> => {
  const initialRevision = 0;

  const initialClientAndSocketsVisualizationState: ClientAndSocketsVisualizationState<
    SnapshotT,
    OpT
  > = {
    toServer: [],
    fromServer: [],
    initialSynchronizationState: {
      status: SynchronizationStateStatus.SYNCHRONIZED,
      serverRevision: initialRevision,
    },
    clientLog: [],
    snapshot: initialSnapshot,
  };

  return {
    alice: initialClientAndSocketsVisualizationState,
    bob: initialClientAndSocketsVisualizationState,
    server: {
      operations: [],
      snapshot: initialSnapshot,
    },
  };
};

export const makeVisualization = <SnapshotT, OpT>(
  applicationSpecificFunctions: ApplicationSpecificFunctions<SnapshotT, OpT>,
  applicationSpecificComponents: ApplicationSpecificComponents<SnapshotT, OpT>,
): FunctionComponent<{ initialSnapshot: SnapshotT }> => {
  const ServerVisualization = makeServerVisualization(applicationSpecificComponents);
  const ClientAndSocketsVisualization = makeClientAndSocketsVisualization(
    applicationSpecificComponents,
  );
  const aliceLens = makeAliceLens<SnapshotT, OpT>();
  const bobLens = makeBobLens<SnapshotT, OpT>();

  return ({ initialSnapshot }) => {
    const classes = useStyles();

    const [visualizationState, setVisualizationState] = useState<
      VisualizationState<SnapshotT, OpT>
    >(() => makeInitialVisualizationState(initialSnapshot));

    const makeClientProps = (
      clientLens: Lens<
        VisualizationState<SnapshotT, OpT>,
        ClientAndSocketsVisualizationState<SnapshotT, OpT>
      >,
      clientName: ClientName,
    ): Pick<
      ClientAndSocketsVisualizationProps<SnapshotT, OpT>,
      "state" | "onClientOperation" | "onServerReceiveClick" | "onClientReceiveClick"
    > => ({
      state: clientLens.get(visualizationState),
      onClientOperation: (operation) => {
        setVisualizationState((visualizationState) =>
          onClientOperation(
            applicationSpecificFunctions,
            visualizationState,
            clientLens,
            clientName,
            operation,
          ),
        );
      },
      onServerReceiveClick: () => {
        setVisualizationState((visualizationState) =>
          onServerReceive(applicationSpecificFunctions, visualizationState, clientLens),
        );
      },
      onClientReceiveClick: () => {
        let transformedReceivedOperation: Operation<OpT> | undefined = undefined;
        setVisualizationState((visualizationState) => {
          const { newState, transformedReceivedOperationToApply } = onClientReceive(
            applicationSpecificFunctions,
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
            clientName={ClientName.Alice}
            className={classes.alice}
            {...makeClientProps(aliceLens, ClientName.Alice)}
          />
          <ClientAndSocketsVisualization
            clientName={ClientName.Bob}
            className={classes.bob}
            {...makeClientProps(bobLens, ClientName.Bob)}
          />
        </div>
      </div>
    );
  };
};
