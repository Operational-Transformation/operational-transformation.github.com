// @ts-ignore
import React from "react";
import type { FunctionComponent } from "react";
import { SynchronizationState, SynchronizationStateStatus } from "./types/visualizationState";
import { createUseStyles } from "react-jss";
import type { OperationVisualizationComp } from "./OperationVisualization";

const useStyles = createUseStyles({
  synchronizationState: {
    lineHeight: "24px",
    background: "#eee",
    padding: "12px 20px",
  },
  synchronizationStateOperation: {
    margin: "0 2px",
    verticalAlign: "-4px",
  },
  stateLabel: {
    color: "#666",
  },
});

type SynchronizationStateVisualization<OpT> = FunctionComponent<{
  synchronizationState: SynchronizationState<OpT>;
}>;

export const makeSynchronizationStateVisualization =
  <OpT extends unknown>(
    OperationVisualization: OperationVisualizationComp<OpT>,
  ): SynchronizationStateVisualization<OpT> =>
  ({ synchronizationState }) => {
    const clientClasses = useStyles();

    const stateLabel = <span className={clientClasses.stateLabel}>State:</span>;

    switch (synchronizationState.status) {
      case SynchronizationStateStatus.SYNCHRONIZED:
        return (
          <p className={clientClasses.synchronizationState}>
            {stateLabel} Synchronized at server revision {synchronizationState.serverRevision}
          </p>
        );
      case SynchronizationStateStatus.AWAITING_OPERATION:
        return (
          <p className={clientClasses.synchronizationState}>
            {stateLabel} Awaiting operation{" "}
            <OperationVisualization
              operation={synchronizationState.awaitedOperation}
              className={clientClasses.synchronizationStateOperation}
            />
          </p>
        );
      case SynchronizationStateStatus.AWAITING_OPERATION_WITH_BUFFER:
        return (
          <p className={clientClasses.synchronizationState}>
            {stateLabel} Awaiting operation{" "}
            <OperationVisualization
              operation={synchronizationState.awaitedOperation}
              className={clientClasses.synchronizationStateOperation}
            />{" "}
            with buffer{" "}
            <OperationVisualization
              operation={synchronizationState.buffer}
              className={clientClasses.synchronizationStateOperation}
            />
          </p>
        );
    }
  };
