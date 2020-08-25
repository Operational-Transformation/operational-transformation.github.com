import React from "react";
import { makeVisualization } from "./generic/Visualization";
import { createUseStyles } from "react-jss";
import { OperationHoverProvider } from "./generic/OperationHoverProvider";
import {
  initialText,
  plainTextWithScanningOperationsComponents,
  plainTextWithScanningOperationsFunctions,
} from "./applicationSpecific/plainTextWithScanningOperations";

const useStyles = createUseStyles({
  wrapper: {
    width: "940px",
    margin: "20px auto",
  },
  header: {
    margin: "0 0 40px",
    textAlign: "center",
  },
});

const Visualization = makeVisualization(
  plainTextWithScanningOperationsFunctions,
  plainTextWithScanningOperationsComponents,
);

function App() {
  const classes = useStyles();

  return (
    <OperationHoverProvider>
      <div className={classes.wrapper}>
        <h1 className={classes.header}>Visualization of OT with a central server</h1>
        <Visualization initialSnapshot={initialText} />
      </div>
    </OperationHoverProvider>
  );
}

export default App;
