// @ts-ignore
import React from "react";
import { makeVisualization } from "./generic/Visualization";
import { createUseStyles } from "react-jss";
import { OperationHoverProvider } from "./generic/OperationHoverProvider";
import { initialText } from "./applicationSpecific/plainTextShared";
import {
  plainTextWithBasicOperationsComponents,
  plainTextWithBasicOperationsFunctions,
} from "./applicationSpecific/plainTextWithBasicOperations";

const useStyles = createUseStyles({
  wrapper: {
    width: "940px",
    margin: "20px auto",
  },
  header: {
    margin: "0 0 40px",
    textAlign: "center",
    "& abbr": {
      textDecorationStyle: "dotted",
      textDecorationColor: "#aaa",
      textDecorationThickness: "2px",
    },
  },
});

// const Visualization = makeVisualization(
//   plainTextWithScanningOperationsFunctions,
//   plainTextWithScanningOperationsComponents,
// );

const Visualization = makeVisualization(
  plainTextWithBasicOperationsFunctions,
  plainTextWithBasicOperationsComponents,
);

function App() {
  const classes = useStyles();

  return (
    <OperationHoverProvider>
      <div className={classes.wrapper}>
        <h1 className={classes.header}>
          Visualization of <abbr title="Operational Transformation">OT</abbr> with a central server
        </h1>
        <Visualization initialSnapshot={initialText} />
      </div>
    </OperationHoverProvider>
  );
}

export default App;
