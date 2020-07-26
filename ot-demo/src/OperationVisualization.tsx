import { Operation } from "./visualizationState";
import React, { CSSProperties, FunctionComponent } from "react";
import { createUseStyles } from "react-jss";
import clsx from "clsx";

const useOperationStyles = createUseStyles({
  operation: {
    display: "inline-block",
    width: "20px",
    height: "20px",
    borderRadius: "10px",
    background: "#888",
  },
});

interface OperationProps {
  operation: Operation;
  className?: string;
  style?: CSSProperties;
}

export const OperationVisualization: FunctionComponent<OperationProps> = (props) => {
  const classes = useOperationStyles();
  return <div className={clsx(classes.operation, props.className)} style={props.style} />;
};
