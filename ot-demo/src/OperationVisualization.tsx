import { Operation } from "./visualizationState";
import React, { FunctionComponent } from "react";
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

interface OperationProps extends React.HTMLAttributes<HTMLSpanElement> {
  operation: Operation;
}

export const OperationVisualization: FunctionComponent<OperationProps> = (props) => {
  const classes = useOperationStyles();
  const { className, ...otherProps } = props;
  return <span className={clsx(classes.operation, props.className)} {...otherProps} />;
};
