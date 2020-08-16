import { Operation } from "./visualizationState";
import React, { FunctionComponent } from "react";
import { createUseStyles } from "react-jss";
import clsx from "clsx";
import Tooltip from "@material-ui/core/Tooltip";
import { TextOperation } from "ot";
import { getClientColor, replaceInvisibleCharacters } from "./sharedStyles";

const renderOp = (op: string | number) => {
  if (typeof op === "string") {
    return (
      <span style={{ color: "#01FF70" }}>
        insert("{replaceInvisibleCharacters(op).replace(/\\/g, "\\\\").replace(/"/g, '\\"')}")
      </span>
    );
  } else if (op < 0) {
    return <span style={{ color: "#FF4136" }}>delete({-op})</span>;
  } else {
    return <span>retain({op})</span>;
  }
};

const renderTextOperation = (textOperation: TextOperation) =>
  textOperation.ops.flatMap((op, i) => {
    return [...(i > 0 ? [", "] : []), renderOp(op)];
  });

const renderOperation = (operation: Operation) => (
  <>{renderTextOperation(operation.textOperation)}</>
);

const useOperationStyles = createUseStyles({
  operation: {
    display: "inline-block",
    width: "20px",
    height: "20px",
    borderRadius: "10px",
  },
  tooltip: {
    // specificity hack
    "&$tooltip": {
      fontSize: "13px",
    },
  },
});

interface OperationProps
  extends Omit<React.HTMLAttributes<HTMLSpanElement>, "onMouseEnter" | "onMouseLeave"> {
  operation: Operation;
}

export const OperationVisualization: FunctionComponent<OperationProps> = (props) => {
  const classes = useOperationStyles();
  const { className, style, ...otherProps } = props;
  const operation = props.operation;
  console.log(operation.meta.key);
  console.log(getClientColor(operation.meta.author));

  return (
    <Tooltip
      key={operation.meta.key}
      arrow={true}
      classes={{ tooltip: classes.tooltip }}
      title={<>{renderOperation(operation)}</>}
    >
      <span
        key={operation.meta.key}
        className={clsx(classes.operation, props.className)}
        style={{ background: getClientColor(operation.meta.author), ...style }}
        {...otherProps}
      />
    </Tooltip>
  );
};
