import React, { FunctionComponent, useCallback, useEffect, useState } from "react";
import { createUseStyles } from "react-jss";
import clsx from "clsx";
import Tooltip from "@material-ui/core/Tooltip";
import { TextOperation } from "ot";
import { getClientColor, replaceInvisibleCharacters } from "./sharedStyles";
import { Operation } from "./types/operation";
import { useOperationHoverState } from "./OperationHoverProvider";

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

const renderOperation = (operation: Operation): JSX.Element => (
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

export type OperationTooltipPlacement = "top" | "bottom" | "left" | "right";

interface OperationProps
  extends Omit<React.HTMLAttributes<HTMLSpanElement>, "onMouseEnter" | "onMouseLeave"> {
  operation: Operation;
  tooltipPlacement?: OperationTooltipPlacement;
}

const isOneArrayPrefixOfTheOther = (xs: string[], ys: string[]) => {
  const l = Math.min(xs.length, ys.length);
  for (let i = 0; i < l; i++) {
    if (xs[i] !== ys[i]) {
      return false;
    }
  }
  return true;
};

const isRelated = (a: Operation, b: Operation): boolean => a.meta.id === b.meta.id;

const transformationPlural = (count: number): string => {
  switch (count) {
    case 1:
      return "transformation";
    case 2:
      return "two transformations";
    case 3:
      return "three transformations";
    case 4:
      return "four transformations";
    case 5:
      return "five transformations";
    case 6:
      return "six transformations";
    case 7:
      return "seven transformations";
    case 8:
      return "eight transformations";
    case 9:
      return "nine transformations";
    default:
      return "multiple transformations";
  }
};

const renderRelatedOperation = (
  operation: Operation,
  relatedOperation: Operation,
): NonNullable<React.ReactNode> => {
  if (
    isOneArrayPrefixOfTheOther(operation.transformedAgainst, relatedOperation.transformedAgainst)
  ) {
    const transformedAgainstDifference =
      operation.transformedAgainst.length - relatedOperation.transformedAgainst.length;
    if (transformedAgainstDifference < 0) {
      return `before ${transformationPlural(-transformedAgainstDifference)}`;
    }
    if (transformedAgainstDifference > 0) {
      return `after ${transformationPlural(transformedAgainstDifference)}`;
    }
    return "identical";
  }
  return "differently transformed";
};

enum SelfOpenStatus {
  Closed = "CLOSED",
  Closing = "CLOSING",
  Open = "OPEN",
}

export const OperationVisualization: FunctionComponent<OperationProps> = (props) => {
  const classes = useOperationStyles();
  const { className, style, operation, tooltipPlacement, ...otherProps } = props;

  const [tooltipContent, setTooltipContent] = useState<NonNullable<React.ReactNode>>("");
  const [selfOpenStatus, setSelfOpenStatus] = useState<SelfOpenStatus>(SelfOpenStatus.Closed);
  const [hoveredOperation, setHoveredOperation] = useOperationHoverState();

  useEffect(() => {
    if (
      selfOpenStatus === SelfOpenStatus.Closed &&
      hoveredOperation !== undefined &&
      isRelated(hoveredOperation, operation)
    ) {
      setTooltipContent(renderRelatedOperation(operation, hoveredOperation));
    } else if (selfOpenStatus === SelfOpenStatus.Closing && hoveredOperation === undefined) {
      setSelfOpenStatus(SelfOpenStatus.Closed);
    }
  }, [selfOpenStatus, hoveredOperation, operation]);

  const onOpen = useCallback(() => {
    setSelfOpenStatus(SelfOpenStatus.Open);
    setTooltipContent(renderOperation(operation));
    setHoveredOperation(operation);
  }, [operation, setHoveredOperation]);

  const onClose = useCallback(() => {
    setSelfOpenStatus(hoveredOperation ? SelfOpenStatus.Closing : SelfOpenStatus.Closed);
    setHoveredOperation(undefined);
  }, [hoveredOperation, setHoveredOperation]);

  const isRelatedOperationHovered =
    hoveredOperation !== undefined && isRelated(operation, hoveredOperation);

  return (
    <Tooltip
      key={operation.meta.id}
      arrow={true}
      classes={{ tooltip: classes.tooltip }}
      title={tooltipContent}
      open={selfOpenStatus === SelfOpenStatus.Open || isRelatedOperationHovered}
      placement={tooltipPlacement ?? "bottom"}
      onOpen={onOpen}
      onClose={onClose}
    >
      <span
        key={operation.meta.id}
        className={clsx(classes.operation, props.className)}
        style={{
          background: getClientColor(operation.meta.author),
          ...style,
        }}
        {...otherProps}
      />
    </Tooltip>
  );
};
