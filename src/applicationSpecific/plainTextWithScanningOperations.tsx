import type {
  ApplicationSpecificComponents,
  ApplicationSpecificFunctions,
} from "../generic/types/applicationSpecific";
import { TextOperation } from "ot";
import { CodeMirrorAdapter } from "./codemirror-adapter";
import {
  makeCodeMirrorComponent,
  renderSnapshot,
  replaceInvisibleCharacters,
} from "./plainTextShared";

const renderOp = (op: string | number, key: string | number) => {
  if (typeof op === "string") {
    return (
      <span key={key} style={{ color: "#01FF70" }}>
        insert("{replaceInvisibleCharacters(op).replace(/\\/g, "\\\\").replace(/"/g, '\\"')}")
      </span>
    );
  } else if (op < 0) {
    return (
      <span key={key} style={{ color: "#FF4136" }}>
        delete({-op})
      </span>
    );
  } else {
    return <span key={key}>retain({op})</span>;
  }
};

const CodeMirrorComponent = makeCodeMirrorComponent<TextOperation>(
  CodeMirrorAdapter.applyOperationToCodeMirror,
  (changes, editor) => {
    const [operation] = CodeMirrorAdapter.operationFromCodeMirrorChanges(changes, editor);
    return operation;
  },
);

export const plainTextWithScanningOperationsComponents: ApplicationSpecificComponents<
  string,
  TextOperation
> = {
  renderOperation(operation: TextOperation): React.ReactNode {
    return operation.ops.flatMap((op, i) => {
      return [...(i > 0 ? [", "] : []), renderOp(op, i)];
    });
  },
  renderSnapshot,
  EditorComponent: CodeMirrorComponent,
};

export const plainTextWithScanningOperationsFunctions: ApplicationSpecificFunctions<
  string,
  TextOperation
> = {
  transform(a: TextOperation, b: TextOperation): [TextOperation, TextOperation] {
    return TextOperation.transform(a, b) as unknown as [TextOperation, TextOperation]; // because type definition is wrong
  },
  compose(first: TextOperation, second: TextOperation) {
    return first.compose(second);
  },
  apply(operation: TextOperation, snapshot: string) {
    return operation.apply(snapshot);
  },
};
