import {TextOperation} from "../../_snowpack/pkg/ot.js";
import {CodeMirrorAdapter} from "./codemirror-adapter.js";
import {
  makeCodeMirrorComponent,
  renderSnapshot,
  replaceInvisibleCharacters
} from "./plainTextShared.js";
const renderOp = (op, key) => {
  if (typeof op === "string") {
    return /* @__PURE__ */ React.createElement("span", {
      key,
      style: {color: "#01FF70"}
    }, 'insert("', replaceInvisibleCharacters(op).replace(/\\/g, "\\\\").replace(/"/g, '\\"'), '")');
  } else if (op < 0) {
    return /* @__PURE__ */ React.createElement("span", {
      key,
      style: {color: "#FF4136"}
    }, "delete(", -op, ")");
  } else {
    return /* @__PURE__ */ React.createElement("span", {
      key
    }, "retain(", op, ")");
  }
};
const CodeMirrorComponent = makeCodeMirrorComponent(CodeMirrorAdapter.applyOperationToCodeMirror, (changes, editor) => {
  const [operation] = CodeMirrorAdapter.operationFromCodeMirrorChanges(changes, editor);
  return operation;
});
export const plainTextWithScanningOperationsComponents = {
  renderOperation(operation) {
    return operation.ops.flatMap((op, i) => {
      return [...i > 0 ? [", "] : [], renderOp(op, i)];
    });
  },
  renderSnapshot,
  EditorComponent: CodeMirrorComponent
};
export const plainTextWithScanningOperationsFunctions = {
  transform(a, b) {
    return TextOperation.transform(a, b);
  },
  compose(first, second) {
    return first.compose(second);
  },
  apply(operation, snapshot) {
    return operation.apply(snapshot);
  }
};
