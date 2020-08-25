import {
  ApplicationSpecificComponents,
  ApplicationSpecificFunctions,
  EditorHandle,
  EditorProps,
} from "../generic/types/applicationSpecific";
import { TextOperation } from "ot";
import React, {
  forwardRef,
  ForwardRefExoticComponent,
  PropsWithoutRef,
  RefAttributes,
  useCallback,
  useEffect,
  useImperativeHandle,
  useRef,
  useState,
} from "react";
import { createUseStyles } from "react-jss";
import { Editor, EditorChangeLinkedList, EditorConfiguration } from "codemirror";
import "cm-show-invisibles";
import "codemirror/lib/codemirror.css";
import "codemirror/theme/material.css";
import { CodeMirrorAdapter } from "./codemirror-adapter";
import { UnControlled as CodeMirror } from "react-codemirror2";

const replaceInvisibleCharacters = (str: string): string =>
  str.replace(/\n/g, "¬").replace(/ /g, "·");

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

declare module "codemirror" {
  interface EditorConfiguration {
    showInvisibles: true; // provided by addon 'cm-show-invisibles'
  }
}

const editorConfiguration: EditorConfiguration = {
  lineNumbers: true,
  showInvisibles: true,
};

const useCodeMirrorStyles = createUseStyles({
  codeMirrorContainer: {
    border: "1px solid #ccc",
    flex: "1",
    "& .CodeMirror": {
      height: "150px",
    },
  },
});

const CodeMirrorComponent: ForwardRefExoticComponent<
  PropsWithoutRef<EditorProps<string, TextOperation>> & RefAttributes<EditorHandle<TextOperation>>
> = forwardRef<EditorHandle<TextOperation>, EditorProps<string, TextOperation>>(
  ({ snapshot, onUserChange }, ref) => {
    const codeMirrorClasses = useCodeMirrorStyles();

    const [initialText] = useState(() => snapshot);

    const [editor, setEditor] = useState<Editor | undefined>(undefined);

    const applyingOperationFromServerRef = useRef<boolean>(false);

    const onChanges = useCallback(
      (editor: Editor, changes: EditorChangeLinkedList[]) => {
        if (!applyingOperationFromServerRef.current) {
          const [operation] = CodeMirrorAdapter.operationFromCodeMirrorChanges(changes, editor);
          onUserChange(operation);
        }
      },
      [onUserChange, applyingOperationFromServerRef],
    );

    useEffect(() => {
      if (editor !== undefined) {
        editor.on("changes", onChanges);
        return () => {
          editor.off("changes", onChanges);
        };
      }
    }, [editor, onChanges]);

    useImperativeHandle(ref, () => ({
      applyOperation(textOperation) {
        if (editor !== undefined) {
          applyingOperationFromServerRef.current = true;
          CodeMirrorAdapter.applyOperationToCodeMirror(textOperation, editor);
          applyingOperationFromServerRef.current = false;
        }
      },
    }));

    return (
      <CodeMirror
        className={codeMirrorClasses.codeMirrorContainer}
        options={editorConfiguration}
        value={initialText}
        editorDidMount={setEditor}
      />
    );
  },
);

export const plainTextWithScanningOperationsComponents: ApplicationSpecificComponents<
  string,
  TextOperation
> = {
  renderOperation(operation: TextOperation): React.ReactNode {
    return operation.ops.flatMap((op, i) => {
      return [...(i > 0 ? [", "] : []), renderOp(op)];
    });
  },
  renderSnapshot(snapshot: string): React.ReactNode {
    return (
      <span
        style={{
          whiteSpace: "pre",
          backgroundColor: "white",
          fontFamily: "monospace",
        }}
      >
        {replaceInvisibleCharacters(snapshot)}
      </span>
    );
  },
  EditorComponent: CodeMirrorComponent,
};

export const plainTextWithScanningOperationsFunctions: ApplicationSpecificFunctions<
  string,
  TextOperation
> = {
  transform(a: TextOperation, b: TextOperation): [TextOperation, TextOperation] {
    return (TextOperation.transform(a, b) as unknown) as [TextOperation, TextOperation]; // because type definition is wrong
  },
  compose(first: TextOperation, second: TextOperation) {
    return first.compose(second);
  },
  apply(operation: TextOperation, snapshot: string) {
    return operation.apply(snapshot);
  },
};

export const initialText = "Lorem ipsum";
