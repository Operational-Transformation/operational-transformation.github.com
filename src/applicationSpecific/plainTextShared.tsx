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
import type { EditorHandle, EditorProps } from "../generic/types/applicationSpecific";
import type { Editor, EditorChangeLinkedList, EditorConfiguration } from "codemirror";
import { UnControlled as CodeMirror } from "react-codemirror2";
import { createUseStyles } from "react-jss";

import "cm-show-invisibles";
import "codemirror/lib/codemirror.css";
import "codemirror/theme/material.css";

export const replaceInvisibleCharacters = (str: string): string =>
  str.replace(/\n/g, "¬").replace(/ /g, "·");

const useCodeMirrorStyles = createUseStyles({
  codeMirrorContainer: {
    border: "1px solid #ccc",
    flex: "1",
    "& .CodeMirror": {
      height: "150px",
    },
  },
});

declare module "codemirror" {
  interface EditorConfiguration {
    showInvisibles: true; // provided by addon 'cm-show-invisibles'
  }
}

const editorConfiguration: EditorConfiguration = {
  lineNumbers: true,
  showInvisibles: true,
};

export const makeCodeMirrorComponent = <OpT extends unknown>(
  applyOperationToCodeMirror: (operation: OpT, editor: Editor) => void,
  operationFromCodeMirrorChanges: (changes: EditorChangeLinkedList[], editor: Editor) => OpT,
): ForwardRefExoticComponent<
  PropsWithoutRef<EditorProps<string, OpT>> & RefAttributes<EditorHandle<OpT>>
> =>
  forwardRef<EditorHandle<OpT>, EditorProps<string, OpT>>(({ snapshot, onUserChange }, ref) => {
    const codeMirrorClasses = useCodeMirrorStyles();

    const [initialText] = useState(() => snapshot);

    const [editor, setEditor] = useState<Editor | undefined>(undefined);

    const applyingOperationFromServerRef = useRef<boolean>(false);

    const onChanges = useCallback(
      (editor: Editor, changes: EditorChangeLinkedList[]) => {
        if (!applyingOperationFromServerRef.current) {
          onUserChange(operationFromCodeMirrorChanges(changes, editor));
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
          applyOperationToCodeMirror(textOperation, editor);
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
  });

export const renderSnapshot = (snapshot: string): React.ReactNode => (
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

export const initialText = "Lorem ipsum";
