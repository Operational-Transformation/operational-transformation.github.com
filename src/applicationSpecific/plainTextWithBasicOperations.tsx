import type {
  ApplicationSpecificComponents,
  ApplicationSpecificFunctions,
} from "../generic/types/applicationSpecific";
import { TextOperation } from "ot";
import {
  makeCodeMirrorComponent,
  renderSnapshot,
  replaceInvisibleCharacters,
} from "./plainTextShared";
import { CodeMirrorAdapter } from "./codemirror-adapter";
import type { Editor } from "codemirror";
// @ts-ignore
import React from "react";

enum BasicTextOperationType {
  Insert = "INSERT",
  Delete = "DELETE",
}

interface InsertTextOperation {
  type: BasicTextOperationType.Insert;
  position: number;
  content: string;
}

interface DeleteTextOperation {
  type: BasicTextOperationType.Delete;
  position: number;
  length: number;
}

type BasicTextOperation = InsertTextOperation | DeleteTextOperation;

type AggregatedBasicTextOperation = BasicTextOperation[];

const makeInsertOperation = (position: number, content: string): InsertTextOperation => ({
  type: BasicTextOperationType.Insert,
  position,
  content,
});

const makeDeleteOperation = (position: number, length: number): [] | [DeleteTextOperation] =>
  length === 0
    ? []
    : [
        {
          type: BasicTextOperationType.Delete,
          position,
          length,
        },
      ];

function textOperationToAggregatedBasicTextOperation(
  textOperation: TextOperation,
): AggregatedBasicTextOperation {
  const aggregatedBasicTextOperation: AggregatedBasicTextOperation = [];
  let pos = 0;
  for (let op of textOperation.ops) {
    if (typeof op === "string") {
      aggregatedBasicTextOperation.push(makeInsertOperation(pos, op));
      pos += op.length;
    } else {
      if (op < 0) {
        aggregatedBasicTextOperation.push(...makeDeleteOperation(pos, -op));
      } else {
        pos += op;
      }
    }
  }
  return aggregatedBasicTextOperation;
}

function aggregatedBasicTextOperationToTextOperation(
  aggregatedBasicTextOperation: AggregatedBasicTextOperation,
  textLength: number,
): TextOperation {
  let textOperation = new TextOperation().retain(textLength);
  for (let basicTextOperation of aggregatedBasicTextOperation) {
    switch (basicTextOperation.type) {
      case BasicTextOperationType.Insert:
        textOperation = textOperation.compose(
          new TextOperation()
            .retain(basicTextOperation.position)
            .insert(basicTextOperation.content)
            .retain(textLength - basicTextOperation.position),
        );
        textLength += basicTextOperation.content.length;
        break;
      case BasicTextOperationType.Delete:
        textOperation = textOperation.compose(
          new TextOperation()
            .retain(basicTextOperation.position)
            .delete(basicTextOperation.length)
            .retain(textLength - basicTextOperation.position - basicTextOperation.length),
        );
        textLength -= basicTextOperation.length;
        break;
    }
  }
  return textOperation;
}

const CodeMirrorComponent = makeCodeMirrorComponent<AggregatedBasicTextOperation>(
  (operation: AggregatedBasicTextOperation, editor: Editor) => {
    const textLength = editor.getDoc().getValue().length; // TODO: can we implement this more efficiently?
    CodeMirrorAdapter.applyOperationToCodeMirror(
      aggregatedBasicTextOperationToTextOperation(operation, textLength),
      editor,
    );
  },
  (changes, editor) => {
    const [operation] = CodeMirrorAdapter.operationFromCodeMirrorChanges(changes, editor);
    return textOperationToAggregatedBasicTextOperation(operation);
  },
);

const renderBasicOperation = (basicTextOperation: BasicTextOperation): string => {
  switch (basicTextOperation.type) {
    case BasicTextOperationType.Insert:
      return `Insert(pos=${basicTextOperation.position}, content="${replaceInvisibleCharacters(
        basicTextOperation.content,
      )
        .replace(/\\/g, "\\\\")
        .replace(/"/g, '\\"')}")`;
    case BasicTextOperationType.Delete:
      return `Delete(pos=${basicTextOperation.position}, length=${basicTextOperation.length})`;
  }
};

export const plainTextWithBasicOperationsComponents: ApplicationSpecificComponents<
  string,
  AggregatedBasicTextOperation
> = {
  renderOperation(operation: AggregatedBasicTextOperation): React.ReactNode {
    return operation.map((op, i) => <p key={i}>{renderBasicOperation(op)}</p>);
  },
  renderSnapshot,
  EditorComponent: CodeMirrorComponent,
};

function transformInsertAgainstDeleteTextOperation(
  a: InsertTextOperation,
  b: DeleteTextOperation,
): [InsertTextOperation, DeleteTextOperation[]] {
  if (a.position <= b.position) {
    return [a, makeDeleteOperation(b.position + a.content.length, b.length)];
  }
  if (b.position + b.length <= a.position) {
    return [makeInsertOperation(a.position - b.length, a.content), [b]];
  }
  // remaining case: insert in middle of deleted text
  const deletedLengthBeforeInsertion = a.position - b.position;
  return [
    makeInsertOperation(b.position, a.content),
    // delete around newly inserted text:
    [
      ...makeDeleteOperation(b.position, deletedLengthBeforeInsertion),
      ...makeDeleteOperation(
        b.position + a.content.length,
        b.length - deletedLengthBeforeInsertion,
      ),
    ],
  ];
}

function transformBasicTextOperation(
  a: BasicTextOperation,
  b: BasicTextOperation,
): [BasicTextOperation[], BasicTextOperation[]] {
  if (a.type === BasicTextOperationType.Insert) {
    if (b.type === BasicTextOperationType.Insert) {
      // case: both insert
      if (a.position <= b.position) {
        return [[a], [makeInsertOperation(b.position + a.content.length, b.content)]];
      } else {
        return [[makeInsertOperation(a.position + b.content.length, a.content)], [b]];
      }
    } else {
      // case: a insert, b delete
      const [aPrime, bPrime] = transformInsertAgainstDeleteTextOperation(a, b);
      return [[aPrime], bPrime];
    }
  } else {
    if (b.type === BasicTextOperationType.Insert) {
      // case: a delete, b insert
      const [bPrime, aPrime] = transformInsertAgainstDeleteTextOperation(b, a);
      return [aPrime, [bPrime]];
    } else {
      // case: both delete
      if (a.position + a.length < b.position) {
        return [[a], makeDeleteOperation(b.position - a.length, b.length)];
      }
      if (b.position + b.length < a.position) {
        return [makeDeleteOperation(a.position - b.length, a.length), [b]];
      }
      // overlapping
      const minPosition = Math.min(a.position, b.position);
      const maxEndPosition = Math.max(a.position + a.length, b.position + b.length);
      const totalLength = maxEndPosition - minPosition;
      return [
        makeDeleteOperation(minPosition, totalLength - b.length),
        makeDeleteOperation(minPosition, totalLength - a.length),
      ];
    }
  }
}

function transformSingleAgainstList(
  a: BasicTextOperation,
  bs: BasicTextOperation[],
): [BasicTextOperation[], BasicTextOperation[]] {
  if (bs.length === 0) {
    return [[a], bs];
  }
  const [headB, ...tailBs] = bs;
  const [aPrimes, headBPrimes] = transformBasicTextOperation(a, headB);
  const [aPrimesPrimes, tailBPrimes] = transformListAgainstList(aPrimes, tailBs);
  return [aPrimesPrimes, [...headBPrimes, ...tailBPrimes]];
}

function transformListAgainstList(
  as: BasicTextOperation[],
  bs: BasicTextOperation[],
): [BasicTextOperation[], BasicTextOperation[]] {
  // console.log("transformListAgainstList AS", as);
  // console.log("transformListAgainstList BS", bs);
  let currBs = bs;
  const asPrime = as.flatMap((a) => {
    const [aPrimes, bsPrime] = transformSingleAgainstList(a, currBs);
    currBs = bsPrime;
    return aPrimes;
  });
  return [asPrime, currBs];
}

export const plainTextWithBasicOperationsFunctions: ApplicationSpecificFunctions<
  string,
  AggregatedBasicTextOperation
> = {
  transform: transformListAgainstList,
  compose(first: AggregatedBasicTextOperation, second: AggregatedBasicTextOperation) {
    return [...first, ...second];
  },
  apply(operation: AggregatedBasicTextOperation, snapshot: string) {
    return aggregatedBasicTextOperationToTextOperation(operation, snapshot.length).apply(snapshot);
  },
};
