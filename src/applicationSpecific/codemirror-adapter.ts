import { TextOperation } from "ot";
import type { Doc, Position, Editor, EditorChangeLinkedList } from "codemirror";

function cmpPos(a: Position, b: Position) {
  if (a.line < b.line) {
    return -1;
  }
  if (a.line > b.line) {
    return 1;
  }
  if (a.ch < b.ch) {
    return -1;
  }
  if (a.ch > b.ch) {
    return 1;
  }
  return 0;
}
function posLe(a: Position, b: Position) {
  return cmpPos(a, b) <= 0;
}

function codemirrorDocLength(doc: Doc) {
  return doc.indexFromPos({ line: doc.lastLine(), ch: 0 }) + doc.getLine(doc.lastLine()).length;
}

export class CodeMirrorAdapter {
  // Converts a CodeMirror change array (as obtained from the 'changes' event
  // in CodeMirror v4) or single change or linked list of changes (as returned
  // by the 'change' event in CodeMirror prior to version 4) into a
  // TextOperation and its inverse and returns them as a two-element array.
  static operationFromCodeMirrorChanges(
    changes: EditorChangeLinkedList[],
    doc: Doc,
  ): [TextOperation, TextOperation] {
    // Approach: Replay the changes, beginning with the most recent one, and
    // construct the operation and its inverse. We have to convert the position
    // in the pre-change coordinate system to an index. We have a method to
    // convert a position in the coordinate system after all changes to an index,
    // namely CodeMirror's `indexFromPos` method. We can use the information of
    // a single change object to convert a post-change coordinate system to a
    // pre-change coordinate system. We can now proceed inductively to get a
    // pre-change coordinate system for all changes in the linked list.
    // A disadvantage of this approach is its complexity `O(n^2)` in the length
    // of the linked list of changes.

    let docEndLength = codemirrorDocLength(doc);
    let operation = new TextOperation().retain(docEndLength);
    let inverse = new TextOperation().retain(docEndLength);

    let indexFromPos = (pos: Position) => doc.indexFromPos(pos);

    const last = <T>(arr: T[]) => arr[arr.length - 1];

    const sumLengths = (strArr: string[]) => {
      if (strArr.length === 0) {
        return 0;
      }
      var sum = 0;
      for (var i = 0; i < strArr.length; i++) {
        sum += strArr[i].length;
      }
      return sum + strArr.length - 1;
    };

    function updateIndexFromPos(
      indexFromPos: (pos: Position) => number,
      change: EditorChangeLinkedList,
    ): (pos: Position) => number {
      return function (pos: Position) {
        if (posLe(pos, change.from)) {
          return indexFromPos(pos);
        }
        if (posLe(change.to, pos)) {
          return (
            indexFromPos({
              line: pos.line + change.text.length - 1 - (change.to.line - change.from.line),
              ch:
                change.to.line < pos.line
                  ? pos.ch
                  : change.text.length <= 1
                  ? pos.ch - (change.to.ch - change.from.ch) + sumLengths(change.text)
                  : pos.ch - change.to.ch + last(change.text).length,
            }) +
            sumLengths(change.removed ?? []) -
            sumLengths(change.text)
          );
        }
        if (change.from.line === pos.line) {
          return indexFromPos(change.from) + pos.ch - change.from.ch;
        }
        return (
          indexFromPos(change.from) +
          sumLengths((change.removed ?? []).slice(0, pos.line - change.from.line)) +
          1 +
          pos.ch
        );
      };
    }

    for (var i = changes.length - 1; i >= 0; i--) {
      var change = changes[i];
      indexFromPos = updateIndexFromPos(indexFromPos, change);

      var fromIndex = indexFromPos(change.from);
      var restLength = docEndLength - fromIndex - sumLengths(change.text);

      const removedLines: string[] = change.removed ?? [];

      operation = new TextOperation()
        .retain(fromIndex)
        .delete(sumLengths(removedLines))
        .insert(change.text.join("\n"))
        .retain(restLength)
        .compose(operation);

      inverse = inverse.compose(
        new TextOperation()
          .retain(fromIndex)
          .delete(sumLengths(change.text))
          .insert(removedLines.join("\n"))
          .retain(restLength),
      );

      docEndLength += sumLengths(removedLines) - sumLengths(change.text);
    }

    return [operation, inverse];
  }

  // Singular form for backwards compatibility.
  static operationFromCodeMirrorChange(changes: EditorChangeLinkedList[], doc: Doc) {
    return CodeMirrorAdapter.operationFromCodeMirrorChanges(changes, doc);
  }

  // Apply an operation to a CodeMirror instance.
  static applyOperationToCodeMirror(operation: TextOperation, cm: Editor) {
    cm.operation(function () {
      const ops = operation.ops;
      let index: number = 0; // holds the current index into CodeMirror's content
      for (var i = 0, l = ops.length; i < l; i++) {
        var op = ops[i];
        if (TextOperation.isRetain(op)) {
          index += op as number;
        } else if (TextOperation.isInsert(op)) {
          cm.replaceRange(op as string, cm.posFromIndex(index));
          index += (op as string).length;
        } else if (TextOperation.isDelete(op)) {
          var from = cm.posFromIndex(index);
          var to = cm.posFromIndex(index - (op as number));
          cm.replaceRange("", from, to);
        }
      }
    });
  }
}
