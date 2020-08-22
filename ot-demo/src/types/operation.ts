import { TextOperation } from "ot";
import { ClientName } from "./visualizationState";
import { generateUuid } from "../util/uuid";

type OperationId = string; // persistent, not changed by transformation

interface OperationMeta {
  author: ClientName;
  id: OperationId;
}

export interface Operation {
  meta: OperationMeta;
  transformedAgainst: OperationId[];
  textOperation: TextOperation;
}

export interface OperationAndRevision extends Operation {
  revision: number; // non-negative integer
}

export const createNewOperation = (
  textOperation: TextOperation,
  author: ClientName,
): Operation => ({
  textOperation,
  transformedAgainst: [],
  meta: {
    author,
    id: generateUuid(),
  },
});

const transformTextOperation = (
  a: TextOperation,
  b: TextOperation,
): [TextOperation, TextOperation] =>
  (TextOperation.transform(a, b) as unknown) as [TextOperation, TextOperation]; // because type definition is wrong

export const transformOperation = (
  a: Operation, // server operation
  b: OperationAndRevision, // client operation
): [Operation, OperationAndRevision] => {
  const [aTextPrime, bTextPrime] = transformTextOperation(a.textOperation, b.textOperation);
  const aPrime: Operation = {
    textOperation: aTextPrime,
    transformedAgainst: [...a.transformedAgainst, b.meta.id],
    meta: a.meta,
  };
  const bPrime: OperationAndRevision = {
    textOperation: bTextPrime,
    transformedAgainst: [...b.transformedAgainst, a.meta.id],
    meta: b.meta,
    revision: b.revision + 1,
  };
  return [aPrime, bPrime];
};

export const composeOperation = (
  first: OperationAndRevision,
  second: TextOperation,
): OperationAndRevision => {
  if (first.transformedAgainst.length !== 0) {
    throw new Error(
      "you shouldn't compose a transformed operation because composition and transformation are not compatible",
    );
  }

  return {
    meta: {
      author: first.meta.author,
      id: generateUuid(),
    },
    transformedAgainst: [],
    revision: first.revision,
    textOperation: first.textOperation.compose(second),
  };
};
