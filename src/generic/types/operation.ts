import { generateUuid } from "../util/uuid";
import type { CompositionFunction, TransformationFunction } from "./applicationSpecific";

type OperationId = string; // persistent, not changed by transformation

export enum ClientName {
  Alice = "Alice",
  Bob = "Bob",
}

interface OperationMeta {
  author: ClientName;
  id: OperationId;
}

export interface OperationWithoutPayload {
  meta: OperationMeta;
  transformedAgainst: OperationId[];
}

export interface Operation<OpT> extends OperationWithoutPayload {
  base: OpT;
}

export interface OperationAndRevision<OpT> extends Operation<OpT> {
  revision: number; // non-negative integer
}

export const createNewOperation = <OpT>(base: OpT, author: ClientName): Operation<OpT> => ({
  base,
  transformedAgainst: [],
  meta: {
    author,
    id: generateUuid(),
  },
});

export const transformOperation = <OpT>(
  transformationFunction: TransformationFunction<OpT>,
  a: Operation<OpT>, // server operation
  b: OperationAndRevision<OpT>, // client operation
): [Operation<OpT>, OperationAndRevision<OpT>] => {
  const [aWrappedPrime, bTextPrime] = transformationFunction(a.base, b.base);
  const aPrime: Operation<OpT> = {
    base: aWrappedPrime,
    transformedAgainst: [...a.transformedAgainst, b.meta.id],
    meta: a.meta,
  };
  const bPrime: OperationAndRevision<OpT> = {
    base: bTextPrime,
    transformedAgainst: [...b.transformedAgainst, a.meta.id],
    meta: b.meta,
    revision: b.revision + 1,
  };
  return [aPrime, bPrime];
};

export const composeOperation = <OpT>(
  compositionFunction: CompositionFunction<OpT>,
  first: OperationAndRevision<OpT>,
  second: OpT,
): OperationAndRevision<OpT> => {
  return {
    meta: {
      author: first.meta.author,
      id: generateUuid(),
    },
    transformedAgainst: [],
    revision: first.revision,
    base: compositionFunction(first.base, second),
  };
};
