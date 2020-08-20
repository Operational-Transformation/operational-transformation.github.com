import { TextOperation } from "ot";
import { ClientName } from "./visualizationState";

interface OperationMeta {
  author: ClientName;
  key: string;
}

export interface Operation {
  meta: OperationMeta;
  textOperation: TextOperation;
}

export interface OperationAndRevision extends Operation {
  revision: number; // non-negative integer
}
