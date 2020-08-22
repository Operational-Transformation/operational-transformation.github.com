import React, {
  createContext,
  Dispatch,
  FunctionComponent,
  SetStateAction,
  useContext,
  useState,
} from "react";
import { Operation } from "./types/operation";

type StateHandle<S> = [S, Dispatch<SetStateAction<S>>];

type OperationHoverStateHandle = StateHandle<Operation | undefined>;

const OperationHoverContext = createContext<StateHandle<Operation | undefined>>([
  undefined,
  () => {
    throw new Error("you need to wrap this with <OperationHoverProvider />");
  },
]);

export const OperationHoverProvider: FunctionComponent = (props) => {
  const stateHandle = useState<Operation | undefined>(undefined);

  return (
    <OperationHoverContext.Provider value={stateHandle}>
      {props.children}
    </OperationHoverContext.Provider>
  );
};

export const useOperationHoverState = (): OperationHoverStateHandle =>
  useContext(OperationHoverContext);
