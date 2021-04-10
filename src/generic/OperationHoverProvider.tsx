import React, {
  createContext,
  Dispatch,
  FunctionComponent,
  SetStateAction,
  useContext,
  useState,
} from "react";
import type { OperationWithoutPayload } from "./types/operation";

type StateHandle<S> = [S, Dispatch<SetStateAction<S>>];

type OperationHoverStateHandle = StateHandle<OperationWithoutPayload | undefined>;

const OperationHoverContext = createContext<StateHandle<OperationWithoutPayload | undefined>>([
  undefined,
  () => {
    throw new Error("you need to wrap this with <OperationHoverProvider />");
  },
]);

export const OperationHoverProvider: FunctionComponent = (props) => {
  const stateHandle = useState<OperationWithoutPayload | undefined>(undefined);

  return (
    <OperationHoverContext.Provider value={stateHandle}>
      {props.children}
    </OperationHoverContext.Provider>
  );
};

export const useOperationHoverState = (): OperationHoverStateHandle =>
  useContext(OperationHoverContext);
