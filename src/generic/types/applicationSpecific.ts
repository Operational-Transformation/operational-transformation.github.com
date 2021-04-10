import type { ForwardRefExoticComponent, PropsWithoutRef, RefAttributes } from "react";

export interface TransformationFunction<OpT> {
  (a: OpT, b: OpT): [OpT, OpT];
}

export interface CompositionFunction<OpT> {
  (first: OpT, second: OpT): OpT;
}

export interface ApplicationFunction<SnapshotT, OpT> {
  (operation: OpT, snapshot: SnapshotT): SnapshotT;
}

export interface ApplicationSpecificOperationFunctions<OpT> {
  transform: TransformationFunction<OpT>;
  compose: CompositionFunction<OpT>;
}

export interface ApplicationSpecificFunctions<SnapshotT, OpT>
  extends ApplicationSpecificOperationFunctions<OpT> {
  apply: ApplicationFunction<SnapshotT, OpT>;
}

export interface ApplicationSpecificOperationComponents<OpT> {
  renderOperation(operation: OpT): React.ReactNode; // TODO: make component
}

interface ApplicationSpecificSnapshotComponents<SnapshotT> {
  renderSnapshot(snapshot: SnapshotT): React.ReactNode; // TODO: make component
}

export interface EditorProps<SnapshotT, OpT> {
  snapshot: SnapshotT;
  onUserChange: (textOperation: OpT) => void;
}

export interface EditorHandle<OpT> {
  applyOperation(operation: OpT): void;
}

type EditorComp<SnapshotT, OpT> = ForwardRefExoticComponent<
  PropsWithoutRef<EditorProps<SnapshotT, OpT>> & RefAttributes<EditorHandle<OpT>>
>;

export interface ApplicationSpecificComponents<SnapshotT, OpT>
  extends ApplicationSpecificOperationComponents<OpT>,
    ApplicationSpecificSnapshotComponents<SnapshotT> {
  EditorComponent: EditorComp<SnapshotT, OpT>;
}
