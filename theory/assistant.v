Require Import String.
Require Import List.
Require Import Psatz.
Open Scope string_scope.
Module ArithConst.

  Inductive arith :Set    :=
  | Const  (n: nat)
  | Plus (e1 e2 : arith)
  | Times (e1 e2 : arith).

  (* Expression 2+2 *)

  Example ex1 := Plus (Const 2) (Const 2).
Fixpoint size     (e : arith): nat :=
  match e with
  | Const _  => 1
  | Plus e1 e2  => 1 + size e1 + size e2
  | Times e1 e2  => 1 + size e1 + size e2
  end.

Fixpoint depth     (e : arith): nat :=
  match e with
  | Const _  => 1
  | Plus e1 e2  => 1 + max (depth e1) ( depth e2)
  | Times e1 e2  => 1 + max (depth e1) ( depth e2)
  end.

Compute depth ex1.
     
Theorem depth_le_size : forall e,
    depth e <= size e.

Proof.
  intros.
  induction e; simpl;lia.
    Qed.
End ArithConst.


Module ArithVar.

  Inductive arith : Set :=
  | Var (x: string ) 
  | Const  (n: nat)
  | Plus (e1 e2 : arith)
  | Times (e1 e2 : arith).

  (* Expression 2+2 *)

  Example ex1 := Plus (Const 2) (Const 2).
Example ex2 := Plus (Var "x")(Var "y").

Example ex3 := Times ex1 ex2.

Definition variable := string.

Fixpoint size     (e : arith): nat :=
  match e with
  | Const _  => 1
  | Var _  => 1
  | Plus e1 e2  => 1 + size e1 + size e2
  | Times e1 e2  => 1 + size e1 + size e2
  end.


Fixpoint depth     (e : arith): nat :=
  match e with
  | Const _  => 1
  | Var _  => 1
  | Plus e1 e2  => 1 + max (depth e1) ( depth e2)
  | Times e1 e2  => 1 + max (depth e1) ( depth e2)
  end.

Compute depth ex1.
     
Theorem depth_le_size : forall e,
    depth e <= size e.

Proof.
  intros.
  induction e; simpl;lia.
    Qed.

Fixpoint  substitute (e1 e2: arith) (v: variable)  : arith := 
  match e1 with
  |Const _ => e1
  |Var x => if string_dec x  v then e2 else e1
  |Plus a1 a2 => Plus (substitute a1  e2 v) (substitute a2  e2 v)
  |Times a1 a2 => Times (substitute a1 e2 v) (substitute a2 e2 v)
  end.


Compute ex2.
Compute substitute ex2 ex1 "x".

Theorem substitute_depth : forall replaceThis withThis inThis,
 depth (substitute inThis withThis replaceThis) <= depth inThis + depth withThis.

Proof.
  intros.
  induction inThis.
   -simpl. (* lia. *)
Abort All.

Definition bindings := list (string * nat).

Fixpoint getValue (v: string) (bs: bindings):=
  match bs with
  | [] => 0
  | (x, n) :: t => if string_dec x x then n else getValue v t
  end.

Fixpoint eval (state: bindings) (e: arith) : nat :=
  match e with
  | Const n => n
  | Var v => getValue v bindings
  end

End ArithVar.

