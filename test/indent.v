(* A comment
   spanning lines
   (* nested *)
*)
Require Import Nat.

Inductive expr : Type :=
  | Const (n : nat)
  | Add (a b : expr)
  | Let (x : nat) (v body : expr).

Fixpoint eval (e : expr) : option nat :=
  match e with
  | Const n => Some n
  | Add a b =>
    match eval a, eval b with
    | Some x, Some y => Some (x + y)
    | _, _ => None
    end
  | Let x v body =>
    let v' := eval v in
    eval body
  end.

Definition twice : (nat -> nat) -> nat -> nat :=
  fun f x =>
    f (f x).

Definition cond (b : bool) : nat :=
  if b
  then 1
  else 0.

Record point := {
  px : nat;
  py : nat
}.

Definition origin := {| px := 0;
                        py := 0 |}.

Lemma foo : forall n m : nat,
  n + m = m + n.
Proof.
  intros n m.
  induction n as [| n IH].
  - simpl.
    rewrite <- plus_n_O.
    reflexivity.
  - simpl.
    { rewrite IH.
      apply plus_n_Sm. }
Qed.

Lemma bar : forall (n : nat),
  n = n /\ True.
Proof.
  intros n; split.
  - reflexivity.
  - destruct n as [| n'].
    + exact I.
    + apply I.
Admitted.

Lemma baz (H' : True) : True.
Proof.
  assert (H : True /\
          True).
  { split.
    - exact H'.
    - exact H'. }
  destruct H as [H1 H2].
  - exact H1.
Qed.

Ltac solve_it :=
  match goal with
  | [ H : _ |- _ ] =>
    apply H
  | _ => idtac
  end.

Section Foo.
Variable x : nat.
End Foo.

Theorem t : True.
Proof.
  exact I.
Defined.

#[global]
Instance foo_inhabited : Inhabited nat := populate 0.

#[global] Instance bar : EqDecision nat.
Proof.
  refine (λ x y,
            match x, y with
            | 0, 0 => left _
            | _, _ => right _
            end); abstract congruence.
Defined.

Lemma quux : ∀ (n : nat),
  ∃ m, m = n.
Proof using Type.
  intros n. exists n.
  reflexivity.
Qed.

Program Definition pd (n : nat) : {m | m = n} := n.
Next Obligation.
  reflexivity.
Qed.

Definition rec := {|
  px := 1;
  py := 2
|}.

Lemma one_liner : True. Proof. exact I. Qed.
Lemma two : True.
Proof. intros.
  exact I.
Qed.

Ltac t :=
  lazymatch goal with
  | |- ?x = ?x => reflexivity
  | H : ?P |- ?P =>
    exact H
  end.

Definition long_app :=
  Nat.add (Nat.mul 1
           2)
  3.

Lemma triple : {{{ True ∗
                   True }}}
  skip
  {{{ RET #(); True }}}.
Proof.
  { { exact I. }
    exact I. }
Qed.

(** Doc comment with a list:
    - first item, which goes on
      for a while;
    - second item. *)
