Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id : Type :=
| makeId : string -> id.

Definition eqId (x1 : id) (x2 : id) : bool :=
  match x1 with
    makeId s1 => match x2 with
      makeId s2 => andb (prefix s1 s2) (prefix s2 s1)
    end
  end.

(* Types *)

Inductive type : Type :=
| tunit : type
| tarrow : type -> type -> type.

(* Terms *)

Inductive term : Type :=
| eunit : term
| evar : id -> term
| eabs : id -> type -> term -> term
| eapp : term -> term -> term.

(* Contexts *)

Definition context := id -> option type.

Definition emptyContext : context := fun s => None.

Definition extendContext (c : context) (x1 : id) (t : type) : context :=
  fun x2 => if eqId x1 x2 then Some t else c x2.

(* Helper functions *)

Definition lookupVar (c : context) (x : term) := match x with
| evar s => c s
| _ => None
end.

(*  Typing rules *)

Reserved Notation "c '|-' t ':::' T" (at level 40).

Inductive hasType : context -> term -> type -> Prop :=
| aunit : forall c : context, c |- eunit ::: tunit
| avar : forall c x t, lookupVar c x = Some t -> c |- x ::: t
| aabs : forall c x t1 t2 e,
         (extendContext c x t1 |- e ::: t2) ->
         (c |- eabs x t1 e ::: tarrow t1 t2)
| aapp : forall c e1 e2 t1 t2,
         c |- e1 ::: t1 ->
         c |- e2 ::: tarrow t1 t2 ->
         c |- eapp e2 e1 ::: t2
where "c '|-' t ':::' T" := (hasType c t T).

(* An example proof *)

Definition v := eunit.

Theorem v_type : emptyContext |- v ::: tunit.
Proof. apply aunit. Qed.