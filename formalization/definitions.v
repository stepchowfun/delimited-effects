Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id : Type :=
| makeId : string -> id.

Definition eqId i1 i2 : bool :=
  match i1 with
    makeId s1 => match i2 with
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

Definition emptyContext : context := fun i => None.

Definition extendContext c i1 t : context :=
  fun i2 => if eqId i1 i2 then Some t else c i2.

Definition lookupVar (c : context) e := match e with
| evar i => c i
| _ => None
end.

(*  Typing rules *)

Reserved Notation "c '|-' t ':::' T" (at level 40).

Inductive hasType : context -> term -> type -> Prop :=
| aunit : forall c, c |- eunit ::: tunit
| avar : forall c e t, lookupVar c e = Some t -> c |- e ::: t
| aabs : forall c i t1 t2 e,
         (extendContext c i t1 |- e ::: t2) ->
         (c |- eabs i t1 e ::: tarrow t1 t2)
| aapp : forall c e1 e2 t1 t2,
         c |- e1 ::: t1 ->
         c |- e2 ::: tarrow t1 t2 ->
         c |- eapp e2 e1 ::: t2
where "c '|-' t ':::' T" := (hasType c t T).

(* An example proof *)

Definition v := eunit.

Theorem v_tunit : emptyContext |- v ::: tunit.
Proof. apply aunit. Qed.