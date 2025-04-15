;;; rocq-syntax.el --- Font lock expressions for Coq  -*- lexical-binding:t -*-

;; Vernacular commands
(defface rocq-vernac-commands
  `((t :inherit font-lock-keyword-face))
  "")

(defvar rocq-vernac-commands
  '("Section" "Module" "End" "Require" "Import" "Export" "Include" "Variable"
    "Variables" "Parameter" "Parameters" "Axiom" "Axioms" "Hypothesis"
    "Hypotheses" "Notation" "Local" "Tactic" "Reserved" "Scope"
    "Open" "Close" "Bind" "Declare" "Delimit" "Definition" "Example" "Let"
    "Ltac" "Ltac2" "Fixpoint" "CoFixpoint" "Morphism" "Relation" "Implicit"
    "Arguments" "Types" "Contextual" "Strict" "Prenex"
    "Implicits" "Inductive" "CoInductive" "Record" "Structure"
    "Variant" "Canonical" "Coercion" "Theorem" "Lemma" "Fact"
    "Remark" "Corollary" "Proposition" "Property" "Goal"
    "Proof" "Restart" "Save" "Qed" "Defined" "Abort" "Admitted"
    "Hint" "Resolve" "Rewrite" "View" "Search" "Compute" "Eval"
    "Show" "Print" "Printing" "All" "Graph" "Projections" "inside"
    "outside" "Check" "Global" "Instance" "Class" "Existing"
    "Universe" "Polymorphic" "Monomorphic" "Context" "Scheme" "From"
    "Undo" "Fail" "Function" "Program" "Elpi" "Extract" "Opaque"
    "Transparent" "Unshelve" "Next Obligation"))

;; Gallina
(defface rocq-gallina-keywords
  `((t :inherit font-lock-builtin-face))
  "")

(defvar rocq-gallina-keywords
  '("forall" "exists" "exists2" "fun" "fix" "cofix" "struct"
    "match" "end"  "in" "return" "let" "if" "is" "then" "else"
    "for" "of" "nosimpl" "with" "as"))

;; Sorts
(defface rocq-sorts
  `((t :inherit font-lock-builtin-face))
  "")

(defvar rocq-sorts '("Type" "Prop" "SProp" "Set"))

;; Tactics
(defface rocq-tactics
  `((t :inherit font-lock-function-call-face))
  "")

(defvar rocq-tactics
  '("pose" "set" "move" "case" "elim" "apply" "clear" "hnf" "intro"
    "intros" "generalize" "rename" "pattern" "after" "destruct"
    "induction" "using" "refine" "inversion" "injection" "rewrite"
    "congr" "unlock" "compute" "ring" "field" "replace" "fold"
    "unfold" "change" "cutrewrite" "simpl" "have" "suff" "wlog"
    "suffices" "without" "loss" "nat_norm" "assert" "cut" "trivial"
    "revert" "bool_congr" "nat_congr" "symmetry" "transitivity" "auto"
    "split" "left" "right" "autorewrite" "tauto" "setoid_rewrite"
    "intuition" "eauto" "eapply" "econstructor" "etransitivity"
    "constructor" "erewrite" "red" "cbv" "lazy" "vm_compute"
    "native_compute" "subst"))

;; Terminators
(defface rocq-terminators
  `((t :inherit font-lock-function-call-face))
  "")

(defvar rocq-terminators
  '("by" "now" "done" "exact" "reflexivity"
    "tauto" "romega" "omega" "lia" "nia" "lra" "nra" "psatz"
    "assumption" "solve" "contradiction" "discriminate"
    "congruence" "admit"))

;; Control
(defface rocq-control
  `((t :inherit font-lock-function-call-face))
  "")

(defvar rocq-control
  '("do" "last" "first" "try" "idtac" "repeat"))

(provide 'rocq-syntax)
