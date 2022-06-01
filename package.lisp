;;;; package.lisp

(defpackage #:decision-tree
  (:nicknames #:lab.qzuma.dtree #:dtree)
  (:use #:cl)
  (:export #:*decision-trees*
           #:*output*
           #:*code-tree-order*
           #:decision-tree
           #:decisions
           #:criterions
           #:remove-decision-tree
           #:remove-all-decision-tree
           #:code
           #:name
           #:descriptions
           #:criteria
           #:criteria-to-tree
           #:criteria-from-tree
           #:decision
           #:decision-to-tree
           #:decision-from-tree
           #:criteria-to-decision-in-tree
           #:remove-criteria-from-decision-in-tree
           #:remove-criteria-from-tree
           #:remove-decision-from-tree
           #:relations
           #:populate-relations
           #:criteria-code
           #:criteria-codes
           #:count-criteria-code
           #:answer
           #:records
           #:question
           #:question-criteria-code
           #:unknown
           #:decision-from-answer
           #:decision-from-answers
           #:decision-from-relations
           #:positive-answer
           #:negative-answer
           #:code-tree
           #:decision-from-interactive
           #:print-decision))

(defpackage #:decision-tree/tests
  (:nicknames #:dtree-tests)
  (:use #:cl #:decision-tree #:fiveam)
  (:export #:run-suite-tests
           #:run-interactive-test))
