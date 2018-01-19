;;;; package.lisp

(defpackage #:decision-tree
  (:nicknames #:lab.qzuma.dtree #:dtree)
  (:use #:cl)
  (:export :*decision-trees*
           :*output*
           :decision-tree
           :decision-tree-criterions
           :decision-tree-decisions
           :argument-question
           :argument-unknown
           :remove-decision-tree
           :remove-all-decision-tree
           :base-code
           :base-name
           :criteria
           :criteria-to-tree
           :criteria-from-tree
           :decision
           :decision-descriptions
           :decision-criterions
           :decision-to-tree
           :decision-from-tree
           :criteria-to-decision-in-tree
           :remove-criteria-from-decision-in-tree
           :remove-criteria-from-tree
           :remove-decision-from-tree
           :temporary-relations
           :temporary-records
           :populate-temporary-relations
           :criteria-codes
           :count-criteria-code
           :criteria-code
           :question-criteria-code
           :answer
           :decision-from-answer
           :decision-from-answers
           :decision-from-relations
           :positive-answer
           :negative-answer
           :decision-from-interactive
           :print-decision))

(defpackage #:decision-tree-test
  (:nicknames :dtree-test)
  (:use #:cl #:decision-tree)
  (:export :run-all-test
           :run-interactive-test))
