(cl:in-package #:cl-user)

(defpackage #:decision-tree
  (:nicknames #:lab.qzuma.dtree #:dtree)
  (:use #:cl)
  (:export #:*code-tree-order*
           #:*decision-trees*
           #:*output*
           #:answer
           #:code
           #:code-tree
           #:count-criteria-code
           #:criteria
           #:criteria-code
           #:criteria-codes
           #:criteria-from-tree
           #:criteria-to-decision-in-tree
           #:criteria-to-tree
           #:criterions
           #:decision
           #:decision-from-answer
           #:decision-from-answers
           #:decision-from-interactive
           #:decision-from-relations
           #:decision-from-tree
           #:decision-to-tree
           #:decision-tree
           #:decisions
           #:descriptions
           #:name
           #:negative-answer
           #:populate-relations
           #:positive-answer
           #:print-decision
           #:question
           #:question-criteria-code
           #:records
           #:relations
           #:remove-all-decision-tree
           #:remove-criteria-from-decision-in-tree
           #:remove-criteria-from-tree
           #:remove-decision-from-tree
           #:remove-decision-tree
           #:unknown))

