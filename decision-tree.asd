;;;; decision-tree.asd

(asdf:defsystem #:decision-tree
  :description "Decision tree with CLOS approach."
  :author "Panji Kusuma <epanji@gmail.com>"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "decision-tree")))

(asdf:defsystem #:decision-tree-test
  :description "Decision tree with CLOS approach test."
  :version "0.0.1"
  :depends-on (:decision-tree)
  :components ((:file "decision-tree-test")))
