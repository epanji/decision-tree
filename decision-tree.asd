;;;; decision-tree.asd

(defsystem #:decision-tree
  :description "Decision tree with CLOS approach."
  :author "Panji Kusuma <epanji@gmail.com>"
  :license "Public Domain"
  :version "0.0.1"
  :components ((:file "package")
               (:file "decision-tree" :depends-on ("package"))))

(defsystem #:decision-tree/tests
  :depends-on ("decision-tree")
  :components ((:file "decision-tree-tests")))
