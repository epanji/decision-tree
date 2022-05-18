(cl:in-package #:asdf-user)

(defsystem "decision-tree"
  :description "Decision tree with CLOS approach."
  :author "Panji Kusuma <epanji@gmail.com>"
  :license "Public Domain"
  :version "0.0.1"
  :components ((:file "package")
               (:file "decision-tree" :depends-on ("package")))
  :in-order-to ((test-op (load-op "decision-tree/tests")))
  :perform (test-op (o c) (symbol-call :decision-tree/tests "RUN-SUITE-TESTS")))

(defsystem "decision-tree/tests"
  :depends-on ("decision-tree" "fiveam")
  :components ((:file "decision-tree-tests")))

