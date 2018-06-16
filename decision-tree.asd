;;;; decision-tree.asd

(defsystem #:decision-tree
  :description "Decision tree with CLOS approach."
  :author "Panji Kusuma <epanji@gmail.com>"
  :license "Public Domain"
  :version "0.0.1"
  :components ((:file "package")
               (:file "decision-tree" :depends-on ("package")))
  :in-order-to ((test-op (load-op decision-tree/tests))))

(defmethod perform ((o asdf:test-op) (c (eql (find-system :decision-tree))))
  (funcall (intern "RUN-SUITE-TESTS" :decision-tree/tests)))

(defsystem #:decision-tree/tests
  :defsystem-depends-on (:fiveam)
  :depends-on ("decision-tree")
  :components ((:file "decision-tree-tests")))
