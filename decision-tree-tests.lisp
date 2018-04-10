;;;; decision-tree-tests.lisp

(in-package #:decision-tree/tests)

;; run all test
(defun run-all-test ()
  (format t "------------------------------------------------------------------------------------------------------------~%")
  (test-get-decision-tree)
  (test-remove-decision-tree-by-name)
  (test-arguments)
  (test-remove-all-decision-tree)
  (test-base-code-and-name)
  (test-criteria-decision-all)
  (test-answer-response))

;; :decision-tree
(defun test-get-decision-tree ()
  (format t "(typep (decision-tree \"test\") 'decision-tree) => ~a~%"
          (typep (decision-tree "test") 'decision-tree))
  (format t "------------------------------------------------------------------------------------------------------------~%"))

;; :remove-decision-tree
(defun test-remove-decision-tree-by-name ()
  (when (gethash "test" *decision-trees*)
    (format t "(remove-decision-tree \"test\") => ~a~%"
            (remove-decision-tree "test"))
    (format t "------------------------------------------------------------------------------------------------------------~%")))

;; :argument-question
;; :argument-unknown
(defun test-arguments ()
  (format t "(string-equal \"test\" (setf (argument-question (decision-tree \"test\")) \"test\")) => ~a~%"
          (string-equal "test" (setf (argument-question (decision-tree "test")) "test")))
  (format t "(string-equal \"test\" (setf (argument-unknown (decision-tree \"test\")) \"test\")) => ~a~%"
          (string-equal "test" (setf (argument-unknown (decision-tree "test")) "test")))
  (format t "------------------------------------------------------------------------------------------------------------~%")
  (remove-decision-tree "test"))

;; :remove-all-decision-tree
(defun test-remove-all-decision-tree ()
  (format t "(zerop (hash-table-count (remove-all-decision-tree)) => ~a~%"
          (zerop (hash-table-count (remove-all-decision-tree))))
  (format t "------------------------------------------------------------------------------------------------------------~%"))

;; :base-code
;; :base-name
(defun test-base-code-and-name ()
  (let* ((code "d-1")
         (name "test")
         (decision (make-instance 'decision :code code :name name)))
    (format t "(string-equal (base-code decision) code) => ~s~%"
            (string-equal (base-code decision) code))
    (format t "(string-equal (base-name decision) name) => ~s~%"
            (string-equal (base-name decision) name))
    (format t "------------------------------------------------------------------------------------------------------------~%")))

;; :criteria-to-tree
;; :criteria-from-tree
;; :decision-to-tree
;; :decision-from-tree
;; :criteria-to-decision-in-tree
;; :remove-criteria-from-decision-in-tree
;; :remove-criteria-from-tree
;; :temporary-relations
;; :populate-temporary-relations
;; :criteria-codes
;; :count-criteria-code
;; :criteria-code
;; :remove-decision-from-tree
(defun test-criteria-decision-all ()
  (remove-decision-tree "test")
  (let ((decision-tree (decision-tree "test"))
        (d1 (make-instance 'decision
                           :code "d-1"
                           :name "decision one"
                           :descriptions '("description"
                                           "another descripsion")))
        (d2 (make-instance 'decision
                           :code "d-2"
                           :name "decision two"
                           :descriptions '("description"
                                           "another descripsion")))
        (c1 (make-instance 'criteria
                           :code "c-1"
                           :name "criteria one"))
        (c2 (make-instance 'criteria
                           :code "c-2"
                           :name "criteria two"))
        (c3 (make-instance 'criteria
                           :code "c-3"
                           :name "criteria three")))
    ;; criteria to and from - 1 2 x 1 1
    (format t
            "(eq (criteria-to-tree \"test\" c1) (criteria-from-tree \"test\" \"c-1\")) => ~s~%"
            (eq (criteria-to-tree "test" c1) (criteria-from-tree "test" "c-1")))
    ;; criteria to and from - 2 2 x 2 1
    (format t
            "(eq (criteria-to-tree decision-tree c2) (criteria-from-tree decision-tree \"c-2\")) => ~s~%"
            (eq (criteria-to-tree decision-tree c2) (criteria-from-tree decision-tree "c-2")))
    ;; decision to and from - 1 2 x 1 1
    (format t
            "(eq (decision-to-tree \"test\" d1) (decision-from-tree \"test\" \"d-1\")) => ~s~%"
            (eq (decision-to-tree "test" d1) (decision-from-tree "test" "d-1")))
    ;; decision to and from - 2 2 x 2 1
    (format t
            "(eq (decision-to-tree decision-tree d2) (decision-from-tree decision-tree \"d-2\")) => ~s~%"
            (eq (decision-to-tree decision-tree d2) (decision-from-tree decision-tree "d-2")))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree nil d1 c1) => ~s~%" (criteria-to-decision-in-tree nil d1 c2))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree decision-tree d1 c1) => ~s~%" (criteria-to-decision-in-tree decision-tree d1 c1))
    (format t "(remove-criteria-from-decision-in-tree decision-tree d1 c1) => ~s~%" (remove-criteria-from-decision-in-tree decision-tree d1 c1))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree \"test\" \"d-1\" \"c-1\") => ~s~%" (criteria-to-decision-in-tree "test" "d-1" "c-1"))
    (format t "(remove-criteria-from-decision-in-tree \"test\" \"d-1\" \"c-1\") => ~s~%" (remove-criteria-from-decision-in-tree "test" "d-1" "c-1"))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree decision-tree \"d-1\" \"c-1\") => ~s~%" (criteria-to-decision-in-tree decision-tree "d-1" "c-1"))
    (format t "(remove-criteria-from-decision-in-tree decision-tree \"d-1\" \"c-1\") => ~s~%" (remove-criteria-from-decision-in-tree decision-tree "d-1" "c-1"))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree \"test\" d1 c1) => ~s~%" (criteria-to-decision-in-tree "test" d1 c1))
    (format t "(remove-criteria-from-decision-in-tree \"test\" d1 c1) => ~s~%" (remove-criteria-from-decision-in-tree "test" d1 c1))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree decision-tree \"d-1\" c1) => ~s~%" (criteria-to-decision-in-tree decision-tree "d-1" c1))
    (format t "(remove-criteria-from-decision-in-tree decision-tree \"d-1\" c1) => ~s~%" (remove-criteria-from-decision-in-tree decision-tree "d-1" c1))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree \"test\" d1 \"c-1\") => ~s~%" (criteria-to-decision-in-tree "test" d1 "c-1"))
    (format t "(remove-criteria-from-decision-in-tree \"test\" d1 \"c-1\") => ~s~%" (remove-criteria-from-decision-in-tree "test" d1 "c-1"))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree decision-tree d1 \"c-1\") => ~s~%" (criteria-to-decision-in-tree decision-tree d1 "c-1"))
    (format t "(remove-criteria-from-decision-in-tree decision-tree d1 \"c-1\") => ~s~%" (remove-criteria-from-decision-in-tree decision-tree d1 "c-1"))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree \"test\" \"d-1\" c1) => ~s~%" (criteria-to-decision-in-tree "test" "d-1" c1))
    (format t "(remove-criteria-from-decision-in-tree \"test\" \"d-1\" c1) => ~s~%" (remove-criteria-from-decision-in-tree "test" "d-1" c1))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(remove-criteria-from-tree \"test\" \"c-2\") => ~s~%" (remove-criteria-from-tree "test" "c-2"))
    (format t "(criteria-to-decision-in-tree \"test\" \"d-1\" c2) => ~s~%" (criteria-to-decision-in-tree "test" "d-1" c2))
    (format t "(remove-criteria-from-tree \"test\" c2) => ~s~%" (remove-criteria-from-tree "test" c2))
    (format t "(criteria-to-decision-in-tree \"test\" \"d-1\" c2) => ~s~%" (criteria-to-decision-in-tree "test" "d-1" c2))
    (format t "(remove-criteria-from-tree decision-tree \"c-2\") => ~s~%" (remove-criteria-from-tree decision-tree "c-2"))
    (format t "(criteria-to-decision-in-tree \"test\" \"d-1\" c2) => ~s~%" (criteria-to-decision-in-tree "test" "d-1" c2))
    (format t "(remove-criteria-from-tree decision-tree c-2) => ~s~%" (remove-criteria-from-tree decision-tree c2))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-to-decision-in-tree \"test\" \"d-1\" \"c-1\") => ~s~%" (criteria-to-decision-in-tree "test" "d-1" "c-1"))
    (format t "(criteria-to-decision-in-tree \"test\" \"d-1\" c2) => ~s~%" (criteria-to-decision-in-tree "test" "d-1" c2))
    (format t "(criteria-to-decision-in-tree \"test\" \"d-2\" c1) => ~s~%" (criteria-to-decision-in-tree "test" "d-2" c1))
    (format t "(criteria-to-decision-in-tree \"test\" \"d-2\" c3) => ~s~%" (criteria-to-decision-in-tree "test" "d-2" c3))
    (format t "(populate-temporary-relations \"test\") => ~s~%" (populate-temporary-relations "test"))
    (format t "(populate-temporary-relations decision-tree) => ~s~%" (populate-temporary-relations decision-tree))
    (format t "(temporary-relations decision-tree) => ~s~%" (temporary-relations decision-tree))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-codes \"test\") => ~s~%" (criteria-codes "test"))
    (format t "(criteria-codes decision-tree) => ~s~%" (criteria-codes decision-tree))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(count-criteria-code decision-tree \"c-1\") => ~s~%" (count-criteria-code decision-tree "c-1"))
    (format t "(count-criteria-code decision-tree c1) => ~s~%" (count-criteria-code decision-tree c1))
    (format t "(count-criteria-code \"test\" \"c-2\") => ~s~%" (count-criteria-code "test" "c-2"))
    (format t "(count-criteria-code \"test\" c2) => ~s~%" (count-criteria-code "test" c2))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(criteria-code decision-tree) => ~s~%" (criteria-code decision-tree))
    (format t "(criteria-code \"test\") => ~s~%" (criteria-code "test"))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(question-criteria-code \"test\" \"c-1\") => ~s~%" (question-criteria-code "test" "c-1"))
    (format t "(question-criteria-code \"test\" c1) => ~s~%" (question-criteria-code "test" c1))
    (format t "(question-criteria-code decision-tree \"c-1\") => ~s~%" (question-criteria-code decision-tree "c-1"))
    (format t "(question-criteria-code decision-tree c1) => ~s~%" (question-criteria-code decision-tree c1))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(decision-tree-criterions decision-tree) => ~s~%" (decision-tree-criterions decision-tree))
    (format t "(decision-tree-decisions decision-tree) => ~s~%" (decision-tree-decisions decision-tree))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (format t "(remove-decision-from-tree \"test\" \"d-1\") => ~s~%" (remove-decision-from-tree "test" "d-1"))
    (format t "(remove-decision-from-tree \"test\" d2) => ~s~%" (remove-decision-from-tree "test" d2))
    (decision-to-tree "test" d1)
    (decision-to-tree "test" d2)
    (format t "(remove-decision-from-tree decision-tree \"d-1\") => ~s~%" (remove-decision-from-tree decision-tree "d-1"))
    (format t "(remove-decision-from-tree decision-tree d2) => ~s~%" (remove-decision-from-tree decision-tree d2))
    (format t "------------------------------------------------------------------------------------------------------------~%"))
  (remove-decision-tree "test"))

;; :positive-answer
;; :negative-answer
;; :decision-from-answer
;; :decision-from-answers
;; :decision-from-relations
(defun test-answer-response ()
  (let ((decision-tree (decision-tree "test"))
        (d3 (make-instance 'decision
                           :code "d-3"
                           :name "decision three"
                           :descriptions '("description"
                                           "another descripsion"))))
    ;; dummy relations and d3
    (decision-to-tree decision-tree d3)
    (setf (temporary-relations decision-tree)
          '(("d-1" "c-1" "c-2" "c-3")
            ("d-2" "c-2" "c-3")
            ("d-3" "c-2")
            ("d-4" "c-3")))
    (format t "(temporary-relations decision-tree) => ~s~%" (temporary-relations decision-tree))
    (format t "(decision-from-answer decision-tree '(\"c-2\" . t)) => ~s~%" (decision-from-answer decision-tree '("c-2" . t)))
    (format t "     (positive-answer decision-tree \"c-2\") => ~s~%" (temporary-relations decision-tree))
    (format t "(decision-from-answer decision-tree '(\"c-3\" . nil)) => ~s~%" (decision-from-answer decision-tree '("c-3" . nil)))
    (format t "     (negative-answer \"test\" \"c-3\") => ~s~%" (temporary-relations decision-tree))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (setf (temporary-relations decision-tree)
          '(("d-1" "c-1" "c-2" "c-3")
            ("d-2" "c-2" "c-3")
            ("d-3" "c-2")
            ("d-4" "c-3")))
    (format t "(decision-from-answers decision-tree '((\"c-2\" . t) (\"c-3\" . nil))) => ~s~%"
            (decision-from-answers decision-tree '(("c-2" . t) ("c-3" . nil))))
    (setf (temporary-relations decision-tree)
          '(("d-1" "c-1" "c-2" "c-3")
            ("d-2" "c-2" "c-3")
            ("d-3" "c-2")
            ("d-4" "c-3")))
    (format t "(decision-from-answers \"test\" '((\"c-2\" . t) (\"c-3\" . nil))) => ~s~%"
            (decision-from-answers "test" '(("c-2" . t) ("c-3" . nil))))
    (format t "(decision-from-relations decision-tree) => ~s~%" (decision-from-relations decision-tree))
    (format t "(decision-from-relations \"test\") => ~s~%" (decision-from-relations "test"))
    (format t "------------------------------------------------------------------------------------------------------------~%")
    (remove-decision-tree "test")))

;; :decision-from-interactive
;; :print-decision
(defun run-interactive-test ()
  (decision-to-tree "test" (make-instance 'decision :name "Decision one" :code "d-1"
                                                    :descriptions '("Vel pretium lectus quam id leo in vitae turpis massa sed elementum"
                                                                    "semper auctor neque, vitae tempus quam pellentesque nec nam aliquam"
                                                                    "sem et tortor consequat id porta nibh venenatis cras sed felis eget"
                                                                    "velit aliquet sagittis id consectetur? Magna fermentum iaculis eu"
                                                                    "non diam phasellus vestibulum lorem sed. Platea dictumst vestibulum"
                                                                    "praesent semper feugiat nibh.")))
  (decision-to-tree "test" (make-instance 'decision :name "Decision two" :code "d-2"
                                                    :descriptions '("Vel pretium lectus quam id leo in vitae turpis massa sed elementum"
                                                                    "tempus egestas sed sed risus pretium. Et molestie ac, feugiat sed"
                                                                    "lectus vestibulum mattis ullamcorper velit sed ullamcorper morbi"
                                                                    "tincidunt ornare massa, eget egestas purus viverra accumsan"
                                                                    "a. Risus nec feugiat in fermentum posuere urna nec tincidunt"
                                                                    "praesent semper feugiat nibh.")))
  (decision-to-tree "test" (make-instance 'decision :name "Decision three" :code "d-3"
                                                    :descriptions '("Vel pretium lectus quam id leo in vitae turpis massa sed elementum"
                                                                    "tempus egestas sed sed risus pretium. Et molestie ac, feugiat sed"
                                                                    "lectus vestibulum mattis ullamcorper velit sed ullamcorper morbi"
                                                                    "Magnis dis parturient montes, nascetur ridiculus mus mauris vitae"
                                                                    "a. Risus nec feugiat in fermentum posuere urna nec tincidunt"
                                                                    "praesent semper feugiat nibh.")))
  (decision-to-tree "test" (make-instance 'decision :name "Decision four" :code "d-4"
                                                    :descriptions '("Vel pretium lectus quam id leo in vitae turpis massa sed elementum"
                                                                    "sed risus. Interdum consectetur libero, id faucibus nisl tincidunt"
                                                                    "eget nullam non nisi est, sit amet facilisis magna etiam tempor,"
                                                                    "orci eu lobortis elementum, nibh tellus molestie nunc. Dignissim"
                                                                    "a. Risus nec feugiat in fermentum posuere urna nec tincidunt"
                                                                    "praesent semper feugiat nibh.")))

  (criteria-to-decision-in-tree "test" "d-1" (make-instance 'criteria :name "Criteria one" :code "c-1"))
  (criteria-to-decision-in-tree "test" "d-1" (make-instance 'criteria :name "Criteria two" :code "c-2"))
  (criteria-to-decision-in-tree "test" "d-1" (make-instance 'criteria :name "Criteria three" :code "c-3"))
  (criteria-to-decision-in-tree "test" "d-2" "c-2")
  (criteria-to-decision-in-tree "test" "d-2" "c-3")
  (criteria-to-decision-in-tree "test" "d-3" "c-2")
  (criteria-to-decision-in-tree "test" "d-4" "c-3")
  (format t "------------------------------------------------------------------------------------------------------------~%")
  (populate-temporary-relations "test")
  (print-decision (decision-from-interactive "test"))
  (format t "------------------------------------------------------------------------------------------------------------~%")
  (format t "(temporary-records (decision-tree \"test\")) => ~s~%" (temporary-records (decision-tree "test")))
  (format t "------------------------------------------------------------------------------------------------------------~%")
  (remove-decision-tree "test"))
