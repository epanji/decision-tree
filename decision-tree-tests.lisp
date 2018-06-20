;;;; decision-tree-tests.lisp

(in-package #:decision-tree/tests)

(defmacro with-new-decision-trees (&body body)
  `(let ((*decision-trees* (make-hash-table :test 'equal)))
     ,@body))

(defmacro with-dummy-data ((&rest ignored) &body body)
  `(with-new-decision-trees
     (let ((decision-tree (decision-tree "test"))
           (d1 (make-instance 'decision :name "Decision one" :code "d-1"
                                        :descriptions '("Vel pretium lectus quam id leo in vitae turpis massa sed elementum"
                                                        "semper auctor neque, vitae tempus quam pellentesque nec nam aliquam"
                                                        "sem et tortor consequat id porta nibh venenatis cras sed felis eget"
                                                        "velit aliquet sagittis id consectetur? Magna fermentum iaculis eu"
                                                        "non diam phasellus vestibulum lorem sed. Platea dictumst vestibulum"
                                                        "praesent semper feugiat nibh.")))
           (d2 (make-instance 'decision :name "Decision two" :code "d-2"
                                        :descriptions '("Vel pretium lectus quam id leo in vitae turpis massa sed elementum"
                                                        "tempus egestas sed sed risus pretium. Et molestie ac, feugiat sed"
                                                        "lectus vestibulum mattis ullamcorper velit sed ullamcorper morbi"
                                                        "tincidunt ornare massa, eget egestas purus viverra accumsan"
                                                        "a. Risus nec feugiat in fermentum posuere urna nec tincidunt"
                                                        "praesent semper feugiat nibh.")))
           (d3 (make-instance 'decision :name "Decision three" :code "d-3"
                                        :descriptions '("Vel pretium lectus quam id leo in vitae turpis massa sed elementum"
                                                        "tempus egestas sed sed risus pretium. Et molestie ac, feugiat sed"
                                                        "lectus vestibulum mattis ullamcorper velit sed ullamcorper morbi"
                                                        "Magnis dis parturient montes, nascetur ridiculus mus mauris vitae"
                                                        "a. Risus nec feugiat in fermentum posuere urna nec tincidunt"
                                                        "praesent semper feugiat nibh.")))
           (d4 (make-instance 'decision :name "Decision four" :code "d-4"
                                        :descriptions '("Vel pretium lectus quam id leo in vitae turpis massa sed elementum"
                                                        "sed risus. Interdum consectetur libero, id faucibus nisl tincidunt"
                                                        "eget nullam non nisi est, sit amet facilisis magna etiam tempor,"
                                                        "orci eu lobortis elementum, nibh tellus molestie nunc. Dignissim"
                                                        "a. Risus nec feugiat in fermentum posuere urna nec tincidunt"
                                                        "praesent semper feugiat nibh.")))
           (c1 (make-instance 'criteria :name "Criteria one" :code "c-1"))
           (c2 (make-instance 'criteria :name "Criteria two" :code "c-2"))
           (c3 (make-instance 'criteria :name "Criteria three" :code "c-3")))
       (declare (ignore ,@ignored))
       ,@body)))

;; :decision-from-interactive
;; :print-decision
(defun run-interactive-test ()
  (with-dummy-data ()
    (decision-to-tree "test" d1)
    (decision-to-tree "test" d2)
    (decision-to-tree "test" d3)
    (decision-to-tree "test" d4)
    (criteria-to-tree "test" c1)
    (criteria-to-tree "test" c2)
    (criteria-to-tree "test" c3)
    (criteria-to-decision-in-tree "test" "d-1" "c-1")
    (criteria-to-decision-in-tree "test" "d-1" "c-2")
    (criteria-to-decision-in-tree "test" "d-1" "c-3")
    (criteria-to-decision-in-tree "test" "d-2" "c-2")
    (criteria-to-decision-in-tree "test" "d-2" "c-3")
    (criteria-to-decision-in-tree "test" "d-3" "c-2")
    (criteria-to-decision-in-tree "test" "d-4" "c-3")
    (format t "-----------------------------------------------------------------------------~%")
    (populate-relations decision-tree)
    (print-decision (decision-from-interactive "test"))
    (format t "-----------------------------------------------------------------------------~%")
    (format t "(records (decision-tree \"test\")) => ~s~%" (records (decision-tree "test")))
    (format t "-----------------------------------------------------------------------------~%")))

(defun run-suite-tests ()
  (run! 'decision-tree-suite))

(def-suite decision-tree-suite)

(in-suite decision-tree-suite)

(test class-in-package
  (is (find-class 'decision-tree))
  (is (find-class 'decision-tree::element))
  (is (find-class 'criteria))
  (is (find-class 'decision)))

;; :decision-tree
(test ensure-decision-tree
  (with-new-decision-trees
    (is (typep (decision-tree "test") 'decision-tree))))

;; :question
;; :unknown
(test change-control-string
  (with-new-decision-trees
    (let ((decision-tree (decision-tree "test")))
      (setf (question decision-tree) "question~a~a")
      (setf (unknown decision-tree) "unknown")
      (is (equal (question decision-tree) "question~a~a"))
      (is (equal (unknown decision-tree) "unknown")))))

;; :name
;; :code
(test element-slots
  (with-new-decision-trees
    (let* ((code "some-code")
           (name "some-name")
           (desc '("description 1" "description 2"))
           (decision (make-instance 'decision :code code :name name))
           (criteria (make-instance 'criteria :code code :name name)))
      (is (string-equal (code decision) code))
      (is (string-equal (name decision) name))
      (is (string-equal (code criteria) code))
      (is (string-equal (name criteria) name))
      (is (equal (setf (code criteria) "code-1") "code-1"))
      (is (equal (setf (name decision) "name-1") "name-1"))
      (is (equal (setf (descriptions decision) desc) desc)))))

;; :decision-to-tree
;; :decision-from-tree
;; :remove-decision-from-tree
(test decision-to-and-from-tree
  (with-dummy-data (d3 d4 c1 c2 c3)
    ;; decision to and from tree 1 2 x 1 1, 2 2 x 2 1
    (is (eq (decision-to-tree "test" d1) (decision-from-tree "test" "d-1")))
    (is (eq (decision-to-tree decision-tree d2) (decision-from-tree decision-tree "d-2")))
    ;; remove decision in tree
    (is (remove-decision-from-tree "test" d1))
    (is (remove-decision-from-tree decision-tree "d-2"))))

;; :criteria-to-tree
;; :criteria-from-tree
;; :remove-criteria-from-tree
(test criteria-to-and-from-tree
  (with-dummy-data (d1 d2 d3 d4 c3)
    ;; criteria to and from - 1 2 x 1 1, 2 2 x 2 1
    (is (eq (criteria-to-tree "test" c1) (criteria-from-tree "test" "c-1")))
    (is (eq (criteria-to-tree decision-tree c2) (criteria-from-tree decision-tree "c-2")))
    ;; remove criteria in tree
    (is (remove-criteria-from-tree decision-tree "c-1"))
    (is (remove-criteria-from-tree "test" c2))))

(test criteria-code-to-decision-without-tree
  (with-dummy-data (decision-tree d2 d3 d4 c2 c3)
    (criteria-to-decision-in-tree nil d1 c1)
    (is (equal (criterions d1) '("c-1")))))

;; :criteria-to-decision-in-tree
;; :remove-criteria-from-decision-in-tree
(test criteria-code-to-decision-in-tree
  (with-dummy-data (d2 d3 d4 c3)
    ;; add and remove criteria code in decision
    (is (equal (criteria-to-decision-in-tree decision-tree d1 c1) '("c-1")))
    (is (null (remove-criteria-from-decision-in-tree decision-tree d1 c1)))
    (is (equal (criteria-to-decision-in-tree "test" d1 c1) '("c-1")))
    (is (null (remove-criteria-from-decision-in-tree "test" d1 c1)))
    (is (equal (criteria-to-decision-in-tree "test" "d-1" c2) '("c-2")))
    (is (null (remove-criteria-from-decision-in-tree "test" "d-1" c2)))
    (is (equal (criteria-to-decision-in-tree "test" "d-1" "c-1") '("c-1")))
    (is (null (remove-criteria-from-decision-in-tree "test" "d-1" "c-1")))
    (is (equal (criteria-to-decision-in-tree decision-tree "d-1" "c-1") '("c-1")))
    (is (null (remove-criteria-from-decision-in-tree decision-tree "d-1" "c-1")))
    (is (equal (criteria-to-decision-in-tree decision-tree d1 "c-1") '("c-1")))
    (is (null (remove-criteria-from-decision-in-tree decision-tree d1 "c-1")))))

(test side-effect-removing-criteria-from-tree
  (with-dummy-data (decision-tree d2 d3 d4)
    (criteria-to-decision-in-tree "test" d1 c1)
    (criteria-to-decision-in-tree "test" "d-1" c2)
    (criteria-to-decision-in-tree "test" "d-1" c3)
    (remove-criteria-from-tree "test" "c-2")
    (is (equal (criterions d1) '("c-3" "c-1")))))

;; :populate-relations
;; :relations
;; :criteria-codes
;; :count-criteria-code
;; :criteria-code
(test calculation-codes-in-relations
  (with-dummy-data ()
    (decision-to-tree "test" d1)
    (decision-to-tree "test" d2)
    (decision-to-tree "test" d3)
    (decision-to-tree "test" d4)
    (criteria-to-tree "test" c1)
    (criteria-to-tree "test" c2)
    (criteria-to-tree "test" c3)
    (criteria-to-decision-in-tree "test" "d-1" "c-1")
    (criteria-to-decision-in-tree "test" "d-1" "c-2")
    (criteria-to-decision-in-tree "test" "d-1" "c-3")
    (criteria-to-decision-in-tree "test" "d-2" "c-2")
    (criteria-to-decision-in-tree "test" "d-2" "c-3")
    (criteria-to-decision-in-tree "test" "d-3" "c-2")
    (criteria-to-decision-in-tree "test" "d-4" "c-3")
    ;; populate relations
    (is (equal (populate-relations decision-tree)
               '(("d-1" "c-3" "c-2" "c-1")
                 ("d-2" "c-3" "c-2")
                 ("d-3" "c-2")
                 ("d-4" "c-3"))))
    (is (equal (relations decision-tree)
               '(("d-1" "c-3" "c-2" "c-1")
                 ("d-2" "c-3" "c-2")
                 ("d-3" "c-2")
                 ("d-4" "c-3"))))
    (setf (relations decision-tree) '())
    (is (null (relations decision-tree)))
    (is (equal (populate-relations "test")
               '(("d-1" "c-3" "c-2" "c-1")
                 ("d-2" "c-3" "c-2")
                 ("d-3" "c-2")
                 ("d-4" "c-3"))))
    ;; available criteria codes
    (is (equal (criteria-codes "test") '("c-1" "c-2" "c-3")))
    (is (equal (criteria-codes decision-tree) '("c-1" "c-2" "c-3")))
    ;; count
    (is (= (count-criteria-code decision-tree "c-1") 1))
    (is (= (count-criteria-code decision-tree c1) 1))
    (is (= (count-criteria-code "test" "c-2") 3))
    (is (= (count-criteria-code "test" c2) 3))
    ;; modus criteria code
    (is (equal (criteria-code decision-tree) "c-2"))
    (is (equal (criteria-code "test") "c-2"))
    ;; ensure question
    (is (equal (question-criteria-code "test" "c-1") "(c-1) Is it have criteria one?"))
    (is (equal (question-criteria-code "test" c1) "(c-1) Is it have criteria one?"))
    (is (equal (question-criteria-code decision-tree "c-1") "(c-1) Is it have criteria one?"))
    (is (equal (question-criteria-code decision-tree c1) "(c-1) Is it have criteria one?"))
    ;; decision-tree slots
    (is (= (hash-table-count (criterions decision-tree)) 3))
    (is (= (hash-table-count (decisions decision-tree)) 4))))

;; :positive-answer
;; :negative-answer
;; :decision-from-answer
;; :decision-from-answers
;; :decision-from-relations
(test answer-response ()
  (with-dummy-data ()
    (decision-to-tree "test" d1)
    (decision-to-tree "test" d2)
    (decision-to-tree "test" d3)
    (decision-to-tree "test" d4)
    (criteria-to-tree "test" c1)
    (criteria-to-tree "test" c2)
    (criteria-to-tree "test" c3)
    (criteria-to-decision-in-tree "test" "d-1" "c-1")
    (criteria-to-decision-in-tree "test" "d-1" "c-2")
    (criteria-to-decision-in-tree "test" "d-1" "c-3")
    (criteria-to-decision-in-tree "test" "d-2" "c-2")
    (criteria-to-decision-in-tree "test" "d-2" "c-3")
    (criteria-to-decision-in-tree "test" "d-3" "c-2")
    (criteria-to-decision-in-tree "test" "d-4" "c-3")
    ;; before populate
    (is (equal (answer "test" c1) '("c-1" . nil)))
    (is (equal (answer "test" "c-1") '("c-1" . nil)))
    ;; positive negative answer
    (populate-relations decision-tree)
    (is (equal (relations decision-tree)
               '(("d-1" "c-3" "c-2" "c-1")
                 ("d-2" "c-3" "c-2")
                 ("d-3" "c-2")
                 ("d-4" "c-3"))))
    (positive-answer "test" "c-2")
    (is (equal (relations decision-tree)
               '(("d-1" "c-3" "c-1") ("d-2" "c-3") ("d-3"))))
    (negative-answer "test" "c-3")
    (is (equal (relations decision-tree) '(("d-3"))))
    ;; decision from answer
    (populate-relations decision-tree)
    (decision-from-answer "test" '("c-2" . t))
    (is (equal (relations decision-tree)
               '(("d-1" "c-3" "c-1") ("d-2" "c-3") ("d-3"))))
    (decision-from-answer "test" '("c-3" . nil))
    (is (equal (relations decision-tree) '(("d-3"))))
    ;; decision from answers
    (populate-relations decision-tree)
    (decision-from-answers "test" '(("c-2" . t) ("c-3" . nil)))
    (is (equal (relations decision-tree) '(("d-3"))))
    ;; decision from relations
    (is (equal (decision-from-relations decision-tree) d3))
    (is (equal (decision-from-relations "test") d3))
    ;; avoid miss behavior repeated answer
    (populate-relations decision-tree)
    (is (equal (decision-from-answers decision-tree '(("c-2" . t) ("c-2" . t) ("c-3" . nil))) d3))
    (is (equal (decision-from-answers "test" '(("c-2" . t) ("c-2" . t) ("c-3" . nil))) d3))))

(test stream-and-others
  (with-dummy-data (decision-tree d3 d4 c2 c3)
    (let ((message "unknown")
          (*output* (make-string-output-stream))
          (new-hash (make-hash-table :test 'equal)))
      (is (null (print-decision d1)))
      (is (null (print-object d2 *output*)))
      (is (null (print-object c1 *output*)))
      (setf (unknown (decision-tree "test")) message)
      (is (equal (decision-from-interactive "test") message))
      (is (eq (setf (decisions (decision-tree "test")) new-hash) new-hash)))))

;; :remove-decision-tree
;; :remove-all-decision-tree
(test remove-decision-tree
  (with-new-decision-trees
    (decision-tree "test")
    (is (remove-decision-tree "test"))
    (decision-tree "one")
    (decision-tree "two")
    (decision-tree "three")
    (remove-all-decision-tree)
    (is (= (hash-table-count *decision-trees*) 0))))
