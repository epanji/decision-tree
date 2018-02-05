;;;; decision-tree.lisp

(in-package #:decision-tree)


;;;; IO handler

(defparameter *output* cl:*standard-output*)


;;;; CLASS decision-tree

(defclass temporary ()
  ((records
    :type list
    :initform '()
    :accessor temporary-records)
   (relations
    :type list
    :initform '()
    :accessor temporary-relations))
  (:documentation "Temporary relations and records in association list."))

(defclass argument ()
  ((question
    :accessor argument-question
    :initform "(~a)~6tIs it have ~a?")
   (unknown
    :accessor argument-unknown
    :initform "~%Can not make decision.~%"))
  (:documentation "Format arguments in string."))

(defclass decision-tree (argument temporary)
  ((criterions
    :accessor decision-tree-criterions
    :initform (make-hash-table :test 'equal))
   (decisions
    :accessor decision-tree-decisions
    :initform (make-hash-table :test 'equal)))
  (:documentation "Holder for collections of decision-tree elements."))


;;;; Global decision-tree holder

(defvar *decision-trees* (make-hash-table :test 'equal)
  "A `hash-table' to hold all `decision-tree' creations.")

(defun decision-tree (name)
  "Get `decision-tree' from name or create new one."
  (or (gethash name *decision-trees*)
      (setf (gethash name *decision-trees*)
            (make-instance 'decision-tree))))

(defun remove-decision-tree (name)
  "Remove `decision-tree' by name from `*decision-trees*'."
  (remhash name *decision-trees*))

(defun remove-all-decision-tree ()
  "Remove all `decision-tree' from `*decision-trees*'."
  (clrhash *decision-trees*))


;;;; CLASS for decision-tree elements

(defclass base ()
  ((code
    :initarg :code
    :type string
    :initform ""
    :accessor base-code
    :documentation "A `decision' or `criteria' code.")
   (name
    :initarg :name
    :type string
    :initform ""
    :accessor base-name
    :documentation "A `decision' or `criteria' name."))
  (:documentation "Superclass for decision and criteria"))

(defclass criteria (base)
  ()
  (:documentation "Class with all slot inherit from `base'."))

(defclass decision (base)
  ((criterions
    :initarg :criterions
    :type list
    :initform '()
    :accessor decision-criterions)
   (descriptions
    :initarg :descriptions
    :type list
    :initform '()
    :accessor decision-descriptions))
  (:documentation "Class inheritance from `base' with additional slots."))


;;;; Generic function for decision-tree

;;; Get criteria object from tree (decision-tree).
;;; Parameter tree could be string or object.
;;; Parameter key has to be string.
(defgeneric criteria-from-tree (tree key)
  (:documentation "Generic function to get `criteria' from tree."))

;;; interupt decision-tree creation.
(defmethod criteria-from-tree :around ((key-tree string) (key-criteria string))
  (unless (null (gethash key-tree *decision-trees*))
    (call-next-method)))

(defmethod criteria-from-tree ((key-tree string) (key-criteria string))
  ;; 1 1
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-from-tree decision-tree key-criteria)))

(defmethod criteria-from-tree
    ((decision-tree decision-tree) (key-criteria string))
  ;; 2 1
  (with-slots (criterions) decision-tree
    (gethash key-criteria criterions)))

;;; Set criteria object to tree (decision-tree).
;;; Parameter tree could be string or object.
;;; Parameter criteria has to be criteria object.
(defgeneric criteria-to-tree (tree criteria)
  (:documentation "Generic function to add `criteria' to tree."))

(defmethod criteria-to-tree ((key-tree string) (criteria criteria))
  ;; 1 2
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-to-tree decision-tree criteria)))

;;; interupt overite criteria.
(defmethod criteria-to-tree :around
    ((decision-tree decision-tree) (criteria criteria))
  (if (null (criteria-from-tree decision-tree (base-code criteria)))
      (call-next-method)
      (format *output* "Code ~s already exists.~%" (base-code criteria))))

(defmethod criteria-to-tree
    ((decision-tree decision-tree) (criteria criteria))
  ;; 2 2
  (with-slots (criterions) decision-tree
    (setf (gethash (base-code criteria) criterions) criteria)))

;;; Get decision object from tree (decision-tree).
;;; Parameter tree could be string or object.
;;; Parameter key has to be string.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric decision-from-tree (tree key)
  (:documentation "Generic function to get `decision' from tree."))

;;; interupt decision-tree creation.
(defmethod decision-from-tree :around
    ((key-tree string) (key-decision string))
  (unless (null (gethash key-tree *decision-trees*))
    (call-next-method)))

(defmethod decision-from-tree ((key-tree string) (key-decision string))
  ;; 1 1
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-tree decision-tree key-decision)))

(defmethod decision-from-tree
    ((decision-tree decision-tree) (key-decision string))
  ;; 2 1
  (with-slots (decisions) decision-tree
    (gethash key-decision decisions)))

;;; Set decision object to tree (decision-tree).
;;; Parameter tree could be string or object.
;;; Parameter decision has to be a decision object.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric decision-to-tree (tree decision)
  (:documentation "Generic function to add `decision' to tree."))

(defmethod decision-to-tree ((key-tree string) (decision decision))
  ;; 1 2
  (let ((decision-tree (decision-tree key-tree)))
    (decision-to-tree decision-tree decision)))

;;; interupt overite decision.
(defmethod decision-to-tree :around
    ((decision-tree decision-tree) (decision decision))
  (if (null (decision-from-tree decision-tree (base-code decision)))
      (call-next-method)
      (format *output* "Code ~s already exists.~%" (base-code decision))))

(defmethod decision-to-tree
    ((decision-tree decision-tree) (decision decision))
  ;; 2 2
  (with-slots (decisions) decision-tree
    (setf (gethash (base-code decision) decisions) decision)))

;;; Set criteria to decision in tree (decision-tree) or not.
;;; It will add to tree (decision-tree) if exists.
;;; If parameter tree not string or decision-tree object, it will add
;;; criteria object to decision object. In this case decision and
;;; criteria must be an object.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric criteria-to-decision-in-tree (tree decision criteria)
  (:documentation "Generic function to add `criteria' code to decision."))

(defmethod criteria-to-decision-in-tree ((key-tree string)
                                         (key-decision string)
                                         (key-criteria string))
  ;; 1 1 1
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-to-decision-in-tree decision-tree key-decision key-criteria)))

(defmethod criteria-to-decision-in-tree
    :around ((decision-tree decision-tree)
             (key-decision string)
             (key-criteria string))
  (if (null (decision-from-tree decision-tree key-decision))
      (format *output* "Code ~s does not exists.~%" key-decision)
      (call-next-method)))

(defmethod criteria-to-decision-in-tree ((decision-tree decision-tree)
                                         (key-decision string)
                                         (key-criteria string))
  ;; 2 1 1
  (let ((decision (decision-from-tree decision-tree key-decision)))
    (criteria-to-decision-in-tree decision-tree decision key-criteria)))

(defmethod criteria-to-decision-in-tree ((key-tree string)
                                         (decision decision)
                                         (key-criteria string))
  ;; 1 2 1
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-to-decision-in-tree decision-tree decision key-criteria)))

(defmethod criteria-to-decision-in-tree
    :around ((decision-tree t)
             (decision decision)
             (key-criteria string))
  (if (null (criteria-from-tree decision-tree key-criteria))
      (format *output* "Code ~s does not exists.~%" key-criteria)
      (call-next-method)))

(defmethod criteria-to-decision-in-tree ((decision-tree t) ; accept all
                                         (decision decision)
                                         (key-criteria string))
  ;; 2 2 1
  ;; The reason type of decision tree set to T is to enable adding
  ;; just criteria code for decision instance without knowing which
  ;; tree the decision came from.
  (let ((criteria (criteria-from-tree decision-tree key-criteria)))
    (criteria-to-decision-in-tree decision-tree decision criteria)))

(defmethod criteria-to-decision-in-tree ((key-tree string)
                                         (decision decision)
                                         (criteria criteria))
  ;; 1 2 2
  (let ((decision-tree (decision-tree key-tree)))
    (decision-to-tree decision-tree decision)
    (criteria-to-tree decision-tree criteria)
    (criteria-to-decision-in-tree decision-tree decision criteria)))

(defmethod criteria-to-decision-in-tree ((decision-tree t) ; accept all
                                         (decision decision)
                                         (criteria criteria))
  ;; 2 2 2
  ;; The reason type of decision tree set to T is to enable adding
  ;; just criteria code for decision instance without knowing which
  ;; tree the decision came from.
  (with-slots (criterions) decision
    (let ((criteria-code (base-code criteria))
          (decision-code (base-code decision)))
      (if (null (find criteria-code criterions :test 'equal))
          (pushnew criteria-code criterions)
          (format *output* "Code ~s already added to ~s.~%"
                  criteria-code decision-code)))))

(defmethod criteria-to-decision-in-tree ((key-tree string)
                                         (key-decision string)
                                         (criteria criteria))
  ;; 1 1 2
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-to-decision-in-tree decision-tree key-decision criteria)))

(defmethod criteria-to-decision-in-tree
    :around ((decision-tree decision-tree)
             (key-decision string)
             (criteria criteria))
  (if (null (decision-from-tree decision-tree key-decision))
      (format *output* "Code ~s does not exists.~%" key-decision)
      (call-next-method)))

(defmethod criteria-to-decision-in-tree ((decision-tree decision-tree)
                                         (key-decision string)
                                         (criteria criteria))
  ;; 2 1 2
  (let ((decision (decision-from-tree decision-tree key-decision)))
    (criteria-to-tree decision-tree criteria)
    (criteria-to-decision-in-tree decision-tree decision criteria)))

;;; Remove criteria from decision in tree (decision-tree) or not.
;;; If parameter tree not string or decision-tree object, it will
;;; remove criteria object from decision object. In this case decision
;;; and criteria must be an object.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric remove-criteria-from-decision-in-tree (tree decision criteria)
  (:documentation
   "Generic function to remove `criteria' code from decision."))

(defmethod remove-criteria-from-decision-in-tree ((key-tree string)
                                                  (key-decision string)
                                                  (key-criteria string))
  ;; 1 1 1
  (let ((decision-tree (decision-tree key-tree)))
    (remove-criteria-from-decision-in-tree
     decision-tree key-decision key-criteria)))

(defmethod remove-criteria-from-decision-in-tree ((key-tree string)
                                                  (key-decision string)
                                                  (criteria criteria))
  ;; 1 1 2
  (let ((decision-tree (decision-tree key-tree)))
    (remove-criteria-from-decision-in-tree
     decision-tree key-decision criteria)))

(defmethod remove-criteria-from-decision-in-tree
    ((decision-tree decision-tree)
     (key-decision string)
     (key-criteria string))
  ;; 2 1 1
  (let ((decision (decision-from-tree decision-tree key-decision)))
    (remove-criteria-from-decision-in-tree
     decision-tree decision key-criteria)))

(defmethod remove-criteria-from-decision-in-tree
    ((decision-tree decision-tree)
     (key-decision string)
     (criteria criteria))
  ;; 2 1 2
  (let ((decision (decision-from-tree decision-tree key-decision)))
    (remove-criteria-from-decision-in-tree
     decision-tree decision criteria)))

(defmethod remove-criteria-from-decision-in-tree ((decision-tree t)
                                                  (decision decision)
                                                  (criteria criteria))
  ;; 1 2 2 / 2 2 2
  ;; The reason type of decision-tree set to T is to enable removing
  ;; criteria code from decision instance without knowing from which
  ;; tree the decision came from with nil as parameter.
  (let ((key-criteria (base-code criteria)))
    (remove-criteria-from-decision-in-tree
     decision-tree decision key-criteria)))

(defmethod remove-criteria-from-decision-in-tree ((decision-tree t)
                                                  (decision decision)
                                                  (key-criteria string))
  ;; 1 2 1 / 2 2 1
  ;; The reason type of decision-tree set to T is to enable removing
  ;; criteria code from decision instance without knowing from which
  ;; tree the decision came from with nil as parameter.
  (with-slots (criterions) decision
    (flet ((predicate (item) (string-equal item key-criteria)))
      (setf criterions (remove-if #'predicate criterions)))))

;; Remove criteria from tree with side effect remove all existed
;; criteria in decision criterions.
(defgeneric remove-criteria-from-tree (tree criteria)
  (:documentation
   "Generic function to remove `criteria' from `decision-tree' and all \
criteria code from `decision' if exists."))

(defmethod remove-criteria-from-tree
    ((decision-tree decision-tree) (criteria criteria))
  (remove-criteria-from-tree decision-tree (base-code criteria)))

(defmethod remove-criteria-from-tree ((key-tree string) (criteria criteria))
  (let ((decision-tree (decision-tree key-tree)))
    (remove-criteria-from-tree decision-tree criteria)))

(defmethod remove-criteria-from-tree
    ((key-tree string) (key-criteria string))
  (let ((decision-tree (decision-tree key-tree)))
    (remove-criteria-from-tree decision-tree key-criteria)))

(defmethod remove-criteria-from-tree
    ((decision-tree decision-tree) (key-criteria string))
  (with-slots (criterions decisions) decision-tree
    (unless (null (gethash key-criteria criterions))
      (loop for decision being the hash-value in decisions
            do (remove-criteria-from-decision-in-tree decision-tree
                                                      decision
                                                      key-criteria))
      (remhash key-criteria criterions))))

;;; Remove decision from tree (decision-tree).
(defgeneric remove-decision-from-tree (tree decision)
  (:documentation
   "Generic function to remove `decision' from `decision-tree'."))

(defmethod remove-decision-from-tree ((key-tree string)
                                      (key-decision string))
  (let ((decision-tree (decision-tree key-tree)))
    (remove-decision-from-tree decision-tree key-decision)))

(defmethod remove-decision-from-tree ((key-tree string)
                                      (decision decision))
  (let ((decision-tree (decision-tree key-tree)))
    (remove-decision-from-tree decision-tree decision)))

(defmethod remove-decision-from-tree ((decision-tree decision-tree)
                                      (decision decision))
  (let ((key-decision (base-code decision)))
    (remove-decision-from-tree decision-tree key-decision)))

(defmethod remove-decision-from-tree ((decision-tree decision-tree)
                                      (key-decision string))
  (with-slots (decisions) decision-tree
    (remhash key-decision decisions)))

;;; Populate temporary relations.
(defgeneric populate-temporary-relations (tree)
  (:documentation "Populate slot relations with association list."))

(defmethod populate-temporary-relations ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (populate-temporary-relations decision-tree)))

(defmethod populate-temporary-relations
    :before ((decision-tree decision-tree))
  (setf (temporary-records decision-tree) '()))

(defmethod populate-temporary-relations ((decision-tree decision-tree))
  (with-slots (decisions relations) decision-tree
    (setf relations
          (loop for decision being the hash-value in decisions
                for code = (base-code decision)
                for codes = (decision-criterions decision)
                collect (cons code codes)))))


;;;; Implementation about getting decision from answered question.

;;; Get criteria codes from relations in list (no duplicate).
(defgeneric criteria-codes (tree)
  (:documentation "Get list of codes for criteria from relations."))

(defmethod criteria-codes ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-codes decision-tree)))

(defmethod criteria-codes ((decision-tree decision-tree))
  (with-slots (relations) decision-tree
    (let* ((combined (apply #'append (mapcar #'cdr relations)))
           (unique (remove-duplicates combined :test #'equal)))
      (sort unique #'string-lessp))))

;;; Count criteria code appeared in relations.
(defgeneric count-criteria-code (tree criteria)
  (:documentation "Count for criteria code inside relations."))

(defmethod count-criteria-code ((key-tree string)
                                (key-criteria string))
  (let ((decision-tree (decision-tree key-tree)))
    (count-criteria-code decision-tree key-criteria)))

(defmethod count-criteria-code ((key-tree string)
                                (criteria criteria))
  (let ((decision-tree (decision-tree key-tree)))
    (count-criteria-code decision-tree criteria)))

(defmethod count-criteria-code ((decision-tree decision-tree)
                                (criteria criteria))
  (let ((key-criteria (base-code criteria)))
    (count-criteria-code decision-tree key-criteria)))

(defmethod count-criteria-code ((decision-tree decision-tree)
                                (key-criteria string))
  (with-slots (relations) decision-tree
    (loop for item in relations
          sum (count key-criteria item :test 'equal))))

;;; Get highest criteria in relations (modus operandi statistic).
(defgeneric criteria-code (tree)
  (:documentation "Get most code for criteria from relations."))

(defmethod criteria-code ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-code decision-tree)))

(defmethod criteria-code ((decision-tree decision-tree))
  (loop with codes = (criteria-codes decision-tree)
        for code in codes
        for amount = (count-criteria-code decision-tree code)
        collect (list code amount) into counted
        finally (return (caar (sort counted #'> :key #'cadr)))))

;;; Question for code
(defgeneric question-criteria-code (tree criteria)
  (:documentation "Get question from criteria code."))

(defmethod question-criteria-code ((key-tree string)
                                   (key-criteria string))
  (let ((decision-tree (decision-tree key-tree)))
    (question-criteria-code decision-tree key-criteria)))

(defmethod question-criteria-code ((key-tree string)
                                   (criteria criteria))
  (let ((decision-tree (decision-tree key-tree)))
    (question-criteria-code decision-tree criteria)))

(defmethod question-criteria-code ((decision-tree decision-tree)
                                   (key-criteria string))
  (let ((criteria (criteria-from-tree decision-tree key-criteria)))
    (question-criteria-code decision-tree criteria)))

(defmethod question-criteria-code ((decision-tree decision-tree)
                                   (criteria criteria))
  (format nil (argument-question decision-tree)
          (base-code criteria)
          (string-downcase (base-name criteria))))

;;; Get answer from user input.
(defgeneric answer (tree criteria)
  (:documentation "Standard y or n question to get answer for criteria."))

(defmethod answer ((key-tree string) (key-criteria string))
  (let ((decision-tree (decision-tree key-tree)))
    (answer decision-tree key-criteria)))

(defmethod answer ((key-tree string) (criteria criteria))
  (let ((decision-tree (decision-tree key-tree)))
    (answer decision-tree criteria)))

(defmethod answer ((decision-tree decision-tree) (criteria criteria))
  (let ((key-criteria (base-code criteria)))
    (answer decision-tree key-criteria)))

(defmethod answer ((decision-tree decision-tree) (key-criteria string))
  (cons key-criteria
        (unless (null (temporary-relations decision-tree))
          (y-or-n-p (question-criteria-code decision-tree key-criteria)))))

;;; Get decision or more code from answer.
(defgeneric decision-from-answer (tree answer)
  (:documentation "Get final `decision' or more criteria code to be asked. \
Answer => (code . y-or-n)"))

(defmethod decision-from-answer ((key-tree string)
                                 (answer cons))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-answer decision-tree answer)))

(defmethod decision-from-answer
    :before ((decision-tree decision-tree) (answer cons))
  (with-slots (records) decision-tree
    (setf records (append records (list answer)))))

(defmethod decision-from-answer ((decision-tree decision-tree)
                                 (answer cons))
  (let ((key-criteria (car answer)))
    (if (cdr answer)
        (positive-answer decision-tree key-criteria)
        (negative-answer decision-tree key-criteria))
    (decision-from-relations decision-tree)))

;;; Get decision from answers.
(defgeneric decision-from-answers (tree answers)
  (:documentation "Get result from recorded answers."))

(defmethod decision-from-answers ((key-tree string)
                                  (answers list))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-answers decision-tree answers)))

(defmethod decision-from-answers ((decision-tree decision-tree)
                                  (answers list))
  (dolist (item answers)
    (decision-from-answer decision-tree item))
  (decision-from-relations decision-tree))

;;; Get decision from relations
(defgeneric decision-from-relations (tree)
  (:documentation "Get `decision' from relations or code criteria."))

(defmethod decision-from-relations ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-relations decision-tree)))

(defmethod decision-from-relations ((decision-tree decision-tree))
  (with-slots (relations decisions) decision-tree
    (let ((length (length relations)))
      (case length
        (0 nil)
        (1 (gethash (caar relations) decisions))
        (t (criteria-code decision-tree))))))

;;; Process positive response
(defgeneric positive-answer (tree criteria)
  (:documentation "Process for positive response."))

(defmethod positive-answer ((key-tree string)
                            (key-criteria string))
  (let ((decision-tree (decision-tree key-tree)))
    (positive-answer decision-tree key-criteria)))

(defmethod positive-answer ((key-tree string)
                            (criteria criteria))
  (let ((decision-tree (decision-tree key-tree)))
    (positive-answer decision-tree criteria)))

(defmethod positive-answer ((decision-tree decision-tree)
                            (criteria criteria))
  (let ((key-criteria (base-code criteria)))
    (positive-answer decision-tree key-criteria)))

(defmethod positive-answer ((decision-tree decision-tree)
                            (key-criteria string))
  (with-slots (relations) decision-tree
    (setf relations
          (loop for items in relations
                for found = (find key-criteria items :test 'equal)
                unless (null found)
                  collect (remove key-criteria items :test 'equal)))))

;;; Process negative response
(defgeneric negative-answer (tree criteria)
  (:documentation "Process for negative response."))

(defmethod negative-answer ((key-tree string)
                            (key-criteria string))
  (let ((decision-tree (decision-tree key-tree)))
    (negative-answer decision-tree key-criteria)))

(defmethod negative-answer ((key-tree string)
                            (criteria criteria))
  (let ((decision-tree (decision-tree key-tree)))
    (negative-answer decision-tree criteria)))

(defmethod negative-answer ((decision-tree decision-tree)
                            (criteria criteria))
  (let ((key-criteria (base-code criteria)))
    (negative-answer decision-tree key-criteria)))

(defmethod negative-answer ((decision-tree decision-tree)
                            (key-criteria string))
  (with-slots (relations) decision-tree
    (setf relations
          (loop for items in relations
                for found = (find key-criteria items :test 'equal)
                when (null found)
                  collect items))))

;;; Get decision from question-answer interactively.
;;; It must be populate first to get decision from interactive or it
;;; will calculate empty relations.
;;; Should be reimplementing this method for other packages.
(defgeneric decision-from-interactive (tree)
  (:documentation
   "Interactive question to get `decision'. Do not forget to run
 `populate-temporary-relations' once before this function or series
 of this function."))

(defmethod decision-from-interactive ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-interactive decision-tree)))

(defmethod decision-from-interactive ((decision-tree decision-tree))
  (let* ((key-criteria (criteria-code decision-tree))
         (output (unless (null key-criteria)
                   (decision-from-answer
                    decision-tree (answer decision-tree key-criteria)))))
    (cond ((typep output 'decision) output)
          ((typep output 'string) (decision-from-interactive decision-tree))
          (t (format *output* (argument-unknown decision-tree))))))

;;; Presentate decision for human readable format.
;;; Should be reimplementing this method for other packages.
(defgeneric print-decision (decision)
  (:documentation "Print decision using format for human readable."))

(defmethod print-decision ((decision string))
  (format *output* decision))

(defmethod print-decision ((decision decision))
  (format *output* "~2%~a / ~a~2%~{~a~%~}"
          (base-name decision)
          (base-code decision)
          (decision-descriptions decision)))

;;;; decision-tree.lisp ends here
