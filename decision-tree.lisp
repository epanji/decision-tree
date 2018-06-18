;;;; decision-tree.lisp

(in-package #:decision-tree)


;;;; IO handler

(defparameter *output* cl:*standard-output*)


;;;; CLASS decision-tree

(defclass decision-tree ()
  ((%decisions
    :initform (make-hash-table :test 'equal)
    :accessor decisions
    :documentation "Hash table for decision collections.")
   (%criterions
    :initform (make-hash-table :test 'equal)
    :accessor criterions
    :documentation "Hash table for criteria collections.")
   (%relations
    :type list
    :initform '()
    :accessor relations
    :documentation "List of codes (decision criterions) for calculation.")
   (%records
    :type list
    :initform '()
    :accessor records
    :documentation "List of answered questions during calculation.")
   (%question
    :initform "(~a)~6tIs it have ~a?"
    :accessor question
    :documentation "String for question with two input.")
   (%unknown
    :initform "~%Can not make decision.~%"
    :accessor unknown
    :documentation "String for unknown message without input."))
  (:documentation
   "Holder for collections of decision-tree elements (decision,
criteria), including relations, records, and control-string for
question and unknown messages."))


;;;; Global decision-tree holder

(defvar *decision-trees* (make-hash-table :test 'equal)
  "Global variable to hold all decision-tree creations.")

(defun decision-tree (name)
  "Function to get decision-tree by name or create new one."
  (or (gethash name *decision-trees*)
      (setf (gethash name *decision-trees*)
            (make-instance 'decision-tree))))

(defun remove-decision-tree (name)
  "Remove decision-tree by name from *decision-trees*."
  (remhash name *decision-trees*))

(defun remove-all-decision-tree ()
  "Remove all decision-tree from *decision-trees*."
  (clrhash *decision-trees*))


;;;; CLASS for decision-tree elements

(defclass element ()
  ((%code
    :initarg :code
    :type string
    :initform ""
    :accessor code
    :documentation "String code for elements of decision-tree.")
   (%name
    :initarg :name
    :type string
    :initform ""
    :accessor name
    :documentation "String name for elements of decision-tree."))
  (:documentation "Abstract class for decision-tree elements."))

(defclass criteria (element)
  ()
  (:documentation "Class of decision-tree elements."))

(defclass decision (element)
  ((%criterions
    :initarg :criterions
    :type list
    :initform '()
    :accessor criterions
    :documentation "List of criteria codes for class decision.")
   (%descriptions
    :initarg :descriptions
    :type list
    :initform '()
    :accessor descriptions
    :documentation "List of string for descriptions."))
  (:documentation "Class of decision-tree elements."))


;;;; Generic function for decision-tree

;;; Get criteria object from tree (decision-tree).
;;; Parameter tree could be string or object.
;;; Parameter key has to be string.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric criteria-from-tree (tree key)
  (:documentation
   "Generic function to get criteria from decision-tree."))

;;; Interupt decision-tree creation when trying to get criteria.
(defmethod criteria-from-tree :around ((key-tree string)
                                       (key-criteria string))
  (unless (null (gethash key-tree *decision-trees*))
    (call-next-method)))

(defmethod criteria-from-tree ((key-tree string) (key-criteria string))
  ;; 1 1
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-from-tree decision-tree key-criteria)))

(defmethod criteria-from-tree ((decision-tree decision-tree)
                               (key-criteria string))
  ;; 2 1
  (with-accessors ((criterions criterions)) decision-tree
    (gethash key-criteria criterions)))

;;; Set criteria object to tree (decision-tree).
;;; Parameter tree could be string or object.
;;; Parameter criteria has to be criteria object.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric criteria-to-tree (tree criteria)
  (:documentation "Generic function to add criteria to decision-tree."))

(defmethod criteria-to-tree ((key-tree string) (criteria criteria))
  ;; 1 2
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-to-tree decision-tree criteria)))

;;; Interupt overwrite criteria.
(defmethod criteria-to-tree :around ((decision-tree decision-tree)
                                     (criteria criteria))
  (if (null (criteria-from-tree decision-tree (code criteria)))
      (call-next-method)
      (format *output* "~%Code ~s already exists." (code criteria))))

(defmethod criteria-to-tree ((decision-tree decision-tree)
                             (criteria criteria))
  ;; 2 2
  (with-accessors ((criterions criterions)) decision-tree
    (setf (gethash (code criteria) criterions) criteria)))

;;; Get decision object from tree (decision-tree).
;;; Parameter tree could be string or object.
;;; Parameter key has to be string.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric decision-from-tree (tree key)
  (:documentation "Generic function to get decision from decision-tree."))

;;; Interupt decision-tree creation when trying to get decision.
(defmethod decision-from-tree :around ((key-tree string)
                                       (key-decision string))
  (unless (null (gethash key-tree *decision-trees*))
    (call-next-method)))

(defmethod decision-from-tree ((key-tree string) (key-decision string))
  ;; 1 1
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-tree decision-tree key-decision)))

(defmethod decision-from-tree ((decision-tree decision-tree)
                               (key-decision string))
  ;; 2 1
  (with-accessors ((decisions decisions)) decision-tree
    (gethash key-decision decisions)))

;;; Set decision object to tree (decision-tree).
;;; Parameter tree could be string or object.
;;; Parameter decision has to be a decision object.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric decision-to-tree (tree decision)
  (:documentation "Generic function to add decision to tree."))

(defmethod decision-to-tree ((key-tree string) (decision decision))
  ;; 1 2
  (let ((decision-tree (decision-tree key-tree)))
    (decision-to-tree decision-tree decision)))

;;; Interupt overwrite decision.
(defmethod decision-to-tree :around ((decision-tree decision-tree)
                                     (decision decision))
  (if (null (decision-from-tree decision-tree (code decision)))
      (call-next-method)
      (format *output* "~%Code ~s already exists." (code decision))))

(defmethod decision-to-tree ((decision-tree decision-tree)
                             (decision decision))
  ;; 2 2
  (with-accessors ((decisions decisions)) decision-tree
    (setf (gethash (code decision) decisions) decision)))

;;; Set criteria to decision in tree (decision-tree) or not.
;;; It will add to tree (decision-tree) if exists.
;;; If parameter tree not string or decision-tree object, it will add
;;; criteria object to decision object. In this case decision and
;;; criteria must be an object.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric criteria-to-decision-in-tree (tree decision criteria)
  (:documentation "Generic function to add criteria code to DECISION.
If CRITERIA instance of criteria, it will try to be added in
decision-tree."))

(defmethod criteria-to-decision-in-tree ((key-tree string)
                                         (key-decision string)
                                         (key-criteria string))
  ;; 1 1 1
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-to-decision-in-tree decision-tree
                                  key-decision
                                  key-criteria)))

(defmethod criteria-to-decision-in-tree
    :around ((decision-tree decision-tree)
             (key-decision string)
             (key-criteria string))
  (if (null (decision-from-tree decision-tree key-decision))
      (format *output* "~%Code ~s does not exists." key-decision)
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
      (format *output* "~%Code ~s does not exists." key-criteria)
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
  (with-accessors ((criterions criterions)) decision
    (let ((criteria-code (code criteria))
          (decision-code (code decision)))
      (if (null (find criteria-code criterions :test 'equal))
          (pushnew criteria-code criterions)
          (format *output* "~%Code ~s already added to ~s."
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
      (format *output* "~%Code ~s does not exists." key-decision)
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
   "Generic function to remove criteria code from decision. The
decision could be inside or outside decision-tree."))

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
  (let ((key-criteria (code criteria)))
    (remove-criteria-from-decision-in-tree
     decision-tree decision key-criteria)))

(defmethod remove-criteria-from-decision-in-tree ((decision-tree t)
                                                  (decision decision)
                                                  (key-criteria string))
  ;; 1 2 1 / 2 2 1
  ;; The reason type of decision-tree set to T is to enable removing
  ;; criteria code from decision instance without knowing from which
  ;; tree the decision came from with nil as parameter.
  (with-accessors ((criterions criterions)) decision
    (flet ((predicate (item) (string-equal item key-criteria)))
      (setf criterions (remove-if #'predicate criterions)))))

;; Remove criteria from tree with side effect remove all existed
;; criteria in decision criterions.
(defgeneric remove-criteria-from-tree (tree criteria)
  (:documentation
   "Generic function to remove criteria from decision-tree and all
criteria code from decision if exists."))

(defmethod remove-criteria-from-tree ((decision-tree decision-tree)
                                      (criteria criteria))
  (remove-criteria-from-tree decision-tree (code criteria)))

(defmethod remove-criteria-from-tree ((key-tree string)
                                      (criteria criteria))
  (let ((decision-tree (decision-tree key-tree)))
    (remove-criteria-from-tree decision-tree criteria)))

(defmethod remove-criteria-from-tree ((key-tree string)
                                      (key-criteria string))
  (let ((decision-tree (decision-tree key-tree)))
    (remove-criteria-from-tree decision-tree key-criteria)))

(defmethod remove-criteria-from-tree ((decision-tree decision-tree)
                                      (key-criteria string))
  (with-accessors ((criterions criterions) (decisions decisions))
      decision-tree
    (unless (null (gethash key-criteria criterions))
      (loop for decision being the hash-value in decisions
            do (remove-criteria-from-decision-in-tree decision-tree
                                                      decision
                                                      key-criteria))
      (remhash key-criteria criterions))))

;;; Remove decision from tree (decision-tree).
(defgeneric remove-decision-from-tree (tree decision)
  (:documentation
   "Generic function to remove decision from decision-tree."))

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
  (let ((key-decision (code decision)))
    (remove-decision-from-tree decision-tree key-decision)))

(defmethod remove-decision-from-tree ((decision-tree decision-tree)
                                      (key-decision string))
  (with-accessors ((decisions decisions)) decision-tree
    (remhash key-decision decisions)))

;;; Populate temporary relations.
(defgeneric populate-relations (tree)
  (:documentation "Populate slot relations with association list."))

(defmethod populate-relations ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (populate-relations decision-tree)))

(defmethod populate-relations
    :before ((decision-tree decision-tree))
  (setf (records decision-tree) '()))

(defmethod populate-relations ((decision-tree decision-tree))
  (with-accessors ((decisions decisions)
                   (relations relations))
      decision-tree
    (setf relations
          (loop for decision being the hash-value in decisions
                for code = (code decision)
                for codes = (criterions decision)
                collect (cons code codes)))))


;;;; Implementation about getting decision from answered question.

;;; Get criteria codes from relations in list (no duplicate).
(defgeneric criteria-codes (tree)
  (:documentation "Get list of codes for criteria from relations."))

(defmethod criteria-codes ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-codes decision-tree)))

(defmethod criteria-codes ((decision-tree decision-tree))
  (with-accessors ((relations relations)) decision-tree
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
  (let ((key-criteria (code criteria)))
    (count-criteria-code decision-tree key-criteria)))

(defmethod count-criteria-code ((decision-tree decision-tree)
                                (key-criteria string))
  (with-accessors ((relations relations)) decision-tree
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
  (format nil (question decision-tree)
          (code criteria)
          (string-downcase (name criteria))))

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
  (let ((key-criteria (code criteria)))
    (answer decision-tree key-criteria)))

(defmethod answer ((decision-tree decision-tree) (key-criteria string))
  (cons key-criteria
        (unless (null (relations decision-tree))
          (y-or-n-p (question-criteria-code decision-tree key-criteria)))))

;;; Get decision or more code from answer.
(defgeneric decision-from-answer (tree answer)
  (:documentation "Get final decision or more criteria code to be asked.
Answer => (code . y-or-n)"))

(defmethod decision-from-answer ((key-tree string)
                                 (answer cons))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-answer decision-tree answer)))

(defmethod decision-from-answer :around ((decision-tree decision-tree)
                                         (answer cons))
  (let ((recordable-p (null (find (car answer)
                                  (records decision-tree)
                                  :key #'car
                                  :test #'equal))))
    (when recordable-p
      (with-accessors ((records records)) decision-tree
        (setf records (append records (list answer))))
      (call-next-method))
    (decision-from-relations decision-tree)))

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
  (:documentation "Get decision from relations or code criteria."))

(defmethod decision-from-relations ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-relations decision-tree)))

(defmethod decision-from-relations ((decision-tree decision-tree))
  (with-accessors ((relations relations)
                   (decisions decisions))
      decision-tree
    (let ((length (length relations)))
      (case length
        (0 nil)
        (1 (identity (gethash (caar relations) decisions)))
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
  (let ((key-criteria (code criteria)))
    (positive-answer decision-tree key-criteria)))

(defmethod positive-answer ((decision-tree decision-tree)
                            (key-criteria string))
  (with-accessors ((relations relations)) decision-tree
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
  (let ((key-criteria (code criteria)))
    (negative-answer decision-tree key-criteria)))

(defmethod negative-answer ((decision-tree decision-tree)
                            (key-criteria string))
  (with-accessors ((relations relations)) decision-tree
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
  (:documentation "Generic function to get decision interactively.
Do not forget to run populate-relations once before this
function or series of this function."))

(defmethod decision-from-interactive ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-interactive decision-tree)))

(defmethod decision-from-interactive ((decision-tree decision-tree))
  (let* ((key-criteria (criteria-code decision-tree))
         (output (unless (null key-criteria)
                   (decision-from-answer
                    decision-tree (answer decision-tree key-criteria)))))
    (cond ((typep output 'decision) output)
          ((typep output 'string)
           (decision-from-interactive decision-tree))
          (t (format nil (unknown decision-tree))))))

;;; Presentate decision for human readable format.
;;; Should be reimplementing this method for other packages.
(defgeneric print-decision (decision)
  (:documentation "Print decision using format for human readable."))

(defmethod print-decision ((decision string))
  (format *output* decision))

(defmethod print-decision ((decision decision))
  (format *output* "~2%~a / ~a~2%~{~a~%~}"
          (name decision)
          (code decision)
          (descriptions decision)))

(defmethod print-object ((element element) stream)
  (print-unreadable-object (element stream :type nil :identity nil)
    (format stream "(~a)~6t~a" (code element) (name element))))
