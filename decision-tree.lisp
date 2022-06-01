(cl:in-package #:decision-tree)

;;;; IO handler

(defparameter *output* cl:*standard-output*)


;;;; CLASS decision-tree

(defclass decision-tree ()
  ((%decisions
    :initform (make-hash-table :test 'equal)
    :accessor decisions
    :documentation "Return hash table for decision collections.")
   (%criterions
    :initform (make-hash-table :test 'equal)
    :accessor criterions
    :documentation "Return hash table for criteria collections.")
   (%relations
    :type list
    :initform '()
    :accessor relations
    :documentation "Return list of relation codes for calculation.")
   (%records
    :type list
    :initform '()
    :accessor records
    :documentation "Return list of answered questions during calculation.")
   (%question
    :initform "(~A)~6TIs it have ~A?"
    :accessor question
    :documentation "Return string for question with two arguments.")
   (%unknown
    :initform "~%Can not make decision.~%"
    :accessor unknown
    :documentation "Return string for unknown message without arguments."))
  (:documentation
   "Holder for collections of decision-tree elements (decision,
criteria), including relations, records, and control-string for
question and unknown messages."))


;;;; Global decision-tree holder

(defvar *decision-trees* (make-hash-table :test 'equal)
  "Global variable to hold all decision-tree creations.")

(defun decision-tree (name)
  "Return decision-tree from *decision-trees* by NAME, or create new
one before return it."
  (or (gethash name *decision-trees*)
      (setf (gethash name *decision-trees*)
            (make-instance 'decision-tree))))

(defun remove-decision-tree (name)
  "Remove decision-tree by NAME from *decision-trees*."
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
    :documentation "Return string code for elements of decision-tree.")
   (%name
    :initarg :name
    :type string
    :initform ""
    :accessor name
    :documentation "Return string name for elements of decision-tree."))
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
    :documentation "Return list of criteria code for class decision.")
   (%descriptions
    :initarg :descriptions
    :type list
    :initform '()
    :accessor descriptions
    :documentation "Return list of string for descriptions."))
  (:documentation "Class of decision-tree elements."))


;;;; Generic function for decision-tree

;;; Get criteria object from tree (decision-tree).
;;; Parameter tree could be string or object.
;;; Parameter key has to be string.
;;; For additional comments bellow:
;;; 1 = string
;;; 2 = object
(defgeneric criteria-from-tree (tree key)
  (:documentation "Return criteria from decision-tree."))

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
  (:documentation "Add criteria to decision-tree.
TREE could be string or instance of decision-tree.
CRITERIA has to be instance of criteria."))

(defmethod criteria-to-tree ((key-tree string) (criteria criteria))
  ;; 1 2
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-to-tree decision-tree criteria)))

;;; Interupt overwrite criteria.
(defmethod criteria-to-tree :around ((decision-tree decision-tree)
                                     (criteria criteria))
  (if (null (criteria-from-tree decision-tree (code criteria)))
      (call-next-method)
      (format *output* "~%Code ~S already exists." (code criteria))))

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
  (:documentation "Return decision from decision-tree.
TREE could be string or instance of decision-tree.
KEY has to be a string."))

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
  (:documentation "Add decision to decision-tree.
TREE could be string or instance of decision-tree.
DECISION has to be instance of decision."))

(defmethod decision-to-tree ((key-tree string) (decision decision))
  ;; 1 2
  (let ((decision-tree (decision-tree key-tree)))
    (decision-to-tree decision-tree decision)))

;;; Interupt overwrite decision.
(defmethod decision-to-tree :around ((decision-tree decision-tree)
                                     (decision decision))
  (if (null (decision-from-tree decision-tree (code decision)))
      (call-next-method)
      (format *output* "~%Code ~S already exists." (code decision))))

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
  (:documentation "Add criteria code to DECISION. If CRITERIA instance
of criteria, it will try to be added in decision-tree."))

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
      (format *output* "~%Code ~S does not exists." key-decision)
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
      (format *output* "~%Code ~S does not exists." key-criteria)
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
          (format *output* "~%Code ~S already added to ~S."
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
      (format *output* "~%Code ~S does not exists." key-decision)
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
  (:documentation "Remove criteria code from DECISION. TREE could be
NIL if DECISION is instance of decision."))

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
  (:documentation "Remove criteria from decision-tree and all criteria
code from decision if exists."))

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
  (:documentation "Remove DECISION from decision-tree."))

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
  (:documentation "Return list of code for criteria from relations."))

(defmethod criteria-codes ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-codes decision-tree)))

(defmethod criteria-codes ((decision-tree decision-tree))
  (with-accessors ((relations relations)) decision-tree
    (criteria-codes relations)))

(defmethod criteria-codes ((relations list))
  (let* ((combined (apply #'append (mapcar #'cdr relations)))
         (unique (remove-duplicates combined :test #'equal)))
    (sort unique #'string-lessp)))

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
    (count-criteria-code relations key-criteria)))

(defmethod count-criteria-code ((relations list)
                                (code string))
  (loop for item in relations
        sum (count code item :test 'equal)))

;;; Get highest criteria in relations (modus operandi statistic).
(defgeneric criteria-code (tree)
  (:documentation "Return most code for criteria from relations."))

(defmethod criteria-code ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (criteria-code decision-tree)))

(defmethod criteria-code ((decision-tree decision-tree))
  (let ((relations (relations decision-tree)))
    (criteria-code relations)))

(defmethod criteria-code ((relations list))
  (loop with codes = (criteria-codes relations)
        for code in codes
        for amount = (count-criteria-code relations code)
        collect (list code amount) into counted
        finally (return (caar (sort counted #'> :key #'cadr)))))

;;; Question for code.
(defgeneric question-criteria-code (tree criteria)
  (:documentation "Return string question from criteria code."))

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
  (:documentation "Return final decision or more criteria code to be
asked. Exception for TREE as relations, it will return new
relations. ANSWER => (code . y-or-n)."))

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

(defmethod decision-from-answer ((relations list)
                                 (answer cons))
  ;; instead decision, it will return new relations
  (let ((key-criteria (car answer)))
    (if (cdr answer)
        (positive-answer relations key-criteria)
        (negative-answer relations key-criteria))))

;;; Get decision from answers.
(defgeneric decision-from-answers (tree answers)
  (:documentation "Return final decision or more criteria code to be
asked as a result from recorded answers. Exception for TREE as
relations, it will return new relations."))

(defmethod decision-from-answers ((key-tree string)
                                  (answers list))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-answers decision-tree answers)))

(defmethod decision-from-answers ((decision-tree decision-tree)
                                  (answers list))
  (dolist (item answers)
    (decision-from-answer decision-tree item))
  (decision-from-relations decision-tree))

(defmethod decision-from-answers ((relations list)
                                  (answers list))
  ;; instead decision, it will return new relations
  (dolist (item answers)
    (setf relations (decision-from-answer relations item)))
  relations)

;;; Get decision from relations
(defgeneric decision-from-relations (tree)
  (:documentation "Return final decision or more criteria code to be
asked as a result from relations. Exception for TREE as relations, it
will return final relations."))

(defmethod decision-from-relations ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-relations decision-tree)))

(defmethod decision-from-relations ((decision-tree decision-tree))
  (with-accessors ((relations relations)
                   (decisions decisions))
      decision-tree
    (let ((result (decision-from-relations relations)))
      (typecase result
        (null nil)
        (list (identity (gethash (caar result) decisions)))
        (t (criteria-code decision-tree))))))

(defmethod decision-from-relations ((relations list))
  ;; instead decision, it will return final relations
  (let ((length (length relations)))
    (case length
      (0 nil)
      (1 relations)
      (t (criteria-code relations)))))

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
    (setf relations (positive-answer relations key-criteria))))

(defmethod positive-answer ((relations list)
                           (code string))
  (loop for items in relations
        for found = (find code items :test 'equal)
        unless (null found)
          collect (remove code items :test 'equal)))

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
    (setf relations (negative-answer relations key-criteria))))

(defmethod negative-answer ((relations list)
                            (code string))
  (loop for items in relations
        for found = (find code items :test 'equal)
        when (null found)
          collect items))

;;; Get cons tree from decision tree.
(defparameter *code-tree-order* :c-n-y
  "Ordering code tree for C as CODE, N as NO and Y as YES.")
(declaim (type (member :c-n-y :c-y-n) *code-tree-order*))

(defgeneric code-tree (tree)
  (:documentation "Return cons tree of codes. TREE could be string,
instance of decision-tree, or relations."))

(defmethod code-tree ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (code-tree decision-tree)))

(defmethod code-tree ((decision-tree decision-tree))
  (let ((relations (populate-relations decision-tree)))
    (code-tree relations)))

(defmethod code-tree ((relations list))
  (labels ((update-relations (answers)
             (decision-from-answers relations answers))
           (building-process (code records)
             (let ((left (append records (list (list code))))
                   (right (append records (list (cons code t)))))
               (case *code-tree-order*
                 (:c-n-y (list code
                               (build-tree (update-relations left) left)
                               (build-tree (update-relations right) right)))
                 (:c-y-n (list code
                               (build-tree (update-relations right) right)
                               (build-tree (update-relations left) left))))))
           (build-tree (new-relations &optional answers)
             (let ((result (decision-from-relations new-relations)))
               (typecase result
                 (null "nil")
                 (list (caar result))
                 (string (building-process result answers))))))
    (build-tree relations)))

;;; Get decision from question-answer interactively.
;;; It must be populate first to get decision from interactive or it
;;; will calculate empty relations.
;;; Should be reimplementing this method for other packages.
(defgeneric decision-from-interactive (tree)
  (:documentation "Return decision after interactively asked
questions. Do not forget to run populate-relations once before this
function or series of this function."))

(defmethod decision-from-interactive ((key-tree string))
  (let ((decision-tree (decision-tree key-tree)))
    (decision-from-interactive decision-tree)))

(defmethod decision-from-interactive ((decision-tree decision-tree))
  (let* ((key-criteria (criteria-code decision-tree))
         (output (unless (null key-criteria)
                   (decision-from-answer
                    decision-tree (answer decision-tree key-criteria)))))
    (typecase output
      (decision output)
      (string (decision-from-interactive decision-tree))
      (t (format nil (unknown decision-tree))))))

;;; Presentate decision for human readable format.
;;; Should be reimplementing this method for other packages.
(defgeneric print-decision (decision)
  (:documentation "Print decision using format for human readable."))

(defmethod print-decision ((decision string))
  (format *output* decision))

(defmethod print-decision ((decision decision))
  (format *output* "~2%~A / ~A~2%~{~A~%~}"
          (name decision)
          (code decision)
          (descriptions decision)))

(defmethod print-object ((element element) stream)
  (print-unreadable-object (element stream :type nil :identity nil)
    (format stream "(~A)~6T~A" (code element) (name element))))

