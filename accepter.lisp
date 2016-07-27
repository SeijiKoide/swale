(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ----------------------------------------------------------
;;; Micro TWEAKER
;;; ----------------------------------------------------------
;;; Programmer: David Leake, Alex Kass, Chris Riesbeck

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "mops")
  (require "swale-print-utils")
  )

;;; Explanation evaluation
;;; ----------------------------------------------------------
;;;
;;; (EVALUATE-EXPL explanation) => problems
;;;   Collects problems with the explanation.

(defun evaluate-expl (expl purpose)
  (headline "Micro-Accepter")
  (mainline "Evaluating ~S for the purpose ~S." expl purpose)
  (mainline "The underling XP is")
  (print-mop (<- expl :xp))
  (summarize-results
   (collect-problems expl purpose)
   "Explanation has no problem."))

(defun collect-problems (expl purpose)
  (append (relevance-problems expl)
          (believability-problems expl)
          (usefulness-problems expl purpose)))

;;; Problem evaluation
;;; ----------------------------------------------------------
;;;
;;; Classifies a list of problems as acceptable, tweakable, or 
;;; hopeless.

(defun explanation-status (expl problems)
  (cond ((null problems) 'acceptable)
        (t
         (mainline "Checking seriousness of problems.")
         (summarize-results
          (reduce #'worse-problem-status problems
                  :initial-value 'acceptable)
          "Couldn't determine status!"
          (format nil "... ~S is ~~S." expl)))))

(defun worse-problem-status (status problem)
  (let ((problem-status (<- problem :status)))
    (cond ((worse-status-p problem-status status)
           problem-status)
          (t status))))

(defun worse-status-p (status1 status2)
  (and status1
       (let ((status-scale '(hopeless tweakable acceptable)))
         (< (position status1 status-scale)
            (position status2 status-scale)))))

;;; Relevance
;;; ----------------------------------------------------------

(defun relevance-problems (expl)
  (let ((fact (<- expl :story :anomaly :surprising-fact)))
    (mainline "Checking if explanation accounts for ~S."
              fact)
    (summarize-results (expl-relevance-problems expl fact)
                       "... appears relevant.")))

(defun expl-relevance-problems (expl fact)
  (cond ((abstp (<- expl :xp :events :outcome) fact)
         '())
        (t
         (list (new-problem
                'm-irrelevant-to-surprizing-feature-problem
                :explanation expl
                :feature-unaccounted-for fact)))))

;;; Explanation believability
;;; ----------------------------------------------------------

(defun believability-problems (expl)
  (mainline "Checking believability.")
  (summarize-results (expl-constraints-problems expl)
                     "... no problems."
                     "... explanation is not believable."))

;;; Apply XP constraints to the known outcome
;;; ----------------------------------------------------------

(defun expl-constraints-problems (expl)
  (let ((fact (<- expl :story :anomaly :surprising-fact)))
    (loop for constraint in (<- expl :xp :constraints)
        append (constraint-problems constraint fact expl))))

(defun constraint-problems (constraint fact expl)
  (loop for path in constraint
      when (outcome-path-p path)
      append (apply-constraint constraint fact expl
                               (outcome-path-path path))))

;;; Collect the constraint paths referring to the outcome
;;; ----------------------------------------------------------

(defun outcome-paths (constraint)
  (loop for path in constraint
      when (outcome-path-p path)
      collect (outcome-path-path path)))

(defun outcome-path-p (path)
  (and (consp path)
       (eql (first path) :events)
       (eql (second path) :outcome)))

(defun outcome-path-path (path)
  (rest (rest path)))


;;; Apply the constraint to a particular component of fact
;;; ----------------------------------------------------------

;;; Note: since the fact(s) refer to what was true, and the
;;; outcome refers to what has become true, don't use outcome
;;; related paths as constraints.

(defun apply-constraint (constraint fact expl path)
  (loop for constrainer in constraint
      unless (outcome-path-p constrainer)
      append (norm-filler-problems (expl-norm expl constrainer)
                                   (fact-filler fact path)
                                   constrainer
                                   expl)))

(defun expl-norm (expl constrainer)
  (path-filler (<- expl :xp) constrainer))

(defun fact-filler (fact path)
  (path-filler fact path))

(defun norm-filler-problems (expected observed constrainer expl)
  (unless (or (null expected)
              (abstp expected observed))
    (list (new-problem 'm-non-normative-filler-problem
                       :explanation expl
                       :path constrainer
                       :expected-value expected
                       :observed-value observed))))

;;; Usefulness check
;;; ----------------------------------------------------------

(defun usefulness-problems (expl purpose)
  (mainline "Checking if explanation is useful for the purpose ~S."
            purpose)
  (summarize-results
   (case purpose
     ((accounting-for-event)
      (mainline "... Already tested by relevance check.")
      '())
     ((prediction) (usefulness-for-prediction-problems expl)))
   (format nil "... Explanation is adequate for ~S." purpose)
   (format nil "... Explanation is not adequate for ~S." purpose)))

;;; Check usefulness for prediction
;;; ----------------------------------------------------------

(defun usefulness-for-prediction-problems (expl)
  (mainline "Is the explanation predictive?")
  (loop for (label fact) in (<- expl :xp :events)
        append (knowablility-problems expl label fact)))

(defun knowablility-problems (expl label fact)
  (unless (knowable-p fact)
    (list (new-problem 'm-non-knowable-antecedent-problem
                       :explanation expl
                       :fact fact
                       :event-label label))))


;;; Knowable?
;;; ----------------------------------------------------------

(defun knowable-p (fact)
  (not (abstp 'm-hidden-state fact)))

;;; Evaluation construction
;;; ----------------------------------------------------------

(defun new-evaluation (&rest args)
  (add-instance (gentemp "M-EXPL-EVAL")
                'm-explanation-evaluation
                args))

;;; Problem construction
;;; ----------------------------------------------------------

(defun new-problem (type &rest args)
  (mainline "Problem found!")
  (print-mop (add-instance (gentemp "M-PROB")
                           type
                           args)))

;;; End of module
;;; ----------------------------------------------------------

(provide "accepter")
