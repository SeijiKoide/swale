(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ----------------------------------------------------------
;;; Micro-EXPLAINER
;;; ----------------------------------------------------------
;;; Programmer: Chris Riesbeck

;;; Top-level controller for Micro Explainer

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require  "mops")
  (require  "swale-print-utils")
  (require  "retriever")
  (require  "accepter")
  (require  "tweaker")
  )

;;; The Expainer
;;; ----------------------------------------------------------
;;;
;;; (EXPLAIN &optional story) => explanation
;;;   Returns an explanation for the story, if any.
;;; 
;;; If called again with no arguments, gets another explanation,
;;; if any.
;;;
;;; Ex.  (EXPLAIN 'M-SWALE-STORY) => first explanation
;;;      (EXPLAIN) => second explanation

(defun explain (&optional story)
  (when story
    (initialize-explainer story))
  (run-expainer))


;;; Initialize the explainer for a new story
;;; ----------------------------------------------------------

(defvar *story*)

(defun initialize-explainer (story)
  (headline "Micro Explainer")
  (mainline "Trying to explain ~S" story)
  (mainline "which has the anomaly:")
  (print-mop (<- story :anomaly))
  (setq *story* story)
  (initialize-explanation-queue (retrieve-expls story)))


;;; Run the explainer on the current story
;;; ----------------------------------------------------------

(defun run-expainer ()
  (loop until (null (explanation-queue))
        thereis (acceptable-explanation (next-explanation))))

(defun acceptable-explanation (expl)
  (let ((problems (evaluate-expl expl 'prediction)))
    (case (explanation-status expl problems)
      (acceptable expl)
      (tweakable
       (add-explanations (tweak-expl expl problems))
       (add-explanations (retrieve-expls *story* problems))
       nil)
      (t nil))))


;;; The internal explanation queue
;;; ----------------------------------------------------------

(defvar *expls*)

(defun initialize-explanation-queue (expls)
  (setq *expls* expls))

(defun explanation-queue () *expls*)

(defun next-explanation () (pop *expls*))

(defun add-explanations (expls)
  (setq *expls* (append *expls* expls)))



;;; End of module
;;; ----------------------------------------------------------

(provide "explainer")
