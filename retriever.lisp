(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ----------------------------------------------------------
;;; Micro RETRIEVER
;;; ----------------------------------------------------------
;;; Programmer: Alex Kass, Chris Riesbeck

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require  "mops")
  (require  "applier")
  (require  "swale-print-utils")
  )

;;; Explanation retriever
;;; ----------------------------------------------------------
;;;
;;; (RETRIEVE-EXPLS story) => list of explanations
;;;   Finds candidate XP's for story that haven't been tried yet
;;;   and returns the explanations each generates.

(defun retrieve-expls (story &optional problems)
  (headline "Micro XP Retriever")
  (mainline "Searching for XP's.")
  (loop for xp in (retrieve-untried-xps story problems)
        collect (apply-xp xp story)))


;;; XP retriever
;;; ----------------------------------------------------------

(defun retrieve-untried-xps (story problems)
  (summarize-results
   (loop for xp in (retrieve-xps story problems)
       unless (already-tried-xp-p xp story)
       collect xp)
   "... no XP's found."
   "... found ~{~S~^, ~}."))

(defun retrieve-xps (story problems)
  (retrieve-mops (append (story->cues story)
                         (problems->cues problems))
                 'm-xp))

(defun already-tried-xp-p (xp story)
  (retrieve-mops (list xp story) 'm-explanation))


;;; Generate retrieval cues from story or explanation problems
;;; ----------------------------------------------------------

(defun story->cues (story)
  (list (<- story :anomaly :surprising-fact)))

(defun problems->cues (problems)
  (unless (null problems)
    (summarize-results
     (loop for problem in problems
           append (problem->cues problem))
     "... no cues found.")))

(defun problem->cues (problem)
  (mainline "Looking for retrieval cues in ~S." problem)
  (let ((expected (<- problem :expected-value))
        (observed (<- problem :observed-value)))
    (unless (null expected)
      (mainline "... Cue: observed ~S, rather than ~S."
                observed expected)
      (list observed))))

;;; End of module
;;; ----------------------------------------------------------

(provide "retriever")
