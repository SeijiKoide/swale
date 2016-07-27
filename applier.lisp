(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ----------------------------------------------------------
;;; Micro APPLIER
;;; ----------------------------------------------------------
;;; Programmer: Chris Riesbeck

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "mops")
  )

;;; function summarize-results and mainline in apply-xp is defined 
;;; in print-util.lisp

;;; Apply an XP to a story
;;; ----------------------------------------------------------

(defun apply-xp (xp story)
  "APPLY-XP <xp> <story>
   applies new <xp> to the <story>, and gets the new explanation.
   <xp> is a explanation pattern."
  (mainline "Applying ~S." xp)
  (summarize-results (new-explanation :story story :xp xp)
                    "... didn't work."
                    "... generated ~S."))

;;; Explanation maker
;;; ----------------------------------------------------------

(defun new-explanation (&rest args)
  "NEW-EXPLANATION :story <story> :xp <xp>
   makes a new instance of M-EXPLANATION with 
   the arguments ':story <story> :xp <xp>', and
   returns true if newly created, otherwise false."
  (add-instance (gentemp "M-EXPL")
                'm-explanation
                args))

;;; End of module
;;; ----------------------------------------------------------

(provide "applier")
