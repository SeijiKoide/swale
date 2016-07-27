(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ----------------------------------------------------------
;;; Micro TWEAKER
;;; ----------------------------------------------------------
;;; Programmer: Alex Kass, Chris Riesbeck

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "mops")
  (require "applier")
  (require "swale-print-utils")
  )

;;; Tweak explanation
;;; ----------------------------------------------------------

;;; Deficiency: Can't solve more than one problem per tweak.

(defun tweak-expl (expl problems)
  (headline "Micro Tweaker")
  (loop for problem in problems
        append (tweak-problem expl problem)))

(defun tweak-problem (expl problem)
  (loop for tweak in (retrieve-tweaks problem)
        append (apply-tweak tweak expl problem)))

;;; Retrieving tweaks
;;; ----------------------------------------------------------
;;; 
;;; (RETRIEVE-TWEAKS problem) => list of tweaks
;;;   Returns a list of tweaks appropriate to a problem.

(defun retrieve-tweaks (problem)
  (mainline "Looking for candidate tweaks in memory for ~S."
            problem)
  (summarize-results (retrieve-mops (list problem) 'm-tweak)
                     "... no tweaks found."
                     "... found ~{~S~^, ~}."))

;;; Apply a tweak to an  explanation to resolve a problem
;;; ----------------------------------------------------------

(defun apply-tweak (tweak expl problem)
  (let ((fn (<- tweak :function)))
    (mainline "Trying tweak ~S on ~S." (<- tweak :function) expl)
    (summarize-results (funcall fn expl problem)
                       "... tweak failed."
                       "... tweak generated ~{~S~^, ~}.")))

;;; Tweakers
;;; ----------------------------------------------------------

;;; A tweaker takes an explanation and a problem and returns a 
;;; (possibly empty) list of tweaked explanations.

;;; Tweaker: Replace an actor with a more appropriate one
;;; ----------------------------------------------------------
;;; 
;;; This is a dummy tweak, left as an exercise for the reader

(defun replace-actor-search-through-stereotypes (expl problem)
  (declare (ignore expl problem))
  nil)

;;; Tweaker: Replace an event with a causal equivalent
;;; ----------------------------------------------------------
;;; 
;;; This is a dummy tweak, left as an exercise for the reader

(defun replace-action-search-through-causal-rules (expl problem)
  (declare (ignore expl problem))
  nil)

;;; Tweaker: Replace an event with a scriptal equivalent
;;; ----------------------------------------------------------
;;; 
;;; Algorithm: Given an action that the Accepter says the actor
;;; doesn't do, find the effects of that action in the XP, then
;;; find a script for the actor that includes the same effects.
;;;
;;; Example: The XP says "SWALE jogged," but the accepter rejects
;;; this action for SWALE.  The effect of jogging in the XP was
;;; that SWALE ran.  REPLACE-ACTION-SEARCH-THROUGH-STEREOTYPES
;;; looks for a stereotypical script for SWALE that includes 
;;; running.  It finds the horseracing script, so horseracing
;;; replaces jogging in the tweaked XP and associated explanation.

(defun replace-action-search-through-stereotypes (expl problem)
  (let ((actor (<- problem :observed-value))
        (label (problem-label problem))
        (effects (problem-effects expl problem)))
    (unless (null effects)
      (loop for script in
            (find-scripts-with-effects actor effects)
            collect (replace-expl-event expl label script)))))

(defun problem-label (problem)
  (second (<- problem :path)))

;;; Find the effects an event had in an explanation
;;; ----------------------------------------------------------

(defun problem-effects (expl problem)
  (mainline "Looking for effects to be accounted for.")
  (summarize-results
   (find-consequences expl (problem-label problem))
   "... no effects found."
   "... found ~{~S~^, ~}."))

(defun find-consequences (expl label)
  (let ((labelled-events (label-member (<- expl :xp) label)))
    (unless (null (rest labelled-events))
      (list (labelled-event-event (second labelled-events))))))

;;; Find scripts for actor with desired effects
;;; ----------------------------------------------------------

(defun find-scripts-with-effects (actor effects)
  (mainline "Looking for scripts for ~S that include ~{~S~^, ~}."
            actor effects)
  (summarize-results
   (loop for script in (retrieve-scripts actor)
         when (script-includes-effects-p script effects)
         collect script)
   "... no scripts found."
   "... found ~{~S~^, ~}."))

(defun retrieve-scripts (actor)
  (retrieve-mops (list actor) 'm-script-event))

(defun script-includes-effects-p (script effects)
  (loop for effect in effects
        always (script-includes-effect-p script effect)))

(defun script-includes-effect-p (script effect)
  (loop for (nil event) in (<- script :events)
        thereis (abstp effect event)))

;;; Generate explanation by replacing labelled event in its XP
;;; ----------------------------------------------------------


(defun replace-expl-event (expl label event)
  (apply-xp (replace-xp-event (<- expl :xp) label event)
            (<- expl :story)))

(defun replace-xp-event (xp label new-event)
  (let* ((old-event (label-event xp label))
         (new-events (replace-labelled-event (<- xp :events)
                                             label
                                             new-event))
         (new-xp (new-xp :events new-events
                         :causals (<- xp :causals)
                         :observed (<- xp :observed)
                         :expected (<- xp :expected))))
    (mainline "Creating a new xp ~S with ~S instead of ~S"
              new-xp new-event old-event)
    new-xp))

;;; Functions to handle labelled event lists
;;; ----------------------------------------------------------


(defstruct (labelled-event (:type list)) label event)

(defun label-event (name label)
  (<- name :events label))

(defun label-member (name label)
  (member label (<- name :events)
          :key #'labelled-event-label))

(defun replace-labelled-event (labelled-events label new-event)
  (substitute (make-labelled-event :label label :event new-event)
              (assoc label labelled-events)
              labelled-events))

;;; XP maker
;;; ----------------------------------------------------------

(defun new-xp (&rest args)
  (add-mop (gentemp "M-XP") 'm-xp args))


;;; End of module
;;; ----------------------------------------------------------

(provide "tweaker")
