(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ------------------------------------------------------------
;;; Print utilities for Micro EXPLAINER
;;; ------------------------------------------------------------
;;; Programmer: Chirs Riesbeck

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "mops")
  (require "index")
  )

;;; Printing headings
;;; ------------------------------------------------------------

;;; (headline <string>) => undefined
;;;
;;; Print the string in a highly visible way. Currently, adds
;;; surrounding vertical space and underlining.

(defun headline (str)
  (format t "~2%~A~%~V,,,'-<~>~&" str (length str)))

;;; Printing messages
;;; ------------------------------------------------------------

;;; (mainline <format-string> <arg> <arg> ... ) => undefined
;;;
;;; Print the arguments, on a separate line.

(defun mainline (format-string &rest args)
  (unless (null format-string)
    (apply #'format t "~&~@?~&" format-string args)))

;;; Summarizing results
;;; ------------------------------------------------------------

;;; (summarize-results arg format1 format2) => value of arg
;;;
;;; If the value of arg is nil, (format t format1) else
;;; (format t format2 value-of arg).

(defun summarize-results (results
                          &optional msg-if-null msg-if-non-null)
  (if (null results)
      (unless (null msg-if-null)
        (mainline msg-if-null))
    (unless (null msg-if-non-null)
      (mainline msg-if-non-null results)))
  results)

;;; Printing MOPs
;;; ------------------------------------------------------------

;;; (print-mop name) => name
;;;   Prints the type, abstractions, slots, and other properties
;;;  of a frame.

(defun print-mop (name)
  (print-mop-type (frame-prop name :type))
  (print-mop-absts (absts-of name))
  (print-mop-slots (slots-of name))
  (format t "~%")
  name)

(defun print-mop-type (type)
  (unless (null type)
    (format t "~&  ~S" type)))

(defun print-mop-absts (absts)
  (unless (null absts)
    (format t " isa ~{~S~^, ~}" absts)))

(defun print-mop-slots (slots)
  (loop for (role filler) in slots
      do (format t "~&  ~S: ~S" role filler)))

;;; Print the index in a readable fashion
;;; ------------------------------------------------------------

(defun print-index (index)
  (loop for (labels items) in (index-entries index)
      do (format t "~{~S~^ + ~}~%  => ~S~%"
           labels (first items))
        (format t "~{~5T~S~%~}" (rest items))))

;;; End of module
;;; ------------------------------------------------------------

(provide "swale-print-utils")
