(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ----------------------------------------------------------
;;;   Memory Organization Packages (MOPs)
;;; ----------------------------------------------------------
;;; Programmer: Chris Riesbeck

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "frames")
  (require "index")
  )

;;; DEFMOP and DEFINSTANCE
;;; ----------------------------------------------------------

(defmacro defmop (name &optional absts &rest args)
  `(add-mop ',name ',absts ',args))

(defmacro definstance (name absts &rest args)
  `(add-instance ',name ',absts ',args))

(defun add-mop (name absts slots)
  (add-mop-frame name absts slots :type ':mop))

(defun add-instance (name absts slots)
  (add-mop-frame name absts slots :type ':instance))

(defun add-mop-frame (name absts slots &rest props)
  (unindex-mop name)
  (index-mop
   (add-frame :name name
              :absts absts
              :slots slots
              :props props)))

;;; ----------------------------------------------------------
;;; The MOP index

(defvar *memory-index*)

(setq *memory-index* (make-index))

(defun mop-index-fetch (cues)
  (index-fetch cues *memory-index*))

(defun mop-index-store (labels name)
  (index-store labels name *memory-index*)
  name)

(defun mop-index-remove (labels name)
  (index-remove labels name *memory-index*)
  name)

;;; ----------------------------------------------------------
;;; Automatic MOP indexing

(defun index-mop (name)
  (indexer-map #'mop-index-store name))

(defun unindex-mop (name)
  (indexer-map #'mop-index-remove name))

(defun indexer-map (fn name)
  (loop for abst in (absts-of name)
      do (loop for index in (<- abst :indices)
             do (funcall fn (instantiate-index index name)
                         name)))
  name)

(defun instantiate-index (index name)
  (loop for path in index
      for value = (instantiate-index-path path name)
      unless (null value)
      collect value))

(defun instantiate-index-path (path name)
  (path-filler name (if (listp path) path (list path))))

;;; ----------------------------------------------------------
;;; Retrieving mops

(defun retrieve-mops (cues abst)
  (loop for mop in (mop-index-fetch cues)
      when (abstp abst mop)
      collect mop))

;;; Clearing MOP memory
;;; ----------------------------------------------------------

(defun clear-mop-memory ()
  (clear-frame-memory)
  (clear-index *memory-index*))

;;; End of module
;;; ----------------------------------------------------------

(provide "mops")
