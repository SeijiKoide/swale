(defpackage :cbr (:use :cl))
(in-package :cbr)

(defvar *mop-loading-directory*)
(eval-when (:load-toplevel :execute)
  (setq *mop-loading-directory*
        (make-pathname :name nil
                       :type nil
                       :defaults *load-pathname*))
  )

;;; ------------------------------------------------------------
;;; SWALE Demo
;;; ------------------------------------------------------------
;;; Programmer: Chris Riesbeck

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "explainer")
  )

;;; Run the Swale story wth fresh memory.
;;; ------------------------------------------------------------

(defun explain-swale ()
  (reset-swale-memory)
  (explain 'm-swale-story))

(defun reset-swale-memory ()
  (clear-mop-memory)
  (load-swale-memory))

(defun load-swale-memory ()
  (unprovide "swale-mem")
  (require "swale-mem" (make-pathname :name "swale-mem"
                                      :type "fasl"
                                      :defaults *mop-loading-directory*)))

;;; Warning: in some implementations, the following does't seem
;;; to be enough to "unprovide" a module so that require will
;;; load it again.  Check you manual for information about
;;; require and provide if reset-swale-memory fails to reload
;;; swale-mem.  Replace the require in load-swale-memory with an 
;;; explicit load if you have to.

(defun unprovide (module)
  (setq *modules*
        (remove module *modules*
                :test #'string-equal)))


;;; End of module
;;; ------------------------------------------------------------

(provide "swale")
