(cl:in-package :asdf)


(defsystem :icbe-swale
  :serial T
  :components ((:file "frames")
               (:file "index")
               (:file "mops")
               (:file "print-utils")
               (:file "applier")
               (:file "accepter")
               (:file "retriever")
               (:file "tweaker")
               (:file "explainer")
               (:file "swale")
               (:file "swale-mem")))


;;; *EOF*
