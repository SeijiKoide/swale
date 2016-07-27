(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ----------------------------------------------------------
;;; Index Manager
;;; ----------------------------------------------------------
;;; Programmer: Chris Riesbeck

(defstruct index entries)

(defstruct (index-entry (:type list)) labels items)

;;; Primary indexing functions
;;; ----------------------------------------------------------

(defun clear-index (index)
  "CLEAR-INDEX <index>
   simply clears the <index> of all items."
  (setf (index-entries index) nil))

(defun index-fetch (cues index)
  "INDEX-FETCH <cues> <index>
   retrieves items, given a set of <cues> from <index>, by 
   looking through the list of entries for matching sets of 
   labels. the rule for matching is that a set of labels of 
   <index> entries matches a set of cues if every label is 
   an abstraction of some cue.
   <cues> are a list of symbols, usually the names of mops."
  (loop for (labels items) in (index-entries index)
      when (subsetp labels cues :test #'abstp) ;abstp defined in frame module
      append items))

(defun index-store (labels item index)
  "INDEX-STORE <labels> <item> <index>
   stores an <item> under a set of <labels> into <index>.
   If there is an entry with the same set of <labels>, the 
   new <item> is added. If not, a new entry is made with the
   given set of <labels> and <item>.
   <labels> are a list of symbols, usually the names of MOPs."
  (unless (null labels)
    (let ((entry (index-entry labels index)))
      (if (null entry)
          (add-index-entry labels item index)
        (add-index-item item entry))
      item)))

(defun index-remove (labels item index)
  "INDEX-REMOVE <labels> <item> <index>
   removes an <item> under a set of <labels> from <index>.
   <labels> are a list of symbols, usually the names of MOPs."
  (let ((entry (index-entry labels index)))
    (cond ((null entry) nil)
          ((not (member item (index-entry-items entry))) nil)
          (t (remove-index-item item entry) t))))

;;; Getting the entry for a set of labels (not cues!)
;;; ----------------------------------------------------------

(defun index-entry (labels index)
  "INDEX-ENTRY <labels> <index>
   returns the entry whose labels are same as <labels> in set 
   from <index>."
  (find labels (index-entries index)
        :test #'set-equalp
        :key #'index-entry-labels))

(defun set-equalp (x y)
  "SET-EQUALP <x> <y>
   returns true if <x> and <y> is same as a set.  Testing 
   function 'equal' is used."
  (and (subsetp x y :test #'equal) (subsetp y x :test #'equal)))

;;; Adding and removing items
;;; ----------------------------------------------------------

(defun add-index-entry (labels item index)
  "ADD-INDEX-ENTRY <labels> <item> <index>
   makes new entry with <labels> and a listed <item>, and
   push it into <index>."
  (push (make-index-entry :labels labels
                          :items (list item))
        (index-entries index)))

(defun add-index-item (item entry)
  "ADD-INDEX-ITEM <item> <entry>
   push <item> if not exists into the item list of <entry>."
  (pushnew item (index-entry-items entry)))

(defun remove-index-item (item entry)
  "REMOVE-INDEX-ITEM <item> <entry>
   removes <item> from the item list of <entry>."
  (setf (index-entry-items entry)
        (remove item (index-entry-items entry))))

;;; End of module
;;; ----------------------------------------------------------

(provide "index")

