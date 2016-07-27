(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ----------------------------------------------------------
;;;   Micro Frame System
;;; ----------------------------------------------------------
;;; Programmer: Chris Riesbeck

;;; ----------------------------------------------------------
;;; Frame and slot structures

(defstruct frame name slots absts props specs)

;; We implement slots as simple lists to simplify handling
;; fillers that are labelled lists.

(defstruct (slot (:type list)) role filler)

;;; ----------------------------------------------------------
;;; Creating frames

(defvar *frames* (make-hash-table))

(defun clear-frame-memory ()
  "CLEAR-FRAME-MEMORY
   removes all frames from memory."
  (clrhash *frames*))

(defun frame-of (name)
  "FRAME-OF <name>
   returns the frame in memory with the given <name>."
  (gethash name *frames*))

(defun add-frame (&key name absts slots props)
  "ADD-FRAME &key <name> <absts> <slots> <props>
   adds a new frame to memory with the given <name>, <absts>, <slots>, 
   and <props>. If a frame of that name already exists, it is redefined.
   ･ <name> can be any symbol
   ･ <absts> should be either the name of a frame or a (possibly empty) 
     list of frame names
   ･ <slots> should be a list of the form (role filler role filler ...)
     A role should be a symbol or a number, a filler can be anything. 
     By convention, we presume all names of symbol role with a colon(:).
   ･ <props> should be a list of the form (key value key value ...)."
  (setf (gethash name *frames*)
    (make-frame :name name
                :absts (collect-absts absts)
                :slots (collect-slots slots)
                :props props))
  name)

;;; Auxiliary functions:
;;; ----------------------------------------------------------
;;; (COLLECT-ABSTS abst-spec) => list of names
;;;   Given either a single abstraction or a list of
;;;   abstractions, returns a list of abstractions, with any
;;;   redundancies removed.

(defun collect-absts (abst-spec)
  "COLLECT-ABSTS <abst-spec>
   returns a list of abstractions which has no redundancies from <abst-spec>.
   <abst-spec> is a list of abstractions or may be a single abstraction."
  (remove-redundant-absts
   (if (listp abst-spec) abst-spec (list abst-spec))))

;;; (COLLECT-SLOTS slots-spec => list of slots
;;;   Given a list or the form (role filler role filler ...)
;;;   returns a list of slots, one for each role-filler pair.

(defun collect-slots (slots-spec)
  "COLLECT-SLOTS <slots>
   transforms <slots> which is a slot list like p-list (role filler ...) 
   into a list of slots, or pairs of role and filler ((role filler) ...)."
  (loop for (role filler) on slots-spec by #'cddr
      collect (make-slot :role role :filler filler)))

;;; ----------------------------------------------------------
;;; Getting frame components

(defun slots-of (source)
  "SLOTS-OF <source>
   returns frame slots of <source>, which may be the name of a frame
   or a list. If <source> is a non-NIL list, it is just returned.
   If source is NIL, or not the name of frame, NIL is returned."
  (if (consp source)
    source
    (let ((frame (frame-of source)))
      (when frame (frame-slots frame)))))

(defun absts-of (name)
  "ABSTS-OF <name>
   returns abstractions of <name>. <name> is the name of a frame.
   If <name> does not indicate a frame, NIL is returned."
  (let ((frame (frame-of name)))
    (when frame (frame-absts frame))))

(defun props-of (name)
  "PROPS-OF <name>
   returns properties of <name>. <name> is the name of a frame.
   If <name> does not indicate a frame, NIL is returned."
  (let ((frame (frame-of name)))
    (when frame (frame-props frame))))

;;; Using the abstraction hierarchy
;;; ----------------------------------------------------------
;;; 
;;; (ABSTP abst spec) => true or false
;;;   Return true if abst is spec or an abstraction of spec.
;;; (STRICT-ABSTP abst spec) => true or false
;;;   Return true if abst is an abstraction of spec, but not spec
;;;   itself.
;;; (REMOVE-REDUNDANT-ABSTS absts) => list of absts
;;;   Returns absts, minus duplicates and items which are known
;;;   abstractions of other items in the list.

(defun abstp (abst spec)
  "ABSTP <name1> <name2>
   returns true if <name1> is an abstraction of <name2>, otherwise false.
   <name1> and <name2> are a name of frame."
  (or (eql abst spec)
      (strict-abstp abst spec)))

(defun strict-abstp (abst spec)
  "SRICT-ABSTP <name1> <name2>
   returns true if <name1> is an abstraction of <name2>, but not <name2> 
   itself.  <name1> and <name2> are a name of frame."
  (loop for spec-abst in (absts-of spec)
      thereis (abstp abst spec-abst)))

(defun remove-redundant-absts (absts)
  "REMOVE-REDUNDANT-ABSTS <absts>
   returns absts, minus duplicates and items which are known abstractions
   of other items in the list.  <absts> is a list of abstractions.  Note
   that the result order depends the lisp system."
  (let ((l (remove-duplicates absts)))
    (set-difference l l :test #'strict-abstp)))

;;; Getting the filler of a slot in a frame
;;; ----------------------------------------------------------
;;;
;;; (<- name role role ...) => filler
;;;   Return the filler found by tracing the roles from the frame 
;;;   named through its subcomponents. Fillers may be inherited.

(defun <- (name &rest roles)
  "<- <frame> <role1> <role2> ...
   returns the filler found by tracing <role1> <role2> ... from the <frame>.
   Fillers may be inherited."
  (path-filler name roles))

(defun path-filler (name roles)
  "PATH-FILLER <frame> <roles>
   returns the filler found by tracing <roles> from the <frame>.
   Fillers may be inherited."
  (loop for role in roles
      until (null name)
      do (setq name (inherit-filler name role))
      finally (return name)))

;;; Inheriting slots
;;; ----------------------------------------------------------
;;; 
;;; (INHERIT-FILLER name role) => filler
;;;   Return either the explicit filler of role in the frame
;;;   named, or the most specific filler of role in the frame's 
;;;   abstractions.

(defun inherit-filler (source role)
  "INHERIT-FILLER <source> <role>
   returns either the explicit filler of <role> in the slot <source> or the most 
   specific filler of <role> in the frame <source>'s abstractions.  <source> 
   may be a list of slots or a frame name.  Searching algorithm in abstraction 
   hierarchy is, say, inherit-for-each-absts-most-special algorithm, that 
   is, INHERIT-FILLER inherits fillers for each abstractions of <source>, 
   and chooses the most special filler among them.  Note that a filler is a 
   frame and all retrieved fillers have to share abst-spec relation."
  (or (role-filler source role)
      (most-specific-inherited-filler source role)))

(defun most-specific-inherited-filler (name role)
  "MOST-SPECIFIC-INHERITED-FILLER <name> <role>
   returns the most specific filler of role in the <name>'s abstractions.
   Note that if the inherited filler for all abstractions of <name> is 
   single, it is returned eventhough the filler is not a frame."
  (let ((filler nil))
    (dolist (abst (absts-of name))
      (setq filler (more-specific-filler name role filler abst)))
    filler))

(defun more-specific-filler (name role filler abst)
  "MORE-SPECIFIC-FILLER <name> <role> <filler> <abst>
   compares <filler> and a inherited filler of <role> of <abst>, and returns 
   more specific one. If one is NIL, the other is returned. <name> is needed 
   just for error messages."
  (let ((abst-filler (inherit-filler abst role)))
    (cond ((more-specific-p abst-filler filler) abst-filler)
          ((more-specific-p filler abst-filler) filler)
          (t (error
              "~S in ~S has incompatible fillers: ~S and ~S"
              role name filler abst-filler)))))

(defun more-specific-p (filler1 filler2)
  "MORE-SPECIFIC-P <filler1> <filler2>
   returns true if <filler1> is more specific than <filler2>, or if
   <filler2> is NIL."
  (or (null filler2)
      (abstp filler2 filler1)))

;;; Explicit slots
;;; ----------------------------------------------------------

(defun role-filler (source role)
  "ROLE-FILLER <source> <role>
   returns the filler associated with <role> in the slots of <source>."
  (let ((slot (role-slot source role)))
    (and slot (slot-filler slot))))

(defun role-slot (source role)
  "ROLE-SLOT <source> <role>
   returns a slot whose role is <role> in <source>. 
   <source> may be a slot list or a frame name."
  (find role (slots-of source) :key #'slot-role))

;;; Frame properties
;;; ----------------------------------------------------------

(defun frame-prop (name prop)
  (getf (props-of name) prop))

(defun set-frame-prop (name prop val)
  (setf (getf (frame-props (frame-of name)) prop) val))

(defsetf frame-prop set-frame-prop)

;;; End of module
;;; ----------------------------------------------------------

(provide "frames")
