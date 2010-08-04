;;; -*- package: NOX; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  xml-util.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The Original Software is 
;;;   WILBUR2: Nokia Semantic Web Toolkit for CLOS
;;;
;;;   Copyright (c) 2001-2009 Nokia Corp. and/or its subsidiaries. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;
;;;   This program is licensed under the terms of the GNU Lesser General Public License
;;;   as published by the Free Software Foundation, version 2.1 of the License. Note
;;;   however that a preamble attached below also applies to this program.
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   Preamble to the Gnu Lesser General Public License
;;;
;;;   Copyright (c) 2000 Franz Incorporated, Berkeley, CA 94704
;;;
;;;   The concept of the GNU Lesser General Public License version 2.1 ("LGPL") has been
;;;   adopted to govern the use and distribution of above-mentioned application. However,
;;;   the LGPL uses terminology that is more appropriate for a program written in C than
;;;   one written in Lisp. Nevertheless, the LGPL can still be applied to a Lisp program
;;;   if certain clarifications are made. This document details those clarifications.
;;;   Accordingly, the license for the open-source Lisp applications consists of this
;;;   document plus the LGPL. Wherever there is a conflict between this document and the
;;;   LGPL, this document takes precedence over the LGPL.
;;;
;;;   A "Library" in Lisp is a collection of Lisp functions, data and foreign modules.
;;;   The form of the Library can be Lisp source code (for processing by an interpreter)
;;;   or object code (usually the result of compilation of source code or built with some
;;;   other mechanisms). Foreign modules are object code in a form that can be linked
;;;   into a Lisp executable. When we speak of functions we do so in the most general way
;;;   to include, in addition, methods and unnamed functions. Lisp "data" is also a
;;;   general term that includes the data structures resulting from defining Lisp classes.
;;;   A Lisp application may include the same set of Lisp objects as does a Library, but
;;;   this does not mean that the application is necessarily a "work based on the Library"
;;;   it contains.
;;;
;;;   The Library consists of everything in the distribution file set before any
;;;   modifications are made to the files. If any of the functions or classes in the
;;;   Library are redefined in other files, then those redefinitions ARE considered a
;;;   work based on the Library. If additional methods are added to generic functions in
;;;   the Library, those additional methods are NOT considered a work based on the
;;;   Library. If Library classes are subclassed, these subclasses are NOT considered a
;;;   work based on the Library. If the Library is modified to explicitly call other
;;;   functions that are neither part of Lisp itself nor an available add-on module to
;;;   Lisp, then the functions called by the modified Library ARE considered a work based
;;;   on the Library. The goal is to ensure that the Library will compile and run without
;;;   getting undefined function errors.
;;;
;;;   It is permitted to add proprietary source code to the Library, but it must be done
;;;   in a way such that the Library will still run without that proprietary code present.
;;;   Section 5 of the LGPL distinguishes between the case of a library being dynamically
;;;   linked at runtime and one being statically linked at build time. Section 5 of the
;;;   LGPL states that the former results in an executable that is a "work that uses the
;;;   Library." Section 5 of the LGPL states that the latter results in one that is a
;;;   "derivative of the Library", which is therefore covered by the LGPL. Since Lisp only
;;;   offers one choice, which is to link the Library into an executable at build time, we
;;;   declare that, for the purpose applying the LGPL to the Library, an executable that
;;;   results from linking a "work that uses the Library" with the Library is considered a
;;;   "work that uses the Library" and is therefore NOT covered by the LGPL.
;;;
;;;   Because of this declaration, section 6 of LGPL is not applicable to the Library.
;;;   However, in connection with each distribution of this executable, you must also
;;;   deliver, in accordance with the terms and conditions of the LGPL, the source code
;;;   of Library (or your derivative thereof) that is incorporated into this executable. 
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Purpose: This file contains useful functions and other definitions for
;;;   implementing an XML parser (or some other stuff, for that matter). These
;;;   are separated from the actual parser so that one could replace the actual
;;;   parser with another implementation (not me, but someone else might do it).
;;;


(in-package "NOX")


;;; --------------------------------------------------------------------------------------
;;;
;;;   XML CONDITION CLASSES
;;;
;;;   XML-ERROR                           abstract
;;;     SYNTAX-ERROR                      concrete
;;;       PI-TERMINATION-PROBLEM          concrete
;;;       DTD-TERMINATION-PROBLEM         concrete
;;;       UNEXPECTED-END-TAG              concrete
;;;       UNKNOWN-DECLARATION             concrete
;;;       UNKNOWN-CHARACTER-REFERENCE     concrete
;;;       MALFORMED-URL                   concrete
;;;     XML-FEATURE-NOT-SUPPORTED         concrete
;;;     MISSING-DEFINITION                abstract
;;;       MISSING-ENTITY-DEFINITION       concrete, continuable
;;;       MISSING-NAMESPACE-DEFINITION    concrete, continuable
;;;   XML-WARNING                         concrete, warning
;;;

(define-condition wilbur-error (simple-error)
  ((thing
    :initarg :thing
    :reader error-thing))
  (:report (lambda (condition stream)
             (format stream (simple-condition-format-control condition)
                     (error-thing condition)))))

(define-condition xml-error (wilbur-error)
  ())

(define-condition syntax-error (xml-error)
  ()
  (:default-initargs
    :format-control "XML -- syntax error (why: ~:[unknown~;~:*~A~])"))

(define-condition pi-termination-problem (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- unterminated PI ~S"))

(define-condition dtd-termination-problem (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- improperly terminated DTD"))

(define-condition unexpected-end-tag (syntax-error)
  ((expectation
    :initarg :expectation
    :initform nil
    :reader error-expectation))
  (:report (lambda (condition stream)
             (format stream "XML -- unexpected end tag ~S~@[ (looking for ~S)~]"
                     (error-thing condition)
                     (error-expectation condition)))))

(define-condition unknown-declaration (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- unknown declaration ~S"))

(define-condition unknown-character-reference (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- unknown character reference ~S"))

(define-condition malformed-url (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- unparseable URL ~S"))

(define-condition xml-feature-not-supported (xml-error)
  ()
  (:default-initargs
    :format-control "XML -- ~S is not supported"))

(define-condition missing-definition (xml-error)
  ((definition-type
    :initarg :type
    :reader error-definition-type))
  (:report (lambda (condition stream)
             (format stream "XML -- missing ~A definition ~S"
                     (error-definition-type condition)
                     (error-thing condition)))))

(define-condition missing-entity-definition (missing-definition)
  ()
  (:default-initargs
    :type :entity))

(define-condition missing-namespace-definition (missing-definition)
  ()
  (:default-initargs
    :type :namespace))

(define-condition xml-warning (simple-warning)
  ())

(defmacro xml-warning (message &rest args)
  `(warn 'xml-warning
         :format-control "XML -- ~?"
         :format-arguments (list ,message (list ,@args))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RESOURCE POOLS
;;;

#-:digitool
(defstruct (resource-pool (:conc-name pool-))
  data
  constructor
  initializer 
  destructor)

#-(or :clozure :digitool :excl :sbcl :lispworks)
(defmacro without-interrupts (&body body)
  (warn "No working WITHOUT-INTERRUPTS in this implementation")
  `(progn ,@body))

#-:digitool
(defmacro atomic-push (thing place)
  `(without-interrupts (push ,thing ,place)))

#-:digitool
(defmacro atomic-pop (place)
  `(without-interrupts (pop ,place)))

(defun allocate-resource-from-pool (pool &rest args)
  #+:digitool
  (declare (ignore args))
  #+:digitool
  (ccl::allocate-resource pool)
  #-:digitool
  (let ((res (or (atomic-pop (pool-data pool))
                 (apply (pool-constructor pool) args)))
        (init (pool-initializer pool)))
    (when init
      (funcall init res))
    res))

(defun free-resource-to-pool (pool resource)
  #+:digitool
  (ccl::free-resource pool resource)
  #-:digitool
  (let ((des (pool-destructor pool)))
    (when des
      (funcall des resource))
    (atomic-push resource (pool-data pool))))

(defmacro define-resource-pool (name constructor &optional initializer destructor)
  #+:digitool
  (let ((r (gentemp))
        (c (gentemp))
        (i (gentemp))
        (d (gentemp)))
    `(let ((,c ,constructor)
           (,i ,initializer)
           (,d ,destructor))
       (ccl::defresource ,name
         :constructor (let (,r)
                        (prog1 (setf ,r (funcall ,c))
                          (funcall ,i ,r)))
         :initializer ,i
         :destructor ,d)))
  #-:digitool
  `(defparameter ,name (make-resource-pool :constructor ,constructor
                                           :initializer ,initializer
                                           :destructor ,destructor)))

(defmacro with-resource-from-pool ((var pool) &body body)
  #+:digitool
  `(ccl::using-resource  (,var ,pool) ,@body)
  #-:digitool
  (let ((flag (gentemp))
        (g-pool (gentemp)))
    `(let* ((,g-pool ,pool)
            (,flag (allocate-resource-from-pool ,g-pool))
            (,var ,flag))
       (unwind-protect (progn ,@body)
         (when ,flag (free-resource-to-pool ,g-pool ,flag))))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HELPERS
;;;

(defun make-base-string (size &rest args)
  (declare (dynamic-extent args))
  (remf args :element-type)
  (apply #'make-array size :element-type #+:sbcl 'extended-char #-:sbcl 'base-char args))
   
(declaim (inline make-base-string))

(defvar *current-parser* nil)

(defmacro with-loop&read-char ((char stream &optional (eof-error-p t)) &body body)
  `(loop (let ((,char (read-char ,stream nil nil t)))
	   (cond ((or ,char (not ,eof-error-p))
		  ,@body)
		 (t
		  (error 'syntax-error :thing "eof while scanning"))))))

(defun read-using (readtable stream &optional recursivep)
  (let ((old-readtable *readtable*))
    (handler-case (let ((*readtable* readtable))
		    (read stream t nil recursivep))
      (syntax-error (c)
	(setf *readtable* old-readtable)
	(error c)))))

(defmacro define-readtable (readtablevar copied-from &body body)
  `(defparameter ,readtablevar
     (let ((*readtable* (copy-readtable ,copied-from)))
       ,@body
       *readtable*)))

(defmacro define-macro-character (character (&rest args) &body body)
  `(set-macro-character ',character #'(lambda (,@args) ,@body)))

(defvar *ruc-buffer* nil)
(defvar *ruc-ee-buffer* nil)

(define-resource-pool *xml-parse-buffers* 
    #'(lambda () (make-base-string 2048 :adjustable t :fill-pointer 0))
    #'(lambda (vector) (setf (fill-pointer vector) 0)))

(defvar *leave-unknown-entities-unexpanded-p* t)

(defun read-until-char-expanding-entities (stream char save-last-p
                                           &optional (buffer *ruc-ee-buffer*))
  (setf (fill-pointer buffer) 0)
  (with-loop&read-char (c stream)
    (cond ((char= c char)
           (when save-last-p
             (unread-char c stream))
           (return (concatenate 'string buffer)))
          ((char= c #\&)
           (let ((name (read-until-char stream #\;)))
             (if (char= (char name 0) #\#)
               (multiple-value-bind (c1 c2 c3)
                                    (resolve-character-reference name)
                 (vector-push-extend c1 buffer)
                 (when c2
                   (vector-push-extend c2 buffer)
                   (when c3
                     (vector-push-extend c3 buffer))))
               (let ((def (get-entity *current-parser* name)))
                 (unless def
                   (unless *leave-unknown-entities-unexpanded-p*
		     (cerror "Do not expand" 'missing-entity-definition :thing name))
                   (setf def (format nil "&~A;" name)))
                 (dotimes (i (length def))
                   (vector-push-extend (char def i) buffer))))))
          (t
           (vector-push-extend c buffer)))))

(defun resolve-character-reference (ref)
  (let ((n (ignore-errors (if (char= (char ref 1) #\x)
                            (parse-integer ref :start 2 :radix 16)
                            (parse-integer ref :start 1 :radix 10)))))
    (if (and (integerp n) (< n 65536))
      (let ((b (integer-length n))) ; poor man's UTF8 enconding :-)
        (cond ((<= b 7)
               (code-char n))
              ((<= b 11)
               (values (code-char (logior #b11000000 (ash n -6)))
                       (code-char (logior #b10000000 (logand n #b00111111)))))
              ((<= b 16)
               (values (code-char (logior #b11100000 (ash n -12)))
                       (code-char (logior #b10000000 (logand (ash n -6)) #b00111111))
                       (code-char (logior #b10000000 (logand n #b00111111)))))))
      (error 'unknown-character-reference :thing ref))))

(defun read-until-char (stream char &optional (buffer *ruc-buffer*))
  (setf (fill-pointer buffer) 0)
  (with-loop&read-char (c stream)
    (if (char= c char)
      (return (concatenate 'string buffer))
      (vector-push-extend c buffer))))

(defun read-until-%%> (stream char &aux chars)
  (with-loop&read-char (c stream)
    (cond ((not (char= c char))
           (push c chars))
          ((or (not (char= (setf c (read-char stream t nil t)) char))
               (not (char= (peek-char nil stream t nil t) #\>)))
           (push char chars)
           (push c chars))
          (t
           (read-char stream t nil t) ; skip #\>
           (return (concatenate 'string (nreverse chars)))))))


(defmacro whitespace-char-p (char)
  (with-temps (c)
    `(let ((,c ,char))
       ;; let's assume this works for now :-)
       (or (char= ,c #\Space)
           (not (graphic-char-p ,c))))))


(defequal -whitespace-chars-
  (let ((chars nil))
    (dotimes (i 256)
      (let ((c (code-char i)))
        (when (whitespace-char-p c)
          (push c chars))))
    (concatenate 'string chars)))

(defun name&prefix (string)
  (let ((i (position #\: string)))
    (cond ((null i)
           (values string nil))
          ((zerop i)
           (values (subseq string 1) nil))
          ((= i (1- (length string)))
           (values nil (subseq string 0 i)))
          (t
           (values (subseq string (1+ i)) (subseq string 0 i))))))

(defun skip-whitespace (stream &optional pop-char-p)
  (let ((char (peek-char t stream t nil t)))
    (if pop-char-p
      (read-char stream t nil t)
      char)))

(defun collapse-whitespace (string)
  ;; new version with "poor man's Unicode support" :-(
  (labels ((collapse (mode old new)
	     (if old
	       (dsb (c &rest old) old
		 (cond ((zerop (logand (char-code c) #b10000000))
			(if (whitespace-char-p c)
			  (collapse (if (eq mode :start) :start :white) old new)
			  (collapse :collect old
				    (if (eq mode :white)
				      (list* c #\Space new)
				      (cons c new)))))
		       ((= (logand (char-code c) #b11100000) 192)
			(collapse :collect (cdr old)
				  (if (eq mode :white)
				    (list* (car old) c #\Space new)
				    (list* (car old) c new))))
		       ((= (logand (char-code c) #b11110000) 224)
			(collapse :collect (cddr old)
				  (if (eq mode :white)
				    (list* (cadr old) (car old) c #\Space new)
				    (list* (cadr old) (car old) c new))))
		       (t
			(error "Cannot decode this: ~S" (cons c old)))))
	       (concatenate 'string (nreverse new)))))
    (declare (dynamic-extent #'collapse))
    (collapse :start (coerce string 'list) nil)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   URL FUNCTIONS
;;;
;;;   We currently support http, file, urn, mailto and tel URL parsing.
;;;

(defun parse-url (u &optional (errorp nil))
  (cond ((string= u "http://" :end1 7)
         (flet ((url (host port &optional (path "/"))
                  `(:host ,host
                    :port ,port
                    :path ,(if (zerop (length path)) "/" path))))
           (let ((i (position #\: u :start 7))
                 (j (position #\/ u :start 7)))
             (cond ((and (null i) (null j))
                    (values :http (url (subseq u 7) 80 "/")))
                   ((and i (< i (or j most-positive-fixnum)))
                    (let ((h (subseq u 7 i)))
                        (multiple-value-bind (p j)
                                             (parse-integer u
                                                            :start (1+ i)
                                                            :junk-allowed t)
                          (values :http (url h p (subseq u j))))))
                   (t
                    (values :http (url (subseq u 7 j) 80 (subseq u j))))))))
        #+(or)
        ((string= u "file://" :end1 7)
         (let ((p (subseq u 7)))
           (values :file
                   `(:path ,(translate-logical-pathname
                             (canonical->host-specific-path p))))))
        ((string= u "file://" :end1 7)
         (let* ((p (translate-logical-pathname (subseq u 7)))
                (directory (pathname-directory p))
                (filename (file-namestring p)))
           (values :file
                   `(:path ,(format nil "~:[~;/~]~{~a/~}~a"
                                    (eq (first directory) :absolute) (rest directory) filename)))))
        ((string= u "mailto:" :end1 7)
         (values :mailto
                 `(:path ,(subseq u 7))))
        ((string= u "tel:" :end1 4)
         (if (char= (char u 4) #\+)
           (values :tel
                   `(:number ,(subseq u 5) :plusp t))
           (values :tel
                   `(:number ,(subseq u 4) :plusp nil))))
        ((string= u "urn:" :end1 4)
	 (values :urn `(:path ,(subseq u 4))))
        (errorp
         (error 'malformed-url :thing u))
        (t
         (let ((i (position #\: u)))
           (values :unknown
                   `(:scheme ,(subseq u 0 i) :path ,(subseq u (1+ i))))))))

(defun host-specific->canonical-path (path)
  #+:digitool
  (substitute #\/ #\: (subseq path (position #\: path)))
  #-:digitool ; = unix, since we do not do Windows yet (maybe never)
  path)

(defun canonical->host-specific-path (path)
  #+:digitool
  (let ((p (namestring (translate-logical-pathname "home:"))))
    (concatenate 'string (subseq p 0 (position #\: p)) (substitute #\: #\/ path)))
  #-:digitool ; = unix, since we do not do Windows yet (maybe never)
  path)

(defun make-file-url (pathname)
  (format nil "file://~A"
          (host-specific->canonical-path
           (namestring (translate-logical-pathname (pathname pathname))))))

(defun make-http-url (host port path)
  (format nil "http://~A~@[:~S~]~A" host port (or path "/")))

(defun make-mailto-url (address)
  (format nil "mailto:~A" address))

(defun make-tel-url (number &optional (include-plus-p t))
  (format nil "tel:~@[+~]~A" (not include-plus-p) number))


;;; --------------------------------------------------------------------------------------
;;;
;;;   XML TOKEN CLASSES
;;;
;;;   The lexical scanner of the parser (function read-xml-token) returns instances of
;;;   XML token classes. Some of these are processed by the parser, some are ignored.
;;;
;;;   TOKEN                       abstract
;;;     TAG                       abstract
;;;       OPEN-TAG                processed
;;;       CLOSE-TAG               processed
;;;     PROC-INSTRUCTION          ignored
;;;     DTD-DECLARATION           abstract
;;;       ENTITY-DECLARATION      processed
;;;       ELEMENT-DECLARATION     ignored w/ warning
;;;       ATTLIST-DECLARATION     ignored w/ warning
;;;       COMMENT                 ignored
;;;     CHAR-CONTENT              processed (this is a string, not an instance)
;;;     DTD-BRACKET               abstract
;;;       DTD-START               ignored w/ warning (if external DTD)
;;;       DTD-END                 ignored
;;;

(defclass token ()
  ((string
    :initarg :string
    :accessor token-string)))

(defclass tag (token)
  ((counterpart
    :initform nil
    :accessor tag-counterpart)))

(defmethod print-object ((self tag) stream)
  (print-unreadable-object (self stream :type t)
    (princ (token-string self) stream)))

(defclass open-tag (tag)
  ((original-name
    :initform nil
    :accessor tag-original-name)
   (attributes
    :initform nil
    :accessor tag-attributes)
   (emptyp
    :initform nil
    :accessor tag-empty-p)
   (namespaces
    :initarg :namespaces
    :accessor tag-namespaces)
   (base
    :initarg :base
    :initform nil
    :accessor tag-base)))

(defun tag-attribute (tag attribute) ; assuming OPEN-TAG
  (string-dict-get (tag-attributes tag) attribute))

(defun (setf tag-attribute) (value tag attribute) ; assuming OPEN-TAG
  (setf (tag-attributes tag) (string-dict-add (tag-attributes tag) attribute value))
  value)

(defclass close-tag (tag)
  ())

(defclass proc-instruction (token)
  ())

(defclass dtd-declaration (token)
  ())

(defclass entity-declaration (dtd-declaration)
  ((name
    :initarg :name
    :reader entity-name)))

(defclass element-declaration (dtd-declaration)
  ((name
    :initarg :name
    :reader element-name)
   (contentspec
    :initarg :contentspec
    :reader element-contentspec)))

(defclass attlist-declaration (dtd-declaration)
  ((name
    :initarg :name
    :reader attlist-name)
   ;; should be more
   ))

(defclass comment (dtd-declaration)
  ())

(defclass dtd-bracket (token)
  ())

(defclass dtd-start (dtd-bracket)
  ((externalp
    :initarg :externalp
    :initform nil
    :reader dtd-external-p)
   (stuff
    :initarg :stuff
    :initform nil
    :reader dtd-stuff)))

(defclass dtd-end (dtd-bracket)
  ())


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SAX-CONSUMER
;;;   SIMPLE SAX 1 -LIKE INTERFACE ("CL-SAX")
;;;

(defclass sax-consumer ()
  ((producer
    :initarg :producer
    :initform nil
    :accessor sax-consumer-producer)
   (debugp
    :initarg :debugp
    :initform nil
    :reader sax-consumer-debug-p)))

(defgeneric start-element (consumer tag mode))
(defgeneric end-element (consumer tag mode))
(defgeneric char-content (consumer char-content mode))
(defgeneric proc-instruction (consumer instruction mode))
(defgeneric start-document (consumer locator))
(defgeneric end-document (consumer mode))
(defgeneric maybe-use-namespace (consumer prefix uri))

(defun debug-format (consumer string &rest args)
  (when (sax-consumer-debug-p consumer)
     (apply #'format *debug-io* string args)))

(defmethod find-first-producer ((consumer sax-consumer))
  (find-first-producer (sax-consumer-producer consumer)))

(defmethod sax-consumer-mode ((self sax-consumer))
  nil)

(defmethod start-element ((self sax-consumer) (tag open-tag) mode)
  (debug-format self "~&START ~A ~S ~S ~S"
                (token-string tag) (tag-attributes tag) mode (tag-base tag)))

(defmethod end-element ((self sax-consumer) (tag open-tag) mode)
  (debug-format self "~&END ~A ~S ~S" (token-string tag) mode (tag-base tag)))

(defmethod char-content ((self sax-consumer) (char-content string) mode)
  (debug-format self "~&CHARACTERS ~S ~S" char-content mode))

(defmethod proc-instruction ((self sax-consumer) (tag proc-instruction) mode)
  (debug-format self "~&PI ~S ~S" (token-string tag) mode))

(defmethod start-document ((self sax-consumer) locator)
  (debug-format self "~&START DOCUMENT ~S" locator))

(defmethod end-document ((self sax-consumer) mode)
  (debug-format self "~&END DOCUMENT ~S" mode))

(defmethod maybe-use-namespace ((self sax-consumer) prefix uri)
  (debug-format self "&NAMESPACE ~S ~S" prefix uri))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SAX-PRODUCER
;;;

(defclass sax-producer ()
  ((consumer
    :accessor sax-producer-consumer)))

(defmethod find-first-producer ((producer sax-producer))
  producer)

(defmethod initialize-instance :after ((self sax-producer)
                                       &key (consumer
                                             (make-instance 'sax-consumer
                                               :debugp t))
                                       &allow-other-keys)
  (setf (sax-producer-consumer self) consumer))

(defmethod (setf sax-producer-consumer) :after ((consumer sax-consumer)
                                                (producer sax-producer))
  (setf (sax-consumer-producer consumer) producer))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SAX-FILTER
;;;

(defclass sax-filter (sax-consumer sax-producer)
  ((blockp
    :initform nil
    :initarg :blockp
    :accessor sax-filter-block-p)))

(defmethod start-element ((self sax-filter) (tag open-tag) mode)
  (unless (sax-filter-block-p self)
    (start-element (sax-producer-consumer self) tag mode)))

(defmethod end-element ((self sax-filter) (tag open-tag) mode)
  (unless (sax-filter-block-p self)
    (end-element (sax-producer-consumer self) tag mode)))

(defmethod char-content ((self sax-filter) (content string) mode)
  (unless (sax-filter-block-p self)
    (char-content (sax-producer-consumer self) content mode)))

(defmethod proc-instruction ((self sax-filter) (tag proc-instruction) mode)
  (unless (sax-filter-block-p self)
    (proc-instruction (sax-producer-consumer self) tag mode)))

(defmethod start-document ((self sax-filter) locator)
  (unless (sax-filter-block-p self)
    (start-document (sax-producer-consumer self) locator)))

(defmethod end-document ((self sax-filter) mode)
  (unless (sax-filter-block-p self)
    (end-document (sax-producer-consumer self) mode)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS NODE-POSITION-TRACKER
;;;

(defclass node-position-tracker (sax-filter)
  ((path-taken
    :initform nil
    :accessor tracker-path-taken)
   (horizontal-index
    :initform 0
    :accessor tracker-horizontal-index)))

(defmethod start-element :around ((self node-position-tracker) (tag open-tag) mode)
  (declare (ignore mode))
  (push (1+ (tracker-horizontal-index self)) (tracker-path-taken self))
  (setf (tracker-horizontal-index self) 0)
  (call-next-method))

(defmethod end-element :around ((self node-position-tracker) (tag open-tag) mode)
  (declare (ignore mode))
  (call-next-method)
  (setf (tracker-horizontal-index self) (pop (tracker-path-taken self))))
