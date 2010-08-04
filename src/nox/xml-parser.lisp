;;; -*- package: NOX; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  xml-parser.lisp
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
;;;   Purpose: This file contains an implementation of an XML parser. This
;;;   parser was motivated by RDF, and consequently does not implement all the
;;;   features of XML 1.0. In fact, it needs a lot of work. Tough...
;;;


(in-package "NOX")


;;; --------------------------------------------------------------------------------------
;;;
;;;   NAME READTABLE
;;;


(defequal -name-start-characters-
  (let ((s (concatenate 'string
                        (loop for i from (char-code #\a) to (char-code #\z)
                              collect (code-char i)))))
    (concatenate 'string s (string-upcase s) "_:")))
(defequal -name-characters-
  (let ((v (make-array 256)))
    (dotimes (i 256)
      (setf (svref v i) nil))
    (dolist (c (concatenate 'list -name-start-characters-
                            (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\-)))
      (setf (svref v (char-code c)) t))
    v))

(defvar *nr-buffer* (make-base-string 256 :adjustable t :fill-pointer 0))

(defun name-reader (stream char)
  (setf (fill-pointer *nr-buffer*) 0)
  (vector-push char *nr-buffer*)
  (with-loop&read-char (c stream nil)
    (cond ((and c (svref -name-characters- (char-code c)))
           (vector-push-extend c *nr-buffer*))
          (t
           (when c (unread-char c stream))
           (return (concatenate 'string *nr-buffer*))))))

(defun single-character-reader (stream char)
  (declare (ignore stream))
  char)

(defun not-allowed-reader (stream char)
  (declare (ignore stream))
  (error 'syntax-error :thing char))

(define-readtable *name-reader* nil
  (dotimes (i 256)
    (let ((c (code-char i)))
      (unless (whitespace-char-p c)
        (set-macro-character c #'not-allowed-reader))))
  (set-macro-character #\/ #'single-character-reader)
  (set-macro-character #\! #'name-reader)
  (set-macro-character #\? #'name-reader)
  (map nil #'(lambda (c)
               (set-macro-character c #'name-reader))
       -name-start-characters-))


;;; --------------------------------------------------------------------------------------
;;;
;;;   XML READTABLE
;;;

(defun read-declaration (stream name)
  (declare (special *dtd-reader*))      ; forward ref
  (cond ((string= name "!DOCTYPE")
         (let ((name (read-using *name-reader* stream t))
               (next (skip-whitespace stream)))
           (cond ((not (eql next #\[))
                  (make-instance 'dtd-start
                    :string name :externalp t
                    :stuff (read-delimited-list #\> stream t)))
                 (t
                  (setf (parser-in-dtd-p *current-parser*) t
                        (parser-readtable *current-parser*) *dtd-reader*)
                  (skip-whitespace stream t) ; skip [
                  (make-instance 'dtd-start :string name)))))
        ((string= name "!")
         (let ((char (read-char stream t nil t)))
           (cond ((char= char #\[) ; CDATA, INCLUDE, IGNORE
                  (let ((name (read-until-char stream #\[)))
                    (cond ((string= name "CDATA")
                           (read-until-%%> stream #\]))
                          ((find name '("INCLUDE" "IGNORE") :test #'string=)
                           (error 'xml-feature-not-supported :thing name))
                          (t
                           (error 'syntax-error :thing "!["))))))))
        ((string= name "!--" :end1 3)
         (make-instance 'comment :string (read-until-%%> stream #\-)))
        (t
         (error 'unknown-declaration :thing name))))

(defun open-anglebracket-reader (stream char)
  (declare (ignore char))
  (let ((name (read-using *name-reader* stream t)))
    (cond ((eql name #\/)
           (make-instance 'close-tag
             :string (first (read-delimited-list #\> stream t))))
          ((char= (char name 0) #\!)
           (read-declaration stream name))
          ((char= (char name 0) #\?)
           (let* ((stuff (read-delimited-list #\> stream t)))
             (if (eql (first (last stuff)) #\?)
               (make-instance 'proc-instruction :string name) ; ignore attrs
               (error 'pi-termination-problem :thing name))))
          (t
           (let* ((stuff (read-delimited-list #\> stream t))
                  (parent (first (parser-path *current-parser*)))
                  (tag (make-instance 'open-tag
                         :string name
                         :base (if parent
                                 (tag-base parent)
                                 (parser-locator *current-parser*))))
                  (attr nil))
             (loop (cond ((null stuff)
                          (return tag))
                         ((eql (setf attr (pop stuff)) #\/)
                          (setf (tag-empty-p tag) t)
                          (return tag))
                         ((eql (pop stuff) #\=)
                          (setf (tag-attribute tag attr) (pop stuff)))
                         (t
                          (error 'syntax-error :thing "missing =")))))))))

(defun quoted-string-reader (stream char)
  (read-until-char-expanding-entities stream char nil))

(defun read-xml-token (stream &aux (char (peek-char t stream nil nil)))
  (when char
    (if (or (char= char #\<)
            (and (char= char #\])
                 (parser-in-dtd-p *current-parser*)))
      (read-using (parser-readtable *current-parser*) stream)
      (read-until-char-expanding-entities stream #\< t))))

(define-readtable *xml-reader* nil
  (dotimes (i 256)
    (let ((c (code-char i)))
      (unless (whitespace-char-p c)
        (set-macro-character c #'not-allowed-reader))))
  (set-macro-character #\< #'open-anglebracket-reader)
  (set-macro-character #\> (get-macro-character #\)))
  (set-macro-character #\= #'single-character-reader)
  (set-macro-character #\/ #'single-character-reader)
  (set-macro-character #\? #'single-character-reader)
  (set-macro-character #\' #'quoted-string-reader)
  (set-macro-character #\" #'quoted-string-reader)
  (map nil #'(lambda (c)
               (set-macro-character c #'name-reader))
       -name-start-characters-))


;;; --------------------------------------------------------------------------------------
;;;
;;;   DTD READTABLE
;;;

(defun dtd-open-anglebracket-reader (stream char)
  (declare (ignore char))
  (let ((name (read-using *name-reader* stream t))
        (stuff (read-delimited-list #\> stream t)))
    (cond ((string= name "!ENTITY")
           (make-instance 'entity-declaration
             :name (pop stuff) :string (pop stuff)))
          ((string= name "!ELEMENT")
           (make-instance 'element-declaration
             :name (pop stuff) :contentspec (pop stuff)))
          ((string= name "!ATTLIST")
           (make-instance 'attlist-declaration
             :name (pop stuff)))
          ((string= name "!NOTATION")
           (error 'xml-feature-not-supported :thing name))
          (t
           (error 'unknown-declaration :thing name)))))

(defun dtd-parenthesis-reader (stream char)
  (declare (ignore char))
  (read-delimited-list #\) stream t))

(defun close-bracket-reader (stream char)
  (declare (ignore char))
  (cond ((not (parser-in-dtd-p *current-parser*))
         (error 'syntax-error :thing "]"))
        ((not (char= (skip-whitespace stream t) #\>))
         (error 'dtd-termination-problem))
        (t
         (setf (parser-readtable *current-parser*) *xml-reader*)
         (make-instance 'dtd-end))))

(define-readtable *dtd-reader* *xml-reader*
  (set-macro-character #\< #'dtd-open-anglebracket-reader)
  (set-macro-character #\# (get-macro-character #\A))
  (set-macro-character #\] #'close-bracket-reader)
  (set-macro-character #\( #'dtd-parenthesis-reader)
  (set-macro-character #\) (get-macro-character #\))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS XML-PARSER
;;;

(defclass xml-parser (sax-producer)
  ((expand-namespaces-p
    :initarg :expand-namespaces-p
    :initform t
    :reader parser-expand-namespaces-p)
   (entities
    :initform (make-hash-table :test #'equal)
    :reader parser-entities)
   (in-dtd-p
    :initform nil
    :accessor parser-in-dtd-p)
   (canonical-uris
    :initform (make-hash-table :test #'equal)
    :reader parser-canonical-uris)
   (readtable
    :initform nil
    :accessor parser-readtable)
   (path
    :initform nil
    :accessor parser-path)
   (locator
    :initform nil
    :accessor parser-locator)))

(defequal -standard-entities- '(("gt"   . ">")
                                ("lt"   . "<")
                                ("amp"  . "&")
                                ("quot" . "\"")
                                ("apos" . "'")))

(defmethod initialize-instance :after ((self xml-parser) &rest args)
  (declare (ignore args))
  (dolist (pair -standard-entities-)
    (destructuring-bind (n . e) pair (setf (get-entity self n) e)))
  (setf (get-canonical-uri self -alternate-rdf-uri-) -rdf-uri-
        (get-canonical-uri self -alternate-rdfs-uri-) -rdfs-uri-
        (get-canonical-uri self (subseq -rdfs-uri- 0 (1- (length -rdfs-uri-))))
         -rdfs-uri-))

(defun get-entity (parser name)
  (gethash name (parser-entities parser)))

(defun (setf get-entity) (definition parser name)
  (setf (gethash name (parser-entities parser)) definition))

(defun get-canonical-uri (parser uri)
  (gethash uri (parser-canonical-uris parser) uri))

(defun (setf get-canonical-uri) (new-uri parser uri)
  (setf (gethash uri (parser-canonical-uris parser)) new-uri))

(defmethod parse ((self xml-parser) stream locator)
  (declare (special *xml-parse-buffers*))
  (let ((*current-parser* self)
        (consumer (sax-producer-consumer self)))
    (with-resource-from-pool (*ruc-buffer* *xml-parse-buffers*)
      (with-resource-from-pool (*ruc-ee-buffer* *xml-parse-buffers*)
        (setf (parser-readtable self) *xml-reader*
	      (parser-locator self) locator)
        (handler-bind ((end-of-file #'(lambda (c)
                                        (declare (ignore c))
                                        (error 'syntax-error :thing "eof"))))
          (start-document consumer locator)
          (parse-start self stream nil nil)
          (end-document consumer (sax-consumer-mode consumer)))))))

(defun parse-start (parser stream end namespaces &aux continuep)
  (loop (multiple-value-setq (continuep namespaces)
	  (parse-token parser stream (read-xml-token stream) end namespaces))
	(unless continuep
	  (return-from parse-start nil))))

(defmethod parse-token ((self xml-parser)
                        stream (token string) ; char-content
                        end namespaces)
  (declare (ignore stream))
  (char-content (sax-producer-consumer self) (collapse-whitespace token)
                (sax-consumer-mode (sax-producer-consumer self)))
  (values end namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token open-tag) end namespaces)
  (flet ((expand (name)
           (or (expand-name-with-namespace name namespaces)
               (progn (cerror "Do not expand"
                              'missing-namespace-definition :thing name)
                      name))))
    (declare (dynamic-extent #'expand))
    (let ((consumer (sax-producer-consumer self)))
      (when (parser-expand-namespaces-p self)
        (setf namespaces (add-namespaces self token namespaces))
        (shiftf (tag-original-name token)
                (token-string token)
                (expand (token-string token)))
        (dolist (k&v (tag-attributes token))
          (setf (car k&v) (expand (car k&v)))))
      (do-string-dict (key value (tag-attributes token))
        (when (string= key "xml:base")
          (setf (tag-attributes token)
                (string-dict-del (tag-attributes token) key))
          (setf (tag-base token) value)))
      (setf (tag-namespaces token) namespaces)
      (push token (parser-path self))
      (start-element consumer token (sax-consumer-mode consumer))
      (cond ((tag-empty-p token)
             (end-element consumer token (sax-consumer-mode consumer))
             (pop (parser-path self)))
            (t
             (parse-start self stream token namespaces)))
      (values end namespaces))))

(defun add-namespaces (parser tag namespaces)
  (do-string-dict (key value (tag-attributes tag))
    (multiple-value-bind (n p) (name&prefix key)
      (cond ((string= p "xmlns")
             (let ((uri (get-canonical-uri parser value)))
               (setf (tag-attributes tag)
                     (string-dict-del (tag-attributes tag) key))
               (setf namespaces
                     (string-dict-add namespaces n uri))
               (maybe-use-namespace (sax-producer-consumer parser) n uri)))
            ((and (null p) (string= n "xmlns"))
             (setf (tag-attributes tag)
                   (string-dict-del (tag-attributes tag) key))
             (setf namespaces
                   (string-dict-add namespaces
                                    nil (get-canonical-uri parser value)))))))
  namespaces)

(defun ends-in-hash-p (string)
  (declare (type string string)
	   (optimize (speed 3) (space 3) (safety 0)))
  (let ((c (char string (1- (length string)))))
    (declare (type character c))
    (or (char= c #\#)
        (char= c #\/)
	(char= c #\:))))

(defun expand-name-with-namespace (string namespaces)
  (multiple-value-bind (n p) (name&prefix string)
    (or (and (null p)
             (hack-rdf-attribute-name n namespaces))
        (let ((uri (string-dict-get namespaces p)))
          (cond (uri
                 (values (concatenate 'string uri (and (not (ends-in-hash-p uri)) "#") n)
			 n p))
                ((or (null p) (string-equal p "xml"))
                 (values string nil nil))
                (t
                 (values nil n p)))))))

(defun hack-rdf-attribute-name (name namespaces)
  (and (car (rassoc -rdf-uri- namespaces :test #'string=))
       (cdr (assoc name -rdf-attr-map- :test #'string=))))

(defmethod parse-token ((self xml-parser)
                        stream (token close-tag) end namespaces)
  (declare (ignore stream))
  (cond ((null end)
         (error 'unexpected-end-tag :thing (token-string end)))
        ((string= (tag-original-name end) (token-string token))
         (setf (tag-counterpart token) end
               (tag-counterpart end) token)
         (end-element (sax-producer-consumer self) end
                      (sax-consumer-mode (sax-producer-consumer self)))
         (pop (parser-path self))
         (values nil namespaces))
        (t
         (error 'unexpected-end-tag
                :expectation (tag-original-name end)
                :thing (token-string token)))))

(defmethod parse-token ((self xml-parser)
                        stream (token proc-instruction) end namespaces)
  (declare (ignore stream end))
  (let ((consumer (sax-producer-consumer self)))
    (proc-instruction consumer token (sax-consumer-mode consumer))
    (values t namespaces)))

(defmethod parse-token ((self xml-parser)
                        stream (token entity-declaration) end namespaces)
  (declare (ignore stream end))
  (setf (get-entity self (entity-name token)) (token-string token))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token comment) end namespaces)
  (declare (ignore stream end))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token dtd-start) end namespaces)
  (declare (ignore stream end))
  (when (dtd-external-p token)
    (xml-warning "External DTD ignored:~{ ~S~}" (dtd-stuff token)))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token dtd-end) end namespaces)
  (declare (ignore stream end))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token dtd-declaration) end namespaces)
  (declare (ignore stream end))
  (xml-warning "~S ignored" (class-name (class-of token)))
  (values t namespaces))

(defmethod parse-token ((self xml-parser) stream token end namespaces)
  (declare (ignore stream end))
  (if token
    (error 'syntax-error :thing token)
    (values nil namespaces))) ; null token signifies eof

(defun parse-from-stream (stream locator parser-class &rest options)
  (declare (dynamic-extent options))
  (let ((parser (apply #'make-instance parser-class options)))
    (handler-case (values (parse parser stream locator) parser)
      (xml-error (e)
	(let ((*readtable* (copy-readtable nil)))
	  (cerror "Keep going" e))))))


;;; --------------------------------------------------------------------------------------
;;;
;;;    CLASS XML-FORMATTER
;;;

(defclass xml-formatter (sax-consumer)
  ((stream
    :initarg :stream
    :initform nil
    :reader formatter-stream)
   (level
    :initform 0
    :accessor formatter-level)
   (indent-delta
    :initarg :indent-delta
    :initform 2
    :reader formatter-indent-delta)))

(defmethod replay ((formatter xml-formatter) events)
  (dolist (event events)
    (let ((mode (sax-consumer-mode formatter)))
      (etypecase event
        (open-tag
         (start-element formatter event mode))
        (close-tag
         (end-element formatter (tag-counterpart event) mode))
        (string
         (char-content formatter event mode))))))

(defun reverse-expand-name (name namespaces
			    &optional use-entities-p
			    &aux (nn (length name)))
  (do-string-dict (prefix uri namespaces)
    (let ((un (length uri)))
      (when (and (>= nn un)
                 (string= name uri :end1 un))
        (return-from reverse-expand-name
          (values (if (= nn un)
                    (format nil "~@[~A~]:" prefix)
                    (format nil (if use-entities-p "~@[&~A;~]~A" "~@[~A:~]~A")
                            prefix
                            (let ((n (subseq name un)))
                              (if (char= (char n 0) #\#)
                                (subseq n 1) n))))
                  t)))))
  (values name nil))

(defmethod start-element ((self xml-formatter) (tag open-tag) mode)
  (declare (ignore mode))
  (let ((stream (formatter-stream self)))
    (format stream "~&~V@T<~A"
            (formatter-level self)
            (reverse-expand-name (token-string tag) (tag-namespaces tag)))
    (do-string-dict (attribute value (tag-attributes tag))
      (format stream " ~A=\"~A\""
              (reverse-expand-name attribute (tag-namespaces tag))
              value))
    (princ (if (tag-empty-p tag) "/>" #\>) stream)
    (incf (formatter-level self) (formatter-indent-delta self))))

(defmethod end-element ((self xml-formatter) tag mode)
  (declare (ignore mode))
  (decf (formatter-level self) (formatter-indent-delta self))
  (unless (tag-empty-p tag)
    (format (formatter-stream self) "~&~V@T</~A>"
            (formatter-level self)
            (reverse-expand-name (token-string tag) (tag-namespaces tag)))))

(defmethod char-content ((self xml-formatter) char-content mode)
  (declare (ignore mode))
  (princ (string-trim '(#\Space #\Tab #\Newline) char-content)
         (formatter-stream self)))

(defmethod start-document ((self xml-formatter) locator)
  (declare (ignore locator))
  (format (formatter-stream self) "~&<?xml version=\"1.0\"?>"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS TREE-PARSER
;;;

(defclass tree-parser (sax-consumer)
  ((states
    :initform nil
    :accessor parser-states)
   (package
    :initarg :package
    :initform (find-package :keyword)
    :reader parser-package)))

(defmethod initialize-instance :after ((self tree-parser) &rest args
                                       &key producer &allow-other-keys)
  (declare (ignore args))
  (if producer
    (setf (sax-consumer-producer self) producer
          (sax-producer-consumer producer) self)
    (setf (sax-consumer-producer self) (make-instance 'xml-parser :consumer self))))

(defmethod parser-interpret-content ((parser tree-parser) (content string))
  content)

(defmethod start-element ((parser tree-parser) (tag open-tag) mode)
  (declare (ignore mode))
  (push (list (list (string->keyword (token-string tag) (parser-package parser))))
	(parser-states parser)))

(defmethod end-element ((parser tree-parser) (tag open-tag) mode)
  (declare (ignore mode))
  (push (reverse (first (pop (parser-states parser))))
	(car (first (parser-states parser)))))

(defmethod char-content ((parser tree-parser) (content string) mode)
  (declare (ignore mode))
  (push (parser-interpret-content parser content)
        (car (first (parser-states parser)))))

(defmethod parse ((parser tree-parser) stream locator)
  (setf (parser-states parser) (list (list nil)))
  (parse (find-first-producer parser) stream locator)
  (caar (pop (parser-states parser))))
