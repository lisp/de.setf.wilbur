;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  literal.lisp
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
;;;   Purpose: Definition of the class LITERAL and associated functionality
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   LITERAL PROTOCOL
;;;

(defgeneric literal-string (literal))
(defgeneric literal-language (literal))
(defgeneric literal-datatype (literal))
(defgeneric literal-value (literal))
(defgeneric (setf literal-value) (value literal))
(defgeneric compute-literal-value (literal datatype string))
(defgeneric literal= (literal other-literal))
(defgeneric compute-literal-value-error (literal datatype string &rest options))
(defgeneric literal-value->string (datatype value))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS LITERAL
;;;

(defclass literal ()
  ((string
    :initarg :string
    :initform nil
    :reader literal-string)
   (language
    :initarg :language
    :initform nil
    :reader literal-language)
   (datatype
    :initarg :datatype
    :initform nil
    :reader literal-datatype)
   (value
    :accessor literal-value)))

(defmethod literal-string ((literal string))
  literal)

(defmethod literal-language ((literal string))
  nil)

(defmethod literal-datatype ((literal string))
  nil)

(defmethod literal-value ((literal string))
  literal)

(defmethod literal-value :around ((literal literal))
  (if (slot-boundp literal 'value)
    (call-next-method)
    (setf (literal-value literal)
	  (compute-literal-value literal
				 (literal-datatype literal) (literal-string literal)))))

(defmethod print-object ((self literal) stream)
  (princ #\# stream)
  (print-literal-for-ntriples self stream))

(defmethod compute-literal-value ((literal literal)
                                  (datatype null)
                                  string)
  string)

(defun %literal= (string datatype language other-literal)
  (and (string= string (literal-string other-literal))
       (if datatype
	 (eql datatype (literal-datatype other-literal))
	 (let ((other-language (literal-language other-literal)))
	   (or (and (null language) (null other-language))
	       (string-equal language other-language))))))

(defmethod literal= ((literal string) (other-literal string))
  (string= literal other-literal))

(defmethod literal= ((literal literal) (other-literal literal))
  (%literal= (literal-string literal)
	     (literal-datatype literal) (literal-language literal)
	     other-literal))

(defmethod literal= ((literal literal) (other-literal string))
  (%literal= other-literal nil nil literal))

(defmethod literal= ((literal string) (other-literal literal))
  (%literal= literal nil nil other-literal))

(defmethod literal= (literal other-literal)
  (declare (ignore literal other-literal))
  nil)

(defmethod literal-language-match-p ((literal literal) language)
  (string-equal (literal-language literal) language :end1 (length language)))

(defmethod literal-language-match-p (thing language)
  (declare (ignore thing language))
  nil)

(defmethod compute-literal-value-error ((literal literal)
                                        (datatype node)
                                        string
                                        &key (value string)
                                             (warn-only-p nil))
  (unless warn-only-p
    (cerror (format nil "Use value ~S instead" value)
            'datatype-parse-error :thing string))
  (warn "Ignoring literal datatype ~S for literal ~S" datatype string)
  value)
                                        

;;; --------------------------------------------------------------------------------------
;;;
;;;   LITERAL PARSING
;;;
;;;   This is the template for COMPUTE-LITERAL-VALUE methods:
;;;
;;;   (defmethod compute-literal-value ((literal literal)
;;;                                     (datatype (eql !xsd:...))
;;;                                     string)
;;;     ...)
;;;

(defmethod compute-literal-value ((literal literal)
                                  (datatype node)
                                  string)
  (compute-literal-value-error literal datatype string :warn-only-p t))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:string))
                                  string)
  string)

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:boolean))
                                  string)
  (cond ((or (string= string "1") (string= string "true"))  t)
        ((or (string= string "0") (string= string "false")) nil)
        (t (compute-literal-value-error literal datatype string :value nil))))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:float))
                                  string)
  (compute-literal-value-float literal datatype string))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:double))
                                  string)
  (compute-literal-value-float literal datatype string))

(defun compute-literal-value-float (literal datatype string)
  (multiple-value-bind (value n)
                       (read-from-string string :eof-error-p nil)
    (if (and value (numberp value) (= n (length string)))
      (float value)
      (compute-literal-value-error literal datatype string :value 1.0))))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:dateTime))
                                  string)
  (or (ignore-errors (parse-iso8601-date string))
      (compute-literal-value-error literal datatype string :value 0)))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:date))
                                  string)
  (or (and (= (length string) 10)
	   (ignore-errors (parse-iso8601-date string)))
      (compute-literal-value-error literal datatype string :value 0)))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:normalizedString))
                                  string)
  (flet ((illegalp (c)
	   (or (char= c #\Return)
	       (char= c #\Linefeed)
	       (char= c #\Tab))))
    (declare (dynamic-extent #'illegalp))
    (if (find-if #'illegalp string)
      (compute-literal-value-error literal datatype string
				   :value (substitute-if #\Space #'illegalp string))
      string)))

(defmethod compute-literal-value ((literal literal)
                                  (datatype (eql !xsd:integer))
                                  string)
  (compute-literal-value-integer literal datatype string))

(defmethod compute-literal-value ((literal literal)
				  (datatype (eql !xsd:int))
				  string)
  (compute-literal-value-integer literal datatype string))

(defun compute-literal-value-integer (literal datatype string)
  (or (parse-integer string :junk-allowed t)
      (compute-literal-value-error literal datatype string :value 0)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   OUTPUT FUNCTIONS
;;;

(defmethod print-literal-for-ntriples ((literal literal) stream)
  (let ((datatype (literal-datatype literal)))
    (format stream "~S~@[@~A~]~@[^^<~A>~]"
            (literal-string literal)
            (literal-language literal)
            (and datatype (find-short-name *nodes* (node-uri datatype))))))

(defmethod literal-value->string ((datatype (eql !xsd:string))
                                  (value string))
  value)

(defmethod literal-value->string ((datatype (eql !xsd:boolean))
                                  value)
  (if value "true" "false"))

(defmethod literal-value->string ((datatype (eql !xsd:float))
                                  (value float))
  (prin1-to-string value))

(defmethod literal-value->string ((datatype (eql !xsd:double))
                                  (value float))
  (prin1-to-string value))

(defmethod literal-value->string ((datatype (eql !xsd:dateTime))
                                  (value integer))
  (iso8601-date-string value))

(defmethod literal-value->string ((datatype (eql !xsd:date))
                                  (value integer))
  (iso8601-date-string value t))

(defmethod literal-value->string ((datatype (eql !xsd:normalizedString))
                                  (value string))
  (flet ((illegalp (c)
	   (or (char= c #\Return)
	       (char= c #\Linefeed)
	       (char= c #\Tab))))
    (declare (dynamic-extent #'illegalp))
    (assert (not (find-if #'illegalp value)))
    value))

(defmethod literal-value->string ((datatype (eql !xsd:integer))
				  (value integer))
  (prin1-to-string value))

(defmethod literal-value->string ((datatype (eql !xsd:int))
				  (value integer))
  (prin1-to-string value))

(defmethod literal-value->string ((datatype null)
				  (value string))
  value)


;;; --------------------------------------------------------------------------------------
;;;
;;;   LITERAL SHORTHAND SYNTAX
;;;

(defun literal (string &rest options)
  (declare (dynamic-extent options))
  (apply #'db-make-literal *db* string options))

(defmethod make-load-form ((literal literal) &optional env)
  (declare (ignore env))
  (let ((datatype (literal-datatype literal))
	(language (literal-language literal)))
    `(literal ,(literal-string literal)
              ,@(and datatype `(:datatype ,datatype))
              ,@(and language `(:language ,language)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun inline-literal-reader (stream char arg)
    (declare (ignore arg))
    (unread-char char stream)
    (let ((string (read stream t nil t)))
      ;; later, when I get around to it, we will also read datatype and language
      (literal string)))

  (defun enable-literal-shorthand ()
    (set-dispatch-macro-character #\# #\" #'inline-literal-reader))

  (enable-literal-shorthand))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS INTERNED-LITERAL
;;;   MIXIN CLASS INTERNED-LITERAL-DB-MIXIN
;;;   CLASS INTERNED-LITERAL-INDEXED-DB
;;;

(defclass interned-literal (literal node)
  ())

(defmethod literal= ((literal interned-literal) (other-literal interned-literal))
  (eq literal other-literal))

(defclass interned-literal-db-mixin () ; mix with class db
  ((literal-index
    :initform (make-hash-table :test #'equal)
    :reader db-literal-index))
  (:default-initargs
   :literal-class 'interned-literal))

(defmethod db-literal-index-get ((db interned-literal-db-mixin) string
				 &key datatype language
				 &allow-other-keys)
  (find-if #'(lambda (literal)
	       (%literal= string datatype language literal))
	   (gethash string (db-literal-index db))))

(defmethod (setf db-literal-index-get) ((literal interned-literal)
					(db interned-literal-db-mixin) string)
  ;; "It is an error" to call this without first checking for the prior existence
  ;; of the literal in the index
  (push literal (gethash string (db-literal-index db)))
  literal)

(defmethod db-make-literal ((db interned-literal-db-mixin) string &rest options)
  (declare (dynamic-extent options))
  (or (apply #'db-literal-index-get db string options)
      (setf (db-literal-index-get db string) (call-next-method))))

(defclass interned-literal-indexed-db (interned-literal-db-mixin indexed-db)
  ())

(defmethod db-literal-index-find ((db interned-literal-db-mixin) string)
  (let ((literals nil))
    (maphash #'(lambda (key value)
		 (when (name-contains-pattern-p key string)
		   (setf literals (append value literals))))
	     (db-literal-index db))
    (copy-list literals)))
