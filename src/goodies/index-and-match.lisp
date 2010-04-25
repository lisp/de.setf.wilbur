;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  index-and-match.lisp
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
;;;   Purpose: Some additional triple database functionality.
;;;


(in-package "WILBUR")


;;; ----------------------------------------------------------------------------
;;;
;;;   MIXIN CLASS BLANK-NODE-DB-MIXIN
;;;

(defclass blank-node-db-mixin () ; mix before class db
  ((startup-time
    :initform (get-universal-time)
    :reader db-startup-time)
   (blank-node-uri-prefix
    :initarg :blank-node-uri-prefix
    :initform "anon:"
    :reader db-blank-node-uri-prefix)
   (blank-node-index
    :initform 0
    :accessor db-blank-node-index)
   (blank-node->uri
    :initform (make-hash-table :test #'eq)
    :reader db-blank-node->uri)
   (uri->blank-node
    :initform (make-hash-table :test #'equal)
    :reader db-uri->blank-node)))

(defmethod db-resolve-blank-node-uri ((db blank-node-db-mixin) uri)
  (gethash uri (db-uri->blank-node db)))

(defmethod db-blank-node-uri ((db blank-node-db-mixin) (node node) &optional (createp t))
  (let ((node->uri (db-blank-node->uri db)))
    (or (gethash node node->uri)
	(when createp
	  (let ((uri (format nil "~A~X~X"
			     (db-blank-node-uri-prefix db)
			     (incf (db-blank-node-index db))
			     (db-startup-time db))))
	    (setf (gethash uri (db-uri->blank-node db)) node
		  (gethash node node->uri) uri))))))

(defmethod db-blank-node-uri-p ((db blank-node-db-mixin) uri)
  (let ((prefix (db-blank-node-uri-prefix db)))
    (string= uri prefix :end1 (length prefix))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   MIXIN CLASS INDEXED-LITERAL-DB-MIXIN
;;;

(defclass indexed-literal-db-mixin () ; mix before class db
  ((new-literals-lock
    :initform (make-lock)
    :reader db-new-literals-lock)
   (new-literals
    :initform nil
    :accessor db-new-literals)
   (max-string-length
    :initarg :max-string-length
    :initform 4
    :reader db-literal-index-max-string-length)
   (min-string-length
    :initarg :min-string-length
    :initform 2
    :reader db-literal-index-min-string-length)
   (supports-matching-p
    :initform #+(or :cl-ppcre :excl) t #-(or :cl-ppcre :excl) nil
    :reader db-supports-matching-p)
   (index-literals-p
    :initarg :index-literals-p
    :initform nil
    :reader db-index-literals-p)
   (literal-substring-index
    :initform (make-hash-table :test #'equal)
    :reader db-literal-substring-index)))

(defmethod (setf db-literal-index-get) :after ((literal interned-literal)
					       (db indexed-literal-db-mixin) string)
  (declare (ignore string))
  (with-lock ((db-new-literals-lock db))
    (push literal (db-new-literals db))))

(defun db-literal-index-add-substrings (db string literal)
  (let ((min (db-literal-index-min-string-length db))
	(max (db-literal-index-max-string-length db))
	(hash (db-literal-substring-index db)))
    (mapl #'(lambda (s1)
	      (mapl #'(lambda (s2)
			(when (<= min (length s2) max)
			  (pushnew literal
				   (gethash (concatenate 'string (reverse s2)) hash)
				   :test #'literal=)))
		    (reverse s1)))
	  (concatenate 'list string))
    literal))

(defmethod db-find-literals ((db indexed-literal-db-mixin) substring)
  ;; Note: this function matches strings approximately (the substring is broken into
  ;;  segments shorter than the max indexed substring, and segments shorter than the
  ;;  min indexed substring are thrown away. Assumption is that this function is only
  ;;  used to perform initial filtering for an implementation of (say) a regexp match.
  (let ((min (db-literal-index-min-string-length db))
	(max (db-literal-index-max-string-length db))
	(n (length substring))
	(hash (db-literal-substring-index db)))
    (if (<= n max)
      (gethash substring hash)
      (reduce #'(lambda (s1 s2)
		  (intersection s1 s2 :test #'literal=))
	      (mapcar #'(lambda (s)
			  (gethash s hash))
		      (let ((substrings nil))
			(dotimes (i (ceiling n max))
			  (let* ((j (* i max))
				 (k (min (+ j max) n)))
			    (when (>= (- k j) min)
			      (push (subseq substring j k) substrings))))
			substrings))))))

(defmethod db-find-literals-multiple ((db indexed-literal-db-mixin)
				      substring &rest more-substrings)
  (declare (dynamic-extent more-substrings))
  (if more-substrings
    (intersection (apply #'db-find-literals-multiple db more-substrings)
		  (db-find-literals db substring)
		  :test #'literal=)
    (db-find-literals db substring)))

(defmethod db-index-literals ((db indexed-literal-db-mixin))
  (when (db-index-literals-p db)
    (loop (let ((literal (with-lock ((db-new-literals-lock db))
			   (pop (db-new-literals db)))))
	    (if literal
	      (db-literal-index-add-substrings db (literal-string literal) literal)
	      (return-from db-index-literals))))))

(defun convert-match-pattern (pattern)
  (let* ((chars nil)
	 (strings (mapcan #'(lambda (c)
			      (cond ((char= c #\*)
				     (list (concatenate 'string
							(nreverse (shiftf chars nil)))
					   nil))
				    ((= (char-code c) 0)
				     (when chars
				       (list (concatenate 'string (nreverse chars)))))
				    (t
				     (push c chars)
				     nil)))
			  (concatenate 'list pattern (string (code-char 0))))))
    (values (remove-if #'(lambda (s)
			   (or (null s)
			       (< (length s) 2)))
		       strings)
	    (apply #'concatenate 'string (mapcar #'(lambda (s)
						     (or s ".*"))
						 strings)))))

#+(and :cl-ppcre (not :excl))
(defmethod db-match-literals ((db indexed-literal-db-mixin) (pattern string))
  (multiple-value-bind (substrings pattern)
		       (convert-match-pattern pattern)
    (let ((regexp (cl-ppcre:create-scanner pattern)))
      (remove-if-not #'(lambda (literal)
			 (cl-ppcre:all-matches regexp (literal-string literal)))
		     (apply #'db-find-literals-multiple db substrings)))))

#+(and :excl (not :cl-ppcre))
(defmethod db-match-literals ((db indexed-literal-db-mixin) (pattern string))
  (multiple-value-bind (substrings pattern)
		       (convert-match-pattern pattern)
    (let ((regexp (compile-regexp pattern)))
      (remove-if-not #'(lambda (literal)
			 (match-regexp regexp (literal-string literal)))
		     (apply #'db-find-literals-multiple db substrings)))))

#-(or :cl-ppcre :excl)
(defmethod db-match-literals ((db indexed-literal-db-mixin) (pattern string))
  (declare (ignore db pattern))
  nil)
