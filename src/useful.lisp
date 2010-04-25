;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  useful.lisp
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
;;;   Purpose: Useful functions and macros
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   GENERALLY USEFUL STUFF
;;;

(defmacro with-temps ((&rest variables) &body body)
  `(let (,@(mapcar #'(lambda (variable)
		       `(,variable (gentemp)))
		   variables))
    ,@body))

(defmacro dolist+ ((pattern list &optional (value nil value-supplied-p)) &body body)
  (if (symbolp pattern)
    `(dolist (,pattern ,list ,@(and value-supplied-p (list value)))
       ,@body)
    (let ((i (gentemp)))
      `(dolist (,i ,list ,@(and value-supplied-p (list value)))
	 (destructuring-bind ,pattern ,i
	   ,@body)))))

(defmacro dsb (pattern form &body body)
  `(destructuring-bind ,pattern ,form ,@body))

(defun remove-weird (sequence item &rest options)
  (declare (dynamic-extent options))
  (apply #'remove item sequence options))

(defun delete-weird (sequence item &rest options)
  (declare (dynamic-extent options))
  (apply #'delete item sequence options))

(define-modify-macro removef (items &rest options) remove-weird)

(define-modify-macro deletef (items &rest options) delete-weird)

(define-modify-macro unionf (items) union)

(defun eq~ (x y)
  (or (null x)
      (null y)
      (eq x y)))

(declaim (inline eq~))

(defun string->keyword (string &optional (package :keyword))
  (if package (intern (string-upcase string) package) string))


;;; --------------------------------------------------------------------------------------
;;;
;;;   STRING DICTIONARY
;;;
;;;   Some care must be taken when using this, since (in the interest of making the
;;;   implementation not cons so much) we have used destructive operations.
;;;

(defun string-dict-get (keys&values key)
  (cdr (assoc key keys&values :test #'string=)))

(defun string-dict-get-by-value (keys&values value)
  (car (rassoc value keys&values :test #'string=)))

(defun string-dict-add (keys&values key value)
  (acons key value keys&values))

(defun string-dict-del (keys&values key)
  (delete key keys&values :key #'car :test #'string=))

(defmacro do-string-dict ((key value dict) &body body)
  `(loop for (,key . ,value) in ,dict do (progn ,@body)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   LIST MANIPULATION
;;;

(defun split-list (head tail n &optional (no-split-p nil))
  (if no-split-p
    (values tail nil)
    (if (and tail (plusp n))
      (split-list (cons (first tail) head) (rest tail) (1- n) no-split-p)
      (values (nreverse head) tail))))

(defun prioritize-list (list possible-priority-items
			&key (test #'eql) (key #'identity))
  (prioritize list :prefer possible-priority-items :test test :key key))

(defun prioritize (list
		   &key (prefer nil)
		        (exclude nil)
		        (test #'eql)
		        (key #'identity)
		        (splitp nil))
  (let* ((items (remove-if #'(lambda (item)
			       (find-if #'(lambda (e)
					    (funcall test e (funcall key item)))
					exclude))
			   list))
	 (priority-items (mapcan #'(lambda (p)
				     (let ((item (find p items :test test :key key)))
				       (and item (list item))))
				 prefer))
	 (other-items (remove-if #'(lambda (item)
				     (find-if #'(lambda (p)
						  (funcall test
							   (funcall key p)
							   (funcall key item)))
					      priority-items))
				 items)))
    (if splitp
      (values priority-items other-items)
      (append priority-items other-items))))
