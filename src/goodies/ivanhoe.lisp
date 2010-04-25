;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  ivanhoe.lisp
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
;;;   Purpose: The old "db-hiding" frame API ("Ivanhoe") is grandfathered here.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   OLD "TOP-LEVEL" API
;;;

(defvar *db* nil) ; "current" database

(defun triple (subject predicate object &optional source)
  (db-make-triple *db* subject predicate object source))

(defun add-triple (triple)
  (db-add-triple *db* triple))

(defun del-triple (triple)
  (db-del-triple *db* triple))

(defun query (subject predicate object)
  (db-query *db* subject predicate object))

(defun reify (triple &key (statement-uri nil) (source nil))
  (db-reify triple *db* statement-uri source))


;;; --------------------------------------------------------------------------------------
;;;
;;;   FRAME SYSTEM API IMPLEMENTATION
;;;

(defun frame (uri &rest slot/value-pairs)
  (let ((frame (node uri)))
    (dolist (slot/value-pair slot/value-pairs)
      (destructuring-bind (slot . value) slot/value-pair
        (add-value frame slot value)))
    frame))

(defun own-slots (frame)
  (remove-duplicates (mapcar #'triple-predicate (db-query *db* frame nil nil))))

(defun value (frame &rest paths)
  (declare (dynamic-extent paths))
  (flet ((v (path) (get-value frame path *db*)))
    (declare (dynamic-extent #'v))
  (apply #'values (mapcar #'v paths))))

(defun all-values (frame &rest paths)
  (declare (dynamic-extent paths))
  (flet ((av (path) (get-all-values frame path *db*)))
    (declare (dynamic-extent #'av))
    (apply #'values (mapcar #'av paths))))

(defun add-value (frame path value)
  (db-add-triple *db* (db-make-triple *db* frame path value))
  value)

(defun del-value (frame path &optional value)
  (dolist (triple (db-query *db* frame path value))
    (db-del-triple *db* triple)))

(defun relatedp (source path sink &optional action)
  (frames-related-p source path sink *db* action))

(defun load-db (source &rest options)
  (declare (dynamic-extent options))
  (apply #'db-load *db* source options))
