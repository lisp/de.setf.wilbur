;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  platform.lisp
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
;;;   Purpose: This file contains various platform-dependent functions and macros.
;;;   Currently, we support MCL, OpenMCL, Allegro and SBCL. There is no reason why Wilbur
;;;   wouldn't run on other Common Lisps too, but some of these functions will have to be
;;;   ported separately.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   FEATURES, PACKAGES, ETC.
;;;

#+digitool
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :realmcl *features*)
  (require :opentransport))

;; #+(or :excl :sbcl)
#+excl ;; 2010-08-03 aserve failed to build with sbcl 1.0.36
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; adding this feature suppresses other HTTP client implementations
  (pushnew :http-using-aserve *features*)
  ;; other implementations may have other means of installing Portable AServe
  (require :aserve))

#+:excl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(mp:process-kill mp:process-wait mp:process-run-function)))

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sb-ext:process-wait)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :wilbur *features*)
  (pushnew :wilbur2 *features*))


;;; --------------------------------------------------------------------------------------
;;;
;;;   LOCKS
;;;

#-(and :digitool :CCL-5.2)              ; already defined
(defmacro with-lock ((lock &rest args) &body body)
  #+(or :digitool :clozure)  `(with-lock-grabbed (,lock ,@args) ,@body)
  #+:excl `(mp:with-process-lock (,lock ,@args) ,@body)
  #+:sbcl `(sb-thread:with-recursive-lock (,lock ,@args) ,@body)
  #-(or :digitool :clozure :excl :sbcl :lispworks) (error "No locking defined (WITH-LOCK)"))

#-(or :digitool :lispworks :clozure) ; unless already implemented
(defun make-lock ()
  #+:excl            (mp:make-process-lock)
  #+:sbcl            (sb-thread:make-mutex)
  #-(or :excl :sbcl) (error "No locking implemented"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   EXTERNAL PROCESSES, ETC.
;;;

(defun simple-external-process (cmd &rest args)
  (declare (dynamic-extent args))
  #+:openmcl (external-process-output-stream
	      (run-program cmd (remove nil args) :output :stream :wait nil))
  #+:excl    (run-shell-command (format nil "~A~{~@[ '~A'~]~}" cmd args)
				:output :stream :wait nil)
  #+:sbcl    (sb-ext:process-output
	      (sb-ext:run-program cmd (remove nil args) :output :stream :wait nil))
  #-(or :openmcl :excl :sbcl)
  (error "Cannot execute \"~A~{~@[ ~A~]~}\". No external processes" cmd args))

(defun quit-lisp-process ()
  #+(or :digitool :openmcl)   (ccl:quit)
  #+:excl                     (excl:exit)
  #+:sbcl                     (sb-ext:quit)
  #-(or :openmcl :excl :digitool :sbcl)
  (warn "Don't know how to quit Lisp"))

(defun get-env (key)
  #+:openmcl                  (ccl:getenv key)
  #+:excl                     (sys:getenv key)
  #+:digitool                 (bsd:getenv key)
  #+:sbcl                     (sb-ext:posix-getenv (string key))
  #-(or :openmcl :excl :digitool :sbcl)
  (error "Cannot get the environment variable ~S" key))
