;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  data-sources.lisp
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
;;;   Purpose: Implements the interface to various (loadable) data sources.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   BASIC DATA SOURCE INTERFACE
;;;

(defmethod db-load ((db db) source
		    &rest options
		    &key (error-handling :signal)
		         (merge-results-p (eq error-handling :signal))
		         (clear-temporary-db-p t)
		         (verbosep *load-verbose*)
		         (appendp nil)
		    &allow-other-keys)
  ;; returns: SOURCE-DESC, # TRIPLES, ERRORS
  (declare (dynamic-extent options))
  (remf options :merge-results-p)
  (when verbosep
    (format *error-output* "~&Loading RDF: ~S..." (source-locator source))
    (force-output *error-output*))
  (multiple-value-bind (source-desc temporary-db errors)
		       (apply #'db-load-using-source db source options)
    (when (and source-desc temporary-db merge-results-p)
      (unless appendp
	(db-del-source db (source-desc-url source-desc)))
      (db-merge db temporary-db))
    (multiple-value-prog1 (values source-desc
				  (if (and temporary-db clear-temporary-db-p)
				    (prog1 (length (db-triples temporary-db))
				      (unless (eq db temporary-db)
					(db-clear temporary-db)))
				    temporary-db)
				  errors)
      (when source-desc
	(setf (source-desc-load-annotations source-desc) errors))
      (when verbosep
	(format *error-output* "~:[done~;failed~].~%" errors)))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SOURCE-DESC
;;;

(defclass source-desc ()
  ((url
    :initarg :url
    :initform nil
    :reader source-desc-url)
   (loaded-from
    :initform nil
    :accessor source-desc-loaded-from
    :initarg :locator
    :reader source-locator)
   (load-time
    :initform nil
    :accessor source-desc-load-time)
   (load-annotations
    :initform nil
    :accessor source-desc-load-annotations)
   (prefix
    :initarg :prefix
    :initform nil
    :accessor source-desc-prefix)))

(defmethod print-object ((self source-desc) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (prin1 (source-desc-url self) stream)))

(defmethod source-locator :around ((self source-desc))
  (or (call-next-method)
      (source-desc-url self)))

(defmethod db-find-source-desc ((db db) (url string) &optional (createp t))
  (db-find-source-desc db (make-url url) createp))

(defmethod db-find-source-desc ((db db) (url url) &optional (createp t))
  (let ((sources (db-source-descs db))
	(url-node (node (url-string url))))
    (or (find url-node sources :key #'source-desc-url)
	(find url-node sources :key #'source-desc-loaded-from)
	(and createp
	     (first (push (make-instance 'source-desc :url url-node)
			  (db-source-descs db)))))))

(defmethod db-find-source-desc ((db db) (url node) &optional (createp t))
  (db-find-source-desc db (node-uri url) createp))

(defmethod db-source-real-url ((db db) (source node))
  (let ((desc (db-find-source-desc db source nil)))
    (and desc (source-desc-loaded-from desc))))

(defmethod db-source-loaded-p ((db db) source)
  (declare (ignore source))
  nil)


;;; --------------------------------------------------------------------------------------
;;;
;;;   DB-LOAD-USING-SOURCE AND FRIENDS
;;;

(defmethod db-load-using-source ((db db) (source string) &rest options)
  (declare (dynamic-extent options))
  (apply #'db-load-using-source db (db-find-source-desc db source) options))

(defmethod db-load-using-source ((db db) (source url)
				 &rest options)
  (declare (dynamic-extent options))
  (apply #'db-load-using-source db (db-find-source-desc db source) options))

(defmethod db-load-using-source ((db db) source ; was: source-desc
				 &rest options
				 &key (locator nil locatorp)
				      (error-handling :signal)
				 &allow-other-keys)
  ;; returns: SOURCE-DESC, TEMPORARY DB, ERRORS
  (declare (dynamic-extent options))
  (assert (typep source 'source-desc))
  (unless (db-find-source-desc db (source-desc-url source) nil)
    (push source (db-source-descs db)))
  (let ((errors nil))
    ;; This mimics the possible expansion of WITH-OPEN-FILE
    (multiple-value-bind (temporary-db source-node)
	(handler-case (let ((abortp t))
			(multiple-value-bind (stream true-url)
					     (source-open-stream source)
			  (remf options :error-handling)
			  (unwind-protect (multiple-value-prog1
					      (apply #'source-fill-db source nil stream
						     (if locatorp
						       locator
						       (url-string true-url))
						     options)
					    (setf abortp nil))
			    (source-close-stream source stream abortp))))
	  (error (e)
	    (ecase error-handling
	      (:signal
	       (cerror "Keep going" e))
	      (:collect
	       (push e errors)
	       (continue e))
	      (:collect-first
	       (push e errors)
	       nil))))
      (when (source-desc-prefix source)
	(add-namespace (source-desc-prefix source)
		       (node-uri (if (source-desc-loaded-from source)
				   (source-desc-url source)
				   source-node))))
      (unless errors
	(setf (source-desc-load-time source) (get-universal-time)
	      (source-desc-loaded-from source) source-node))
      (values source temporary-db errors))))

(defmethod source-fill-db (source db stream locator &rest options)
  (declare (ignore source db))
  (apply #'parse-db-from-stream stream locator options))

(defmethod source-close-stream (source stream &optional abortp)
  (declare (ignore source))
  (close stream :abort abortp))

(defmethod source-locator ((source string)) ; assuming it is a URL
  source)

(defmethod source-locator ((source url))
  (url-string source))

(defmethod source-open-stream ((source source-desc))
  (source-open-stream (node-uri (or (source-desc-loaded-from source)
				    (source-desc-url source)))))

(defmethod source-open-stream ((source string))
  (source-open-stream (make-url source)))

(defmethod source-open-stream ((source file-url))
  (values (open (url-path source)) source))

(defmethod source-open-stream ((source http-url))
  (multiple-value-bind (response true-url)
		       (http-request source :get)
    (values (http-body response)
	    (or true-url source))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SOURCE-WITH-MODIFICATION
;;;

(defclass source-with-modification ()
  ((original-source
    :accessor source-original-source)
   (original-stream
    :initform nil
    :accessor source-original-stream)))

(defmethod initialize-instance :after ((self source-with-modification)
				       &rest options
				       &key original-source
				       &allow-other-keys)
  (declare (ignore options))
  (setf (source-original-source self) (if (stringp original-source)
					(make-url original-source)
					original-source)))

(defgeneric source-modification (source original-stream))

(defmethod source-locator ((source source-with-modification))
  (source-locator (source-original-source source)))

(defmethod source-open-stream ((source source-with-modification))
  (source-modification source
		       (setf (source-original-stream source)
			     (source-open-stream (source-original-source source)))))

(defmethod source-close-stream :after ((source source-with-modification) stream
				       &optional abortp)
  (declare (ignore stream))
  (source-close-stream (source-original-source source)
		       (shiftf (source-original-stream source) nil)
		       abortp))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS COMPUTED-SOURCE
;;;

(defclass computed-source ()
  ())

(defmethod db-load-using-source ((db db) (source computed-source)
				 &rest options
				 &key (locator (source-locator source))
				 &allow-other-keys)
  (multiple-value-bind (source-desc temp-db errors)
		       (apply #'source-fill-db source db nil locator options)
    (setf (source-desc-load-time source-desc) (get-universal-time)
	  (source-desc-loaded-from source-desc) (source-desc-url source-desc))
    (values source-desc temp-db errors)))

(defmethod source-make-temporary-db ((source computed-source) (db db))
  nil)


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS STRING-SOURCE
;;;

(defclass string-source (computed-source)
  ((locator
    :initarg :locator
    :initform nil
    :reader source-locator)
   (string
    :initarg :string
    :initform nil
    :reader source-string)))

(defmethod source-fill-db ((source string-source) db stream locator &rest options)
  (declare (ignore stream))
  (multiple-value-bind (temporary-db source-node)
		       (with-input-from-string (stream (source-string source))
			 (apply #'parse-db-from-stream stream locator options))
    (values (db-find-source-desc db (make-url (node-uri source-node)))
	    temporary-db
	    nil)))
