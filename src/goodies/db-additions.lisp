;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  db-additions.lisp
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
;;;   Purpose: Some additional triple database functionality
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defmethod db-reify ((triple triple) (db db)
                     &optional (statement-uri nil)
                               (source nil))
  (let ((node (node statement-uri)))
    (flet ((make-and-add-triple (p o)
	     (db-add-triple db (db-make-triple db node (node p) o source))))
      (make-and-add-triple -rdf-subject-uri-   (triple-subject triple))
      (make-and-add-triple -rdf-predicate-uri- (triple-predicate triple))
      (make-and-add-triple -rdf-object-uri-    (triple-object triple))
      (make-and-add-triple -rdf-type-uri-      (node -rdf-statement-uri-))
      node)))

(defmethod is-container-p ((db db) (node node) &optional errorp)
  ;; We may have to extend this to handle subclasses of containers
  (let ((container-types (list (node -rdf-bag-uri-)
                               (node -rdf-seq-uri-)
                               (node -rdf-alt-uri-))))
    (dolist (triple (db-query db node (node -rdf-type-uri-) nil))
      (when (find (triple-object triple) container-types)
        (return-from is-container-p t)))
    (when errorp
      (cerror "Ignore" 'container-required :thing node))))

(defmethod db-find-cbd ((db db) (node node))
  ;; Calculates the Concise Bounded Description as per Patrick Stickler's spec at
  ;; http://www.w3.org/Submission/2004/SUBM-CBD-20040930/
  (cbd (list node) nil nil nil db))

(defun cbd (nodes triples cbd-nodes cbd-triples db)
  (cond (nodes
	 (let ((n (first nodes)))
	   (if (member n cbd-nodes)
	     (cbd (rest nodes) triples cbd-nodes cbd-triples db)
	     (cbd (rest nodes)
		  (append triples (db-query db n nil nil))
		  (cons n cbd-nodes)
		  cbd-triples
		  db))))
	(triples
	 (let ((tr (first triples)))
	   (if (member tr cbd-triples)
	     (cbd nil (rest triples) cbd-nodes cbd-triples db)
	     (cbd (let ((s (triple-reified-p tr db))
			(o (triple-object tr)))
		    (if (and (typep o 'node)
			     (not (typep o 'literal))
			     (null (node-uri o)))
		      (cons o s)
		      s))
		  (rest triples)
		  cbd-nodes
		  (cons tr cbd-triples)
		  db))))
	(t
	 (values cbd-triples cbd-nodes))))

(defmethod db-node-local-properties ((db db) (node node))
  (remove-duplicates (mapcar #'triple-predicate (db-query db node nil nil))))

(defun triple-reified-p (triple db)
  (let ((s-statements (db-query db nil !rdf:subject (triple-subject triple))))
    (when s-statements
      (let ((o-statements (db-query db nil !rdf:object (triple-object triple))))
	(when o-statements
	  (let ((predicate (triple-predicate triple)))
	    (flet ((predicate-not-found (node)
		     (null (db-query db node !rdf:predicate predicate))))
	      (declare (dynamic-extent #'predicate-not-found))
	      (remove-if #'predicate-not-found
			 (intersection (mapcar #'triple-subject s-statements)
				       (mapcar #'triple-subject o-statements))))))))))

(defun get-some-values (frame path db index)
  (assert (null index))
  (db-get-values db frame path))


;;; --------------------------------------------------------------------------------------
;;;
;;;   QUERY EXPRESSION MANIPULATION
;;;

(defun merge-query-expressions (query1 query2)
  (canonical-path `(:or ,(canonical-path query1)
		        ,(canonical-path query2))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS LITERAL-TRANSFORM-DB-MIXIN
;;;

(defclass literal-transform-db-mixin ()
  ())

(defmethod db-make-literal ((db literal-transform-db-mixin) string
			    &key language datatype property)
  (multiple-value-bind (string language datatype)
		       (db-transform-literal db string property
					     :language language :datatype datatype)
    (call-next-method db string :language language :datatype datatype)))

(defmethod db-transform-literal ((db literal-transform-db-mixin) string property
				 &key language datatype)
  (declare (ignore property))
  (values string language datatype))

(defclass date-cleanup-db-mixin (literal-transform-db-mixin)
  ())

(defmethod db-transform-literal ((db date-cleanup-db-mixin) string (property node)
				 &key language datatype)
  ;; Heuristically transforms time stamps into xsd:date (or xsd:dateTime) literals.
  ;; Only attempt this for dc:date and its recursive sub-properties
  (if (or datatype (not (frames-related-p property !rdfs:subPropertyOf !dc:date db nil)))
    (call-next-method)
    (multiple-value-bind (universal-time omit-time-p)
			 ;; Is it an EXIF-style timestamp?
			 (parse-exif-date string)
      (unless universal-time
	;; Is it an ISO8601 timestamp?
	(multiple-value-setq (universal-time omit-time-p)
	  (ignore-errors (parse-iso8601-date string))))
      (if universal-time
	(values (iso8601-date-string universal-time omit-time-p)
		language
		(if omit-time-p !xsd:date !xsd:dateTime))
	(call-next-method)))))
