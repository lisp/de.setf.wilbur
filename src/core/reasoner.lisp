;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  reasoner.lisp
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
;;;   Purpose: This file implements an RDFS reasoner, originally based on (but extending)
;;;   this paper:
;;;
;;;      Ora Lassila: "Taking the RDF Model Theory Out for a Spin", in: Ian Horrocks &
;;;      James Hendler (eds.): "The Semantic Web - ISWC 2002", Lecture Notes in Computer
;;;      Science 2342, pp.307-317, Springer Verlag, 2002
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   MIXIN CLASS DEDUCTIVE-CLOSURE-DB-MIXIN
;;;

(defclass deductive-closure-db-mixin ()
  ((closurep
    :initarg :closurep
    :initform t
    :accessor db-closure-p)
   (use-rule-engine-p
    :initarg :use-rule-engine-p
    :initform t
    :reader db-use-rule-engine-p)
   (sameas-clusters
    :initform (make-hash-table :test #'eq)
    :reader db-sameas-clusters)
   (ifp-additions
    :initform (make-triple-collection)
    :reader db-ifp-additions))
  (:default-initargs
    :rdf-schema-pathname "wilbur:schemata;true-rdf-schema.rdf"))

(defmacro without-closure ((db) &body body)
  (let ((d (gentemp))
	(closurep (gentemp)))
    `(let* ((,d ,db)
	    (,closurep nil))
      (unwind-protect (progn
			(shiftf ,closurep (db-closure-p ,d) nil)
			,@body)
	(setf (db-closure-p ,d) ,closurep)))))

(defvar *subprop-query* nil)
(defvar *subprops-of-subprop* (list !rdfs:subPropertyOf))

(declaim (special *rewritten-paths*)) ; forward ref.

(defmethod db-add-triple ((db deductive-closure-db-mixin) (triple triple)
			  &optional source)
  (declare (ignore source))
  (multiple-value-bind (triple addedp new-sources)
                       (call-next-method)
    (when addedp
      (db-add-triple-post-process db triple))
    (values triple addedp new-sources)))

(defmethod db-add-triple-post-process ((db deductive-closure-db-mixin) (triple triple))
  (let ((p (triple-predicate triple))
	(o (triple-object triple)))
    (db-add-triple db (db-make-triple db p !rdf:type !rdf:Property) :closure)
    (cond ((eq p !rdf:type)
	   (db-add-triple db (db-make-triple db o !rdf:type !rdfs:Class) :closure))
	  ((eq p !owl:sameAs)
	   (db-update-sameas-clusters db (triple-subject triple)))
	  ((member p *subprops-of-subprop*)
	   (db-clear-reasoner-cache db)
	   (when (member o *subprops-of-subprop*)
	     (setf *subprops-of-subprop*
		     (db-get-values db !rdfs:subPropertyOf (subprop-query db))
		   *subprop-query* nil)))
	  ((and (typep o 'literal)
		(literal-datatype o))
	   (db-add-triple db (db-make-triple db o !rdf:type !rdf:XMLLiteral)
			  :closure)))))

(defmethod db-del-triple ((db deductive-closure-db-mixin) (triple triple)
			  &optional source)
  (declare (ignore source))
  (multiple-value-bind (triple deletedp new-sources)
		       (call-next-method)
    (when (and deletedp (eq (triple-predicate triple) !owl:sameAs))
      (db-update-sameas-clusters db (triple-subject triple))
      (db-update-sameas-clusters db (triple-object triple)))
    (values triple deletedp new-sources)))

(defmethod db-merge :after ((to deductive-closure-db-mixin) (from db) &optional source)
  (declare (ignore source))
  (db-clear-reasoner-cache to))

(defmethod db-new-container-membership-property ((db deductive-closure-db-mixin)
						 (property node))
  (flet ((tr (s p o)
	   (db-add-triple db (db-make-triple db s p o))))
    (tr property !rdf:type !rdfs:ContainerMembershipProperty)
    (tr property !rdfs:subPropertyOf !rdfs:member)))

(defmethod db-get-values :around ((db deductive-closure-db-mixin) (frame node) path)
  ;; We cannot check the status of reasoning, since WilburQL queries are executed without
  ;; reasoning, yet owl:sameAs support has to work... this could be a problem, but for now
  ;; we just ignore the whole matter.
  (let* ((sameas-clusters (db-sameas-clusters db))
	 (other-frames (gethash frame sameas-clusters)))
    ;;(remove-duplicates
    (cond ((eq path !owl:sameAs)
	   (if (db-closure-p *db*) ; why is this *db* ??
	     other-frames
	     (call-next-method)))
	  ((rest other-frames)
	   (reduce #'union (mapcar #'(lambda (f)
				       (call-next-method db f path))
				   other-frames)))
	  (t
	   (call-next-method)))
     ;; :test #'(lambda (x y)
     ;; (find x (gethash y sameas-clusters))))
     ))

(defmethod db-get-values ((db deductive-closure-db-mixin) (frame node) path)
  (let ((path (rewrite-path path db)))
    (without-closure (db)
      (call-next-method db frame path))))

(defmethod db-get-values ((db deductive-closure-db-mixin)
			  (frame (eql !rdfs:Resource))
			  (path inverse-slot))
  (let ((link (inverse-slot-node path)))
    (if (or (eq link !rdf:type) (eq link !rdfs:subClassOf))
      ;;(cons :all (call-next-method))
      (list :all)
      (call-next-method))))

(defmethod frames-related-p ((source node) path (sink node)
                             (db deductive-closure-db-mixin)
                             action)
  (let ((path (rewrite-path path db)))
    (without-closure (db)
      (call-next-method source path sink db action))))

(defmethod db-update-sameas-clusters ((db deductive-closure-db-mixin)
				      (node node))
  (let ((cluster (without-closure (db)
		   (db-get-values db node '(:rep* (:or !owl:sameAs (:inv !owl:sameAs))))))
	(clusters (db-sameas-clusters db)))
    (if (rest cluster)
      (dolist (i cluster)
	(setf (gethash i clusters) cluster))
      (remhash node clusters))))

(defun show-sameas-clusters (db)
  (maphash #'(lambda (key value)
	       (format t "~&~S: ~S" key value))
	   (db-sameas-clusters db)))

(defmethod db-node-duplicates ((db deductive-closure-db-mixin) (node node))
  (gethash node (db-sameas-clusters db)))

(defmethod db-nodes-same-p ((db deductive-closure-db-mixin) (node1 node) (node2 node))
  (member node2 (db-node-duplicates db node1)))

(defmethod db-remove-node-duplicates ((db deductive-closure-db-mixin) nodes)
  ;; this needs to be optimized for long lists (how long? what's the threshold?)
  (let ((to-be-removed nil))
    (delete-if #'(lambda (node)
		   (or (member node to-be-removed)
		       (dolist (dup (db-node-duplicates db node))
			 (push dup to-be-removed))))
	       (sort (copy-list nodes)
		     #'(lambda (node1 node2)
			 (declare (ignore node2))
			 (node-uri node1))))))

(defmethod db-identified-node ((db deductive-closure-db-mixin) (node node)
			       &optional (error-if-unidentified-p nil))
  (let* ((nodes (db-node-duplicates db node))
	 (candidate (if nodes
		      (or (find-if #'node-uri nodes) node)
		      node)))
    (when (and error-if-unidentified-p (null (node-uri candidate)))
      (cerror "Ignore" 'unidentifed-node :thing node))
    candidate))

(defmethod db-identified-node ((db deductive-closure-db-mixin) (literal literal)
			       &optional error-if-unidentified-p)
  (declare (ignore error-if-unidentified-p))
  literal)


;;; --------------------------------------------------------------------------------------
;;;
;;;   PATH REWRITING
;;;

(defvar *rewritten-paths* (make-hash-table :test #'equal))
(defvar *known-rewrite-rules* (make-hash-table :test #'eq))

(defun rewrite-path (path db)
  (if (or (typep path 'path)
	  (not (db-closure-p db)))
    path
    (or (gethash path *rewritten-paths*)
	(let ((p (rewrite-path-for-subproperties
		  (rewrite-path-for-types (if (db-use-rule-engine-p db)
					    (rewrite-path-using-rule-engine path db)
					    path)
					  db)
		  db)))
	  (setf (gethash path *rewritten-paths*)
		(make-instance 'path :db db :expression p))))))

(defmacro define-rewrite-function (name (expr db) &body body)
  (let ((op (gentemp))
	(var (gentemp)))
    `(defun ,name (,expr ,db)
       (typecase ,expr
	 (node ,@body)
	 (cons (let ((,op (first ,expr)))
		 (case ,op
		   ((:value :norewrite) ,expr)
		   (t (cons ,op
			    (mapcar #'(lambda (,var)
					(,name ,var ,db))
				    (rest ,expr)))))))
	 (t ,expr)))))

(define-rewrite-function rewrite-path-for-types (path db)
  (case path
    (!rdf:type
     '(:or (:seq !rdf:type (:rep* !rdfs:subClassOf))
           (:seq :predicate-of-object !rdfs:range (:rep* !rdfs:subClassOf))
           (:seq :predicate-of-subject !rdfs:domain (:rep* !rdfs:subClassOf))
           (:value !rdfs:Resource)))
    (!rdfs:subClassOf
     ;; '(:or (:rep* !rdfs:subClassOf) (:value !rdfs:Resource))
     '(:or (:seq+ (:rep+ !rdfs:subClassOf) (:value !rdfs:Resource)) :self))
    (!rdfs:subPropertyOf
     '(:rep* !rdfs:subPropertyOf))
    (t
     path)))

(define-rewrite-function rewrite-path-for-subproperties (path db)
  (let ((props (db-get-values db path (subprop-query db))))
    (if (rest props)
      `(:or ,@props)
      (first props))))

(defvar *rule-rewrite-change-p*)

(define-rewrite-function rewrite-path-using-rules (path db)
  (let ((new-path (gethash path *known-rewrite-rules*)))
    (cond (new-path
	   (setf *rule-rewrite-change-p* t)
	   new-path)
	  (t
	   path))))

(defun rewrite-path-using-rule-engine (path db)
  ;; Keep rewriting until nothing changes...
  (loop (let ((*rule-rewrite-change-p* nil))
	  (setf path (rewrite-path-using-rules path db))
	  (unless *rule-rewrite-change-p*
	    (return-from rewrite-path-using-rule-engine path)))))

(defun subprop-query (db)
  (or *subprop-query*
      (progn
	(clrhash *rewritten-paths*)
	(setf *subprop-query*
	      (make-instance 'path
		:db db
		:expression `(:rep* (:inv (:or ,@*subprops-of-subprop*))))))))

(defun db-collect-rewrite-rules (db)
  (clrhash *known-rewrite-rules*)
  (dolist (rule (db-get-values db !wilbur:Rule '(:inv !rdf:type)))
    (setf (gethash rule *known-rewrite-rules*)
	  (cond ((db-node-type-p db rule !wilbur:PathRewriteRule)
		 (read-from-string
		  (literal-string (first (db-get-values db rule !wilbur:path)))))
		((db-node-type-p db rule !wilbur:AccessDaemon)
		 (make-access-daemon rule))))))

(defun db-clear-reasoner-cache (db)
  ;;(when (typep db 'pre-rewrite-cached-access-mixin)
  ;;  (db-clear-cache db))
  (clrhash (db-path-fsas db))
  (clrhash *rewritten-paths*)
  (setf *subprop-query* nil)
  (db-update-ifps db))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CONVENIENCE CLASS EDB
;;;

(defclass edb (deductive-closure-db-mixin interned-literal-indexed-db)
  ())


;;; --------------------------------------------------------------------------------------
;;;
;;;   TYPE HIERARCHY ANALYSIS
;;;

(defmethod db-node-type-p ((db deductive-closure-db-mixin) (node node) (type node))
  (frames-related-p node !rdf:type type *db* nil))

(defmethod db-node-subtype-p ((db deductive-closure-db-mixin)
			      (type node) (supertype node))
  (frames-related-p type !rdfs:subClassOf supertype db nil))

(defmethod db-node-types ((db db) (node node))
  (sort-types (db-get-values db node !rdf:type) db))

(defun sort-types (types db &key (key #'identity))
  (sort types
	#'(lambda (c1 c2)
	    (db-node-subtype-p db c1 c2))
	:key key))

(defmethod db-node-types-expanded ((db db) (node node))
  (labels ((partition (type other-types)
	     (let* ((supers (sort-types (db-get-values db type '(:rep* !rdfs:subClassOf))
					db))
		    (others (remove-if #'(lambda (type)
					   (member type supers))
				       other-types)))
	       (if others
		 (cons supers (partition (first others) (rest others)))
		 (list supers)))))
    (let ((types (db-node-types db node)))
      (when types
	(partition (first types) (rest types))))))

(defmethod db-node-types-expanded ((db deductive-closure-db-mixin) (node node))
  (labels ((partition (type other-types)
	     (let* ((supers (sort-types (db-get-values db type !rdfs:subClassOf) db))
		    (others (remove-if #'(lambda (type)
					   (member type supers))
				       other-types)))
	       (if others
		 (cons supers (partition (first others) (rest others)))
		 (list supers)))))
    (let ((types (db-node-types db node)))
      (when types
	(partition (first types) (rest types))))))

(defmethod db-node-properties ((db deductive-closure-db-mixin) (node node))
  (let ((triples (make-triple-collection (db-query db node nil nil)))
	(sameas (gethash node (db-sameas-clusters db))))
    (flet ((add-queried-triples (prop)
	     (dolist (value (db-get-values db node prop))
	       (triple-collection-add triples (db-make-triple db node prop value)))))
      (add-queried-triples !rdf:type)
      (add-queried-triples !rdfs:subClassOf)
      (add-queried-triples !rdfs:subPropertyOf)
      (dolist (n sameas)
	(unless (eq n node)
	  (triple-collection-add triples (db-make-triple db node !owl:sameAs n))))
      (triple-collection-triples triples))))

(defmethod db-node-properties-partitioned ((db deductive-closure-db-mixin) (node node)
					   &aux (hintsp nil))
  (let ((types (db-node-types-expanded db node))
	(triples (make-triple-collection))
	(sameas (pushnew node (gethash node (db-sameas-clusters *db*)))))
    (labels ((add-queried-properties (n property exclude)
	       (dolist (value (db-get-values db n property))
		 (unless (find value exclude)
		   (triple-collection-add triples (triple node property value)))))
	     (collect-properties (n)
	       (dolist (triple (db-query db n nil nil))
		 (unless (eq (triple-predicate triple) !owl:sameAs)
		   (triple-collection-add triples triple)))
	       (add-queried-properties n !rdf:type '(!rdfs:Resource))
	       (add-queried-properties n !rdfs:subClassOf sameas)
	       (add-queried-properties n !rdfs:subPropertyOf sameas))
	     (triple~ (a b)
	       (and (eq (triple-predicate a) (triple-predicate b))
		    (eq (triple-object a) (triple-object b)))))
      (dolist (n sameas)
	(collect-properties n))
      (let ((properties
	     (remove-duplicates (triple-collection-triples triples) :test #'triple~)))
	(flet ((construct-property-sets (some-types)
		 (let ((props nil)
		       (type-props nil)
		       (used-types nil))
		   (dolist (property properties)
		     (let ((p (triple-predicate property))
			   (o (triple-object property)))
		       (cond ((and (eq p !rdf:type)
				   (find o some-types))
			      (push property type-props)
			      (removef some-types o)
			      (push o used-types)
			      (removef properties property))
			     ((and (not (eq (first (db-get-values db p !rdfs:domain))
					    !rdfs:Resource))
				   (some #'(lambda (type)
					     (unless (eq type !rdfs:Resource)
					       (frames-related-p p !rdfs:domain type
								 db nil)))
					 (append used-types some-types)))
			      (push property props)
			      (removef properties property)))))
		   (if hintsp
		     `(:types ,(sort-types (mapcar #'triple-object type-props) db)
		       :properties ,props)
		     (append (sort-types type-props db :key #'triple-object) props)))))
	  (append (mapcar #'construct-property-sets types)
		  (list (if hintsp
			  `(:types (!rdfs:Resource) :properties ,properties)
			  (cons (db-make-triple db node !rdf:type !rdfs:Resource)
				properties)))))))))

(defmethod db-node-properties :around ((db deductive-closure-db-mixin) (node node))
  (let ((nodes (gethash node (db-sameas-clusters db))))
    (if nodes
      (reduce #'(lambda (x y)
		  (union x y :test #'triple=))
	      (mapcar #'(lambda (n)
			  (call-next-method db n))
		      nodes))
      (call-next-method))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   INVERSE FUNCTIONAL PROPERTIES
;;;
;;;   This is a hack, and will go away at some point when we figure out a better way to
;;;   do inverse functional properties. In the meantime, I am sorry.
;;;

(defmethod db-update-single-ifp ((db deductive-closure-db-mixin) (ifp node)
				 &optional (obj-subj-map (make-hash-table :test #'eq)))
  (let ((additions (db-ifp-additions db)))
    (dolist (triple (db-query db nil ifp nil))
      (push (triple-subject triple) (gethash (triple-object triple) obj-subj-map)))
    (maphash #'(lambda (value nodes)
		 (declare (ignore value))
		 (when (rest nodes)
		   (let ((root (first nodes)))
		     (dolist (node (rest nodes))
		       (let ((triple (db-make-triple db root !owl:sameAs node)))
			 (db-add-triple db triple)
			 (triple-collection-add additions triple))))))
	     obj-subj-map)))

(defmethod db-update-ifps ((db deductive-closure-db-mixin))
  (dolist (triple (triple-collection-triples (db-ifp-additions db)))
    (let ((triple (db-find-triple db triple)))
      (unless (triple-sources triple)
	(db-del-triple db triple))))
  (triple-collection-clear (db-ifp-additions db))
  (let ((obj-subj-map (make-hash-table :test #'eq)))
    (dolist (ifp (db-get-values db !owl:InverseFunctionalProperty '(:inv !rdf:type)))
      (clrhash obj-subj-map)
      (db-update-single-ifp db ifp obj-subj-map))))
