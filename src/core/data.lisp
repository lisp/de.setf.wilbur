;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  data.lisp
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
;;;   Purpose: This file contains functionality for managing "RDF data", namely nodes,
;;;   triples, etc.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   TOP-LEVEL NODE API
;;;

(declaim (special *nodes*)) ; forward reference

(defun node (thing)
  (etypecase thing
    (string
     (or (find-node *nodes* thing)
         (setf (find-node *nodes* thing) (dictionary-make-node *nodes* thing))))
    (null
     (dictionary-make-node *nodes* nil))
    (url
     (node (url-string thing)))
    (node
     thing)))

(defun add-namespace (prefix uri)
  (dictionary-add-namespace *nodes* prefix uri))

(defun del-namespace (prefix)
  (dictionary-remove-namespace *nodes* prefix))

(defun namespaces ()
  (mapcar #'first (dictionary-namespaces *nodes*)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF CONDITION CLASSES
;;;
;;;   RDF-ERROR                       abstract
;;;     FEATURE-NOT-SUPPORTED         concrete, continuable
;;;     ABOUT-AND-ID-BOTH-PRESENT     concrete, continuable
;;;     ABOUT-AND-NODEID-BOTH-PRESENT concrete, continuable
;;;     UNKNOWN-PARSETYPE             concrete, continuable
;;;     ILLEGAL-CHARACTER-CONTENT     concrete, continuable
;;;     CONTAINER-REQUIRED            concrete, continuable
;;;     OUT-OF-SEQUENCE-INDEX         concrete
;;;     DUPLICATE-NAMESPACE-PREFIX    concrete
;;;     QUERY-SYNTAX-ERROR            concrete
;;;     DATATYPE-PARSE-ERROR          concrete, continuable
;;;     CANNOT-INVERT-DEFAULT-VALUE   concrete
;;;     UNIDENTIFIED-NODE             concrete, continuable
;;;     UNSPECIFIED-LOAD-ERROR        concrete, continuable
;;;

(define-condition rdf-error (wilbur-error)
  ())

(define-condition feature-not-supported (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- ~S not supported"))

(define-condition feature-disabled (rdf-error)
  ()
  (:default-initargs 
    :format-control "RDF -- ~S is disabled"))

(define-condition about-and-id-both-present (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- \"about\" and \"ID\" both present"))

(define-condition about-and-nodeid-both-present (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- \"about\" and \"nodeID\" both present"))

(define-condition unknown-parsetype (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- unknown parsetype ~S"))

(define-condition illegal-character-content (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- character content not allowed: ~S"))

(define-condition container-required (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- ~S is not a container"))

(define-condition out-of-sequence-index (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- index URI ~S allocated out of sequence"))

(define-condition duplicate-namespace-prefix (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- duplicate namespace prefix ~S"))

(define-condition query-syntax-error (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- no operands for query operator ~A"))

(define-condition cannot-invert-default-value (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- cannot invert a default value expression ~S"))

(define-condition datatype-parse-error (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- cannot parse datatype literal ~S"))

(define-condition unidentified-node (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- externally unidentifiable node ~S"))

(define-condition unspecified-load-error (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- load of ~S failed"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS NODE
;;;

(defclass node ()
  ((uri
    :initarg :uri
    :initform nil
    :accessor node-uri)
   (name-resolved-p
    :initarg :name-resolved-p
    :initform t
    :accessor node-name-resolved-p)))

(defmethod print-object ((node node) stream)
  (declare (special *nodes*)) ; forward ref.
  (let ((uri (node-uri node)))
    (cond ((null uri)
           (print-unreadable-object (node stream :type t :identity t)
	     (princ "--" stream)))
          ((node-name-resolved-p node)
           (multiple-value-bind (name successp)
                                (find-short-name *nodes* uri)
             (format stream "!~:[~S~;~A~]" successp name)))
          (t
           (format stream "!~A" uri)))))

(defmethod node-name ((node node))
  (let ((uri (node-uri node)))
    (when uri
      (if (node-name-resolved-p node)
        (find-short-name *nodes* uri)
        uri))))

(defmethod make-load-form ((node node) &optional env)
  (declare (ignore env))
  (if (node-name-resolved-p node)
    `(node ,(node-uri node))
    `(unresolved-node ,(node-uri node))))

(defvar *index-uris* (make-array 32 :fill-pointer 0 :adjustable t))

(defun index-uri (index db)
  (let ((delta (- (length *index-uris*) index)))
    (cond ((>= delta 0)
           (elt *index-uris* (1- index)))
          ((= delta -1)
           (let ((u (node (rdf-uri (format nil "_~S" index)))))
             (vector-push-extend u *index-uris*)
	     ;; doing path-based reasoning may require this:
             (db-new-container-membership-property db u)
             u))
          (t
           (error 'out-of-sequence-index :thing index)))))

(defun index-uri-p (node)
  (find node *index-uris*))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS DICTIONARY
;;;

(defclass dictionary ()
  ((nodes
    :initform (make-hash-table :test #'equal)
    :initarg :nodes
    :reader dictionary-nodes)
   (namespaces
    :initform nil
    :accessor dictionary-namespaces)
   (unresolved-nodes
    :initform (make-hash-table :test #'equal)
    :initarg :unresolved-nodes
    :accessor dictionary-unresolved-nodes)
   (node-class
    :initarg :node-class
    :initform 'node
    :reader dictionary-node-class)))

(defmethod initialize-instance :after ((self dictionary) &rest args)
  (declare (ignore args))
  (dictionary-add-namespace self "rdf" -rdf-uri-)
  (dictionary-add-namespace self "rdfs" -rdfs-uri-)
  (dictionary-add-namespace self "owl" -owl-uri-)
  (dictionary-add-namespace self "xsd" -xsd-uri-))

(defmethod dictionary-add-namespace ((dictionary dictionary) prefix uri)
  (declare (special *db*)) ; forward ref.
  (let* ((namespaces (dictionary-namespaces dictionary))
	 (old-uri (string-dict-get namespaces prefix)))
    (cond ((null old-uri)
	   (setf (dictionary-namespaces dictionary)
		 (string-dict-add namespaces prefix uri))
	   (maphash #'(lambda (name node)
			(let ((uri (find-long-name dictionary name)))
			  (when uri
			    (remhash name (dictionary-unresolved-nodes dictionary))
			    (setf (node-uri node) uri
				  (node-name-resolved-p node) t
				  (find-node dictionary uri) node)
			    (db-node-resolved *db* node name))))
		    (dictionary-unresolved-nodes dictionary)))
	  ((not (string= uri old-uri))
	   (setf prefix (string-downcase (symbol-name (gentemp prefix))))
	   (setf (dictionary-namespaces dictionary)
		 (string-dict-add namespaces prefix uri))))
    (when (and (boundp '*db*) *db*)
      (db-add-namespace *db* prefix uri))
    prefix))

(defmethod dictionary-remove-namespace ((dictionary dictionary) prefix)
  (setf (dictionary-namespaces dictionary)
        (string-dict-del (dictionary-namespaces dictionary) prefix))
  prefix)

(defmethod dictionary-rename-namespace ((dictionary dictionary)
                                        old-prefix new-prefix)
  (if (string-dict-get (dictionary-namespaces dictionary) new-prefix)
    (error 'duplicate-namespace-prefix :thing new-prefix)
    (let ((uri (string-dict-get (dictionary-namespaces dictionary) old-prefix)))
      (dictionary-remove-namespace dictionary old-prefix)
      (dictionary-add-namespace dictionary new-prefix uri)
      new-prefix)))

(defmethod dictionary-make-node ((dictionary dictionary) uri)
  (make-instance (dictionary-node-class dictionary) :uri uri))

(defmethod find-node ((dictionary dictionary) uri)
  (when uri
    (gethash uri (dictionary-nodes dictionary))))

(defmethod (setf find-node) (node (dictionary dictionary) uri)
  (when uri
    (setf (gethash uri (dictionary-nodes dictionary)) node)))

(defmethod find-short-name ((dictionary dictionary) uri &optional use-entities-p)
  (reverse-expand-name uri (dictionary-namespaces dictionary) use-entities-p))

(defmethod find-long-name ((dictionary dictionary) name)
  (expand-name-with-namespace name (dictionary-namespaces dictionary)))

(defun unresolved-node (name)
  (let ((uri (find-long-name *nodes* name)))
    (if uri
      (node uri)
      (let ((unresolved (dictionary-unresolved-nodes *nodes*)))
        (or (gethash name unresolved)
            (setf (gethash name unresolved)
                  (make-instance 'node :uri name :name-resolved-p nil)))))))

(defmethod find-unresolved-nodes ((dictionary dictionary))
  (let ((nodes nil))
    (maphash #'(lambda (uri node)
                 (declare (ignore uri))
                 (push node nodes))
             (dictionary-unresolved-nodes dictionary))
    nodes))

(defmethod dictionary-apropos-list ((dictionary dictionary)
                                    (pattern string))
  (let ((nodes nil))
    (maphash #'(lambda (name node)
		 (when (name-contains-pattern-p name pattern)
		   (push node nodes)))
	     (dictionary-nodes dictionary))
    (sort nodes #'string< :key #'node-uri)))

(defun name-contains-pattern-p (name pattern)
  ;; NOTE: This is a naive string search algorithm; I will switch to, say, Boyer-Moore
  ;;  when I have more time.
  (let ((nn (length name))
        (np (length pattern)))
    (cond ((= nn np)
           (string= name pattern))
          ((> nn np)
           (dotimes (i (- nn np -1))
             (when (string= name pattern :start1 i :end1 (+ i np))
               (return-from name-contains-pattern-p t)))))))

(defvar *nodes* (make-instance 'dictionary))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun inline-node-reader (stream char)
    (declare (ignore char))
    (if (char= (peek-char nil stream t nil t) #\")
      (node (read stream t nil t))
      (unresolved-node (read-using *name-reader* stream t))))

  (defun enable-node-shorthand ()
    (set-macro-character #\! #'inline-node-reader t))

  (enable-node-shorthand))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS TRIPLE
;;;

#+:wilbur-triples-as-classes
(defclass triple ()
  ((subject
    :initarg :subject
    :reader triple-subject)
   (predicate
    :initarg :predicate
    :reader triple-predicate)
   (object
    :initarg :object
    :accessor triple-object)
   (sources
    :initarg :sources
    :initform nil
    :accessor triple-sources)))

#-:wilbur-triples-as-classes
(defstruct (triple
	     (:constructor %make-triple (subject predicate object &optional sources)))
  subject
  predicate
  object
  sources)

(defmethod print-object ((triple triple) stream)
  (print-unreadable-object (triple stream :type t :identity t)
    (format stream "~S ~S ~S"
            (triple-subject triple)
            (triple-predicate triple)
            (triple-object triple))))

(defmethod triple= ((triple1 triple) (triple2 triple))
  (and (eq (triple-subject triple1) (triple-subject triple2))
       (eq (triple-predicate triple1) (triple-predicate triple2))
       (eq (triple-object triple1) (triple-object triple2))))

(defmethod triple= (thing1 thing2)
  (declare (ignore thing1 thing2))
  nil)


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS DB
;;;

(defclass db ()
  ((triples
    :initform nil
    :accessor db-triples)
   (source-descs
    :initform nil
    :accessor db-source-descs)
   (path-fsas
    :initform (make-hash-table :test #'equal)
    :reader db-path-fsas)
   (literal-class
    :initarg :literal-class
    :initform 'literal ; could also be STRING or any subclass of LITERAL 
    :reader db-literal-class)))

(defmethod initialize-instance :after ((self db) &key (emptyp t) &allow-other-keys)
  (unless emptyp
    (warn "Schema loading not supported for ~S" self)))

(defmethod print-object ((db db) stream)
  (print-unreadable-object (db stream :type t :identity t)
    (format stream "size ~S" (length (db-triples db)))))

(defmethod db-make-literal ((db db) string &rest options)
  (declare (dynamic-extent options))
  (let ((class (db-literal-class db)))
    (if (eq class 'string)
      string
      (apply #'make-instance class :string string :allow-other-keys t options))))

(defmethod db-make-triple ((db db) subject predicate object &optional source)
  #+:wilbur-triples-as-classes
  (make-instance 'triple
    :subject subject :predicate predicate :object object
    :sources (and source (list source)))
  #-:wilbur-triples-as-classes
  (%make-triple subject predicate object (and source (list source))))

(defmethod db-add-triple ((db db) (triple triple)
			  &optional (source nil source-supplied-p))
  (let ((sources (triple-sources triple))
	(old-triple (db-find-triple db triple)))
    (cond (old-triple
	   (let ((old-sources (triple-sources old-triple)))
	     (cond ((or (null sources)
			(if source-supplied-p
			  (member source old-sources)
			  (subsetp sources old-sources)))
		    (values old-triple nil nil))
		   (t
		    (unionf (triple-sources old-triple) sources)
		    (values old-triple nil sources)))))
	  (t
	   (push triple (db-triples db))
	   (values triple t sources)))))

(defmethod db-del-triple ((db db) (triple triple) &optional source)
  (when source
    (let ((new-sources (removef (triple-sources triple) source)))
      (when new-sources
	(return-from db-del-triple (values triple nil new-sources)))))
  (removef (db-triples db) triple)
  (values triple t nil))

(defmethod db-del-source ((db db) (source node))
  (removef (db-triples db) source :key #'triple-sources :test #'find))

(defmethod db-query-by-source ((db db) (source node))
  (remove source (db-triples db) :key #'triple-sources :test-not #'find))

(defmethod db-sources ((db db))
  ;; Bogus implementation, for small databases only, included for "completeness"
  (let ((sources nil))
    (dolist (triple (db-triples db) sources)
      (dolist (source (triple-sources triple))
	(pushnew source sources)))))

(defmethod db-query ((db db) subject predicate object)
  (flet ((matching-triple-p (triple)
	   (and (eq~ (triple-subject triple) subject)
		(eq~ (triple-predicate triple) predicate)
		(eq~ (triple-object triple) object))))
    (declare (dynamic-extent #'matching-triple-p))
    (remove-if-not #'matching-triple-p (db-triples db))))

(defmethod db-find-triple ((db db) (triple triple))
  (find triple (db-triples db) :test #'triple=))

(defmethod db-merge ((to db) (from db) &optional (source nil))
  (dolist (triple (if source
		    (db-query-by-source from source)
		    (db-triples from)))
    (db-add-triple to triple)))

(defmethod db-clear ((db db))
  (setf (db-triples db) nil))

(defmethod db-count-triples ((db db))
  (length (db-triples db)))

(defmethod db-new-container-membership-property ((db db) (property node))
  nil)

(defmethod db-add-namespace ((db db) prefix uri)
  (declare (ignore prefix uri))
  nil)

(defmethod db-node-resolved ((db null) (node node) old-name)
  (warn "Node name ~S resolved with NULL database" old-name)
  (values node old-name))

(defmethod db-node-resolved ((db db) (node node) old-name)
  (values node old-name))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS FAST-TEMPORARY-DB
;;;

(defclass fast-temporary-db (db)
  ())

(defmethod db-add-triple ((db fast-temporary-db) (triple triple) &optional source)
  (declare (ignore source))
  (push triple (db-triples db))
  (values triple t (triple-sources triple)))

(defmethod db-del-triple ((db fast-temporary-db) (triple triple) &optional source)
  (declare (ignore source))
  (removef (db-triples db) triple)
  (values triple t nil))


;;; --------------------------------------------------------------------------------------
;;;
;;;   "CLASS" TRIPLE-INDEX
;;;

(defun make-triple-index (multiple-components-p)
  (declare (ignore multiple-components-p))
  (wilbur-make-hash-table :test #'eq))

(eval-when (:load-toplevel :compile-toplevel :execute)
  
  (defmacro triple-index-get (index &rest components)
    (if (rest components)
      `(%triple-index-get-double ,index ,@components)
      `(%triple-index-get-single ,index ,(first components))))
  
  (defmacro triple-index-add (triple index &rest components)
    (if (rest components)
      `(%triple-index-add-double ,triple ,index ,@components)
      `(%triple-index-add-single ,triple ,index ,(first components))))
  
  (defmacro triple-index-rem (triple index &rest components)
    (if (rest components)
      `(%triple-index-rem-double ,triple ,index ,@components)
      `(%triple-index-rem-single ,triple ,index ,@components)))
  
  (defmacro %triple-index-get-single (index component)
    (with-temps (i)
      `(let ((,i ,index))
	 (wilbur-gethash ,component ,i))))

  (defmacro %triple-index-get-double (index c1 c2)
    (with-temps (i sub-index)
      `(let* ((,i ,index)
	      (,sub-index (wilbur-gethash ,c1 ,i)))
	 (when ,sub-index
	   (wilbur-gethash ,c2 ,sub-index)))))

  )

(defun triple-index-clear (index)
  (wilbur-clrhash index))

;; (declaim (inline %triple-index-get-single))

(defun %triple-index-add-single (triple index component)
  (push triple (wilbur-gethash component index)))

(declaim (inline %triple-index-add-single))

(defun %triple-index-rem-single (triple index component)
  (removef (wilbur-gethash component index) triple))

(declaim (inline %triple-index-rem-single))

(defun ensure-sub-index (key1 index)
  (or (wilbur-gethash key1 index)
      (setf (wilbur-gethash key1 index) (wilbur-make-hash-table :test #'eq :size 30))))

(declaim (inline ensure-sub-index))

(defun %triple-index-add-double (triple index c1 c2)
  (push triple (wilbur-gethash c2 (ensure-sub-index c1 index))))

(declaim (inline %triple-index-add-double))

(defun %triple-index-rem-double (triple index c1 c2)
  (removef (wilbur-gethash c2 (ensure-sub-index c1 index)) triple))

(declaim (inline %triple-index-rem-double))

(defmacro with-spo-case (((sub pre obj) subject predicate object)
			 &key spo sp so s po p o all)
  `(let ((,sub ,subject)
	 (,pre ,predicate)
	 (,obj ,object))
    (cond (,sub (cond (,pre (if ,obj ,spo
				     ,sp))
		      (,obj          ,so)
		      (t             ,s)))
	  (,pre (if ,obj             ,po
		                     ,p))
	  (,obj                      ,o)
	  (t                         ,all))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS TRIPLE-COLLECTION
;;;

(defstruct (triple-collection
	     (:constructor %make-triple-collection ()))
  (triples nil)
  (index (make-triple-index t)))

(defun make-triple-collection (&optional triples)
  (let ((collection (%make-triple-collection)))
    (dolist (triple triples)
      (triple-collection-add collection triple))
    collection))

(defun triple-collection-find (collection triple)
  (find (triple-object triple)
	(triple-index-get (triple-collection-index collection)
			  (triple-predicate triple)
			  (triple-subject triple))
	:key #'triple-object))

(defun triple-collection-add (collection triple)
  (or (triple-collection-find collection triple)
      (progn (push triple (triple-collection-triples collection))
	     (triple-index-add triple (triple-collection-index collection)
			       (triple-predicate triple)
			       (triple-subject triple))
	     triple)))

(defun triple-collection-clear (collection)
  (triple-index-clear (triple-collection-index collection))
  (setf (triple-collection-triples collection) nil))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS INDEXED-DB
;;;

(defclass indexed-db (db)
  ((index-sp
    :initform (make-triple-index t)
    :reader db-index-sp)
   (index-po
    :initform (make-triple-index t)
    :reader db-index-po)
   (index-s
    :initform (make-triple-index nil)
    :reader db-index-s)
   (index-p
    :initform (make-triple-index nil)
    :reader db-index-p)
   (index-o
    :initform (make-triple-index nil)
    :reader db-index-o)
   (by-source
    :initform (make-triple-index nil)
    :reader db-by-source))
  (:default-initargs
    :rdf-schema-pathname "wilbur:schemata;rdf-schema.rdf"
    :populate-with nil))

(defmethod initialize-instance :after ((self indexed-db)
                                       &key (emptyp nil)
                                            rdf-schema-pathname
				            populate-with
                                       &allow-other-keys)
  (unless emptyp
    (db-load self (make-file-url rdf-schema-pathname) :db self :merge-results-p nil)
    (dolist (url populate-with)
      (db-load self (make-url url) :db self :merge-results-p nil))))

(defmethod db-add-triple ((db indexed-db) (triple triple) &optional source)
  (declare (ignore source))
  (multiple-value-bind (actual-triple addedp new-sources)
                       (call-next-method)
    (cond (addedp      ; CASE 1: Triple actually added
	   (let ((s (triple-subject triple))
		 (p (triple-predicate triple))
		 (o (triple-object triple)))
	     (triple-index-add triple (db-index-sp  db) p s  )
	     (triple-index-add triple (db-index-po  db)   p o)
	     (triple-index-add triple (db-index-s   db) s    )
	     (triple-index-add triple (db-index-p   db)   p  )
	     (triple-index-add triple (db-index-o   db)     o)
	     (when new-sources
	       (dolist (source new-sources)
		 (triple-index-add triple (db-by-source db) source)))
	     (values triple t new-sources)))
	  (new-sources ; CASE 2: Only new source(s) added
	   (dolist (source new-sources)
	     (triple-index-add triple (db-by-source db) source))
	   (values actual-triple nil new-sources))
	  (t           ; CASE 3: Nothing added, source null
	   (values actual-triple nil nil)))))

;;; needs to be fixed vis-a-vis sources
(defmethod db-del-triple ((db indexed-db) (triple triple) &optional source)
  (let ((sources (triple-sources triple)))
    (multiple-value-bind (triple deletedp new-sources)
			 (call-next-method)
      (cond (deletedp
	     (let ((s (triple-subject triple))
		   (p (triple-predicate triple))
		   (o (triple-object triple)))
	       (triple-index-rem triple (db-index-sp  db) p s  )
	       (triple-index-rem triple (db-index-po  db)   p o)
	       (triple-index-rem triple (db-index-s   db) s    )
	       (triple-index-rem triple (db-index-p   db)   p  )
	       (triple-index-rem triple (db-index-o   db)     o)
	       (dolist (src sources)
		 (triple-index-rem triple (db-by-source db) src))
	       (values triple t nil)))
	    (t
	     (triple-index-rem triple (db-by-source db) source)
	     (values triple nil new-sources))))))

(defmethod db-query ((db indexed-db) subject predicate object)
  (macrolet ((filter (k tr s)
	       `(remove ,k ,tr :test-not #'eq :key ,s)))
    (with-spo-case ((s p o) subject predicate object)
      :spo (filter o (triple-index-get (db-index-sp db) p s) #'triple-object)
      :sp  (triple-index-get (db-index-sp db) p s)
      :so  (filter o (triple-index-get (db-index-s db) s) #'triple-object)
      :s   (triple-index-get (db-index-s db) s)
      :po  (triple-index-get (db-index-po db) p o)
      :p   (triple-index-get (db-index-p db) p)
      :o   (triple-index-get (db-index-o db) o)
      :all (db-triples db))))

#+:junk
(defmethod db-query ((db indexed-db) subject predicate object)
  (macrolet ((filter (k tr s)
	       `(remove ,k ,tr :test-not #'eq :key ,s)))
    (cond (subject
	   (cond (predicate
		  (if object
		    (filter object
			    (triple-index-get (db-index-sp db) predicate subject)
			    #'triple-object)
		    (triple-index-get (db-index-sp db) predicate subject)))
		 (object
		  ;; "SO": should we use subject or object index?
		  (filter object
			  (triple-index-get (db-index-s db) subject)
			  #'triple-object))
		 (t
		  (triple-index-get (db-index-s db) subject))))
	  (object
	   (if predicate
	     (triple-index-get (db-index-po db) predicate object)
	     (triple-index-get (db-index-o db) object)))
	  (predicate
	   (triple-index-get (db-index-p db) predicate))
	  (t
	   (db-triples db)))))

(defmethod db-find-triple ((db indexed-db) (triple triple))
  (find (triple-object triple)
	(triple-index-get (db-index-sp db)
			  (triple-predicate triple)
			  (triple-subject triple))
	:key #'triple-object))

(defmethod db-del-source ((db indexed-db) (source node))
  (dolist (triple (db-query-by-source db source))
    (db-del-triple db triple source)))

(defmethod db-query-by-source ((db indexed-db) (source node))
  (triple-index-get (db-by-source db) source))

(defmethod db-sources ((db indexed-db))
  (let ((sources nil))
    (wilbur-maphash #'(lambda (key data)
			(declare (ignore data))
			(push key sources))
		    (db-by-source db))
    sources))

(defmethod db-clear :after ((db indexed-db))
  (triple-index-clear (db-index-sp db))
  (triple-index-clear (db-index-po db))
  (triple-index-clear (db-index-s db))
  (triple-index-clear (db-index-p db))
  (triple-index-clear (db-index-o db))
  (triple-index-clear (db-by-source db)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS LOCKED-DB-MIXIN
;;;

(defclass locked-db-mixin ()
  ((triple-lock
    :initform (make-lock)
    :reader db-triple-lock)))

(defmacro with-triple-lock (db &body body)
  `(with-lock ((db-triple-lock ,db)) ,@body))

(defmethod db-add-triple :around ((db locked-db-mixin) (triple triple) &optional source)
  (declare (ignore source))
  (with-triple-lock db
    (call-next-method)))

(defmethod db-del-triple :around ((db locked-db-mixin) (triple triple) &optional source)
  (declare (ignore source))
  (with-triple-lock db
    (call-next-method)))

(defmethod db-merge :around ((to locked-db-mixin) (from db) &optional source)
  (declare (ignore source))
  (with-triple-lock to
    (call-next-method)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   "TOP-LEVEL" DATA API
;;;

(defvar *db* nil) ; "current" database

(defun triple (subject predicate object &optional source)
  (db-make-triple *db* subject predicate object source))

(defun add-triple (triple)
  (db-add-triple *db* triple))

(defun del-triple (triple)
  (db-del-triple *db* triple nil))

(defun query (subject predicate object)
  (db-query *db* subject predicate object))

(defun reify (triple &key (statement-uri nil) (source nil))
  (db-reify triple *db* statement-uri source))

(defun local-properties (node)
  (db-node-local-properties *db* node))

(defun all-values (frame path)
  (db-get-values *db* frame path))

(defun add-value (frame path value)
  (db-add-triple *db* (db-make-triple *db* frame path value))
  value)

(defun del-value (frame path &optional value)
  (dolist (triple (db-query *db* frame path value))
    (db-del-triple *db* triple nil)))

(defun value (frame path)
  (let ((v (first (db-get-values *db* frame path))))
    (if (typep v 'literal)
      (literal-value v)
      v)))

(defun relatedp (source path sink &optional action)
  (frames-related-p source path sink *db* action))
