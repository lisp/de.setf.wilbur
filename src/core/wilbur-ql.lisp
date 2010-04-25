;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  wilbur-ql.lisp
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
;;;   Purpose: This file implements the Wilbur Query Language (WilburQL) which essentially
;;;   is a simple API on top of the RDF data manager (in "core-data.lisp"). Much of the
;;;   functionality is modeled after the BEEF frame system:
;;;
;;;      Ora Lassila: "BEEF Reference Manual - A Programmer's Guide to the BEEF Frame
;;;        System", Second Version, Report HTKK-TKO-C46, Otaniemi (Finland), Department of
;;;        Computer Science, Helsinki University of Technology, 1991
;;;
;;;      Juha Hynynen and Ora Lassila: "On the Use of Object-Oriented Paradigm in a
;;;        Distributed Problem Solver", AI Communications 2(3): 142-151 (1989)
;;;
;;;   A description of the WilburQL itself can be found in the following paper:
;;;
;;;      Ora Lassila: "Taking the RDF Model Theory Out for a Spin", in: Ian Horrocks &
;;;      James Hendler (eds.): "The Semantic Web - ISWC 2002", Lecture Notes in Computer
;;;      Science 2342, pp.307-317, Springer Verlag, 2002
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS PATH
;;;
;;;   The path grammar implementation is derived from BEEF frame system.
;;;

(defclass path ()
  ((expression
    :accessor path-expression)
   (db
    :initarg :db
    :reader path-db)
   (fsa
    :accessor path-fsa)))

(defmethod print-object ((path path) stream)
  (print-unreadable-object (path stream :type t)
    (prin1 (path-expression path) stream)))

(defmethod initialize-instance :after ((self path) &rest args &key expression)
  (declare (ignore args))
  (multiple-value-bind (fsa expression)
		       (db-make-path-fsa (path-db self) expression)
    (setf (path-fsa self) fsa
	  (path-expression self) expression)))

(defmethod invert ((path path))
  (make-instance 'path
    :db (path-db path)
    :expression (invert-path (path-expression path))))

(defstruct (path-node
            (:conc-name pn-)
            (:copier nil)
            (:constructor new-pn (link)))
  (link nil :read-only t)                    ; slot name i.e. link in the path
  (follows nil))                             ; possible followers of this node

(defstruct (path-fsa-state
            (:conc-name ps-)
            (:copier nil)
            (:constructor new-ps (positions)))
  (positions nil :read-only t)               ; positions defining this state
  (transitions nil))                         ; transitions from this state

(defstruct (path-fsa-transition
            (:conc-name pt-)
            (:copier nil)
            (:constructor new-pt (input index)))
  (input nil :read-only t)                   ; input symbol (= predicate name)
  (index nil :read-only t))                  ; index of the target state

(defmethod print-object ((self path-fsa-transition) stream)
  (print-unreadable-object (self stream :type t)
    (let ((input (pt-input self)))
      (typecase input
        (inverse-slot
         (format stream ":inv ~S->~D" (inverse-slot-node input) (pt-index self)))
        (node
         (format stream "~S->~D" input (pt-index self)))
        (default-value
         (format stream "(:value ~S)->~D" (default-value-value input) (pt-index self)))
        (t
         (format stream "~S->~D" input (pt-index self)))))))

(defun canonical-path (expr)
  (db-canonical-path *db* expr))

(defmethod db-canonical-path-op ((db db) op &rest args)
  (declare (ignore op args))
  nil)

(defmethod db-canonical-path ((db db) expr)
  (labels ((canonical (expr)
	     (etypecase expr
	       (cons
		(destructuring-bind (op arg &rest args) expr
		  (if arg
		    (case op
		      ((:rep* :rep+ :inv :value :filter :restrict
			:lang :test :daemon :norewrite)
		       (assert (null args) nil "Extra operands for ~S in ~S" op expr)
		       (case op
			 (:rep+      (canonical `(:seq ,arg (:rep* ,arg))))
			 (:rep*      `(,op ,(canonical arg)))
			 (:inv       (canonical (invert-path arg)))
			 (:value     (make-default-value arg))
			 (:filter    (make-instance 'path-uri-filter :key arg))
			 (:restrict  (make-instance 'path-node-restriction :key arg))
			 (:lang      (make-instance 'path-lang-filter :key arg))
			 (:test      (make-instance 'functional-restriction :key arg))
			 (:daemon    (make-access-daemon arg))
			 (:norewrite (canonical arg))))
		      ((:seq :seq+ :or)
		       (if args
			 (let ((arg (canonical arg)))
			   (unless (or (atom arg) (not (eq op (first arg))))
			     (psetq arg (second arg)
				    args (append (cddr arg) args)))
			   (cond ((rest args)
				  (canonical `(,op ,arg (,op ,@args))))
				 ((eq op :or)
				  `(,op ,(canonical (first args))
				        ,(canonical arg)))
				 (t
				  `(,op ,(canonical arg)
				        ,(canonical (first args))))))
			 (canonical arg)))
		      (t
		       (or (apply #'db-canonical-path-op db op
				  (mapcar #'canonical (cons arg args)))
			   (error "Unknown query operator ~S" op))))
		    (error 'query-syntax-error :thing op))))
	       (string
		(node expr))
	       (keyword
		(assert (member expr '(:members :any
				       :predicate-of-object :predicate-of-subject :self)))
		expr)
	       ((or node inverse-slot default-value path-filter access-daemon)
		expr)
	       (symbol
		(error "Symbol ~S not allowed as a path expression" expr)))))
    (canonical expr)))

(defun invert-path (expr &optional (canonicalizep nil))
  (labels ((remove-defaults (x)
	     (remove-if #'(lambda (y)
			    (typecase y
			      (default-value t)
			      (cons (eq (car y) :value))))
			x))
	   (i (p)
             (etypecase p
               (cons
                (ecase (first p)
                  (:or
                   `(,(first p)
                     ,@(mapcar #'i (reverse (remove-defaults (rest p))))))
                  ((:seq :seq+)
                   `(,(first p) ,@(mapcar #'i (reverse (remove-defaults (rest p))))))
                  ((:rep* :rep+)	; REP+ added, is this correct?
                   `(,(first p) ,(i (second p))))
		  (:inv
		   (second p))))
               (node
                (make-inverse-slot p))
               (inverse-slot
                (inverse-slot-node p))
	       (keyword
		(if (eq p :self) :self (make-inverse-slot p)))
               (default-value
                (error 'cannot-invert-default-value :thing (default-value-value p)))
	       (path-filter
		p))))
    (i (if canonicalizep (canonical-path expr) expr))))

(defun make-path-fsa (expr)
  (warn "Use of MAKE-PATH-FSA is deprecated. Use DB-MAKE-PATH-FSA instead.")
  (db-make-path-fsa *db* expr))

(defmethod db-make-path-fsa ((db db) expr &aux e)
  (when expr
    (let ((path-fsas (db-path-fsas db)))
      (values (or (gethash expr path-fsas)
		  (setf e (db-canonical-path db expr)
			(gethash expr path-fsas) (db-construct-new-path-fsa db e)))
	      e))))

(defvar *fsa-states/temporary* (make-array 8 :adjustable t :fill-pointer 0))

(defun construct-new-path-fsa (expr)
  (declare (ignore expr))
  (error "Use of CONSTRUCT-NEW-PATH-FSA is deprecated."))

(defmethod db-construct-new-path-fsa ((db db) expr &aux inputs)
  (labels ((decorate (x)
             (if (atom x)
               (let ((node (list (new-pn x))))
                 (pushnew x inputs)
                 (values node node nil))
               (case (pop x)
                 (:seq  (multiple-value-bind (first1 last1 null1) (decorate (first x))
                          (multiple-value-bind (first2 last2 null2) (decorate (second x))
                            (add-followers last1 first2)
                            (values (if null1 (union first1 first2) first1)
                                    (if null2 (union last1 last2) last2)
                                    (and null1 null2)))))
                 (:seq+ (multiple-value-bind (first1 last1 null1) (decorate (first x))
                          (multiple-value-bind (first2 last2) (decorate (second x))
                            (add-followers last1 first2)
                            (values (if null1 (union first1 first2) first1)
                                    (union last1 last2)
                                    null1))))
                 (:or   (multiple-value-bind (first1 last1 null1) (decorate (first x))
                          (multiple-value-bind (first2 last2 null2) (decorate (second x))
                            (values (union first1 first2)
                                    (union last1 last2)
                                    (or null1 null2)))))
                 (:rep* (multiple-value-bind (first last) (decorate (first x))
                          (add-followers last first)
                          (values first last t))))))
           (add-followers (from to)
             (dolist (i from) (unionf (pn-follows i) to)))
           (add-state (positions)
             (or (position positions *fsa-states/temporary*
                           :key #'ps-positions
                           :test #'(lambda (x y)
                                     (and (subsetp x y) (subsetp y x))))
                 (vector-push-extend (new-ps positions) *fsa-states/temporary*))))
    (setf (fill-pointer *fsa-states/temporary*) 0)
    (add-state (decorate `(:seq ,expr nil)))
    (do ((i 0 (1+ i)))
        ((= i (length *fsa-states/temporary*)))
      (let ((state (elt *fsa-states/temporary* i)))
        (dolist (input inputs)
          (let ((positions nil))
            (dolist (p (ps-positions state))
              (when (eq (pn-link p) input)
                (unionf positions (pn-follows p))))
            (when positions
              (let ((index (add-state positions)))
                (when input
                  (push (new-pt input index) (ps-transitions state)))))))))
    (map 'simple-vector #'(lambda (s)
			    (cons (and (member nil (ps-positions s) :key #'pn-link) t)
				  (reverse (ps-transitions s))))
	 *fsa-states/temporary*)))

(defmacro with-hash-pool ((var pool) &body body)
  `(let* ((,pool ,pool)
	  (,var (clrhash (or (pop ,pool) (make-hash-table :test #'eq)))))
    (unwind-protect (progn ,@body)
      (clrhash ,var))))

(defvar *walk-states/temporary* (list (make-hash-table :test #'eq)
                                      (make-hash-table :test #'eq)
                                      (make-hash-table :test #'eq)))

(defvar *collect-nodes/temporary* (list (make-hash-table :test #'eq)
					(make-hash-table :test #'eq)
					(make-hash-table :test #'eq)))

(defun walk-using-fsa (root fsa action db)
  (with-hash-pool (states *walk-states/temporary*)
    (labels ((walk (f i)
	       (unless (member i (gethash f states) :test #'=)
		 (push i (gethash f states))
		 (let ((transitions (svref fsa i)))
		   (or (when (first transitions)
			 (funcall action f))
		       (when (or (typep f 'node) (typep f 'literal) (null f))
			 ;;(when (typep f 'node)
			 (dolist (link (rest transitions))
			   (dolist (v (db-get-values db f (pt-input link)))
			     (let ((values (walk v (pt-index link))))
			       (when values
				 (return-from walk-using-fsa values)))))))))))
      (declare (dynamic-extent #'walk))
      (when fsa
	(walk root 0)))))

(defun walk-using-fsa-remembering-path (root fsa action db)
  (with-hash-pool (states *walk-states/temporary*)
    (labels ((walk (f i pn pp)
	       ;; (format t "~&WALK: ~S ~S ~S ~S ~S" f i pn pp (gethash f states))
               (unless (member i (gethash f states) :test #'=)
                 (push i (gethash f states))
		 ;; (format t "~&PUSH: ~S ~S" f i)
                 (let ((transitions (svref fsa i))
		       (pn (cons f pn)))
                   (or (when (first transitions)
                         (funcall action f pn pp))
                       (when (typep f 'node)
                         (dolist (link (rest transitions))
			   (multiple-value-bind (values predicates)
						(db-get-values db f (pt-input link))
			     (assert (= (length values) (length predicates)))
			     (loop for v in values
				   for p in predicates
				   do (progn
					;; (format t "~&LOOP: ~S ~S" v p)
					(let ((values (walk v (pt-index link)
							    pn (cons p pp))))
					  (when values
					    (return-from walk-using-fsa-remembering-path
					      values)))))))))))))
      (declare (dynamic-extent #'walk))
      (when fsa
        (walk root 0 nil nil)))))

(defun collect-using-fsa (root fsa db)
  (with-hash-pool (node-hash *collect-nodes/temporary*)
    (let ((nodes nil))
      (flet ((collect-results (n)
	       (unless (gethash n node-hash)
		 (setf (gethash n node-hash) t)
		 (push n nodes)
		 nil)))
	(declare (dynamic-extent #'collect-results))
	(walk-using-fsa root fsa #'collect-results db)
	(nreverse nodes)))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   STRUCTURE CLASS INVERSE-SLOT
;;;   STRUCTURE CLASS DEFAULT-VALUE
;;;

(defvar *inverse-slots* (make-hash-table :test #'eq))

(defun make-inverse-slot (node)
  (or (gethash node *inverse-slots*)
      (setf (gethash node *inverse-slots*) (%make-inverse-slot node))))

(defstruct (inverse-slot
	     (:copier nil)
	     (:constructor %make-inverse-slot (node)))
  node)

(defmethod print-object ((self inverse-slot) stream)
  (print-unreadable-object (self stream :type t)
    (let ((node (inverse-slot-node self)))
      (if (typep node 'node)
	(multiple-value-bind (name shortp)
			     (find-short-name *nodes* (node-uri (inverse-slot-node self)))
	  (format stream (if shortp "!~A" "!~S") name))
	(prin1 node stream)))))

(defstruct (default-value
	     (:copier nil)
	     (:constructor make-default-value (value)))
  value)

(defmethod print-object ((self default-value) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (default-value-value self) stream)))


(defstruct (access-daemon
	     (:copier nil)
	     (:constructor make-access-daemon (property)))
  property)

(defmethod print-object ((self access-daemon) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (access-daemon-property self) stream)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS PATH-FILTER
;;;   CLASS PATH-URI-FILTER
;;;   CLASS FUNCTIONAL-RESTRICTION
;;;

(defclass path-filter ()
  ((key
    :initarg :key
    :reader path-filter-key)))

(defmethod print-object ((self path-filter) stream)
  (print-unreadable-object (self stream :type t)
    (prin1 (path-filter-key self) stream)))

(defgeneric path-filter-match-p (filter node))

(defmethod path-filter-match-p ((filter path-filter) node)
  (declare (ignore node))
  nil)

(defclass path-uri-filter (path-filter)
  ())

(defmethod path-filter-match-p ((filter path-uri-filter) (node node))
  (name-contains-pattern-p (node-uri node) (path-filter-key filter)))

(defmethod path-filter-match-p ((filter path-uri-filter) (literal literal))
  (name-contains-pattern-p (literal-string literal) (path-filter-key filter)))

(defclass path-lang-filter (path-filter)
  ())

(defmethod path-filter-match-p ((filter path-lang-filter) (literal literal))
  (let ((key (path-filter-key filter))
	(lang (literal-language literal)))
    (cond ((null lang)
	   (null key))
	  (key
	   (let ((n (length key)))
	     (and (<= n (length lang))
		  (string= key lang :end2 n)))))))

(defclass path-node-restriction (path-filter)
  ())

(defmethod path-filter-match-p ((filter path-node-restriction) (node node))
  (eq node (path-filter-key filter)))

(defmethod path-filter-match-p ((filter path-node-restriction) (node null))
  t)

(defmethod path-filter-match-p ((filter path-node-restriction) (literal literal))
  nil)

(defclass functional-restriction (path-filter)
  ())

(defmethod path-filter-match-p ((filter functional-restriction) node)
  (funcall (path-filter-key filter) node))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS NODE (FRAME SYSTEM API ADDITIONS)
;;;

;; It is difficult to use EQL-specializers here, since the reasoner may rewrite path
;; expressions and thus change the applicable methods list on the fly...
(defmethod db-get-values ((db db) (frame literal) path)
  (if (eq path !rdf:type)
    (let ((datatype (literal-datatype frame)))
      (if datatype
	(list datatype !rdf:XMLLiteral !rdfs:Resource)
	(list !rdfs:Literal !rdfs:Resource)))
    (call-next-method)))

(defun collect-inverse-members (node db)
  (mapcan #'(lambda (u)
	      (mapcar #'triple-subject (db-query db nil u node)))
	  (coerce *index-uris* 'list)))

(defun db-extract (db frame path triples key)
  (declare (ignore db frame path))
  (when triples
    (mapcar key triples)))

(defmethod db-get-values ((db db) frame path
			  &aux (f (if (eq frame :all) nil frame)))
  ;; Here's where Wilbur spends its time...
  (etypecase path
    (node          (mapcar #'triple-object (db-query db f path nil)))
    (inverse-slot  (let ((slot (inverse-slot-node path)))
		     (etypecase slot
		       (node
			(mapcar #'triple-subject (db-query db nil slot f)))
		       (symbol
			(ecase slot
			  (:predicate-of-object
			   (mapcar #'triple-object (db-query db nil f nil)))
			  (:predicate-of-subject
			   (mapcar #'triple-subject (db-query db nil f nil)))
			  (:members
			   (collect-inverse-members f db))
			  (:any
			   (mapcar #'triple-subject (db-query db nil nil f))))))))
    (path          (collect-using-fsa f (path-fsa path) db))
    (symbol        (ecase path
		     (:predicate-of-object
		      (mapcar #'triple-predicate
			      (remove-duplicates (db-query db nil nil f)
						 :key #'triple-predicate)))
		     (:predicate-of-subject
		      (mapcar #'triple-predicate
			      (remove-duplicates (db-query db f nil nil)
						 :key #'triple-predicate)))
		     (:self
		      (list f))
		     (:members
		      (loop for i from 1
			 for v = (first (db-get-values db f (node (index-uri i db))))
			 while v collect v))
		     (:any
		      (mapcar #'triple-object (db-query db f nil nil)))))
    (default-value (list (default-value-value path)))
    (cons           (collect-using-fsa f (db-make-path-fsa db path) db))
    (path-filter    (and (path-filter-match-p path f)
			 (list (or f (path-filter-key path)))))
    (access-daemon  (db-compute-daemon-values db f (access-daemon-property path)))))

(defmethod db-compute-daemon-values ((db db) frame slot)
  (case slot
    (!wilbur:timeStamp
     (list (db-make-literal db (iso8601-date-string (get-universal-time))
			    :datatype !xsd:dateTime)))
    (!wilbur:tripleCount
     (when (find frame (db-sources db))
       (list (db-make-literal db (prin1-to-string (length (db-query-by-source db frame)))
			      :datatype !xsd:integer))))))

(defmethod frames-related-p ((source node)
                             path
                             (sink node)
                             (db db)
                             action)
  (%frames-related-p source path sink db action))

(defun %frames-related-p (source path sink db action)
  ;; this does not do any reasoning!
  (typecase path
    (node
     (not (null (db-query db source path sink))))
    (inverse-slot
     (frames-related-p sink (inverse-slot-node path) source db action))
    (cons
     (frames-related-p source (make-instance 'path :db db :expression path)
		       sink db action))
    (path
     (flet ((is-sink-p (node)
	      (when action
		(funcall action node))
	      (eq node sink)))
       (declare (dynamic-extent #'is-sink-p))
       (walk-using-fsa source (path-fsa path) #'is-sink-p db)))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RESUMABLE-QUERY
;;;

(defclass resumable-query ()
  ((results
    :initarg :results
    :initform nil
    :accessor resumable-query-results)))

(defmethod db-get-values-resumable ((db db) frame path
				    &key (resumable-query (db-get-values db frame path))
				         (count most-positive-fixnum))
  (let* ((results (resumable-query-results resumable-query))
	 (n (length results)))
    (cond ((> count n)
	   (let ((head (subseq results 0 count)))
	     (setf (resumable-query-results resumable-query)
		   (subseq results count))
	     (values head resumable-query)))
	  (t
	   (setf (resumable-query-results resumable-query) nil)
	   (values results nil)))))
