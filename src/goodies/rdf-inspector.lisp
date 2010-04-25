;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  rdf-inspector.lisp
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
;;;   Purpose: This file contains various kinds of functionality for visualizing as well
;;;   as browsing RDF data and WilburQL queries (using the MCL Inspector, PowerGrapher
;;;   and GraphWiz).
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS NODE-INSPECTOR
;;;
;;;   Extension of the MCL Inspector to allow browsing of RDF graphs.
;;;

(defclass node-inspector (inspector::usual-inspector)
  ((out-links
    :accessor inspector-out-links)
   (in-links
    :accessor inspector-in-links)))

(defmethod inspector::inspector-class ((node node))
  'node-inspector)

(defmethod initialize-instance :after ((i node-inspector) &rest args)
  (declare (ignore args))
  (flet ((sort-triples (triples)
           (sort (copy-list triples) #'string<
                 :key #'(lambda (x)
                          (node-uri (triple-predicate x))))))
    (let ((node (inspector::inspector-object i)))
      (setf (inspector-out-links i) (sort-triples (query node nil nil))
            (inspector-in-links i) (sort-triples (query nil nil node))))))

(defmethod inspector::compute-line-count ((i node-inspector))
  (+ (length (inspector-out-links i)) (length (inspector-in-links i)) 3))

(defmethod inspector::line-n ((i node-inspector) n)
  (let ((node (inspector::inspector-object i))
        (k (+ (length (inspector-out-links i)) 2)))
    (cond ((zerop n)
           (values (node-uri node) "URI" :colon))
          ((= n 1)
           (values nil "Properties" :comment))
          ((< n k)
           (let ((triple (elt (inspector-out-links i) (- n 2))))
             (values (triple-object triple)
                     (node-name (triple-predicate triple))
                     :colon)))
          ((= n k)
           (values nil "Incoming" :comment))
          (t
           (let ((triple (elt (inspector-in-links i) (- n k 1))))
             (values (triple-subject triple)
                     (node-name (triple-predicate triple))
                     :colon))))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-CLASS-TREE
;;;   CLASS RDF-CLASS-NODE
;;;
;;;   Extension of PowerGrapher to allow visualization of RDF graphs and class trees.
;;;

(defclass rdf-class-tree (pg:tree)
  ())

(defmethod pg:compute-root-nodes ((self rdf-class-tree)
                                  &key root (level most-positive-fixnum)
                                  &allow-other-keys)
  (let ((class (node root)))
    (list (pg:make-node self 'rdf-class-node class level nil :class class))))

(defclass rdf-class-node (wu:selectable-rectangle-mixin pg:text-node)
  ((class
    :initarg :class
    :reader pg:node-key)))

(defmethod pg:compute-node-text ((self rdf-class-node))
  (find-short-name *nodes* (node-uri (pg:node-key self))))

(defmethod pg:compute-node-children ((self rdf-class-node) level)
  (mapcar #'(lambda (triple)
              (let ((class (triple-subject triple)))
                (pg:make-node (pg:node-collection self)
                              (class-of self) class level self
                              :class class)))
          (query nil !rdfs:subClassOf (pg:node-key self))))

(defmethod wu:item-action ((self rdf-class-node))
  (inspect (pg:node-key self)))

(defun make-rdf-class-tree (root &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'pg:tree-window
         :root root
         :window-title (find-short-name *nodes* (node-uri root))
         :tree-class 'rdf-class-tree
         args))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-QUERY-TREE
;;;
;;;   Extension of PowerGrapher to allow visualization of WQL queries.
;;;

(defclass rdf-query-tree (pg:tree)
  ((children
    :initform nil
    :accessor tree-children)
   (terminal-nodes
    :initform nil
    :accessor tree-terminal-nodes)))

(defmethod pg:compute-root-nodes ((self rdf-query-tree)
                                  &key root (level most-positive-fixnum) query
                                  &allow-other-keys)
  (let ((n (node root))
        (children (make-hash-table :test #'eq))
        (terminal-nodes nil))
    (instrumented-walk-using-fsa n (make-path-fsa query)
                                 #'(lambda (node terminalp transitions)
                                     (when terminalp
                                       (pushnew node terminal-nodes))
                                     (dolist (tr transitions)
                                       (dolist (value (db-get-slot-values *db* node
                                                                          (pt-input tr)))
                                         (pushnew value (gethash node children))))
                                     nil)
                                 *db*)
    (setf (tree-children self) children
          (tree-terminal-nodes self) terminal-nodes)
    (list (pg:make-node self 'rdf-query-node n level nil :node n))))

(defclass rdf-query-node (pg:text-node)
  ((node
    :initarg :node
    :reader pg:node-key)
   (terminalp
    :initarg :terminalp
    :reader node-terminal-p)))

(defmethod view-draw-contents :after ((self rdf-query-node))
  (when (member (pg:node-key self) (tree-terminal-nodes (pg:node-collection self)))
    (wu:draw-rect self #@(1 1) (subtract-points (view-size self) #@(1 1)) :black nil)))

(defmethod pg:compute-node-children ((self rdf-query-node) level)
  (let ((node (pg:node-key self))
        (tree (pg:node-collection self)))
    (etypecase node
      (string nil)
      (node (mapcar #'(lambda (child)
                        (pg:make-node tree (class-of self) child level self
                                      :node child))
                    (gethash node (tree-children tree)))))))

(defmethod pg:compute-node-text ((self rdf-query-node))
  (let ((key (pg:node-key self)))
    (etypecase key
      (string (format nil "~S" key))
      (node (let ((uri (node-uri key)))
              (if uri
                (find-short-name *nodes* uri)
                (format nil "(~S)" (sxhash key))))))))

(defun make-rdf-query-tree (root query &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'pg:tree-window
         :root root
         :query query
         :tree-class 'rdf-query-tree
         :window-title (prin1-to-string query)
         args))


;;; --------------------------------------------------------------------------------------
;;;
;;;   Code to allow visualization of RDF graphs and WQL queries using GraphViz.
;;;

(defun make-rdf-query-dot (root query stream)
  (let ((n (node root))
        (links (make-hash-table :test #'eq))
        (terminal-nodes nil))
    (flet ((node= (x y)
             (eq (if (typep x 'inverse-slot)
                   (inverse-slot-node x)
                   x)
                 (if (typep y 'inverse-slot)
                   (inverse-slot-node x)
                   y))))
      (instrumented-walk-using-fsa n (make-path-fsa query)
                                   #'(lambda (node terminalp transitions)
                                       (when terminalp
                                         (pushnew node terminal-nodes))
                                       (dolist (tr transitions)
                                         (dolist (value (db-get-slot-values *db* node
                                                                            (pt-input tr)))
                                           (pushnew  (cons value (pt-input tr))
                                                     (gethash node links)
                                                     :test #'(lambda (x y)
                                                               (and (eq (car x) (car y))
                                                                    (node= (cdr x) (cdr y)))))))
                                       nil)
                                   *db*)
      (format stream "digraph G {~%")
      (maphash #'(lambda (node links)
                   (dolist (link links)
                     (destructuring-bind (child . prop) link
                       (etypecase prop
                         (inverse-slot
                          (format stream "  ~S -> ~S [label=~S];~%"
                                  (find-short-name *nodes* (node-uri node))
                                  (find-short-name *nodes* (node-uri child))
                                  (find-short-name *nodes* (node-uri (inverse-slot-node prop)))))
                         (node
                          (format stream "  ~S -> ~S [label=~S];~%"
                                  (find-short-name *nodes* (node-uri node))
                                  (find-short-name *nodes* (node-uri child))
                                  (find-short-name *nodes* (node-uri prop))))))))
               links)
      (dolist (node terminal-nodes)
        (format stream "  ~S [peripheries=2];~%" (find-short-name *nodes* (node-uri node))))
      (format stream "}~%"))))

(defun instrumented-walk-using-fsa (root fsa action db)
  (let* ((*walk-states/temporary* *walk-states/temporary*)
         (states (clrhash (or (pop *walk-states/temporary*)
                              (make-hash-table :test #'eq)))))
    (labels ((w (f i)
               (unless (member i (gethash f states) :test #'=)
                 (push i (gethash f states))
                 (let ((transitions (svref fsa i)))
                   (or (funcall action f (first transitions) (rest transitions))
                       (when (typep f 'node)
                         (dolist (link (rest transitions))
                           (dolist (v (db-get-slot-values db f (pt-input link)))
                             (let ((values (w v (pt-index link))))
                               (when values
                                 (return-from instrumented-walk-using-fsa
                                   values)))))))))))
      (declare (dynamic-extent #'w))
      (when fsa
        (w root 0)))))

(defun make-dot (db output-file)
  (with-open-file-output (stream output-file)
    (make-dot-into-stream db stream)))

(defun make-dot-into-stream (db stream)
  (let ((nodes (make-hash-table :test 'eq)))
    (flet ((normalize (node)
             (cond ((typep node 'literal)
                    (let ((sym (gentemp)))
                      (setf (gethash sym nodes) (literal-string node))
                      sym))
                   ((null (node-uri node))
                    (let ((sym (gentemp)))
                      (setf (gethash sym nodes) node)
                      sym))
                   (t
                    (find-short-name *nodes* (node-uri node))))))
      (format stream "digraph G {~%")
      (dolist (tr (db-triples db))
        (let ((s (normalize (triple-subject tr)))
              (p (normalize (triple-predicate tr)))
              (o (normalize (triple-object tr))))
          (format stream "~S -> ~S [label=~S];~%" s o p)))
      (maphash #'(lambda (node label)
                   (cond ((stringp label)
                          (format stream "~S [shape=plaintext, label=\"\\\"~A\\\"\"];~%"
                                  node label))
                         ((null label)
                          (unless label
                            (format stream "~S [label=\"\"];~%"
                                    node)))))
               nodes)
      (format stream "}~%"))))

(defun fsa->dot (fsa stream)
  (format stream "digraph G {~%")
  (dotimes (i (length fsa))
    (destructuring-bind (terminalp &rest transitions) (elt fsa i)
      (dolist (tr transitions)
        (format stream "  ~S -> ~S [label=~S];~%"
                i (pt-index tr)
                (find-short-name *nodes* (node-uri (pt-input tr)))))
      (when terminalp
        (format stream "  ~S [peripheries=2];~%" i))))
  (format stream "}~%"))
