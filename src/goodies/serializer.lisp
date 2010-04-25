;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  serializer.lisp
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
;;;   Purpose: Functionality for serializing RDF content in various formats.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   FUNCTIONS AND MACROS FOR MARKUP GENERATION
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defun strip-attributes (tag)
    (let ((i (position #\Space tag :test #'char=)))
      (if i (subseq tag 0 i) tag)))
  
  (defmacro with-open-file-output ((stream pathname) &body body)
    `(with-open-file (,stream ,pathname
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
       ,@body)))

(defmacro with-tags ((stream &rest tags) &body body)
  (with-temps (s)
    (if (every #'stringp tags)
      (let ((open (format nil "~{<~A>~}~%" tags))
	    (close (format nil "~{</~A>~}~%"
			   (nreverse (mapcar #'strip-attributes tags)))))
	`(let ((,s ,stream))
	  (princ ,open ,s)
	  (multiple-value-prog1 (progn ,@body)
	    (princ ,close ,s))))
      (with-temps (e)
	`(let ((,s ,stream)
	       (,e (list ,@tags)))
	  (format ,s "~{<~A>~}~%" ,e)
	  (multiple-value-prog1 (progn ,@body)
	    (format ,s "~{</~A>~}~%" (nreverse (mapcar #'strip-attributes ,e)))))))))

(defmacro format-with-tags ((stream &rest tags) control &rest args)
  (with-temps (s)
    `(let ((,s ,stream))
      (with-tags (,s ,@tags)
	(format ,s ,control ,@args)))))

(defmacro princ-with-tags ((stream &rest tags) form)
  (with-temps (s)
    `(let ((,s ,stream))
       (with-tags (,s ,@tags)
	 (princ ,form ,s)))))

(defun comma-separated (items stream &optional (mapper nil) (only-non-null-p t))
  (let ((items (if only-non-null-p (remove nil items) items)))
    (format stream "~{~A~^, ~}" (if mapper (mapcar mapper items) items))))

(defun xml-preamble (stream)
  (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%"))

(defun xhtml-preamble (stream)
  (xml-preamble stream)
  (format stream "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ~
                  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~%"))

(defmacro with-rdf-page ((stream prefixes) &body body)
  (with-temps (s ps p)
    `(let ((,s ,stream)
	   (,ps ,prefixes))
      (xml-preamble ,s)
      (format ,s "<!DOCTYPE rdf [~%")
      (dolist (,p ,ps)
	(format ,s "<!ENTITY ~A \"~A\">~%"
		,p (string-dict-get (dictionary-namespaces *nodes*) ,p)))
      (format ,s "]>~%")
      (with-tags (,s (format nil "rdf:RDF~{ xmlns:~A=\"&~:*~A;\"~}" ,ps))
	,@body))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   DB DUMP MECHANISM
;;;

(defmethod db-dump ((db db) (where string) what style &optional namespaces)
  (db-dump db (pathname where) what style namespaces))

(defmethod db-dump ((db db) (where pathname) what style &optional namespaces)
  (with-open-file-output (stream where)
    (db-dump db stream what style namespaces)))

(defmethod db-dump ((db db) (where (eql t)) what style &optional namespaces)
  (db-dump db *standard-output* what style namespaces))

(defmethod db-dump ((db db) (where stream) (what db) style &optional namespaces)
  (db-dump db where (db-triples db) style namespaces))

(defmethod db-dump ((db db) (where stream) (thing node) style &optional namespaces)
  (db-dump db where (db-find-cbd db thing) style namespaces))

(defmethod db-dump ((db db) (where stream) (thing list) (style (eql :ntriples))
		    &optional namespaces)
  (declare (ignore namespaces))
  (dump-as-ntriples thing where))

(defmethod db-dump ((db db) (where stream) (thing list) (style (eql :rdf/xml))
		    &optional namespaces)
  (dump-as-rdf/xml thing where namespaces))

(defmethod db-dump ((db db) (where stream) (thing list) style &optional namespaces)
  (declare (ignore namespaces))
  (error "Don't know how to dump in format ~S" style))

(defun single-subject-triples (subject &rest predicates&objects)
  (declare (dynamic-extent predicates&objects))
  (loop for predicate in predicates&objects by #'cddr
	for object in (cdr predicates&objects) by #'cddr
	collect (db-make-triple *db* subject predicate object)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   NTRIPLES DUMP
;;;

(defun dump-as-ntriples (triples stream)
  (let ((bnodes (make-hash-table :test #'eq))
	(index 0))
    (flet ((dump-element (element)
	     (cond ((typep element 'literal)
		    (print-literal-for-ntriples element stream)
		    (princ #\Space stream))
		   ((node-uri element)
		    (format stream "<~A> " (node-uri element)))
		   (t
		    (format stream "~A "
			    (or (gethash element bnodes)
				(setf (gethash element bnodes)
				      (format nil "_:A~S" (incf index)))))))))
    (dolist (triple triples)
      (dump-element (triple-subject triple))
      (dump-element (triple-predicate triple))
      (dump-element (triple-object triple))
      (format stream ".~%")))))

(defun escape-ntriples-char (char)
  (cdr (assoc char '((#\\ . "\\\\")
		     (#\" . "\\\"")
		     (#\Linefeed . "\\n")
		     (#\Return . "\\r")
		     (#\Tab . "\\t"))
	      :test #'char=)))

(defun escape-ntriples-string (string)
  (escape-string string #'escape-ntriples-char))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF/XML DUMP
;;;

(defun dump-as-rdf/xml (triples stream namespaces)
  (let ((subjects (wilbur-make-hash-table :test #'eq))
	(bnode-objects (wilbur-make-hash-table :test #'eq))
	(subjects-done (wilbur-make-hash-table :test #'eq))
	(namespaces (or namespaces (namespaces))))
    (labels ((qname (node use-entities-p)
	       (find-short-name *nodes* (node-uri node) use-entities-p))
	     (dump (frame properties &optional (level 0))
	       (unless (and (zerop level)
			    (wilbur-gethash frame subjects-done)
			    (wilbur-gethash frame bnode-objects))
		 (with-tags (stream (format nil "rdf:Description~@[ rdf:about=\"~A\"~]"
					    (qname frame t)))
		   (dolist+ ((predicate . object) properties)
		     (let ((tag (qname predicate nil)))
		       (cond ((typep object 'literal)
			      (with-tags (stream tag)
				(princ (escape-xml-string (literal-string object))
				       stream)))
			     ((node-uri object)
			      (format stream "<~A rdf:resource=\"~A\"/>~%"
				      tag (qname object t)))
			     ((wilbur-gethash object bnode-objects)
			      (with-tags (stream tag)
				(dump object (wilbur-gethash object subjects) (1+ level))
				(setf (wilbur-gethash object subjects-done) object)))
			     (t
			      (error "Cannot serialize")))))))))
      (declare (dynamic-extent #'qname #'dump))
      (with-rdf-page (stream namespaces)
	(dolist (triple triples)
	  (let ((subject (triple-subject triple))
		(object (triple-object triple)))
	    (push (cons (triple-predicate triple) object)
		  (wilbur-gethash subject subjects))
	    (unless (or (typep object 'literal)
			(node-uri object)
			(wilbur-gethash object bnode-objects))
	      (setf (wilbur-gethash object bnode-objects) object))))
	(maphash #'dump subjects)))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CHARACTER & STRING ESCAPING
;;;

(defun escape-xml-char (char)
  (cdr (assoc char '((#\< . "&lt;")
                     (#\> . "&gt;")
		     (#\& . "&amp;")
                     (#\' . "&apos;")
                     (#\" . "&quot;"))
              :test #'char=)))

(defun escape-xml-string (string &optional (extended-chars-p #+:sbcl t #-:sbcl nil))
  (components->string (mapcar #'(lambda (c)
				  (if (< c 128)
				    (or (escape-xml-char (code-char c))
					(code-char c))
				    (format nil "&#x~X;" c)))
			      (if extended-chars-p
				(extended-string->char-codes string)
				(utf8-string->char-codes string)))))

(defun escape-json-string (string &optional (extended-chars-p #+:sbcl t #-:sbcl nil))
  (components->string (mapcar #'(lambda (c)
				  (cond ((= c 34)  "\\\"") ; double-quote
					((> c 127) (format nil "\\u~4,'0X" c))
					(t         (code-char c))))
			      (if extended-chars-p
				(extended-string->char-codes string)
				(utf8-string->char-codes string)))))

(defun escape-string (string char-escape-function)
  ;; This tries to be clever about stuff that does not need to be escaped
  (labels ((escape (s n i parts)
             (let ((j (position-if char-escape-function s :start i)))
               (cond (j (escape s n (1+ j)
                                (list* (funcall char-escape-function (char s j))
                                       (subseq s i j)
                                       parts)))
                     (parts (components->string (nreverse (cons (subseq s i) parts))))
                     (t s)))))
    (escape string (length string) 0 nil)))

#-:allegro
(defun 8bit-char-string->octets (string)
  (let ((octets nil))
    (map nil
	 #'(lambda (char)
	     (let ((c (char-code char)))
	       (cond ((< c 128)
		      (push c octets))
		     (t
		      (push (logior (ash c -6) #b11000000) octets)
		      (push (logior (logand c #b00111111) #b10000000) octets)))))
	 string)
    (nreverse (cons 0 octets))))

#-:allegro
(defun utf8-string->octets (string)
  (mapcar #'char-code (coerce string 'list)))

(defun extended-string->char-codes (string)
  (mapcar #'char-code (coerce string 'list)))
  
(defun utf8-string->char-codes (string)
  (labels ((utf8 (octets codes)
	     (dsb (&optional octet &rest octets) octets
	       (cond ((or (null octet) (zerop octet))
		      (nreverse codes))
		     ((= (logand octet #b10000000) 0)
		      (utf8 octets (cons octet codes)))
		     ((= (logand octet #b11100000) #b11000000)
		      (dsb (octet2 &rest octets) octets
			(utf8 octets
			      (cons (logior (ash (logand octet #b00011111) 6)
					    (logand octet2 #b00111111))
				    codes))))
		     ((= (logand octet #b11110000) #b11100000)
		      (dsb (octet2 octet3 &rest octets) octets
			(utf8 octets
			      (cons (logior (ash (logand octet #b00011111) 12)
					    (ash (logand octet2 #b00111111) 6)
					    (logand octet3 #b00111111))
				    codes))))
		     ((= (logand octet #b11111000) #b11110000)
		      (dsb (octet2 octet3 octet4 &rest octets) octets
			(utf8 octets
			      (cons (logior (ash (logand octet #b00011111) 18)
					    (ash (logand octet2 #b00111111) 12)
					    (ash (logand octet3 #b00111111) 6)
					    (logand octet4 #b00111111))
				    codes))))
		     (t
		      ;; This could be a hack, and I am not sure if it is correct
		      (utf8 (list* (logior (ash octet -6) #b11000000)
				   (logior (logand octet #b00111111) #b10000000)
				   octets)
			    codes))))))
    (and string
	 (utf8 #+:allegro (coerce (excl:string-to-octets string) 'list)
	       #-:allegro (utf8-string->octets string)
	       nil))))

(defun utf8-string->extended-string (string)
  (coerce (mapcar #'code-char (utf8-string->char-codes string)) 'string))

(defun components->string (components)
  (with-output-to-string (stream)
    (dolist (component components)
      (princ component stream))))
