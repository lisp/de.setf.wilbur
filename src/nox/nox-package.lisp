;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  nox-package.lisp
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
;;;   Purpose: Definition for the package NOX
;;;


(in-package "CL-USER")


;;; --------------------------------------------------------------------------------------
;;;
;;;   PACKAGE NOX
;;;

(defpackage "NOX"
  (:nicknames "NOKIA-XML-CL"
	      "WILBUR-XML")
  (:use "COMMON-LISP"
	#+:mcl "CCL"
	#+:excl "EXCL"
	#+:sbcl "SB-SYS")
  (:export "XML-ERROR"                  ; from xml-util.lisp
	   "ERROR-THING"
	   "SYNTAX-ERROR"
	   "PI-TERMINATION-PROBLEM"
	   "DTD-TERMINATION-PROBLEM"
	   "UNEXPECTED-END-TAG"
	   "ERROR-EXPECTATION"
	   "UNKNOWN-DECLARATION"
	   "UNKNOWN-CHARACTER-REFERENCE"
	   "MALFORMED-URL"
	   "FEATURE-NOT-SUPPORTED"
	   "MISSING-DEFINITION"
	   "ERROR-DEFINITION-TYPE"
	   "MISSING-ENTITY-DEFINITION"
	   "MISSING-NAMESPACE-DEFINITION"
	   "XML-WARNING"
	   "*CURRENT-PARSER*"
	   "READ-USING"
	   "STRING-DICT-GET"
	   "STRING-DICT-GET-BY-VALUE"
	   "STRING-DICT-ADD"
	   "STRING-DICT-DEL"
	   "DO-STRING-DICT"
	   "MAKE-FILE-URL"
	   "MAKE-HTTP-URL"
	   "PARSE-URL"
	   "TOKEN"
	   "TOKEN-STRING"
	   "OPEN-TAG"
	   "CLOSE-TAG"
	   "ENTITY-DECLARATION"
	   "ENTITY-NAME"
	   "COMMENT"
	   "CHAR-CONTENT"
	   "TAG-COUNTERPART"
	   "TAG-ATTRIBUTE"
	   "TAG-ATTRIBUTES"
	   "TAG-EMPTY-P"
	   "TAG-NAMESPACES"
	   "START-ELEMENT"
	   "END-ELEMENT"
	   "CHAR-CONTENT"
	   "PROC-INSTRUCTION"
	   "START-DOCUMENT"
	   "END-DOCUMENT"
	   "MAYBE-USE-NAMESPACE"
	   "SAX-CONSUMER"
	   "SAX-CONSUMER-PRODUCER"
	   "SAX-CONSUMER-MODE"
	   "SAX-PRODUCER"
	   "SAX-PRODUCER-CONSUMER"
	   "SAX-FILTER"
	   "FIND-FIRST-PRODUCER"
	   "-WHITESPACE-CHARS-"
	   "WITH-RESOURCE-FROM-POOL"
	   "DEFINE-RESOURCE-POOL"
	   "COLLAPSE-WHITESPACE"
	   "*NAME-READER*"              ; from xml-parser.lisp
	   "XML-PARSER"
	   "GET-ENTITY"
	   "GET-CANONICAL-URI"
	   "PARSE"
	   "EXPAND-NAME-WITH-NAMESPACE"
	   "PARSE-FROM-STREAM"
	   "PARSE-FROM-FILE"
	   "XML-FORMATTER"
	   "REPLAY"
	   "REVERSE-EXPAND-NAME"
	   "TREE-PARSER"
	   "STRING->KEYWORD"
	   "PARSER-INTERPRET-CONTENT"
	   "-RDF-URI-"                  ; from rdf-constants.lisp
	   "-RDFS-URI-"
	   "-XSD-URI-"
	   "RDF-URI"
	   "RDFS-URI"
	   "XSD-URI"
	   "OWL-URI"
	   "-RDF-ATTRS-"
	   "-RDF-ATTR-MAP-"
	   "-RDF-ID-URI-"
	   "-RDF-RESOURCE-URI-"
	   "-RDF-ABOUT-URI-"
	   "-RDF-ABOUTEACH-URI-"
	   "-RDF-ABOUTEACHPREFIX-URI-"
	   "-RDF-BAGID-URI-"
	   "-RDF-PARSETYPE-URI-"
	   "-RDF-DATATYPE-URI-"
	   "-RDF-NODEID-URI-"
	   "-XML-LANG-ATTR-"
	   "-RDF-DESCRIPTION-URI-"
	   "-RDF-TYPE-URI-"
	   "-RDF-RDF-URI-"
	   "-RDF-LI-URI-"
	   "-RDF-STATEMENT-URI-"
	   "-RDF-SUBJECT-URI-"
	   "-RDF-PREDICATE-URI-"
	   "-RDF-OBJECT-URI-"
	   "-RDF-BAG-URI-"
	   "-RDF-SEQ-URI-"
	   "-RDF-ALT-URI-"
	   "-RDF-FIRST-URI-"
	   "-RDF-REST-URI-"
	   "-RDF-NIL-URI-"
	   "-RDFS-RESOURCE-URI-"
	   "-RDFS-CLASS-URI-"
	   "-RDFS-SUBCLASSOF-URI-"
	   "-RDFS-SUBPROPERTYOF-URI-"
	   "-RDFS-SEEALSO-URI-"
	   "-RDFS-ISDEFINEDBY-URI-"
	   "-RDFS-CONSTRAINTRESOURCE-URI-"
	   "-RDFS-CONSTRAINTPROPERTY-URI-"
	   "-RDFS-RANGE-URI-"
	   "-RDFS-DOMAIN-URI-"
	   "-RDFS-COMMENT-URI-"
	   "-RDFS-LABEL-URI-"
	   "-RDFS-LITERAL-URI-"
	   "-RDFS-CONTAINER-URI-"
	   "-OWL-LIST-URI-"
	   "-OWL-FIRST-URI-"
	   "-OWL-REST-URI-"
	   "-OWL-NIL-URI-"
	   "-OWL-IMPORTS-URI-"))
