;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  packages.lisp
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
;;;   Purpose: This file contains the package definition for WILBUR.
;;;


(in-package "CL-USER")


;;; --------------------------------------------------------------------------------------
;;;
;;;   PACKAGE WILBUR
;;;

(defpackage "WILBUR"
  (:nicknames "W"
	      "NOX")			; so as not to have many packages anymore
  (:use "COMMON-LISP"
	#+(or :digitool :clozure) "CCL"
	#+:excl "EXCL"
	#+:excl "SOCKET"
	#+:excl "MOP"
	#+:sbcl "SB-SYS"
        #+:sbcl "SB-GRAY"
	#+:lispworks "MP")
  (:export "*CURRENT-PARSER*" 
	   "*DB*" 
	   "*NAME-READER*" 
	   "*NODES*" 
	   "-DAML+OIL-URI-" 
	   "-DAML-FIRST-URI-" 
	   "-DAML-LIST-URI-" 
	   "-DAML-NIL-URI-" 
	   "-DAML-REST-URI-" 
	   "-OWL-FIRST-URI-" 
	   "-OWL-IMPORTS-URI-" 
	   "-OWL-LIST-URI-" 
	   "-OWL-NIL-URI-" 
	   "-OWL-REST-URI-" 
	   "-RDF-ABOUT-URI-" 
	   "-RDF-ABOUTEACH-URI-" 
	   "-RDF-ABOUTEACHPREFIX-URI-" 
	   "-RDF-ALT-URI-" 
	   "-RDF-ATTR-MAP-" 
	   "-RDF-ATTRS-" 
	   "-RDF-BAG-URI-" 
	   "-RDF-BAGID-URI-" 
	   "-RDF-DATATYPE-URI-" 
	   "-RDF-DESCRIPTION-URI-" 
	   "-RDF-ID-URI-" 
	   "-RDF-LI-URI-" 
	   "-RDF-NODEID-URI-" 
	   "-RDF-OBJECT-URI-" 
	   "-RDF-PARSETYPE-URI-" 
	   "-RDF-PREDICATE-URI-" 
	   "-RDF-RDF-URI-" 
	   "-RDF-RESOURCE-URI-" 
	   "-RDF-SEQ-URI-" 
	   "-RDF-STATEMENT-URI-" 
	   "-RDF-SUBJECT-URI-" 
	   "-RDF-TYPE-URI-" 
	   "-RDF-URI-" 
	   "-RDFS-CLASS-URI-" 
	   "-RDFS-COMMENT-URI-" 
	   "-RDFS-CONSTRAINTPROPERTY-URI-" 
	   "-RDFS-CONSTRAINTRESOURCE-URI-" 
	   "-RDFS-CONTAINER-URI-" 
	   "-RDFS-DOMAIN-URI-" 
	   "-RDFS-ISDEFINEDBY-URI-" 
	   "-RDFS-LABEL-URI-" 
	   "-RDFS-LITERAL-URI-" 
	   "-RDFS-RANGE-URI-" 
	   "-RDFS-RESOURCE-URI-" 
	   "-RDFS-SEEALSO-URI-" 
	   "-RDFS-SUBCLASSOF-URI-" 
	   "-RDFS-SUBPROPERTYOF-URI-" 
	   "-RDFS-URI-" 
	   "-WHITESPACE-CHARS-" 
	   "-XML-LANG-ATTR-" 
	   "ABOUT-AND-ID-BOTH-PRESENT" 
	   "ABOUT-AND-NODEID-BOTH-PRESENT" 
	   "ADD-NAMESPACE" 
	   "ADD-TRIPLE" 
	   "ADD-VALUE" 
	   "ALL-VALUES" 
	   "ATTACH-TO-PARENT" 
	   "BLANK-NODE-DB-MIXIN" 
	   "CHAR-CONTENT" 
	   "CHAR-CONTENT" 
	   "CLOSE-RDF-ELEMENT" 
	   "CLOSE-TAG" 
	   "COLLAPSE-WHITESPACE" 
	   "COLLECT-USING-FSA" 
	   "COMMENT" 
	   "CONTAINER-REQUIRED" 
	   "DAML-CONS" 
	   "DAML-LIST" 
	   "DAML-PARSER"
	   "DATE-CLEANUP-DB-MIXIN"
	   "DB" 
	   "DB-ADD-TRIPLE" 
	   "DB-BLANK-NODE-URI" 
	   "DB-BLANK-NODE-URI-P" 
	   "DB-CLEAR" 
	   "DB-CLEAR-REASONER-CACHE" 
	   "DB-DEL-SOURCE" 
	   "DB-DEL-TRIPLE" 
	   "DB-FIND-CBD" 
	   "DB-FIND-SOURCE-DESC"
	   "DB-GET-VALUES"
	   "DB-INDEX-LITERALS" 
	   "DB-INDEX-LITERALS-P" 
	   "DB-LOAD" 
	   "DB-LOAD-USING-SOURCE" 
	   "DB-MAKE-TRIPLE" 
	   "DB-MATCH-LITERALS" 
	   "DB-MERGE" 
	   "DB-NODE-PROPERTIES-PARTITIONED" 
	   "DB-NODE-TYPE-P" 
	   "DB-NODE-TYPES" 
	   "DB-QUERY" 
	   "DB-QUERY-BY-SOURCE" 
	   "DB-REIFY" 
	   "DB-RESOLVE-BLANK-NODE-URI" 
	   "DB-RESOLVE-BLANK-NODE-URI" 
	   "DB-SAMEAS-CLUSTERS" 
	   "DB-SOURCE-DESCS" 
	   "DB-SOURCE-REAL-URL" 
	   "DB-SOURCES" 
	   "DB-STARTUP-TIME" 
	   "DB-SUPPORTS-MATCHING-P"
	   "DB-TRANSFORM-LITERAL"
	   "DB-TRIPLE-LOCK" 
	   "DB-TRIPLES" 
	   "DB-URI->BLANK-NODE" 
	   "DEDUCTIVE-CLOSURE-DB-MIXIN" 
	   "DEFER-TASK" 
	   "DEFINE-READTABLE" 
	   "DEFINE-RESOURCE-POOL" 
	   "DEL-NAMESPACE" 
	   "DEL-TRIPLE" 
	   "DEL-VALUE" 
	   "DICTIONARY" 
	   "DICTIONARY-ADD-NAMESPACE" 
	   "DICTIONARY-APROPOS-LIST" 
	   "DICTIONARY-NAMESPACES" 
	   "DICTIONARY-NODE-CLASS" 
	   "DICTIONARY-NODES" 
	   "DICTIONARY-REMOVE-NAMESPACE" 
	   "DICTIONARY-RENAME-NAMESPACE" 
	   "DICTIONARY-UNRESOLVED-NODES" 
	   "DO-STRING-DICT" 
	   "DOLIST+" 
	   "DSB" 
	   "DTD-TERMINATION-PROBLEM" 
	   "DUPLICATE-NAMESPACE-PREFIX" 
	   "ENABLE-LITERAL-SHORTHAND" 
	   "ENABLE-NODE-SHORTHAND" 
	   "END-DOCUMENT" 
	   "END-ELEMENT" 
	   "ENTITY-DECLARATION" 
	   "ENTITY-NAME" 
	   "ERROR-DEFINITION-TYPE" 
	   "ERROR-EXPECTATION" 
	   "ERROR-THING" 
	   "EXECUTE-DEFERRED-TASK" 
	   "EXPAND-NAME-WITH-NAMESPACE" 
	   "FEATURE-NOT-SUPPORTED" 
	   "FILE-URL" 
	   "FIND-FIRST-PRODUCER" 
	   "FIND-HTTP-PROXY" 
	   "FIND-LONG-NAME" 
	   "FIND-NODE" 
	   "FIND-SHORT-NAME" 
	   "FRAME" 
	   "FRAMES-RELATED-P" 
	   "GET-ALL-VALUES" 
	   "GET-CANONICAL-URI" 
	   "GET-ENTITY" 
	   "GET-HEADER" 
	   "GET-VALUE" 
	   "HTTP-BODY" 
	   "HTTP-GET" 
	   "HTTP-HEAD" 
	   "HTTP-HEADERS" 
	   "HTTP-MESSAGE" 
	   "HTTP-STATUS" 
	   "HTTP-URL" 
	   "HTTP-VERSION" 
	   "ILLEGAL-CHARACTER-CONTENT" 
	   "INDEX-URI" 
	   "INDEX-URI-P" 
	   "INDEXED-DB" 
	   "INDEXED-LITERAL-DB-MIXIN" 
	   "INTERNED-LITERAL" 
	   "INTERNED-LITERAL-DB-MIXIN" 
	   "INVERT-PATH" 
	   "IS-CONTAINER-P" 
	   "ISO8601-DATE-STRING" 
	   "LITERAL"
	   "LITERAL-DATATYPE"
	   "LITERAL-LANGUAGE" 
	   "LITERAL-LANGUAGE-MATCH-P" 
	   "LITERAL-STRING"
	   "LITERAL-TRANSFORM-DB-MIXIN"
	   "LITERAL-VALUE"
	   "LOAD-DB" 
	   "LOAD-DB-FROM-STREAM" 
	   "LOCKED-DB-MIXIN" 
	   "MAKE-CONTAINER" 
	   "MAKE-FILE-URL" 
	   "MAKE-HTTP-URL" 
	   "MAKE-LOCK" 
	   "MAKE-TRIPLE-COLLECTION" 
	   "MAKE-URL" 
	   "MALFORMED-URL" 
	   "MAYBE-USE-NAMESPACE" 
	   "MISSING-DEFINITION" 
	   "MISSING-ENTITY-DEFINITION" 
	   "MISSING-NAMESPACE-DEFINITION" 
	   "NAMESPACES" 
	   "NODE" 
	   "NODE-NAME-RESOLVED-P" 
	   "NODE-URI" 
	   "OPEN-HTTP-STREAM" 
	   "OPEN-TAG" 
	   "OUT-OF-SEQUENCE-INDEX" 
	   "OWL-URI" 
	   "OWN-SLOTS" 
	   "PARSE" 
	   "PARSE-DB-FROM-FILE" 
	   "PARSE-DB-FROM-STREAM"
	   "PARSE-EXIF-DATE"
	   "PARSE-FROM-FILE"
	   "PARSE-FROM-STREAM" 
	   "PARSE-HTTP-DATE" 
	   "PARSE-ISO8601-DATE" 
	   "PARSE-URL" 
	   "PARSE-USING-PARSETYPE" 
	   "PARSER-DB" 
	   "PARSER-INTERPRET-CONTENT" 
	   "PARSER-NODE" 
	   "PARSER-PROPERTY" 
	   "PATH" 
	   "PATH-EXPRESSION" 
	   "PI-TERMINATION-PROBLEM"
	   "PRIORITIZE"
	   "PRIORITIZE-LIST"
	   "PROC-INSTRUCTION" 
	   "QUERY"
	   "QUIT-LISP-PROCESS"
	   "RDF-ERROR" 
	   "RDF-PARSER" 
	   "RDF-SYNTAX-NORMALIZER" 
	   "RDF-URI" 
	   "RDFS-URI" 
	   "READ-USING" 
	   "REIFY" 
	   "RELATEDP" 
	   "REPLAY" 
	   "REVERSE-EXPAND-NAME" 
	   "SAX-CONSUMER" 
	   "SAX-CONSUMER-MODE" 
	   "SAX-CONSUMER-PRODUCER" 
	   "SAX-FILTER" 
	   "SAX-PRODUCER" 
	   "SAX-PRODUCER-CONSUMER" 
	   "SIMPLE-EXTERNAL-PROCESS" 
	   "SOURCE-CLOSE-STREAM" 
	   "SOURCE-DESC" 
	   "SOURCE-DESC-LOAD-TIME" 
	   "SOURCE-DESC-LOADED-FROM" 
	   "SOURCE-DESC-URL" 
	   "SOURCE-LOCATOR" 
	   "SOURCE-MODIFICATION" 
	   "SOURCE-OPEN-STREAM" 
	   "SOURCE-ORIGINAL-STREAM" 
	   "SOURCE-WITH-MODIFICATION"
	   "SPLIT-LIST"
	   "START-DOCUMENT" 
	   "START-ELEMENT" 
	   "STRING->KEYWORD" 
	   "STRING-DICT-ADD" 
	   "STRING-DICT-DEL" 
	   "STRING-DICT-GET" 
	   "STRING-DICT-GET-BY-VALUE" 
	   "STRING-SOURCE" 
	   "SYNTAX-ERROR" 
	   "TAG-ATTRIBUTE" 
	   "TAG-ATTRIBUTES" 
	   "TAG-COUNTERPART" 
	   "TAG-EMPTY-P" 
	   "TAG-NAMESPACES" 
	   "TASK" 
	   "TASK-NODE" 
	   "TASK-PARAMETER" 
	   "TASK-TYPE" 
	   "TOKEN" 
	   "TOKEN-STRING" 
	   "TREE-PARSER" 
	   "TRIPLE" 
	   "TRIPLE-COLLECTION-ADD" 
	   "TRIPLE-COLLECTION-TRIPLES" 
	   "TRIPLE-OBJECT"
	   "TRIPLE-PREDICATE" 
	   "TRIPLE-SOURCES" 
	   "TRIPLE-SUBJECT" 
	   "TRIPLE=" 
	   "UNEXPECTED-END-TAG" 
	   "UNKNOWN-CHARACTER-REFERENCE" 
	   "UNKNOWN-DECLARATION" 
	   "UNKNOWN-PARSETYPE" 
	   "URL" 
	   "URL-HOST" 
	   "URL-PATH" 
	   "URL-PORT" 
	   "URL-STRING" 
	   "VALUE" 
	   "WALK-USING-FSA" 
	   "WITH-DB-LOCK" 
	   "WITH-HTTP-RESPONSE" 
	   "WITH-LOCK" 
	   "WITH-RESOURCE-FROM-POOL" 
           "WITH-SPO-CASE"
	   "WITH-TEMPS" 
	   "WITHOUT-CLOSURE" 
	   "XML-ERROR" 
	   "XML-FEATURE-NOT-SUPPORTED" 
	   "XML-FORMATTER" 
	   "XML-PARSER" 
	   "XML-WARNING" 
	   "XSD-URI"

	   "WITH-TAGS"
	   "FORMAT-WITH-TAGS"
	   "PRINC-WITH-TAGS"
	   "COMMA-SEPARATED"
	   "XHTML-PREAMBLE"
	   "XML-PREAMBLE"
	   "WITH-RDF-PAGE"
	   "ESCAPE-JSON-STRING"
	   "ESCAPE-XML-STRING"
	   "SERIALIZER"
	   "SERIALIZER-STREAM"
	   "SERIALIZER-DUMP"
	   "SINGLE-SUBJECT-TRIPLES"
	   "RDF/XML-SERIALIZER"))
