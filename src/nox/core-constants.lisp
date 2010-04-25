;;; -*- package: NOX; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  core-constants.lisp
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
;;;   Purpose: This file contains definitions for various constants used by the
;;;   RDF parser (mostly URIs). Given that the XML parser has to deal with the
;;;   issue of RDF M+S vagueness on the namespaces of RDF attributes (such as
;;;   "about"), the definitions in this file are in the NOX package.
;;;
;;;   Generally, I hate this stuff since I never seem to get the constant
;;;   definitions right vis-a-vis compile time vs. load-time. :-(
;;;


(in-package "NOX")


;;; --------------------------------------------------------------------------------------
;;;
;;;   HELPERS
;;;
  
(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdf-uri-  #."http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  (defconstant -rdfs-uri- #."http://www.w3.org/2000/01/rdf-schema#")
  (defconstant -xsd-uri-  #."http://www.w3.org/2001/XMLSchema#")
  (defconstant -owl-uri-  #."http://www.w3.org/2002/07/owl#")
  (defconstant -daml-uri- #."http://www.daml.org/2000/12/daml+oil#")
  
  (defmacro rdf-uri (string)  `(concatenate 'string -rdf-uri- ,string))
  (defmacro rdfs-uri (string) `(concatenate 'string -rdfs-uri- ,string))
  (defmacro xsd-uri (string)  `(concatenate 'string -xsd-uri- ,string))
  (defmacro owl-uri (string)  `(concatenate 'string -owl-uri- ,string))
  (defmacro daml-uri (string) `(concatenate 'string -daml-uri- ,string))

  (defconstant -alternate-rdf-uri-
    #."http://www.w3.org/TR/REC-rdf-syntax/")
  (defconstant -alternate-rdfs-uri-
    #."http://www.w3.org/TR/1999/PR-rdf-schema-19990303#"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF M+S ATTRIBUTE URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdf-id-uri-              #.(rdf-uri "ID"))
  (defconstant -rdf-resource-uri-        #.(rdf-uri "resource"))
  (defconstant -rdf-about-uri-           #.(rdf-uri "about"))
  (defconstant -rdf-abouteach-uri-       #.(rdf-uri "aboutEach"))
  (defconstant -rdf-abouteachprefix-uri- #.(rdf-uri "aboutEachPrefix"))
  (defconstant -rdf-bagid-uri-           #.(rdf-uri "bagID"))
  (defconstant -rdf-parsetype-uri-       #.(rdf-uri "parseType"))
  (defconstant -rdf-datatype-uri-        #.(rdf-uri "datatype"))
  (defconstant -rdf-nodeid-uri-          #.(rdf-uri "nodeID"))
  (defconstant -xml-lang-attr-           "xml:lang"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF M+S RESOURCE, PROPERTY, ETC. URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdf-description-uri- #.(rdf-uri "Description"))
  (defconstant -rdf-type-uri-        #.(rdf-uri "type"))
  (defconstant -rdf-rdf-uri-         #.(rdf-uri "RDF"))
  (defconstant -rdf-li-uri-          #.(rdf-uri "li"))
  (defconstant -rdf-statement-uri-   #.(rdf-uri "Statement"))
  (defconstant -rdf-subject-uri-     #.(rdf-uri "subject"))
  (defconstant -rdf-predicate-uri-   #.(rdf-uri "predicate"))
  (defconstant -rdf-object-uri-      #.(rdf-uri "object"))
  (defconstant -rdf-xmlliteral-uri-  #.(rdf-uri "XMLLiteral"))
  (defconstant -rdf-bag-uri-         #.(rdf-uri "Bag"))
  (defconstant -rdf-seq-uri-         #.(rdf-uri "Seq"))
  (defconstant -rdf-alt-uri-         #.(rdf-uri "Alt"))
  (defconstant -rdf-list-uri-        #.(rdf-uri "List"))
  (defconstant -rdf-first-uri-       #.(rdf-uri "first"))
  (defconstant -rdf-rest-uri-        #.(rdf-uri "rest"))
  (defconstant -rdf-nil-uri-         #.(rdf-uri "nil")))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF SCHEMA URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdfs-resource-uri-           #.(rdfs-uri "Resource"))
  (defconstant -rdfs-class-uri-              #.(rdfs-uri "Class"))
  (defconstant -rdfs-subclassof-uri-         #.(rdfs-uri "subClassOf"))
  (defconstant -rdfs-subpropertyof-uri-      #.(rdfs-uri "subPropertyOf"))
  (defconstant -rdfs-seealso-uri-            #.(rdfs-uri "seeAlso"))
  (defconstant -rdfs-isdefinedby-uri-        #.(rdfs-uri "isDefinedBy"))
  (defconstant -rdfs-constraintresource-uri- #.(rdfs-uri "ConstraintResource"))
  (defconstant -rdfs-constraintproperty-uri- #.(rdfs-uri "ConstraintProperty"))
  (defconstant -rdfs-range-uri-              #.(rdfs-uri "range"))
  (defconstant -rdfs-domain-uri-             #.(rdfs-uri "domain"))
  (defconstant -rdfs-comment-uri-            #.(rdfs-uri "comment"))
  (defconstant -rdfs-label-uri-              #.(rdfs-uri "label"))
  (defconstant -rdfs-literal-uri-            #.(rdfs-uri "Literal"))
  (defconstant -rdfs-datatype-uri-           #.(rdfs-uri "Datatype"))
  (defconstant -rdfs-container-uri-          #.(rdfs-uri "Container "))
  (defconstant -rdfs-member-uri-             #.(rdfs-uri "member")))


;;; --------------------------------------------------------------------------------------
;;;
;;;   XSD URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -xsd-string-uri-             #.(xsd-uri "string"))
  (defconstant -xsd-boolean-uri-            #.(xsd-uri "boolean"))
  ;;(defconstant -xsd-decimal-uri-            #.(xsd-uri "decimal"))
  (defconstant -xsd-float-uri-              #.(xsd-uri "float"))
  (defconstant -xsd-double-uri-             #.(xsd-uri "double"))
  (defconstant -xsd-datetime-uri-           #.(xsd-uri "dateTime"))
  ;;(defconstant -xsd-time-uri-               #.(xsd-uri "time"))
  (defconstant -xsd-date-uri-               #.(xsd-uri "date"))
  ;;(defconstant -xsd-gyearmonth-uri-         #.(xsd-uri "gYearMonth"))
  ;;(defconstant -xsd-gyear-uri-              #.(xsd-uri "gYear"))
  ;;(defconstant -xsd-gmonthday-uri-          #.(xsd-uri "gMonthDay"))
  ;;(defconstant -xsd-gday-uri-               #.(xsd-uri "gDay"))
  ;;(defconstant -xsd-gmonth-uri-             #.(xsd-uri "gMonth"))
  ;;(defconstant -xsd-hexbinary-uri-          #.(xsd-uri "hexBinary"))
  ;;(defconstant -xsd-base64binary-uri-       #.(xsd-uri "base64Binary"))
  ;;(defconstant -xsd-anyuri-uri-             #.(xsd-uri "anyURI"))
  (defconstant -xsd-normalizedstring-uri-   #.(xsd-uri "normalizedString"))
  ;;(defconstant -xsd-token-uri-              #.(xsd-uri "token"))
  ;;(defconstant -xsd-language-uri-           #.(xsd-uri "language"))
  ;;(defconstant -xsd-nmtoken-uri-            #.(xsd-uri "NMTOKEN"))
  ;;(defconstant -xsd-name-uri-               #.(xsd-uri "Name"))
  ;;(defconstant -xsd-ncname-uri-             #.(xsd-uri "NCName"))
  (defconstant -xsd-integer-uri-            #.(xsd-uri "integer"))
  ;;(defconstant -xsd-nonpositiveinteger-uri- #.(xsd-uri "nonPositiveInteger"))
  ;;(defconstant -xsd-negativeinteger-uri-    #.(xsd-uri "negativeInteger"))
  ;;(defconstant -xsd-long-uri-               #.(xsd-uri "long"))
  (defconstant -xsd-int-uri-                #.(xsd-uri "int"))
  ;;(defconstant -xsd-short-uri-              #.(xsd-uri "short"))
  ;;(defconstant -xsd-byte-uri-               #.(xsd-uri "byte"))
  ;;(defconstant -xsd-nonnegativeinteger-uri- #.(xsd-uri "nonNegativeInteger"))
  ;;(defconstant -xsd-unsignedlong-uri-       #.(xsd-uri "unsignedLong"))
  ;;(defconstant -xsd-unsignedint-uri-        #.(xsd-uri "unsignedInt"))
  ;;(defconstant -xsd-unsignedshort-uri-      #.(xsd-uri "unsignedShort"))
  ;;(defconstant -xsd-unsignedbyte-uri-       #.(xsd-uri "unsignedByte"))
  ;;(defconstant -xsd-positiveinteger-uri-    #.(xsd-uri "positiveInteger"))
  )


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF ATTRIBUTE LISTS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -rdf-attrs- '#.`(,-rdf-id-uri-
				,-rdf-resource-uri-
				,-rdf-about-uri-
				,-rdf-abouteach-uri-
				,-rdf-abouteachprefix-uri-
				,-rdf-bagid-uri-
				,-rdf-parsetype-uri-
				,-rdf-datatype-uri-
				,-rdf-nodeid-uri-
				,-xml-lang-attr-))

  (defconstant -rdf-attr-map- #.`'((,"ID"              . ,-rdf-id-uri-)
				   (,"resource"        . ,-rdf-resource-uri-)
				   (,"about"           . ,-rdf-about-uri-)
				   (,"aboutEach"       . ,-rdf-abouteach-uri-)
				   (,"aboutEachPrefix" . ,-rdf-abouteachprefix-uri-)
				   (,"bagID"           . ,-rdf-bagid-uri-)
				   (,"parseType"       . ,-rdf-parsetype-uri-)
				   (,"datatype"        . ,-rdf-datatype-uri-)
				   (,"nodeID"          . ,-rdf-nodeid-uri-))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   OWL URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -owl-list-uri-      #.(owl-uri "List"))
  (defconstant -owl-first-uri-     #.(owl-uri "first"))
  (defconstant -owl-rest-uri-      #.(owl-uri "rest"))
  (defconstant -owl-nil-uri-       #.(owl-uri "nil"))
  (defconstant -owl-imports-uri-   #.(owl-uri "imports")))


;;; --------------------------------------------------------------------------------------
;;;
;;;   DAML+OIL URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (defconstant -daml-list-uri-  #.(daml-uri "List"))
  (defconstant -daml-first-uri- #.(daml-uri "first"))
  (defconstant -daml-rest-uri-  #.(daml-uri "rest"))
  (defconstant -daml-nil-uri-   #.(daml-uri "nil")))
