;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  http.lisp
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
;;;   Purpose: This is a simple implementation of an HTTP client, conditionalized for
;;;   several platforms and environments. OK, so it is somewhat braindead, but at least
;;;   it works. Note: We currently support MCL, OpenMCL and Allegro.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS URL
;;;   CLASS HTTP-URL
;;;   CLASS FILE-URL
;;;   CLASS URN
;;;   CLASS TEL-URL
;;;   CLASS MAILTO-URL
;;;   CLASS UNKNOWN-SCHEME-URL
;;;

(defclass url ()
  ((string
    :initform nil
    :accessor url-string)
   (path
    :initarg :path
    :reader url-path)))

(defmethod print-object ((url url) stream)
  (print-unreadable-object (url stream :type t)
    (prin1 (url-string url) stream)))

(defclass http-url (url)
  ((host
    :initarg :host
    :reader url-host)
   (port
    :initarg :port
    :initform nil
    :reader url-port)))

(defmethod initialize-instance :after ((url http-url) &rest initargs)
  (declare (ignore initargs))
  (let ((port (url-port url)))
    (setf (url-string url)
	  (format nil "http://~A~@[:~S~]~@[~A~]"
		  (url-host url)
		  (and port (not (= port 80)) port)
		  (url-path url)))))

(defclass file-url (url)
  ())

(defmethod initialize-instance :after ((url file-url) &rest initargs)
  (declare (ignore initargs))
  (setf (url-string url) (format nil "file://~A" (namestring (url-path url)))))

(defclass urn (url)
  ())

(defmethod initialize-instance :after ((url urn) &rest initargs)
  (declare (ignore initargs))
  (setf (url-string url) (format nil "urn:~A" (url-path url))))

(defclass tel-url (url)
  ((number
    :initarg :number
    :reader url-number)
   (plusp
    :initarg :plusp
    :initform nil
    :reader url-plus-p)))

(defmethod initialize-instance :after ((url tel-url) &rest initargs)
  (declare (ignore initargs))
  (setf (url-string url) (format nil "tel:~:[~;+~]~A" (url-plus-p url) (url-number url))))

(defclass mailto-url (url)
  ())

(defmethod initialize-instance :after ((url mailto-url) &rest initargs)
  (declare (ignore initargs))
  (setf (url-string url) (format nil "mailto:~A" (url-path url))))

(defclass unknown-scheme-url (url)
  ((scheme
    :initarg :scheme
    :reader url-scheme)))

(defmethod initialize-instance :after ((url unknown-scheme-url) &rest initargs)
  (declare (ignore initargs))
  (setf (url-string url) (format nil "~A:~A" (url-scheme url) (url-path url))))

(defvar *url-scheme->class*
  '(:http   http-url
    :file   file-url
    :urn    urn
    :tel    tel-url
    :mailto mailto-url))

(defun make-url (string)
  (multiple-value-bind (scheme args)
                       (parse-url string)
    (apply #'make-instance (getf *url-scheme->class* scheme 'unknown-scheme-url) args)))

(defun make-relative-url-string (string base)
  (let ((ns (length string))
	(nb (length base))
	(i (position #\/ base :from-end t)))
    (cond ((and (> ns nb)
		(char= (char string nb) #\#)
		(string= string base :end1 nb))
	   (subseq string nb))
	  ((and i
		(> ns (1+ i))
		(string= string base :end1 (1+ i) :end2 (1+ i)))
	   (subseq string (1+ i)))
	  (t
	   string))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP CONDITIONS
;;;

(define-condition http-error (wilbur-error)
  ((thing
    :initarg :thing
    :reader http-error-thing))
  (:default-initargs 
   :format-control "HTTP --- ~A")
  (:report (lambda (c s)
             (funcall #'report-http-error c s))))

(define-condition http-bad-response (http-error)
  ((got
    :initform nil
    :initarg :got
    :reader http-error-got))
  (:default-initargs 
   :format-control "HTTP --- Expected ~A, got ~A"))

(define-condition http-bad-redirect (http-bad-response)
  ()
  (:default-initargs 
   :format-control "HTTP --- ~A header not found"))

(define-condition http-not-found (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Entity ~A not found"))

(define-condition http-too-many-redirects (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Too many redirects: last was ~A"))

(define-condition http-incomplete-entity (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Incomplete entity: expected ~A more bytes"))

(define-condition http-bad-request (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Bad request"))

(define-condition http-server-error (http-error)
  ()
  (:default-initargs 
   :format-control "HTTP --- Server error"))

(define-condition http-unsupported-status (http-error)
  ()
  (:default-initargs
   :format-control "HTTP --- Unsupported status code ~S"))

(defmethod report-http-error ((condition http-error) stream)
  (apply #'format stream (simple-condition-format-control condition)
         (http-error-thing condition) (simple-condition-format-arguments condition)))

(defmethod report-http-error ((condition http-bad-response) stream)
  (format stream (simple-condition-format-control condition)
          (http-error-thing condition) (http-error-got condition)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   LOW-LEVEL HTTP STREAM GENERIC FUNCTIONS
;;;

(defgeneric open-http-stream (url proxy))

(defgeneric make-http-body-stream (socket-stream))

(defgeneric http-stream-character-count (stream))

(defgeneric (setf http-stream-character-count) (new-value stream))

(defgeneric http-stream-enable-input-chunking (stream))

(defgeneric http-stream-disable-input-chunking (stream))

(defgeneric http-stream-chunked-p (http-stream))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HIGH-LEVEL HTTP REQUEST GENERIC FUNCTIONS
;;;

(defgeneric http-request (url method &key proxy accept))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS HTTP-MESSAGE
;;;

(defclass http-message ()
  ((status
    :initarg :status
    :reader http-status)
   (version
    :initarg :version
    :reader http-version)
   (headers
    :initarg :headers
    :initform nil
    :accessor http-headers)
   (request-time
    :initarg :request-time
    :initform (get-universal-time)
    :reader http-request-time)
   (response-time
    :initform (get-universal-time)
    :reader http-response-time)
   (url
    :initform nil
    :initarg :url
    :reader http-url)
   (body
    :initarg :body
    :initform nil
    :reader http-body)
   #+:http-using-aserve
   (native-request
    :initarg :request
    :initform nil
    :reader http-native-request)))

#+(and :excl :http-using-aserve)
(defun finalize-http-message (message)
  (let ((request (http-native-request message)))
    (when request
      (net.aserve.client:client-request-close request))))
  
#+(and :excl :http-using-aserve)
(defmethod initialize-instance :after ((self http-message) &rest args)
  (declare (ignore args))
  (schedule-finalization self #'finalize-http-message))

(defmethod print-object ((self http-message) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (prin1 (http-status self) stream)))

(defmethod get-header ((message http-message) (header string))
  (get-header (http-headers message) header))

(defmethod get-header ((headers list) (header string))
  #-:http-using-aserve
  (string-dict-get headers header)
  #+:http-using-aserve
  (cdr (assoc (intern (string-upcase header) :keyword) headers)))

(defmethod add-header ((headers list) (header string) value)
  (string-dict-add headers header value))

(defun infer-character-count (headers)
  (parse-integer (get-header headers "Content-Length") :junk-allowed t))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP CLIENT API
;;;

#+(and :realmcl (not :http-using-aserve))
(defmethod http-request ((url http-url) method
			 &key (proxy (find-http-proxy))
			      (accept "application/rdf+xml, application/xml, text/xml"))
  (let ((time (get-universal-time)))
    (with-open-stream (input (open-http-stream url proxy))
      (multiple-value-bind (status version headers)
	                   (http-get-headers input url
					     (ecase method (:get "GET") (:head "HEAD"))
					     accept)
	(make-instance 'http-message
	  :status status :version version :headers headers :request-time time :url url
	  :body (and (eq method :get)
		     (let ((chunkedp
			    (string= (get-header headers "Transfer-Encoding") "chunked"))
			   (stream (make-http-body-stream input)))
		       (if chunkedp
			 (http-stream-enable-input-chunking stream)
			 (setf (http-stream-character-count stream) 
			       (infer-character-count headers)))
		       stream)))))))

#+(and :openmcl (not :http-using-aserve))
(defmethod http-request ((url http-url) method
			 &key (proxy (find-http-proxy))
			      (accept
			       "application/rdf+xml, application/xml, text/xml, */*"))
  (let ((time (get-universal-time)))
    (multiple-value-bind (status version headers body)
                         (make-curl-http-request method url proxy accept)
      (when (eq method :head)
	(close (shiftf body nil)))
      (make-instance 'http-message
	:status status :version version :headers headers :request-time time :url url
	:body body))))

#+:http-using-aserve
(defmethod http-request ((url http-url) method
			 &key (proxy (find-http-proxy))
			      (accept "application/rdf+xml"))
  (let* ((time (get-universal-time))
	 (request
	  (net.aserve.client:make-http-client-request (url-string url)
						      :method method
						      :accept accept :proxy proxy)))
    (net.aserve.client:read-client-response-headers request)
    (make-instance 'http-message
      :status (net.aserve.client:client-request-response-code request)
      :version nil
      :headers (net.aserve.client:client-request-headers request)
      :request-time time
      :url url
      :body (net.aserve.client:client-request-socket request))))

#+(and :openmcl (not :http-using-aserve))
(defun make-curl-http-request (method url proxy accept)
  (declare (special *http-parse-buffers*))
  (let* ((input (simple-external-process "curl"
					 "-s"
					 (and proxy "-x") proxy
					 "--header" (format nil "Accept: ~A" accept)
					 (ecase method
					   (:get "-i")
					   (:head "--head"))
					 (strip-trailing-hash (url-string url)))))
    (multiple-value-bind (status version headers)
	                 (with-resource-from-pool (parse-buffer *http-parse-buffers*)
			   (read-headers-into-pb parse-buffer input)
			   (compute-response parse-buffer))
      (values status version headers input))))

(defconstant -new-line-string- (concatenate 'string (list #\Return #\Linefeed)))

(defun make-http-request (method url-path url-host accept)
  (format nil "~@:(~A~) ~A HTTP/1.1~A~
               Host: ~A~A~
               Accept: ~A~A~
               Connection: close~A~A"
	  method (strip-trailing-hash url-path) -new-line-string-
	  url-host -new-line-string-
	  accept -new-line-string- -new-line-string- -new-line-string-))

(defun http-get-headers (input url operation accept)
  (declare (special *http-parse-buffers*))
  (write-sequence (make-http-request operation (url-path url) (url-host url) accept)
		  input)
  (force-output input)
  (with-resource-from-pool (parse-buffer *http-parse-buffers*)
    (read-headers-into-pb parse-buffer input)
    (compute-response parse-buffer)))

(defun http-connection-reusable-p (headers)
  (not (string= "close" (get-header headers "Connection"))))

(defun strip-trailing-hash (string)
  (let ((i (1- (length string))))
    (if (char= (char string i) #\#)
      (subseq string 0 i)
      string)))

(defun add-trailing-hash (string)
  (if (ends-in-hash-p string)
    string
    (concatenate 'string string "#")))

(defparameter *http-max-redirects* 5) ; see RFC 2616

(defmethod http-request :around ((url http-url) method &rest args)
  (declare (dynamic-extent args))
  (let ((true-url nil))
    (dotimes (i *http-max-redirects*)
      (let* ((response (apply #'call-next-method url method args))
	     (status (http-status response)))
	(case status
	  ;; OK
	  (200 (return-from http-request (values response true-url)))
	  ;; Moved Permanently, Found, See Other
	  ((301 302 303)
	   (let ((location (get-header response "Location")))
	     (if (null location)
	       (error 'http-bad-redirect :thing "Location")
	       (let ((stream (http-body response)))
		 (when stream
		   (close stream))
		 (setf url (make-url (strip-trailing-hash location)))
		 (when (= status 303)
		   (setf true-url url))))))
	  ;; Bad Request
	  (400 (error 'http-bad-request))
	  ;; Not Found
	  (404 (error 'http-not-found :thing (url-string url)))
	  ;; Internal Server Error
	  (500 (error 'http-server-error))
	  ;; something else...
	  (t (error 'http-unsupported-status :thing (http-status response))))))
    (error 'http-too-many-redirects :thing url)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   FAST HTTP HEADER PARSING
;;;

(defparameter *http-parse-buffer-size* 4096)

(defparameter *http-expected-headers* 20)

(defstruct (http-response-parse-buffer (:conc-name pb-))
  (buf (make-string *http-parse-buffer-size*))
  (left-bounds (make-array *http-expected-headers*))
  (right-bounds (make-array *http-expected-headers*))
  seen-headers)

(defun clear-pb (pb)
  (setf (pb-seen-headers pb) 0))

(define-resource-pool *http-parse-buffers* 
  #'make-http-response-parse-buffer #'clear-pb)

(defun stretch-pb-buf (pb delta)
  (let* ((old (pb-buf pb))
         (new (make-string (+ (length old) delta))))
    (setf (substring new 0) old
          (pb-buf pb) new)))

(defun stretch-pb-headers (pb delta)
  (let* ((old-left (pb-left-bounds pb))
         (old-right (pb-right-bounds pb))
         (len (+ (length old-left) delta))
         (new-left (make-array len))
         (new-right (make-array len)))
    (setf (subseq new-left 0) old-left
          (subseq new-right 0) old-right
          (pb-left-bounds pb) new-left
          (pb-right-bounds pb) new-right)))

(defparameter *http-parsepuf-buffer-default-delta* 2048) 

(defparameter *http-parsebuf-headers-default-delta* 20)

(defun read-headers-into-pb (pb stream)
  (let ((len (length (pb-buf pb)))
        (max-lines (length (pb-left-bounds pb))))
    (flet ((push-char (char index)
             (when (>= index len)
               (stretch-pb-buf pb *http-parsepuf-buffer-default-delta*)
               (incf len *http-parsepuf-buffer-default-delta*))
             (setf (char (pb-buf pb) index) char))
           (push-left-bound (pos index)
             (when (>= index max-lines)
               (stretch-pb-headers pb *http-parsebuf-headers-default-delta*)
               (incf max-lines *http-parsebuf-headers-default-delta*))
             (setf (svref (pb-left-bounds pb) index) pos))
           (push-right-bound (pos index)
             (setf (svref (pb-right-bounds pb) index) pos)))
      (declare (inline push-char push-left-bound push-right-bound))
      (loop with state = :want-cr
            with bounds-index = 0
            initially (push-left-bound 0 0)
            for index from 0
            do (let ((ch (read-char stream nil nil)))
		 (cond ((null ch)
			(error 'http-bad-response :thing state :got :eof))
		       (t
			(push-char ch index)
			(ecase state
			  (:want-cr
			   (when (eql #\Return ch)
			     (setf state :need-lf)
			     (push-right-bound index bounds-index)
			     (incf bounds-index)))
			  (:need-lf
			   (unless (eql #\Linefeed ch)
			     (error 'http-bad-response :thing #\Linefeed :got ch))
			   (setf state :maybe-end))
			  (:maybe-end
			   (push-left-bound index bounds-index)
			   (setf state (if (eql #\Return ch) :end :want-cr)))
			  (:end
			   (unless (eql #\Linefeed ch)
			     (error 'http-bad-response :thing #\Linefeed :got ch))
			   (setf (pb-seen-headers pb) bounds-index)
			   (return))))))))))

(defun compute-response (pb) 
  (let ((buf (pb-buf pb)))
    (declare (type string buf))
    (flet ((parse-response-line (start end)
             (declare (ignore end))
             (let (version)
               (cond ((string= buf "HTTP/1.0" :start1 start :end1 (+ start 8))
                      (setf version :http/1.0))
                     ((string= buf "HTTP/1.1" :start1 start :end1 (+ start 8))
                      (setf version :http/1.1))
                     (t (error 'http-bad-response :thing "HTTP/1.0 or HTTP/1.1"
                               :got (substring buf start (+ start 8)))))
               (values (parse-integer buf :start 9 :end 12 :radix 10) version)))
           (parse-header-line (start end)
             (multiple-value-bind (header index)
		                  (collect-to-char #\: buf :start start :end end)
               (if (not index)
                 (error 'http-bad-response :thing #\:)
                 (let ((value-start (position #\Space buf
					      :start (1+ index) :end end
					      :test-not #'char=)))
                   (values header (substring buf value-start end)))))))
      (declare (inline parse-response-line parse-header-line))
      (let ((left (pb-left-bounds pb))
            (right (pb-right-bounds pb)))
        (multiple-value-bind (response-code protocol-version)
                             (parse-response-line (svref left 0) (svref right 0))
          (do ((index 1 (1+ index))
               (headers nil)
               (line-count (pb-seen-headers pb)))
              ((>= index line-count) (values response-code protocol-version headers))
            (multiple-value-bind (header value)
                                 (parse-header-line (svref left index)
						    (svref right index))
              (setf headers (add-header headers header value)))))))))

(defun substring (string start &optional end downcasep)
  (declare (type string string) (type fixnum start) (optimize (speed 3) (safety 0)))
  (let* ((end (or end (length string)))
         (rv (make-string (- end start))))
    (declare (type fixnum end))
    (do ((r-index start (1+ r-index))
         (w-index 0 (1+ w-index)))
        ((>= r-index end) rv)
      (declare (type fixnum r-index w-index))
      (setf (char rv w-index) 
            (let ((c (char string r-index)))
              (if downcasep
                (char-downcase c)
                c))))))

(defun (setf substring) (new-value string &optional (start 0) end downcasep)
  (declare (type string string new-value))
  (let* ((end (or end (length string)))
         (end (min (+ start (length new-value)) end)))
    (declare (type fixnum end))
    (do ((w-index start (1+ w-index))
         (r-index 0 (1+ r-index)))
        ((>= w-index end) new-value)
      (declare (type fixnum r-index w-index))
      (setf (char string w-index) 
            (let ((c (char new-value r-index)))
              (if downcasep
                (char-downcase c)
                c))))))

(defun collect-to-char (char string &key (start 0) end downcasep)
  (declare (type string string)
           (type fixnum start)
           (optimize (speed 3) (safety 0)))
  (let ((end-index (position char string :start start :end end :test #'char=)))
    (when end-index 
      (values (substring string start end-index downcasep) end-index))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP DATE PARSING
;;;
;;;   RFC 2616 says the following:
;;;
;;;       HTTP-date    = rfc1123-date | rfc850-date | asctime-date
;;;       rfc1123-date = wkday "," SP date1 SP time SP "GMT"
;;;       rfc850-date  = weekday "," SP date2 SP time SP "GMT"
;;;       asctime-date = wkday SP date3 SP time SP 4DIGIT
;;;       date1        = 2DIGIT SP month SP 4DIGIT
;;;                      ; day month year (e.g., 02 Jun 1982)
;;;       date2        = 2DIGIT "-" month "-" 2DIGIT
;;;                      ; day-month-year (e.g., 02-Jun-82)
;;;       date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
;;;                      ; month day (e.g., Jun  2)
;;;       time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
;;;                      ; 00:00:00 - 23:59:59
;;;       wkday        = "Mon" | "Tue" | "Wed"
;;;                    | "Thu" | "Fri" | "Sat" | "Sun"
;;;       weekday      = "Monday" | "Tuesday" | "Wednesday"
;;;                    | "Thursday" | "Friday" | "Saturday" | "Sunday"
;;;       month        = "Jan" | "Feb" | "Mar" | "Apr"
;;;                    | "May" | "Jun" | "Jul" | "Aug"
;;;                    | "Sep" | "Oct" | "Nov" | "Dec
;;;

(defun parse-http-date (string)
  (labels ((parse-month (s i)
             (do ((j 1 (1+ j))
                  (m '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                     (rest m)))
                 ((or (> j 12) (string= (first m) s :start2 i :end2 (+ i 3)))
                  (and (< j 13) j))))
           (parse-int (s start end)
             (parse-integer s :start start :end end :junk-allowed t))
           (encode-date (year month date hour minute second)
             (when (and year month date hour minute second)
               (encode-universal-time second minute hour date month year 0)))
           (parse-time (s i)
             (values (parse-int s i       (+ i 2))
                     (parse-int s (+ i 3) (+ i 5))
                     (parse-int s (+ i 6) (+ i 8)))))
    (cond
     ;; -- RFC 1123
     ((char= (char string 3) #\,)
      (let ((date            (parse-int   string 5 7))
            (month           (parse-month string 8))
            (year            (parse-int   string 12 16)))
        (multiple-value-bind (hour minute second)
                             (parse-time  string 17)
          (encode-date year month date hour minute second))))
     ;; -- ASCTIME
     ((char= (char string 3) #\Space)
      (let ((month           (parse-month string 4))
            (date            (parse-int   string 8 10)))
        (multiple-value-bind (hour minute second)
                             (parse-time  string 11)
          (multiple-value-bind (a b c d e year)
                               (decode-universal-time (get-universal-time))
            (declare (ignore a b c d e))
            (encode-date year month date hour minute second)))))
     ;; -- RFC 850, with the assumption of 21st century
     (t
      (let ((p (position #\, string :test #'char=)))
        (when p
          (let ((date            (parse-int   string (+ p 2) (+ p 4)))
                (month           (parse-month string (+ p 5)))
                (year            (parse-int   string (+ p 9) (+ p 11))))
            (multiple-value-bind (hour minute second)
                                 (parse-time  string (+ p 12))
              (encode-date (and year (+ year 2000))
                           month date hour minute second)))))))))

(defun parse-iso8601-date (string)
  (labels ((fail-char-p (s c p)
             (not (and (> (length s) p)
                       (char= (char s p) c))))
           (time-zone (s p)
             (ecase (char s p)
               (#\Z 0)
               (#\- (parse-integer string :start (1+ p) :end (+ p 3)))
               (#\+ (- (parse-integer string :start (1+ p) :end (+ p 3)))))))
    (let ((year (parse-integer string :start 0 :end 4)))
      (if (fail-char-p string #\- 4)
        (values (encode-universal-time 0 0 0 1 1 year) t)
        (let ((month (parse-integer string :start 5 :end 7)))
          (if (fail-char-p string #\- 7)
            (values (encode-universal-time 0 0 0 1 month year) t)
            (let ((day (parse-integer string :start 8 :end 10)))
              (if (fail-char-p string #\T 10)
                (values (encode-universal-time 0 0 0 day month year) t)
                (let ((hour (parse-integer string :start 11 :end 13))
                      (min (parse-integer string :start 14 :end 16)))
                  (if (fail-char-p string #\: 16)
                    (values (encode-universal-time 0 min hour day month year
						   (time-zone string 16))
			    nil)
                    (let ((sec (parse-integer string :start 17 :end 19)))
                      (values (encode-universal-time sec min hour day month year
						     (time-zone string 19))
			      nil))))))))))))

(defun parse-exif-date (string)
  (when (and (>= (length string) 10)
	     (char= (char string 4) #\:)
	     (char= (char string 7) #\:))
    (ignore-errors
      (let ((day (parse-integer string :start 8 :end 10))
	    (month (parse-integer string :start 5 :end 7))
	    (year (parse-integer string :start 0 :end 4))
	    (omit-time-p t)
	    (hour 0)
	    (min 0)
	    (sec 0))
	(when (and (>= (length string) 16)
		   (char= (char string 13) #\:))
	  (setf hour (parse-integer string :start 11 :end 13)
		min (parse-integer string :start 14 :end 16)
		omit-time-p nil)
	  (when (and (= (length string) 19)
		     (char= (char string 16) #\:))
	    (setf sec (parse-integer string :start 17 :end 19))))
	(values (encode-universal-time sec min hour day month year) omit-time-p)))))
  
(defun iso8601-date-string (universal-time &optional omit-time-p)
  (multiple-value-bind (sec min hour day month year weekday dst-p time-zone)
                       (decode-universal-time universal-time)
    (declare (ignore weekday))
    (when (and dst-p (not (zerop time-zone)))
      (decf time-zone)) ; thanks to Richard Newman for thinking of this
    (if omit-time-p
      (format nil "~4,'0D-~2,'0D-~2,'0D"
              year month day)
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~A~@[~2,'0D:00~]"
              year month day hour min sec
              (cond ((zerop time-zone) "Z")
                    ((> time-zone 0)   "-")
                    (t                 "+"))
              (and (not (zerop time-zone)) (abs time-zone))))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   PROXIES
;;;

;;#-(and :openmcl :darwin :uffi)
(defvar *http-proxy* nil)

;;#-(and :openmcl :darwin :uffi)
(defun find-http-proxy ()
  (or (get-env "HTTP-PROXY") *http-proxy*))

;;#+(and :openmcl :darwin :uffi)
#+:junk
(eval-when (:compile-toplevel :load-toplevel :execute)
  (uffi:load-foreign-library
   (translate-logical-pathname "wilbur:libs;FindProxies;build;FindProxies.dylib")))

#+(and :openmcl :darwin :uffi)
(uffi:def-function ("FindHTTPProxy" %find-http-proxy)
		   ((host (* :char))
		    (host-size :unsigned-int))
  :returning :unsigned-byte)

;;#+(and :openmcl :darwin :uffi)
#+:junk
(defun find-http-proxy ()
  (let ((buffer (uffi:allocate-foreign-string 256)))
    (unwind-protect (progn
		      (%find-http-proxy buffer 256)
		      (let ((proxy-string (unless (%null-ptr-p buffer)
					    ;; UFFI:CONVERT-FROM-FOREIGN-STRING broken
					    (%get-cstring buffer))))
			(unless (zerop (length proxy-string))
			  proxy-string)))
      (uffi:free-foreign-object buffer))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS SIMPLE-UNTYI-MIXIN
;;;

(defclass simple-untyi-mixin ()
  ((last-char
    :initform nil
    :accessor stream-last-char)))

(defmethod stream-tyi :around ((stream simple-untyi-mixin))
  (or (shiftf (stream-last-char stream) nil)
      (call-next-method)))

(defmethod stream-untyi ((stream simple-untyi-mixin) char)
  (if (stream-last-char stream)
    (error "Two UNTYIs in a row on ~S" stream)
    (setf (stream-last-char stream) char)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS HTTP-NETWORK-STREAM (FOR MCL)
;;;

#+(and :realmcl (not :http-using-aserve))
(defclass http-network-stream (ccl::opentransport-tcp-stream)
  ((via-proxy-p
    :initform nil
    :initarg :via-proxy-p
    :reader stream-via-proxy-p)
   (url-path
    :initarg :url-path
    :initform nil
    :accessor stream-url-path))
  (:default-initargs
    :reuse-local-port-p t
    :writebufsize ccl::*ot-conn-outbuf-size*))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS HTTP-BODY-STREAM (FOR MCL)
;;;

#+(and :realmcl (not :http-using-aserve))
(defclass http-body-stream (simple-untyi-mixin input-stream)
  ((chunkedp
    :initform nil
    :initarg :chunkedp
    :accessor http-stream-chunked-p)
   (count
    :initform 0
    :initarg :character-count
    :accessor http-stream-character-count)
   (eofp
    :initform nil
    :accessor stream-eofp)
   (network
    :initarg :network-stream
    ;; XXX: Why did I change this? It's totally gratutious.
    ;; Make a decision before the final release.
    :reader http-stream-network-stream)))

;;; Now we eagerly read the chunk length. Note that there is still a serious problem with 
;;; this stream, in that it doesn't know if it's really eofp and our buffering seems to
;;; get in the way of the correct OT errors being raised.  I thought that reimplementing
;;; would fix that problem, but further reflection showed it just catches one case.
;;;
;;; Ultimately, it looks like the only solution is to implement this directly over OT, 
;;; which is what CL-HTTP does.  Or just tell people that we don't support read-sequence.

#+(and :realmcl (not :http-using-aserve))
(defmethod stream-tyi ((stream http-body-stream))
  (with-slots (network count chunkedp eofp) stream
    (cond (chunkedp 
           (flet ((read-chunk-length ()
                    (let* ((line (loop for char = (stream-tyi network)
                                       while (not (char= char #\Linefeed))
                                       collecting char))
                           (new-count (parse-integer (concatenate 'string line)
                                                     :radix 16 :junk-allowed t)))
                      (setf count new-count)
                      (when (zerop new-count) (setf eofp t)))))
             (declare (dynamic-extent read-chunk-length))
             (when (null count) (read-chunk-length))
             (prog1 (stream-tyi network)
               (when (zerop (decf count))
                 (stream-tyi network)
                 (stream-tyi network)
                 (read-chunk-length)))))
          ((null count)
	   (stream-tyi network))
          (t
           (prog1 (stream-tyi network)
             (when (zerop (decf count))
               (setf eofp t)))))))
  
#+(and :realmcl (not :http-using-aserve))
(defmethod stream-close :after ((stream http-body-stream))
  (stream-close (http-stream-network-stream stream)))
  
#+(and :realmcl (not :http-using-aserve))
(defmethod stream-abort :after ((stream http-body-stream))
  (stream-abort (http-stream-network-stream stream)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HTTP STREAM METHODS (FOR MCL)
;;;

#+(and :realmcl (not :http-using-aserve))
(defmethod open-http-stream ((url http-url) (proxy http-url))
  (make-instance 'http-network-stream
    :host (url-host proxy) :port (url-port proxy) :via-proxy-p t))

#+(and :realmcl (not :http-using-aserve))
(defmethod open-http-stream ((url http-url) (proxy null))
  (make-instance 'http-network-stream
    :host (url-host url) :port (url-port url)))

#+(and :realmcl (not :http-using-aserve))
(defmethod make-http-body-stream ((stream http-network-stream))
  (make-instance 'http-body-stream :network-stream stream))

#+(and :realmcl (not :http-using-aserve))
(defmethod http-stream-enable-input-chunking ((stream http-body-stream))
  (setf (http-stream-chunked-p stream) t)
  (setf (http-stream-character-count stream) nil))

#+(and :realmcl (not :http-using-aserve))
(defmethod http-stream-disable-input-chunking ((stream http-body-stream))
  (setf (http-stream-chunked-p stream) nil))
