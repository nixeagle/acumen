(defpackage #:mediawiki-dump-parser
  (:use :cl :iterate :eos)
  (:export #:namespace-names))

(in-package :mediawiki-dump-parser)

(defun parse-mediawiki-namespaces (source)
  "Parse the site namespaces from SOURCE into an alist.

On mediawiki dumps the list of namespaces is one of the first things in
the file, so be sure to invoke this before trying to invoke anything else
if namespace lookup is to work."
  (klacks:find-element source "namespaces")
  (iter (for start-element = (klacks:find-element source "namespace"))
        (collect (parse-mediawiki-namespace-element source))
        (klacks:find-event source :end-element)
        (klacks:peek-next source)       ;skip the content...
        (until (eql :end-element (klacks:peek-next source)))))

(defun parse-mediawiki-namespace-element (source)
  "Go to the next <namespace> attribute in SOURCE"
  (klacks:expect source :start-element nil "namespace")
  (cons (parse-integer (klacks:get-attribute source "key"))
        (nth-value 1 (klacks:peek-next source))))

(defgeneric namespace-names (source)
  (:documentation "Return a list of names without the index numbers."))

(defmethod namespace-names ((source cxml::cxml-source))
  "Destructively parse out the namespaces from SOURCE.

SOURCE will no longer be able to access the head of the document."
  (mapcar (lambda (x)
            (concatenate 'string (cdr x) ":"))
          (parse-mediawiki-namespaces source)))

(defmethod namespace-names ((source pathname))
  "Create a new cxml source from SOURCE and use it."
  (namespace-names (cxml:make-source source)))

(defun mainspacep (title-string namespaces)
  "True if title-string is not in any of the given namespaces."
  (every (lambda (x)
           (not (search x title-string
                    :start1 0
                    :end1 (length x))))
         namespaces))