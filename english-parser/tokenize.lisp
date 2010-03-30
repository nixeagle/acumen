(defpackage #:nisp.tokenize
  (:use :cl :alexandria :anaphora :eos :iterate)
  (:nicknames :acumen.english-parser.tokenize
              :ep.tokenize)
  (:export #:tokenize-string))

(in-package :nisp.tokenize)

(defun tokenize-stream (stream)
  "Tokenize stream into a set of words."
  (iter (while (peek-char nil stream nil nil))
        (collect (parse-word stream))))

(defun parse-word (stream)
  (aif (member (peek-char nil stream nil nil) (list #\, #\; #\: #\. #\? #\!))
       (prog1 (string (read-char stream))
         (iter (while (eql (peek-char nil stream nil nil) #\Space))
               (read-char stream nil nil)))
       (iter (for char = (read-char stream nil nil))
             (when (member char (list #\, #\; #\: #\. #\? #\!))
               (unread-char char stream))
             (print char *trace-output*)
             (until (or (not char) (member char (list #\space #\, #\; #\: #\. #\? #\!))))
             (collect char :result-type 'string))))

(defun tokenize-string (string)
  (with-input-from-string (s string)
    (mapcar (lambda (word)
              (multiple-value-bind (pos-list found?)
                  (wiktionary:lookup-pos word)
                (cons word (if found? pos-list :unknown))))
            (tokenize-stream s))))

(defmethod raw-tokenize ((stream stream))
  (tokenize-stream stream))

(defmethod raw-tokenize ((string string))
  (with-input-from-string (s string)
    (raw-tokenize s)))

;;; END
