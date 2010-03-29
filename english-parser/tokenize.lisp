(defpackage #:nisp.tokenize
  (:use :cl :alexandria :anaphora :eos :iterate)
  (:export #:tokenize-string))

(in-package :nisp.tokenize)

(defun tokenize-stream (stream)
  "Tokenize stream into a set of words."
  (iter (while (peek-char nil stream nil nil))
        (collect (parse-word stream))))

(defun parse-word (stream)
  (iter (for char = (read-char stream nil nil))
        (until (or (not char) (eql char #\space)))
        (collect char :result-type 'string)))

(defun tokenize-string (string)
  (with-input-from-string (s string)
    (mapcar (lambda (word)
              (multiple-value-bind (pos-list found?)
                  (wiktionary:lookup-pos word)
                (cons word (if found? pos-list :unknown))))
            (tokenize-stream s))))

(def-suite root
    :description "Main suite for tokenizing english.")


(test (parse-word :suite root)
  (flet ((pw (string)
           (with-input-from-string (s string)
             (parse-word s))))
    (is (string= "Hi" (pw "Hi")))
    (is (string= "Hi" (pw "Hi ")))
    (is (string= "Hi" (pw "Hi how are you?")))
    ;; Needs to ignore trailing ?
    (is (string= "Hi" (pw "Hi?")))))
