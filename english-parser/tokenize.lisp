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
  (tag-words
   (with-input-from-string (s string)
     (mapcar (lambda (word)
               (multiple-value-bind (pos-list found?)
                   (wiktionary:lookup-pos word)
                 (cons word (if found? pos-list :unknown))))
             (tokenize-stream s)))))

(defmethod raw-tokenize ((stream stream))
  (tokenize-stream stream))

(defmethod raw-tokenize ((string string))
  (with-input-from-string (s string)
    (raw-tokenize s)))

(defparameter +pos-lookup-table+
  (alist-hash-table `(((:pronoun :verb) . t))
                    :test #'equal)
  "Hackish way for now to try to reduce some of the options in a word chain.")

(defun set-pos-tag-set (tags)
  (setf (gethash tags +pos-lookup-table+) t))

(defmacro pos-tags (&body tags)
  `(mapcar #'set-pos-tag-set ',tags))

(pos-tags
  (:pronoun :verb)
  (:pronoun :noun)
  (:noun :verb)
  (:article :noun)
  (:preposition :article)
  (:particle :verb))

;;; :noun "to" :noun => ratio
;;; "to" :verb => infinitive

(defun pos-tag-set-p (&rest tags)
  "True if tags are a valid grouping of pos tags."
  (gethash tags +pos-lookup-table+ nil))

(defun tag-words (tokenized-words)
  (iter (for (word . pos) in tokenized-words)
        (for last-pos previous pos)
        (for last-word previous word)
        (when last-pos
          (let ((options (pos-options last-pos pos)))
            (collect (cons last-word (if (car options)
                          (car options)
                          last-pos)) :into out)))
        (finally (return (cons out (list (cons word pos)))))))

(defun split-pos-sets (sets)
  (when sets
   (list (remove-duplicates (mapcar #'car sets))
         (remove-duplicates (mapcar #'second sets)))))

(defun pos-options (pos-set-1 pos-set-2)
  (iter (for x in pos-set-1)
        (appending (iter (for y in pos-set-2)
                         (when (pos-tag-set-p x y)
                           (collect (list x y)))) :into out)
        (finally (return (split-pos-sets out))))
  )



;;; END
