(defpackage #:wiktionary
  (:use :cl :anaphora :alexandria :iterate :eos))

;;; Much of this is generic to any mediawiki wiki.


(in-package :wiktionary)

(defmacro doto (arg1 &rest args)
  "Written in 10 minutes while talking to scott.

Gist of this is replace the second item in a list with arg1 before
evaluating the whole thing inside of a progn."
  (once-only (arg1)
    `(progn ,@(mapcar (lambda (it) (apply #'list (car it)
                                        arg1 (cdr it)))
                    args)
            ,arg1)))


(defvar *dictionary* (make-hash-table :test #'equal)
  "Dictionary of words!")
;;=> NIL


(deftype english-parts-of-speech ()
  '(member :verb :noun :pronoun :adjective :adverb
    :preposition :conjunction :interjection))


#+ () (defclass word ()
  ((name :accessor word-name :initarg :name)
   (type :accessor word-type :initarg :type)
   (ipa :accessor word-ipa :initarg :IPA)
   (rhymes :accessor word-rhymes :initarg :rhymes)
   (homophone :accessor word-homophone :initarg :homophone)
   (hyphenation :accessor hyphenation :initarg :hyphenatione)
   (anagrams :accessor word-anagrams :initarg :anagrams)))

(defstruct word
  (name nil :type string)                ;it will error if no name is given
  (pos nil :type list))

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

(defun run-enwiktionary-filter (source &optional (count 1))
  (let ((namespaces (namespace-names source)))
    (iter (for x from 1 to count)
          (for title = (parse-mediawiki-page-title source))
          (when (mainspacep title namespaces)
            (let* ((text (parse-mediawiki-page-text source))
                   (sections (parse-mediawiki-sections text))
                   (interesting (list-interesting-text sections)))
              (if (zerop (length interesting))
                  (collect title)
                  (setf (gethash title *dictionary*)
                        (make-word
                         :name title
                         :pos (mapcar #'POS-template-to-type
                                      (list-wiktionary-templates-{{en interesting))))))))))


(defun list-wiktionary-templates-{{en (text)
  (ppcre:all-matches-as-strings "{{en-[^}]+}}|{{(infl|abbreviation|acronyms)[^}]+}}" text))

(defun list-wiktionary-templates-IPA (text)
  (mapcar (lambda (x)
            (subseq x 6 (- (length x) 2)))
          (ppcre:all-matches-as-strings "{{IPA\\\|[^}]+}}" text)))

(defun POS-template-to-type (template-string)
  (aif (position #\| template-string)
       (cons (template-name->keyword (subseq template-string 2 it))
             (subseq template-string (1+ it) (- (length template-string) 2)))
       (cons (template-name->keyword (subseq template-string 2 (- (length template-string) 2)))
             nil)))



(defparameter *doto-list* (list 1))


(defparameter +template-name->keyword-mapping+
  (alist-hash-table `(("en-adj" . :adjective)
                      ("en-adj-more" . :adjective-comparative)
                      ("en-adj-most" . :adjective-superlative)
                      ("en-adv" . :adverb)
                      ("en-noun" . :noun)
                      ("en-plural-noun" . :noun-plural)
                      ("en-verb" . :verb)
                      ("en-adverb" . :adverb)
                      ("en-adjective" . :adjective)
                      ("en-proper-noun" . :proper-noun)
                      ("en-proper noun" . :proper-noun)
                      ("en-infl" . :infliction)
                      ("en-conj" . :conjugate)
                      ("en-cont" . :contraction)
                      ("en-det" . :determiner)
                      ("en-intj" . :interjection)
                      ("en-interjection" . :interjection)
                      ("en-prep" . :preposition)
                      ("en-preposition" . :preposition)
                      ("en-phrase" . :phrase)
                      ("infl" . :infliction)
                      ("inflection of" . :infliction)
                      ("inflection of " . :infliction)
                      ("inflected form of" . :infliction)
                      ("en-pron" . :pronoun)
                      ("en-pronoun" . :pronoun)
                      ("en-term" . :term-template)
                      ("initialism" . :initialism)
                      ("abbreviation" :abbreviation)
                      ("abbreviation of" . :abbreviation)
                      ("acronyms" . :acronymn))
                    :test #'equalp))


(defun template-name->keyword (name)
  (or (gethash name +TEMPLATE-NAME->KEYWORD-MAPPING+ nil)
      (print name *trace-output*)))


(defun mainspacep (title-string namespaces)
  (every (lambda (x)
           (not (search x title-string
                    :start1 0
                    :end1 (length x))))
         namespaces))

(defun parse-mediawiki-namespaces (source)
  "Parse the site namespaces from SOURCE into an alist.

On mediawiki dumps the list of namespaces is one of the first things in
the file, so be sure to invoke this before trying to invoke anything else
if namespace lookup is to work."
  (klacks:find-element source "namespaces")
  ;; Once the
  (iter (for start-element = (klacks:find-element source "namespace"))
        (collect (parse-mediawiki-namespace-element source))
        (klacks:find-event source :end-element)
        (klacks:expect source :end-element)
        (klacks:peek-next source)       ;skip the content...
        (until (eql :end-element (klacks:peek-next source)))))

(defun parse-mediawiki-namespace-element (source)
  "Go to the next <namespace> attribute in SOURCE"
  (klacks:expect source :start-element nil "namespace")
  (cons (parse-integer (klacks:get-attribute source "key"))
        (nth-value 1 (klacks:peek-next source))))

(defun parse-mediawiki-page (source)
  (list (parse-mediawiki-page-title source)
        (parse-mediawiki-page-text source)))

(defun parse-mediawiki-page-title (source)
  "Grab the next page title in SOURCE."
  (klacks:find-element source "title")
  (nth-value 1 (klacks:peek-next source)))

(defun parse-mediawiki-page-text (source)
  (klacks:find-element source "text")

  (if (eql :characters (klacks:peek-next source))
      (with-output-to-string (*standard-output*)
        (iter
          (princ (klacks:current-characters source))
          (while (eql :characters (klacks:peek-next source)))))
      ""))


(defun parse-mediawiki-sections (text &optional (level 1))
  (iter (for section-text in (cl-ppcre:split "(==+[^=]+)==+\\\n" text :WITH-REGISTERS-P t))
        (for title previous section-text)
        (for n from 1)
        (when (oddp n)
          (collect (list (cons (and title (subseq title (position #\= title :test-not #'eql)))
                               (or (position #\= title :test-not #'eql) 0))
                         section-text)))))

(defun list-interesting-text (sections)
  (let ((english-level nil))
    (apply #'concatenate 'string
           (nreverse
            (reduce (lambda (last cur)
                      (destructuring-bind ((title . level) text) cur
                        (let ((englishp (member title (list "English" "Translingual") :test #'equalp)))
                          (when englishp
                            (setq english-level level))
                          (if english-level
                              (if (or (< english-level level) englishp)
                                  (progn
                                    (push "==========" text)
                                    (push title text)
                                    (push "==========" text)
                                    (push text last))
                                  (progn (setq english-level nil)
                                         last))
                              last))))
                    sections
                    :initial-value nil)))))