(defpackage #:wiktionary
  (:use :cl :anaphora :alexandria :iterate :eos)
  (:export #:lookup-pos))

;;; Much of this is generic to any mediawiki wiki.


(in-package :wiktionary)
(def-suite root)
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

(defun load-wiktionary-database (full-file-path)
  "Load the wiktionary dump if it has already been parsed/saved.

This may not be safe in sbcl."
  (bt:make-thread (lambda ()
                    (with-open-file (s full-file-path)
                      (setq *dictionary* (read s)))
                    (print "DONE LOADING WIKTIONARY DB"))
                  :name "wiktionary dict read"))

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

(defvar *current-title* "")
(defun run-enwiktionary-filter (source &optional (count 1))
  (let ((namespaces (namespace-names source)))
    (iter (for x from 1 to count)
          (for title = (parse-mediawiki-page-title source))
          (for *current-title* = title)
          (when (mainspacep title namespaces)
            (let* ((text (parse-mediawiki-page-text source))
                   (sections (parse-mediawiki-sections text))
                   (interesting (list-interesting-text sections)))
              (if (zerop (length interesting))
                  (collect title)
                  (setf (gethash title *dictionary*)
                        (make-word
                         :name title
                         :pos (remove nil
                                      (mapcar #'POS-string->type
                                              (list-wiktionary-templates-{{en interesting)))))))))))

(defun POS-string->type (POS-string)
  "Convert whatever POS things we have to symbols."
  (or (POS-template-to-type POS-string)
      (unless (search "en-" POS-string :end1 3)
        (POS-title-to-type POS-string))))

(defun list-wiktionary-templates-{{en (text)
  (ppcre:all-matches-as-strings "{{en-[^}]+}}|{{(infl|abbreviation|acronyms)[^}]+}}|==========[^=]+==========" text))

(defun list-wiktionary-templates-IPA (text)
  (mapcar (lambda (x)
            (subseq x 6 (- (length x) 2)))
          (ppcre:all-matches-as-strings "{{IPA\\\|[^}]+}}" text)))

(defparameter +title-signature+ "=========="
  "For now adding 10 equal signs to mark titles during a portion of the
  parsing stage.")

(defun strip-title-marker (title-string)
  "These have for now ten equal signs. See `+title-signature+'."
  (let ((start (position #\= title-string :test (complement #'eql)))
        (end (1+ (position #\= title-string :from-end t :test (complement #'eql)))))
    (assert (and (= start 10) (= (- (length title-string) 10) end)))
    (subseq title-string start end)))

(test (strip-title-marker :suite root)
  "This is a pretty specific hack for nisp. What we want to do is remove
`+title-signature+' from both sides of the input."
  (is (string= "a" (strip-title-marker "==========a==========")))
  (signals error (strip-title-marker "a")))

(defun POS-title-to-type (title-string)
  (gethash (strip-title-marker title-string)
           +title-name->keyword-mapping+ nil))

(test (POS-title-to-type :suite root
                         :depends-on strip-title-marker)
  (is (eql :noun (POS-title-to-type "==========Noun==========")))
  (is (eql :noun (POS-title-to-type "==========noun=========="))))

(defun POS-template-to-type (template-string)
  (aif (position #\| template-string)
       (cons (template-name->keyword (subseq template-string 2 it))
             (subseq template-string (1+ it) (- (length template-string) 2)))
       (aand (template-name->keyword (subseq template-string 2 (- (length template-string) 2)))
             (cons it nil))))



(defparameter *doto-list* (list 1))

(defparameter +title-name->keyword-mapping+
  ;; Specified at:
  ;; http://en.wiktionary.org/wiki/Wiktionary:Entry_layout_explained/POS_headers#Standard_POS_headers
  (alist-hash-table `(("Noun" . :noun)
                      ("Symbol" . :symbol)
                      ("Verb" . :verb)
                      ("Adjective" . :adjective)
                      ("Adverb" . :adverb)
                      ("Pronoun" . :pronoun)
                      ("Conjunction" . :conjunction)
                      ("Interjection" . :interjection)
                      ("Preposition" . :preposition)
                      ("Proper noun" . :proper-noun)
                      ("Article" . :article)
                      ("Prepositional phrase" . :phrase))
                    :test #'equalp))

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
                      ("acronyms" . :acronymn)
                      ("en-usage-h-an" . :en-usage-h-an)
                      ("en-part" . :en-part)
                      ("en-plural noun" . :noun-plural)
                      ("en-usage-foreignism" . :en-usage-foreignism)
                      ("en-noun-reg-es" . :en-noun-reg-es)
                      ("en-noun-irreg" . :en-noun-irreg)
                      ("en-noun-unc" . :en-noun-unc)
                      ("en-verb2" . :verb)
                      ("en-noun2" . :noun)
                      ("en-infl-noun" . :en-infl-noun)
                      ("en-usage-verb-particle-solid" :en-usage-verb-particle-solid))
                    :test #'equalp))


(defun template-name->keyword (name)
  (when (member name (list "en-noun2" "en-verb2") :test #'equalp)
    (print (list *current-title* name) *trace-output*))
  (gethash name +TEMPLATE-NAME->KEYWORD-MAPPING+ nil))


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
                                    (push +title-signature+ last)
                                    (push title last)
                                    (push +title-signature+ last)
                                    (push text last))
                                  (progn (setq english-level nil)
                                         last))
                              last))))
                    sections
                    :initial-value nil)))))

(defun string-upcase-first-letter (word)
  "Upcase the first letter of WORD.

We don't want to use `string-capitalize' here because we do not want to
change the case of the remaining letters."
  (declare (type string word))
  (string-upcase word :start 0 :end 1))

(defun string-downcase-first-letter (word)
  (declare (type string word))
  (string-downcase word :start 0 :end 1))

(defun ensure-word-POS-keyword (list-or-keyword)
  "Return a part of speech symbol from LIST-OR-KEYWORD

This assumes LIST-OR-KEYWORD actually has the right keyword as the input
or in the car of the given list."
  (if (consp list-or-keyword)
      (car list-or-keyword)
      list-or-keyword))

(defun lookup-pos (word)
  "Look up WORD's the parts of speech.

When WORD is in the dictionary the second value will be t, otherwise we
return nil for the second value."
  (let ((pos-list (gethash word *dictionary* :unknown)))
    (if (eql pos-list :unknown)
        (if (string= word (string-downcase-first-letter word))
            (values nil nil)
            (lookup-pos (string-downcase-first-letter word)))
        (values (remove-duplicates (mapcar #'ensure-word-POS-keyword (word-pos pos-list))) t))))

(defun unknownp (arg)
  "True if ARG is an unknown word token."
  (let ((arg (if (stringp arg) (gethash arg *dictionary*) arg)))
    (or (not (word-p arg))
        (member :unknown (word-pos arg) :key #'cdr))))

(test (english-determiner :suite root)
  "Verify against ground truth that we find and tag properly all determiners."
  (flet ((generate-determiners ()
           (mapcar (lambda (x)
                      (is (member :determiner (lookup-pos x))
                          "Grammar missing english determiner for word '~A'." x))
                    (acumen.english-parser.determiner:list-english-determiners))))
    (generate-determiners)))

(test (string-upcase-first-letter :suite root)
  "The first letter should always get upcased, but the case of what
follows must stay the same."
  (is (string= "Hi" (string-upcase-first-letter "hi")))
  (is (string= "HI" (string-upcase-first-letter "hI"))))

;;; END
