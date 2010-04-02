(defpackage #:wiktionary
  (:use :cl :anaphora :alexandria :iterate :eos)
  (:export #:lookup-pos))

;;; Much of this is generic to any mediawiki wiki.


(in-package :wiktionary)
(in-suite* root)
(defmacro doto (arg1 &rest args)
  "Written in 10 minutes while talking to scott.

Gist of this is replace the second item in a list with arg1 before
evaluating the whole thing inside of a progn."
  (once-only (arg1)
    `(progn ,@(mapcar (lambda (it) (apply #'list (car it)
                                        arg1 (cdr it)))
                    args)
            ,arg1)))

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

(defparameter +interesting-language-headers+ (list "English" "Translingual")
  "These are interesting headers that we care about. Change these to
  something else if we want to load a non english lexicon.")

(defparameter +always-interesting-headers+ (list "Proper Noun")
  "These headers are always interesting in the sense that no matter what
  language we are parsing we need to always include these headers.")

(defvar *dictionary* (make-hash-table :test #'equal)
  "Dictionary of words!")

(defun load-wiktionary-database (full-file-path)
  "Load the wiktionary dump if it has already been parsed/saved.

This may not be safe in sbcl."
  (bt:make-thread (lambda ()
                    (with-open-file (s full-file-path)
                      (let ((*print-pretty* nil)
                            (*print-circle* nil)
                            (*print-readably* t)
                            (*package* (find-package :wiktionary)))
                        (setq *dictionary* (read s))))
                    (print "DONE LOADING WIKTIONARY DB"))
                  :name "wiktionary dict read"))

(defun save-wiktionary-database (full-file-path)
  (bt:make-thread (lambda ()
                    (with-open-file (s full-file-path
                                       :direction :output
                                       :if-exists :supersede)
                      (let ((*print-pretty* nil)
                            (*print-circle* nil)
                            (*print-readably* t)
                            (*package* (find-package :wiktionary)))
                        (print *dictionary* s))))))

(deftype english-parts-of-speech ()
  '(member :verb :noun :pronoun :adjective :adverb
    :preposition :conjunction :interjection))

(defstruct word
  (pos nil :type list))

(defun run-enwiktionary-filter (source &optional (count 1))
  (let ((namespaces (mediawiki-dump-parser::namespace-names source)))
    (iter (for x from 1 to count)
          (for title = (parse-mediawiki-page-title source))
          (when (mediawiki-dump-parser::mainspacep title namespaces)
            (let* ((text (parse-mediawiki-page-text source))
                   (sections (parse-mediawiki-sections text)))
              (multiple-value-bind (interesting-text
                                    interesting-titles)
                  (list-interesting-text sections)
                (if (zerop (length interesting-text))
                    (collect title)
                    (setf (gethash title *dictionary*)
                          (make-word
                           :pos (remove-duplicates (remove nil
                                                           (append (mapcar #'POS-string->type
                                                                    (list-wiktionary-templates-{{en interesting-text))
                                                                   (mapcar #'POS-string->type
                                                                           interesting-titles)) :test #'equal)))))))))))

(defun POS-string->type (POS-string)
  "Convert whatever POS things we have to symbols."
  (or (POS-template-to-type POS-string)
      (unless (search "en-" POS-string :end1 3)
        (POS-title-to-type POS-string))))

(defun list-wiktionary-templates-{{en (text)
  (ppcre:all-matches-as-strings "{{en-[^}]+}}|{{(infl|abbreviation|acronyms)[^}]+}}" text))

(defun POS-title-to-type (title-string)
  (aand (gethash title-string
                 +title-name->keyword-mapping+ nil)
        (cons it nil)))

(defun POS-template-to-type (template-string)
  (aif (position #\| template-string)
       (cons (template-name->keyword (subseq template-string 2 it))
             (subseq template-string (1+ it) (- (length template-string) 2)))
       (aand (template-name->keyword (subseq template-string 2 (- (length template-string) 2)))
             (cons it nil))))


(defun template-name->keyword (name)
  (gethash name +TEMPLATE-NAME->KEYWORD-MAPPING+ nil))

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


(defun parse-mediawiki-sections (text)
  (iter (for section-text in (cl-ppcre:split "(==+[^=]+)==+\\\n" text :WITH-REGISTERS-P t))
        (for title previous section-text)
        (for n from 1)
        (when (oddp n)
          (collect (list (cons (and title (subseq title (position #\= title :test-not #'eql)))
                               (or (position #\= title :test-not #'eql) 0))
                         section-text)))))

(defun list-interesting-text (sections)
  (let ((english-level nil)
        (titles nil))
    (values
     (apply #'concatenate 'string
            (nreverse
             (reduce (lambda (last cur)
                       (destructuring-bind ((title . level) text) cur
                         (let ((englishp (member title +interesting-language-headers+ :test #'equalp)))
                           (when englishp
                             (setq english-level level))
                           (if english-level
                               (if (or (< english-level level) englishp)
                                   (progn
                                     (push title titles)
                                     (push text last))
                                   (progn (setq english-level nil)
                                          last))
                               (if (member title +always-interesting-headers+ :test #'equalp)
                                   (progn
                                     (push title titles)
                                     (push text last))
                                   last)))))
                     sections
                     :initial-value nil)))
     titles)))

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
  (declare (type (or keyword list) list-or-keyword))
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


;;;;;;;;;;;;;
;;; Tests

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
