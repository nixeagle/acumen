(asdf:defsystem :english-parser
  :depends-on (:eos :alexandria :anaphora :iterate :cl-ppcre)
  :serial t
  :components
  ((:file "determiner")
   (:file "wiktionary")
   (:file "tokenize")
   (:module tests
            :components
            ((:file "tokenizer-tests")))))


(eos:in-suite* root)
(eos:in-suite* ep.tokenize::root)
(eos:in-suite* wiktionary::root)