(asdf:defsystem :english-parser
  :depends-on (:eos :alexandria :anaphora :iterate :cl-ppcre)
  :serial t
  :components
  ((:file "determiner")
   (:file "wiktionary")
   (:file "tokenize")
   (:module tests
            :components
            ((:file "unknown-words")
             (:file "tokenizer-tests")))))
