(asdf:defsystem :english-parser
  :depends-on (:eos :alexandria :anaphora :iterate :cl-ppcre
                    :bordeaux-threads :cxml)
  :serial t
  :components
  ((:file "determiner")
   (:file "mediawiki-dump-parsing")
   (:file "wiktionary")
   (:file "tokenize")
   (:module tests
            :components
            ((:file "unknown-words")
             (:file "tokenizer-tests")))))
