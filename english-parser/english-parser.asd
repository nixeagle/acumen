(asdf:defsystem :english-parser
  :serialize t
  :components
  ((:file "determiner")
   (:file "wiktionary")
   (:file "tokenize")))