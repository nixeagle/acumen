(asdf:defsystem :english-parser
  :serialize t
  :components
  ((:file "wiktionary")
   (:file "tokenize")))