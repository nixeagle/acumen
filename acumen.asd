(asdf:defsystem :acumen
  :depends-on (:english-parser :eos)
  :components
  ((:module utils
            :components
            ((:file "test-helpers")))))