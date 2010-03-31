(asdf:defsystem :acumen
  :depends-on (:english-parser)
  :components
  ((:module utils
            :depends-on (:eos)
            :components
            ((:file "test-helpers")))))