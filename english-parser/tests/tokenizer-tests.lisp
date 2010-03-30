(in-package :ep.tokenize)

(eos:in-suite* ep.tokenize::root)
(test (parse-word :suite root)
  (flet ((pw (string)
           (with-input-from-string (s string)
             (parse-word s))))
    (is (string= "Hi" (pw "Hi")))
    (is (string= "Hi" (pw "Hi ")))
    (is (string= "Hi" (pw "Hi how are you?")))
    ;; Needs to ignore trailing ?
    (is (string= "Hi" (pw "Hi?")))))
