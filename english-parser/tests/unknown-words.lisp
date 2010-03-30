(in-package :wiktionary)

(in-suite* root)

(test (capitalized-first-letter :suite root)
  (is (not (unknownp "Hi"))))