(in-package :wiktionary)

(in-suite* root)

(test (capitalized-first-letter :suite root)
  "If a word is capitalized and not a proper noun, we need to check the
lowercase version as well."
  (is (not (unknownp "Hi"))))

(test (foriegn-proper-noun :suite root)
  "We cannot discard words just because they are foriegn."
  ;; Thanks to AllHailTheGeek on freenode.
  (is (not (unknownp "Achmed"))))

(test (period :suite root)
  "Period is one of the unsupported titles in wiktionary."
  (is (not (unknownp "."))))

;;; END