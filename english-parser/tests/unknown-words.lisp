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

(test (letter-a-unknown :suite root)
 "Letter a should report ARTICLE"
 (is (not (unknownp "A"))))

(test (the-is-article :suite root)
 "The should result with ARTICLE"
(is (not (unknownp "the"))))

(test (an-is-article :suite root)
 "An should result with ARTICLE"
(is (not (unknownp "an"))))

(test (period :suite root)
  "Period is one of the unsupported titles in wiktionary."
  (is (not (unknownp "."))))

;;; END