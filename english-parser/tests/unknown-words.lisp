(in-package :wiktionary)

(in-suite* root)

(test (capitalized-first-letter :suite root)
  (is (not (unknownp "Hi"))))

(test (foriegn-proper-noun :suite root)
  "We cannot discard words just because they are foriegn."
  ;; Thanks to AllHailTheGeek on freenode.
  (is (not (unknownp "Achmed"))))