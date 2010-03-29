;;; http://en.wikipedia.org/w/index.php?title=Determiner_%28linguistics%29&oldid=350690683#English_determiners_2
;;;
;;; From the first paragraph at the top of the above wikipedia page:
;;;
;;; A determiner is a noun-modifier that expresses the reference of a noun
;;; or noun-phrase in the context, including quantity, ...
;;;
;;; Based on wikipedia there are 50 or so determiners, this portion of the
;;; english parser simply lists determiners.


;; * Alternative-additive determiners: another, other, somebody else
;; * Articles: a, an, the
;; * Cardinal numbers: one, two, fifty, etc.
;; * Degree determiners: many, much, few, little...
;; * Demonstratives: this, that, these, those, which
;; * Disjunctive determiners: either, neither
;; * Distributive determiners: each, every
;; * Elective determiners: any, either, whichever
;; * Equative determiners: the same
;; * Evaluative determiners: such
;; * Exclamative determiners: what eyes!
;; * Existential determiners: some, any
;; * Interrogative and relative determiners: which, what, whichever, whatever
;; * Negative determiners: no, neither
;; * Personal determiners: we teachers, you guys
;; * Positive-multal determiners: a lot of, many, several
;; * Positive-paucal determiners: a few, a little, some
;; * Possessive determiners: my, your, our, etc.
;; * Qualitative determiners: that, so
;; * Quantifiers: all, few, many, several, some, every, each, any, no, etc.
;; * Sufficiency determiners: enough, sufficient
;; * Uniquitive determiners: the only
;; * Universal determiners: all, both

(defpackage #:acumen.english-parser.determiner
  (:use :cl :eos)
  (:export #:list-english-determiners))

(in-package :acumen.english-parser.determiner)

(defparameter +english-determiners
  ;; Source http://en.wiktionary.org/wiki/Category:English_determiners
  (list "a few" "a little" "a number of" "a whole 'nother"
        "a whole nother" "all" "anny" "another" "any"
        "any and all" "any old" "any-and-all" "beaucoup"
        "both" "certain" "dat" "dis" "each" "each and every"
        "either" "enough" "enuff" "eny" "euerie" "everie"
        "every" "few" "fewer" "fewest" "least" "less" "little"
        "many" "more" "most" "much" "neither" "no" "none"
        "not even one" "other" "overmuch" "own" "said" "several"
        "some" "such" "such-and-such" "sufficient" "that" "them"
        "these" "thine" "this" "those" "umpteen" "us" "various"
        "we" "what" "whatever" "which" "whichever" "yonder" "you")
  "List of all determiners in the english language.

This is a ground truth for our part of speech database.")

(defun list-english-determiners ()
  "List all determiners in the english language."
  +english-determiners+)