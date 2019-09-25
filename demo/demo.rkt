#lang racket

(require (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt")
         (prefix-in strat: "strategies.rkt")
         (prefix-in  algo: "dict-closure.rkt")
         "main.rkt"
         racket/list)

;; YOU CANNOT EASILY CHANGE THE ciphertext AND cipher-word-list PROCESSED BY dict-closure,
;; crack-cipher, secret-word-enumeration IN THIS DEMO AS YOU DON'T HAVE ACCESS TO
;; utils.rkt

;; IF YOU REALLY WANT THAT, YOU CAN'T USE drracket.
;; YOU'LL HAVE TO EDIT THIS IN SOME OTHER TEXT EDITOR, RENAME ".utils.rkt" to "utils.rkt",
;; EDIT IT AS WELL IN SOME EDITOR.
;; RUN `racket demo.rkt`
;; drracket WILL COMPLAIN AS LONG AS THERE IS utils.rkt IN THIS FOLDER.

(define (fuzz-key iters key)
  (if (= iters 0)
      key
      (fuzz-key (sub1 iters) (list-set key (random 26) #\_))))

;; You can experiment with the following functions in here:
;; crack-cipher
;; secret-word-enumeration
;; algo:dictionary-closure

;; strat:etai

;; stats:cipher-monograms
;; stats:cipher-bigrams
;; stats:cipher-neighbourhood
;; stats:cipher-unique-neighbourhood
;; stats:cipher-trigrams
;; stats:cipher-quadgrams
;; stats:cipher-common-words-single
;; stats:cipher-common-words-double
;; stats:cipher-common-words-triple
;; stats:cipher-common-words-quadruple
;; stats:cipher-common-initial-letters
;; stats:cipher-common-final-letters
;; stats:cipher-common-double-letters

;; utils:*

;; For example

(define wisdom (utils:encryption-key "wisdom"))

(define sherlock (utils:read-plaintext "./samples/text-4-sherlockholmes-short.txt"))
(define sherlock-cipher (utils:encrypt wisdom sherlock))
(define sherlock-word-list (utils:cipher-word-list-f sherlock-cipher))

(define key (build-list 26 (lambda (_) #\_)))

;; (crack-cipher (list strat:etai) key)
;; (strat:etai key)
;; (stats:cipher-monograms utils:ciphertext)
;; (stats:cipher-bigrams utils:cipher-word-list)

(define fuzzed (fuzz-key 10 wisdom))
;; (utils:show-key fuzzed)
;; (secret-word-enumeration fuzzed)
;; (algo:dictionary-closure fuzzed)
(define monograms (stats:cipher-monograms utils:ciphertext))
(define table (stats:cipher-unique-neighbourhood
               (stats:cipher-bigrams utils:cipher-word-list) 'both))
(define rev-table (map (lambda (x) (car x)) (reverse table)))
(define single-letters (flatten (map (lambda (x) (string->list x))
                            (stats:cipher-common-words-single utils:cipher-word-list))))
