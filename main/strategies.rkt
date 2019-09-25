#lang racket

;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt")
         "list-comprehension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.

(define (k-element l k)
  (list-ref l (- k 1)))

(define (slice l i j)
  (define (help l c)
    (cond [(null? l) '()]
          [(> c j) '()]
          [(< c i) (help (cdr l) (+ c 1))]
          [else (cons (car l) (help (cdr l) (+ c 1)))]))
  (help l 1))

(define monogram-list (stats:cipher-monograms utils:ciphertext))
(define bigram-list (stats:cipher-bigrams utils:cipher-word-list))
(define trigrams-list (stats:cipher-trigrams utils:cipher-word-list))
(define quadgrams-list (stats:cipher-quadgrams utils:cipher-word-list))
(define table (stats:cipher-unique-neighbourhood
               (stats:cipher-bigrams utils:cipher-word-list) 'both))
(define rev-table (map (lambda (x) (car x)) (reverse table)))
(define (topX l x) (slice l 1 x))
(define single-letters (flatten (map (lambda (x) (string->list x))
                            (stats:cipher-common-words-single utils:cipher-word-list))))
  
(define (search el l)
  (cond [(null? l) #f]
        [(equal? (car l) el) #t]
        [else (search el (cdr l))]))

(define empty-key (make-list 26 #\_))

(define (etai key) 
  (define (help top-x)
    (let ([A (k-element key 1)]
          [E (k-element key 5)]
          [I (k-element key 9)]
          [T (k-element key 20)])

    (define (listT l)
      (cond [(null? l) l]
            [(search (car l) top-x) (cons (car l) (listT (cdr l)))]
            [else (listT (cdr l))]))

    (define (substitutions l1 l2 l3 l4)
      (lc (list (cons #\E e) (cons #\T t) (cons #\A a) (cons #\I i)) :
          e <- l2 t <- l1 a <- l3 i <- l4
          @(not (equal? e t)) @(not (equal? e a)) @(not (equal? e i))
          @(not (equal? a t)) @(not (equal? i t)) @(not (equal? a i))))

    (define (f X l)
      (if (equal? X #\_) l (list X)))
    
    (cond [(= (length single-letters) 1) (cond [(and (equal? A #\_) (equal? I #\_))
                                                (remove-duplicates (append (filter (lambda (x) (utils:is-monoalphabetic? x key))
                                                                                   (substitutions (f T (listT rev-table)) (f E top-x) single-letters top-x))
                                                                           (filter (lambda (x) (utils:is-monoalphabetic? x key))
                                                                                   (substitutions (f T (listT rev-table)) (f E top-x) top-x single-letters))))]
                                               [(and (not (equal? A #\_)) (not (equal? I #\_)))
                                                (filter (lambda (x) (utils:is-monoalphabetic? x key))
                                                        (substitutions (f T (listT rev-table)) (f E top-x) (list A) (list I)))]
                                               [(equal? A #\_)
                                                (filter (lambda (x) (utils:is-monoalphabetic? x key))
                                                        (substitutions (f T (listT rev-table)) (f E top-x) single-letters (list I)))]
                                               [(equal? I #\_)
                                                (filter (lambda (x) (utils:is-monoalphabetic? x key))
                                                        (substitutions (f T (listT rev-table)) (f E top-x) (list A) single-letters))])]      
          [(= (length single-letters) 0) (filter (lambda (x) (utils:is-monoalphabetic? x key))
                                                 (substitutions (f T (listT rev-table)) (f E top-x) (f A top-x) (f I top-x)))]
          [else (cond [(and (equal? A #\_) (equal? I #\_)) (filter (lambda (x) (utils:is-monoalphabetic? x key))
                                                                   (substitutions (f T (listT rev-table)) (f E top-x) single-letters single-letters))]
                      [(and (not (equal? A #\_)) (not (equal? I #\_)))
                       (filter (lambda (x) (utils:is-monoalphabetic? x key))
                               (substitutions (f T (listT rev-table)) (f E top-x) (list A) (list I)))]
                      [(equal? A #\_)
                       (filter (lambda (x) (utils:is-monoalphabetic? x key))
                               (substitutions (f T (listT rev-table)) (f E top-x) single-letters (list I)))]
                      [(equal? I #\_)
                       (filter (lambda (x) (utils:is-monoalphabetic? x key))
                               (substitutions (f T (listT rev-table)) (f E top-x) (list A) single-letters))])])))
  (help (topX monogram-list 8)))

(define (locally-consistent substitution)
  (not (or
        (check-duplicates (for/list ([subst-pair substitution])
                            (car subst-pair)))
        (check-duplicates (for/list ([subst-pair substitution])
                            (cdr subst-pair))))))

(define (mapping cipher plain)
  (if (not (= (string-length cipher) (string-length plain))) '()
      (let* ([sub1 (map cons (string->list plain) (string->list cipher))]
             [sub2 (remove-duplicates sub1)])
        (if (locally-consistent sub2) sub2 '()))))

(define (better-mapping l-cipher l-plain)
  (define (help l1 l2)
      (cond [(null? l1) '()]
            [(null? (mapping (car l1) (car l2))) '()]
            [else (append (mapping (car l1) (car l2)) (help (cdr l1) (cdr l2)))]))
  (cond [(not (= (length l-cipher) (length l-plain))) '()]
        [(locally-consistent (remove-duplicates (help l-cipher l-plain)))
         (remove-duplicates (help l-cipher l-plain))]
        [else '()]))

(define (g l)
  (if (null? l) '(()) l))

(define (high-order-2 l-cipher l-plain key)
  (g (let ([w (topX l-cipher 2)])
  (remove '() (remove-duplicates (lc (better-mapping w (list x y)) :
      x <- (topX l-plain 10)
      y <- (topX l-plain 10) @(not (equal? x y))
      @(utils:is-monoalphabetic? (better-mapping w (list x y)) key)))))))

(define (high-order-1 l-cipher l-plain key)
  (g (let ([w (topX l-cipher 1)])
  (remove '() (remove-duplicates (lc (better-mapping w (list x)) :
      x <- (topX l-plain 10)
      @(utils:is-monoalphabetic? (better-mapping w (list x)) key)))))))

(define (common-words-double key)
  (high-order-2 (stats:cipher-common-words-double utils:cipher-word-list)
                utils:plain-common-words-double key))

(define (common-words-triple key)
  (high-order-2 (stats:cipher-common-words-triple utils:cipher-word-list)
                utils:plain-common-words-triple key))

(define (common-words-quadruple key)
  (high-order-1 (stats:cipher-common-words-quadruple utils:cipher-word-list)
                utils:plain-common-words-quadruple key))

(define (bigrams key)
  (high-order-2 (stats:cipher-bigrams utils:cipher-word-list)
                utils:plain-bigrams key))

(define (trigrams key)
  (high-order-2 (stats:cipher-trigrams utils:cipher-word-list)
                utils:plain-trigrams key))

(define (quadgrams key)
  (high-order-1 (stats:cipher-quadgrams utils:cipher-word-list)
                utils:plain-quadgrams key))

(define plain-initial-letters (topX (map (lambda (x) (car (string->list x)))
                            utils:plain-common-initial-letters) 4))

(define plain-final-letters (topX (map (lambda (x) (car (string->list x)))
                            utils:plain-common-final-letters) 4))

(define plain-double-letters (topX (map (lambda (x) (car (string->list x)))
                            utils:plain-common-double-letters) 4))

(define (initial-final-double key)
  (g (filter (lambda (x) (utils:is-monoalphabetic? x key))
          (remove-duplicates (lc (list (cons b1 a1) (cons b2 a2) (cons b3 a3)) :
                                 a1 <- (topX (stats:cipher-common-initial-letters utils:cipher-word-list) 1)
                                 a2 <- (topX (stats:cipher-common-final-letters utils:cipher-word-list) 1)
                                 a3 <- (topX (stats:cipher-common-double-letters utils:cipher-word-list) 1)
                                 b1 <- plain-initial-letters
                                 b2 <- plain-final-letters
                                 b3 <- plain-double-letters
                                 @(locally-consistent (list (cons a1 b1) (cons a2 b2) (cons a3 b3)))
                                 )))))

;@(utils:is-monoalphabetic? (list (cons a1 b1) (cons a2 b2) (cons a3 b3)) key)
;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list etai
                          common-words-double
                          bigrams
                          common-words-triple
                          trigrams
                          initial-final-double
                          common-words-quadruple
                          quadgrams))
                          ;; common-initial-letters
                          ;; common-final-letters
                          ;; common-double-letters))