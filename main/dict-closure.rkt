#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dictionary-closure key)
  (begin (displayln "Starting a dictionary closure")
  (displayln " ")
  (displayln "Starting at the beginning of the word list:")
  (displayln " ")
         (let ([decrypted-list (utils:cipher-word-list-f (utils:decrypt key utils:ciphertext))])
           (word-checker decrypted-list dict key))))

(define (word-checker l1 l2 key1)
  (cond [(null? l1) (begin (displayln "Dictionary closure won't be of any help further") key1)]
        [(string-upcase? (car l1)) (begin (displayln (string-append (car l1) " : skipping this one"))
                                          (word-checker (cdr l1) l2 key1))]
        [else
         (let ([matches (if (null? l1) '() (matcher (car l1) l2 '()))]) 
           (cond [(null? matches) (begin (displayln (string-append (car l1) " : no match"))
                                         (displayln "-----------------------------------------------------")
                                         (displayln "-----------------------------------------------------")
                                         #f)]
                 [(= (length matches) 1) (let ([possible-sub (if (null? matches) '() (pattern-match (car l1) (car matches)))])
                                           (if (not (utils:is-monoalphabetic? possible-sub key1)) #f
                                               (begin (displayln (string-append (car l1) " : unique match   " (car matches)))
                                               (displayln "DC*:")
                                               (utils:show-key (utils:add-substitution possible-sub key1))
                                               (displayln "-----------------------------------------------------")
                                               (displayln "-----------------------------------------------------")
                                               (displayln "Starting at the beginning of the word list:")
                                               (displayln " ")
                                                      (word-checker (utils:cipher-word-list-f
                                                                     (utils:decrypt (utils:add-substitution possible-sub key1) utils:ciphertext))
                                                                    l2 (utils:add-substitution possible-sub key1)))))]
                 [(= (length matches) 2) (begin (displayln (string-append (car l1) " : multiple matches (" (car matches) " " (cadr matches) "..)"))
                                                (word-checker (cdr l1) l2 key1))]))]))
  
(define (matcher x l matches)
  (cond [(null? l) (if (= (length matches) 2)
                       (if (equal? (car matches) (cadr matches))
                           (list (car matches)) matches) matches)]
        [(= (length matches) 2) (if (equal? (car matches) (cadr matches)) (list (car matches)) matches)]
        [(equal? x (car l)) (matcher x (cdr l) matches)]
        [(equal? (pattern-match x (car l)) #f) (matcher x (cdr l) matches)]
        [else (matcher x (cdr l) (append matches (list (car l))))]))

(define dict utils:dictionary)

(define (zip l1 l2)
  (map cons l1 l2))

(define (remove-self-mapping l)
  (filter (lambda (x) (not (equal? (car x) (cdr x)))) l))

(define (flip l)
  (map (lambda (x) (cons (cdr x) (car x))) l))

(define (string-upcase? x)
  (andmap char-upper-case? (string->list x)))

(define (check-mapping l)
  (andmap (lambda (x) (char-lower-case? (car x))) l))

(define (locally-consistent substitution)
  (not (or
        (check-duplicates (for/list ([subst-pair substitution])
                            (car subst-pair)))
        (check-duplicates (for/list ([subst-pair substitution])
                            (cdr subst-pair))))))

(define (mono-alphabetic l)
  (locally-consistent l))

(define (pattern-match str1 str2)
  (if (not (= (string-length str1) (string-length str2))) #f
      (let* ([l1 (string->list str1)]
             [l2 (string->list str2)]
             [l1-l2 (remove-duplicates (zip l1 l2))]
             [filter-l1-l2 (remove-self-mapping l1-l2)])
        (cond [(null? filter-l1-l2) #f]
              [(not (check-mapping filter-l1-l2)) #f]
              [(not (mono-alphabetic filter-l1-l2)) #f]
              [else (flip filter-l1-l2)]))))