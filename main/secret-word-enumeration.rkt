#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt")
         "list-comprehension.rkt")

(provide secret-word-enumeration
         possible-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)
  (if (equal? key-after-dictionary-closure #f) #f
      (let ([l (possible-keys key-after-dictionary-closure)])
        (cond [(null? l) (begin (displayln "  SWE: No possible key is valid!") #f)]
              [(= (length l) 1) (begin (displayln (string-append "  SWE: potential consistent candidates: "
                                                                 (list->string (slice (complete-key (car l)) 1 6))))
                                       (displayln "  SWE: Completed key found!")
                                       (displayln (string-append "  SWE: Secret-word is "
                                                                 (form-word (complete-key (car l)))))
                                                     (complete-key (car l)))]
              [else (begin (displayln "  SWE: potential consistent candidates: ") (printf (string-appender (possible-secret-words l)))
                           (displayln "  SWE: Partial key found!")
                                  key-after-dictionary-closure)]))))

(define (string-appender l)
  (cond [(null? l) ""]
        [else (string-append (car l) " " (string-appender (cdr l)))]))

(define (form-word key)
  (list->string (slice key 1 6)))

(define (possible-secret-words possible-key-list)
  (define (help l)
    (cond [(null? l) '()]
          [else (cons (form-word (car l)) (help (cdr l)))]))
  (help possible-key-list))
  
(define (slice l i j)
  (define (help l c)
    (cond [(> c j) '()]
          [(< c i) (help (cdr l) (+ c 1))]
          [else (cons (car l) (help (cdr l) (+ c 1)))]))
  (help l 1))

(define (possible-keys key)
  (define (help key1 list-keys acc)
    (cond [(null? list-keys) acc]
          [(equal? (key-match key (car list-keys)) #f) (help key1 (cdr list-keys) acc)]
          [else (help key1 (cdr list-keys) (append acc (list (key-match key (car list-keys)))))]))
  (help key list-of-keys '()))

(define (key-match given-key key)
  (define (help given-key1 key1)
  (cond [(null? key1) key]
        [(equal? (car given-key1) (car key1)) (help (cdr given-key1) (cdr key1))]
        [(equal? #\_ (car key1)) (help (cdr given-key1) (cdr key1))]
        [(equal? (car given-key1) #\_) (help (cdr given-key1) (cdr key1))]
        [else #f]))
  (help given-key key))
        
(define dict (remove-duplicates utils:dictionary))

(define (remove-all x l)
  (cond [(equal? (remove x l) l) l]
        [else (remove-all x (remove x l))]))

(define (x-letters x)
  (define (help l)
    (cond [(null? l) l]
          [(equal? (car l) 0) (help (cdr l))]
          [else (cons (car l) (help (cdr l)))]))
  (help (map (lambda (y) (if (= (string-length y) x) y 0)) dict)))

(define 6-letters (x-letters 6))

(define (convert-to-small x)
  (integer->char (+ 32 (char->integer x))))
  
(define 6-subs
  (map (lambda (x) (list (convert-to-small (list-ref (string->list x) 0))
                         (convert-to-small (list-ref (string->list x) 1))
                         (convert-to-small (list-ref (string->list x) 2))
                         (convert-to-small (list-ref (string->list x) 3))
                         (convert-to-small (list-ref (string->list x) 4))
                         (convert-to-small (list-ref (string->list x) 5)))) 6-letters))

(define (search6 el l)
  (define (help l c)
    (cond [(= c 0) #f]
          [(equal? (car l) el) #t]
          [else (help (cdr l) (- c 1))]))
  (help l 6))

(define (complete-key key)
  (define (help key1 x)
    (let ([alph-x (integer->char x)])
      (cond [(= (length key1) 26) key1]
            [(= x 123) (help key1 97)]
            [(search6 alph-x key1) (help key1 (+ x 1))]
            [else (help (append key1 (list alph-x)) (+ x 1))])))
  (help key (+ (char->integer (car (reverse key))) 1)))

(define list-of-keys
  (map (lambda (x) (complete-key x)) 6-subs))