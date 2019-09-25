#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt")
         "list-comprehension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.

(define (increment el l x)
  (cond [(null? l) l]
        [(equal? (caar l) el) (append (list (cons (caar l) (+ x (cdar l)))) (cdr l))]
        [else (append (list (car l)) (increment el (cdr l) x))]))

(define (search el l)
  (cond [(null? l) #f]
        [(equal? (caar l) el) #t]
        [else (search el (cdr l))]))

(define (qsort l)
  (cond [(null? l) l]
        [(let* ([pivot1 (cdar l)]
                [pivot2 (car l)]
                [lows (qsort (lc x : x <- (cdr l) @(<= (cdr x) pivot1)))]
                [highs (qsort (lc x : x <- (cdr l) @(> (cdr x) pivot1)))])
           (append lows (list pivot2) highs))]))

(define (cipher-monograms ciphertext)
  (let* ([lst (string->list ciphertext)]
         [l1 (build-list 26 (lambda (x) (integer->char (+ x 97))))]
         [l2 (map (lambda (x) (cons x 0)) l1)])
    (define (help l p)
      (cond [(null? l) p]
            [(search (car l) p) (help (cdr l) (increment (car l) p 1))]
            [else (help (cdr l) p)]))
    (define (convert l)
      (cond [(null? l) l]
            [(= (cdar l) 0) (convert (cdr l))]
            [else (cons (caar l) (convert (cdr l)))]))
    (reverse (convert (qsort (help lst l2))))))

;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!

(define (slice l i j)
  (define (help l c)
    (cond [(> c j) '()]
          [(< c i) (help (cdr l) (+ c 1))]
          [else (cons (car l) (help (cdr l) (+ c 1)))]))
  (help l 1))

(define (x-gram-maker str x)
  (let ([l1 (string->list str)])
    (define (help l)
      (cond [(= (length l) (- x 1)) '()]
            [else (cons (list->string (slice l 1 x)) (help (cdr l)))]))
    (help l1)))

(define (cipher-x-grams cipher-word-list x)
  (define (make-list l)
    (cond [(null? l) l]
          [(= (string-length (car l)) x) (cons (cons (car l) 0) (make-list (cdr l)))]
          [(< (string-length (car l)) x) (make-list (cdr l))]
          [else (append (map (lambda (y) (cons y 0)) (x-gram-maker (car l) x)) (make-list (cdr l)))]))
  (define (help l p)
    (cond [(null? l) p]
          [(search (car l) p) (help (cdr l) (increment (car l) p 1))]
          [else (help (cdr l) p)]))
  (define (convert l)
    (cond [(null? l) l]
          [(= (cdar l) 0) (convert (cdr l))]
          [else (cons (caar l) (convert (cdr l)))]))
  (reverse (convert (qsort (help (map (lambda (x) (car x)) (make-list cipher-word-list))
                                 (remove-duplicates (make-list cipher-word-list)))))))

(define (cipher-bigrams cipher-word-list)
  (cipher-x-grams cipher-word-list 2))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  (let* ([l1 (build-list 26 (lambda (x) (integer->char (+ x 97))))]
         [l2 (map (lambda (x) (cons x 0)) l1)])
    (define (help1 l p)
      (cond [(null? l) p]
            [else (help1 (cdr l) (increment (caar l) p 1))]))
    (define (help2 l p)
      (cond [(null? l) p]
            [else (help2 (cdr l) (increment (cadar l) p 1))]))
    (define (help3 l p)
      (cond [(null? l) p]
            [(equal? (caar l) (cadar l)) (help3 (cdr l) (increment (caar l) p 1))]
            [else (help3 (cdr l) (increment (caar l) (increment (cadar l) p 1) 1))]))
    (cond [(equal? mode 'predecessor)
           (reverse (qsort (help1 (map (lambda (x) (string->list x)) cipher-bigrams-list) l2)))]
          [(equal? mode 'successor)
           (reverse (qsort (help2 (map (lambda (x) (string->list x)) cipher-bigrams-list) l2)))]
          [(equal? mode 'both)
           (reverse (qsort (help3 (map (lambda (x) (string->list x)) cipher-bigrams-list) l2)))])))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-word-list mode)
  (define (make-list l)
    (cond [(null? l) l]
          [(= (string-length (car l)) 2) (cons (cons (car l) 0) (make-list (cdr l)))]
          [(< (string-length (car l)) 2) (make-list (cdr l))]
          [else (append (map (lambda (y) (cons y 0)) (x-gram-maker (car l) 2)) (make-list (cdr l)))]))
  (define (help1 l p)
    (cond [(null? l) p]
          [else (help1 (cdr l) (increment (caar l) p 1))]))
  (define (help2 l p)
    (cond [(null? l) p]
          [else (help2 (cdr l) (increment (cadar l) p 1))]))
  (define (help3 l p)
    (cond [(null? l) p]
          [(equal? (caar l) (cadar l)) (help3 (cdr l) (increment (caar l) p 1))]
          [else (help3 (cdr l) (increment (caar l) (increment (cadar l) p 1) 1))]))
  (cond [(equal? mode 'predecessor)
         (reverse (qsort (help1 (map (lambda (x) (string->list (car x))) (make-list cipher-word-list))
                                (map (lambda (x) (cons x 0)) (build-list 26 (lambda (x) (integer->char (+ x 97))))))))]
        [(equal? mode 'successor)
         (reverse (qsort (help2 (map (lambda (x) (string->list (car x))) (make-list cipher-word-list))
                                (map (lambda (x) (cons x 0)) (build-list 26 (lambda (x) (integer->char (+ x 97))))))))]
        [(equal? mode 'both)
         (reverse (qsort (help3 (map (lambda (x) (string->list (car x))) (make-list cipher-word-list))
                                (map (lambda (x) (cons x 0)) (build-list 26 (lambda (x) (integer->char (+ x 97))))))))]))
   
;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  (cipher-x-grams cipher-word-list 3))

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  (cipher-x-grams cipher-word-list 4))

(define (x-words l x)
  (define (make-list l)
    (cond [(null? l) l]
          [(= (string-length (car l)) x) (cons (cons (car l) 0) (make-list (cdr l)))]
          [else (make-list (cdr l))]))
  (define (help l p)
    (cond [(null? l) p]
          [else (help (cdr l) (increment (caar l) p 1))]))
  (map (lambda (x) (car x)) (reverse (qsort (help (make-list l) (remove-duplicates (make-list l)))))))

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
  (x-words cipher-word-list 1))

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  (x-words cipher-word-list 2))

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  (x-words cipher-word-list 3))

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  (x-words cipher-word-list 4))

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
    (define (make-list l)
    (cond [(null? l) l]
          [else (cons (cons (car (string->list (car l))) 0) (make-list (cdr l)))]))
  (define (help l p)
    (cond [(null? l) p]
          [(search (car l) p) (help (cdr l) (increment (car l) p 1))]
          [else (help (cdr l) p)]))
  (define (convert l)
    (cond [(null? l) l]
          [(= (cdar l) 0) (convert (cdr l))]
          [else (cons (caar l) (convert (cdr l)))]))
  (reverse (convert (qsort (help (map (lambda (x) (car x)) (make-list cipher-word-list))
                                 (remove-duplicates (make-list cipher-word-list)))))))

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  (define (make-list l)
    (cond [(null? l) l]
          [else (cons (cons (car (reverse (string->list (car l)))) 0) (make-list (cdr l)))]))
  (define (help l p)
    (cond [(null? l) p]
          [(search (car l) p) (help (cdr l) (increment (car l) p 1))]
          [else (help (cdr l) p)]))
  (define (convert l)
    (cond [(null? l) l]
          [(= (cdar l) 0) (convert (cdr l))]
          [else (cons (caar l) (convert (cdr l)))]))
  (reverse (convert (qsort (help (map (lambda (x) (car x)) (make-list cipher-word-list))
                                 (remove-duplicates (make-list cipher-word-list)))))))

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  (define (make-list l)
    (cond [(null? l) l]
          [(<= (string-length (car l)) 2) (make-list (cdr l))]
          [else (append (double-finder (string->list (car l))) (make-list (cdr l)))]))
(define (double-finder l)
  (cond [(null? l) l]
        [(null? (cdr l)) '()]
        [(equal? (car l) (cadr l)) (cons (cons (car l) 0) (double-finder (cddr l)))]
        [else (double-finder (cdr l))]))
  (define (help l p)
    (cond [(null? l) p]
          [(search (car l) p) (help (cdr l) (increment (car l) p 1))]
          [else (help (cdr l) p)]))
  (define (convert l)
    (cond [(null? l) l]
          [(= (cdar l) 0) (convert (cdr l))]
          [else (cons (caar l) (convert (cdr l)))]))
  (reverse (convert (qsort (help (map (lambda (x) (car x)) (make-list cipher-word-list))
                                 (remove-duplicates (make-list cipher-word-list)))))))

