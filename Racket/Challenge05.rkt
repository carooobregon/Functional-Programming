#lang racket
;; ====================
;; Complete the following functions and submit your file to Canvas.
;; ====================
;; Do not change the names of the functions. 
;; Do not change the number of arguments in the functions.
;; If your file cannot be loaded by the Racket interpreted, your submission may be cancelled. Then, submit only code that works.
;; ====================
;; Grading instructions:
;; There is a series of test cases for each function. In order to state that your function "works as described", your output must be similar to the expected one in each case.

;; === prime? ===

(define (primeAux n c)
  (cond
		[(< n (* c c)) #t]
		[(zero? (modulo n c)) #f]
		[else (primeAux n (add1 c))]
	)
)

(define (prime? n)
	(primeAux n 2 )
)

(display "=== prime? ===\n")
(prime? 467) ;; #t
(prime? 331777) ;; #t
(prime? 8783490) ;; #f

;; === sumDigits ===

(define (sumDigitsAux n aux)
	(cond
		[(>= n 1)
			(set! aux (+ aux (modulo n 10)))
			(set! n (floor(/ n 10)))
			(sumDigitsAux n aux)
		]
		[ aux ]
	)
)

(define (sumDigits n)
	(sumDigitsAux n 0)
)

(display "=== sumDigits ===\n")
(sumDigits 0) ;;0
(sumDigits 111223) ;; 10
(sumDigits 1234102) ;; 13
(sumDigits 99999) ;; 45

;; === xor ===

(define (xor lstA lstB)
	(cond 
		[(null? lstA) null]
		[(not (equal? (car lstA) (car lstB) ))
			(cons #t (xor (cdr lstA) (cdr lstB)))		
		]
		[(cons #f (xor (cdr lstA) (cdr lstB)))]
	)
)

(display "=== xor ===\n")
(xor '(#t #t #t) '(#f #t #f)) ;;'(#t #f #t)
(xor '(#t #f #t #f) '(#f #f #t #t)) ;;'(#t #f #f #t)
(xor '(#t #f #t #f #t) '(#t #t #t #f #f)) ;;'(#f #t #f #f #t)

;; === listToNumber ===

(define (listToNumberAux lst curr)
	(cond
		[(null? lst)
			curr ]
		[ (set! curr (* 10 curr))
			(set! curr (+ curr (car lst)))
			(listToNumberAux (cdr lst) curr)
		]
	)
)

(define (listToNumber lst)
	(listToNumberAux lst 0)
)

(display "=== listToNumber ===\n")
(listToNumber '(3 7 2 8 2 5)) ;;372825
(listToNumber '(1 2 5)) ;;125
(listToNumber '(5 6 7 0 0 1)) ;;567001
(listToNumber '(1 2 3 4 0 0 99)) ;;1234099