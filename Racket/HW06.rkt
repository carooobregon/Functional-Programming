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

;; === sum ===
(require racket/trace)

(define (sumRow rA)
	(if(null? rA)
		0
		(+ (car rA) (sumRow (cdr rA)))
	)
)

(define (sum matrix)
	(if (null? matrix)
		0
		(+ (sumRow(car matrix)) (sum(cdr matrix)))
	)
)

(display "=== sum ===\n")
(sum '((1 2 3) (4 5 6) (7 8 9))) ;; 45
(sum '((1 2) (30 40) (5 6) (70 80))) ;; 234
(sum '((8 9 5 6 7) (3 4 5 4 2))) ;; 53

;; === complete? ===

(define (completeAux graph len)
	(cond
		[(null? graph)
			#t
		]
		[(= (length(car graph)) len)
			(completeAux(cdr graph) len)
		]
		[else
			#f
		]
	)
)

(define (complete? graph)
	(completeAux graph (length graph))
)

(display "=== complete? ===\n")
(complete? '((a b c) (b a c) (c a b) (d e) (e d))) ;; #f
(complete? '((a b) (b a))) ;; #t
(complete? '( (a c) (b a) (c a))) ;; #f

;; === msort ===

(define (merge left right)
	(cond 
		((null? left) right)
		((null? right) left)
		((< (car left) (car right)) (cons (car left) (merge (cdr left) right)))
		(else (cons (car right) (merge left (cdr right))))
	)
)

(define (msort lst)
	(cond
	 	((null? lst) null)
		((= (length lst) 1) lst)
		(else 
			(let 
				(
					(left (take lst (quotient (length lst) 2)))
					(right (drop lst (quotient (length lst) 2)))
				)
				(merge (msort left) (msort right))	
			)
		)
	)	
)

(display "=== msort ===\n")
(msort '()) ;; '()
(msort '(1)) ;; '(1)
(msort '(10 35 8 2.6 4.7 12)) ;; '(2.6 4.7 8 10 12 35)
(msort '(1 4 7 9 3 4 8 10)) ;; '(1 3 4 4 7 8 9 10)

;; === sold-units ===

;; This should not be done! (but it helps)
(define sales 
  '(
    (105 (10 3) (4 2) (9 3))
    (106 (6 4) (8 1) (4 6))
    (107 (9 7) (12 1) (14 1) (10 4))
    (108 (4 1))
    (109 (7 21) (10 4) (14 6) (5 3))
  )
)

(define (unitsAux id sales)
	(cond
		[(null? sales)
			0
		]
		[(= (caar sales) id)
			(+ (unitsAux id (cdr sales)) (cadr (car sales)))
		]
		[else (unitsAux id (cdr sales))]
	)
)

(define (sold-units id sales)
	(cond
		[(null? sales)
			0
		]
		[
			(+ (unitsAux id (cdar sales)) (sold-units id (cdr sales)))
		]
	)
)

(display "=== sold-units ===\n")
(sold-units 2 sales) ;; 0
(sold-units 9 sales) ;; 10
(sold-units 10 sales) ;; 11
(sold-units 14 sales) ;; 7

;; === insert ===

(define (insert x tree)
	(cond
		((null? tree) (cons x '(() ())))
		((null? (car tree)) (cons x '(() ())))
		((> x (car tree)) (append (list (car tree)) (list (cadr tree) (insert x (caddr tree)))))
		(else (append (list (car tree)) (list (insert x (cadr tree))) (cddr tree)))
	)
)

(display "=== insert ===\n")
(insert 1 '()) ;; '(1 () ())
(insert 3 '(1 () (5 () ()))) ;; '(1 () (5 (3 () ()) ()))
(insert 0 '(1 () (5 (3 () ()) (6 () ())))) ;; '(1 (0 () ()) (5 (3 () ()) (6 () ())))

;; == set ===

(define (exist? x lst)
  (if (equal? #f (member x lst))
      #f
      #t
  )
)

(define (set lst)
  	(if (null? lst)
      	null
      	(if (and (number? (car lst)) (not (exist? (car lst) (cdr lst))))
          	(cons (car lst) (set (cdr lst)))
          	(set (cdr lst))
      	)
  	)
)

(display "=== set ===\n")
(set '(1 2 3 2 4 a (1 2) 5 2 3)) ;; '(1 4 5 2 3)
(set '(a b 3 4)) ;; '(3 4)
(set '(10 (a b 3) 4 (8) c d (a b 3) d c 11)) ;; '(10 4 11)

(define (union x y)
	(set (append x y))
)

(display "=== union ===\n")
(union '(1 2 3 4) '(3 4 5 6)) ;; '(1 2 3 4 5 6)
(union '(10 2 8 4) '()) ;; '(10 2 8 4)
(union '(2 a 8 4) '(b c d)) ;; '(2 8 4)

(define (intersection x y)
	(if (or (null? x) (null? y))
		null
		(if (exist? (car x) y)
			(cons (car x) (intersection (cdr x) y))
			(intersection (cdr x) y)
		)
	)
)

(display "=== intersection ===\n")
(intersection '(1 2 3 4) '(3 4 5 6)) ;; '(3 4)
(intersection '(10 2 8 4) '()) ;; '()
(intersection '(2 a 8 4) '(b c 8 d)) ;; '(8)

;;; List to Number
(define (list->number x)
  (string->number (apply string-append (map number->string x)))
)