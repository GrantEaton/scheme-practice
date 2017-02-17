; sales.scm contains all the company's sales.
; You should not modify this file. Your code
; should work for other instances of this file.
(load "sales.scm")

; Helper function
(define (mydisplay value)
	(display value)
	(newline)
	#t
)

; Returns the roots of the quadratic formula, given
; ax^2+bx+c=0. Return only real roots. The list will
; have 0, 1, or two unique roots
(define (quadratic a b c)
	(define root (sqrt(-  (* b b)  (* c (* 4.0 a)))))
	(cond
		((= a 0) '())
		(ELSE
			(removeIrrational (list(  / (- (- b) root ) (* 2.0 a)) 
				(  / (+ (- b) root ) (* 2.0 a)))
			)
		)
	)

)
(define (removeIrrational lst)
	 
	(cond 
		( (NULL? lst) 
			'()
		)
		((rational? (car lst)) (cons (car lst) (removeIrrational (cdr lst))))
		(ELSE (removeIrrational (cdr lst)))
	)
	
)

(mydisplay (quadratic 1 -2 -2))
(mydisplay (quadratic 0 1 0))
(mydisplay (quadratic 3 4 2))

; Return a list with the original list's values doubled.
(define (doubleIt lst)
	(COND	 
		((NULL? lst) '())
		(ELSE (CONS (* (car lst) 2) (doubleIt (cdr lst)) ))
	)
)

(mydisplay (doubleIt '(-1 1 2 3 4 -4 5)))

; Returns the union of two sets. The inputs are flat lists
; of atoms. The result is a flat list with all the elements
; that appear. No duplicates are present in the result. Order
; is not important.
; (union '(a b c) '(1 2 a b c)) -> (a b c 1 2)
; (union '(a b c) '(1 2 a b c 0)) -> (a b c 1 2 0)
(define (union lst1 lst2)
	(deleteDuplicates (append lst1 lst2) '())	
)
(define (checkVal lst var)
	(cond
		((NULL? lst) var)
		((EQV? (car lst) var)
			 '()
		)
		(ELSE 	
			(checkVal (cdr lst) var)
		)
	)
)

(define (deleteDuplicates lst unique)
	(cond
		((NULL? lst) unique)
		(ELSE 
			(COND 
				((NOT (NULL? (checkVal (cdr lst) (car lst)) )) 
					(deleteDuplicates (cdr lst) (cons (checkVal (cdr lst) (car lst)) unique ))
				)
				(ELSE 
					(deleteDuplicates (cdr lst) unique)
				)
			)
		)
	)

)

(mydisplay (union '(a b c) '(1 2 a b c)))
(mydisplay (union '(a b c) '(1 2 a b c 0)))

; Returns a list with the original order reversed.
; The function must use tail recursion.
; (reverseTail '(a b c)) -> (c b a)
; (reverseTail '(a (a a) b) -> (b (a a) a)
; (reverseTail '(0)) -> (0)
;
(define (reverseTail lst)
	(reverseTailHelper lst '())
)

(define (reverseTailHelper lst rev) 
	(COND 
		((NULL? lst) 
			rev
		)	
		(ELSE rev ( reverseTailHelper (cdr lst) (cons (car lst) rev)))		
	)
	
		
)
(display "testing reverse Tail")

(mydisplay (reverseTail '(a b c)))
(mydisplay (reverseTail '(a (a a) b)))
(mydisplay (reverseTail '(0)))

; compose takes two functions and returns a new function that 
; is the composition, F1oF2. The two inputs lambda functions.
(define (compose F1 F2)
	(eval F1 (interaction-environment))
)

(define square '(lambda (x) (* x  x)))
(define cube '(lambda (x) (* x  x x)))
(define clamp '(lambda (x) (if (< x 0) 0 x)))

(define cubeOfClamp (compose cube clamp))
(define sqrOfCube (compose square cube))
(define clampOfCube (compose clamp cube))

(mydisplay (cubeOfClamp -2))
(mydisplay (cubeOfClamp 2))
(mydisplay (sqrOfCube -2))
(mydisplay (sqrOfCube 2))
(mydisplay (clampOfCube -2))
(mydisplay (clampOfCube 2))

; Returns the order information, give a specific order number.
; Returns the empty list, if order number is invalid.
(define (getOrder sales orderNo)
	(COND 
		((NULL? sales) '())		
		((EQV? (caar sales) orderNo) (car sales))
		(ELSE (getOrder(cdr sales) orderNo))
	)
)

(mydisplay (getOrder SALES 0))
(mydisplay (getOrder SALES 51))
(mydisplay (getOrder SALES 56550))

; Returns the total profits for all sales. Returned
; orders are not included in this total
(define (totalProfits sales returns)
	(COND 
		((NULL? sales) 0)		
		((checkReturns returns (caar sales)) 
			(totalProfits(cdr sales) returns)
		)
		(ELSE 
			(+ (totalProfits (cdr sales) returns) 	(caddr (caddar sales)))
		)
		
	)
)
(define (checkReturns returns orderNo)
	(COND 
		((NULL? returns) #f)
		((EQV? orderNo (car returns)) #t)
		(ELSE (checkReturns (cdr returns) orderNo))
	)	
)

;(mydisplay (totalProfits '((3 '("10/13/2010" "10/20/2010")
;	'(261.54 0.04 -213.25 38.94) 
;	'("Regular Air" "Nunavut") 
;	"Eldon Base for stackable storage shelf, platinum")
; '(293 
;	'("10/1/2012" "10/2/2012") 
;	'(10123.02 0.07 457.81 208.16) 
;	'("Delivery Truck" "Nunavut") 
;	"1.7 Cubic Foot Compact Cube Office Refrigerators")
; '(293 
;	'("10/1/2012" "10/3/2012") 
;	'(244.57 0.01 46.71 8.69) 
;	'("Regular Air" "Nunavut") 
;	"Cardinal Slant-D� Ring Binder, Heavy Gauge Vinyl"))
; '(3)) 
; )

(mydisplay (totalProfits SALES RETURNS))

; Returns the set of  provinces that the company sold
; to.
(define (getProvinces sales)
	(getProvincesHelper sales '())	
)
(define (getProvincesHelper sales prov)
	(COND 
		((NULL? sales) prov)
		(ELSE  (getProvincesHelper (cdr sales) (cons(cadar (cdddar sales)) prov) ))
	)
)

(mydisplay (getProvinces SALES))


; Returns the provinces with their profits from that
; province. These are total profits for each province.
(define (provincialProfit sales returns)
	'((prov1 profit1) (prov2 profit2))
)

(mydisplay (provincialProfit SALES RETURNS))

,exit
