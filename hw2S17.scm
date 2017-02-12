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
	'(0 0)
)

(mydisplay (quadratic 1 0 0))
(mydisplay (quadratic 0 1 0))
(mydisplay (quadratic 3 4 2))

; Return a list with the original list's values doubled.
(define (doubleIt lst)
	lst
)

(mydisplay (doubleIt '(-1 1 2 3 4 -4 5)))

; Returns the union of two sets. The inputs are flat lists
; of atoms. The result is a flat list with all the elements
; that appear. No duplicates are present in the result. Order
; is not important.
; (union '(a b c) '(1 2 a b c)) -> (a b c 1 2)
; (union '(a b c) '(1 2 a b c 0)) -> (a b c 1 2 0)
(define (union lst1 lst2)
	(append lst1 lst2)
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
	lst
)

(mydisplay (reverseTail '(a b c)))
(mydisplay (reverseTail '(a (a a) b)))
(mydisplay (reverseTail '(0))))

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
	(car sales)
)

(mydisplay (getOrder SALES 0))
(mydisplay (getOrder SALES 51))
(mydisplay (getOrder SALES 56550))

; Returns the total profits for all sales. Returned
; orders are not included in this total
(define (totalProfits sales returns)
	(caddr (caddar sales))
)

(mydisplay (totalProfits SALES RETURNS))

; Returns the set of  provinces that the company sold
; to.
(define (getProvinces sales)
	'(the provinces go in a list)
)

(mydisplay (getProvinces SALES))


; Returns the provinces with their profits from that
; province. These are total profits fro each province.
(define (provincialProfit sales returns)
	'((prov1 profit1) (prov2 profit2))
)

(mydisplay (provincialProfit SALES RETURNS))

,exit
