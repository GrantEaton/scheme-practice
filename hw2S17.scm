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

(mydisplay (quadratic 1 0 0))
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

(mydisplay (reverseTail '(a b c)))
(mydisplay (reverseTail '(a (a a) b)))
(mydisplay (reverseTail '(0)))

; compose takes two functions and returns a new function that 
; is the composition, F1oF2. The two inputs lambda functions.

;
(define (compose F1 F2)
	(define h (eval F1 (interaction-environment)))
	(define g (eval F2 (interaction-environment)))
	(lambda (x) (g (h x) ))
	
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
(mydisplay (totalProfits SALES RETURNS))

; Returns the set of  provinces that the company sold
; to.
(define (getProvinces sales)
	(getProvincesHelper sales '())	
)
(define (getProvincesHelper sales prov)
	(COND 
		((NULL? sales) prov)
		((NULL? (checkStr prov (cadar (cdddar sales))))
			(getProvincesHelper (cdr sales)  prov )	
		)
		(ELSE  
			 (getProvincesHelper (cdr sales) (cons(cadar (cdddar sales)) prov))
		)
	)
)
(define (checkStr lst var)
	(cond
		((NULL? lst) var)
		((EQUAL? (car lst) var)
			 '()
		)
		(ELSE 	
			(checkStr (cdr lst) var)
		)
	)
)
(mydisplay (getProvinces SALES))


; Returns the provinces with their profits from that
; province. These are total profits for each province.
(define (provincialProfit sales returns)
	(provincialProfitHelper	sales (getProvinces sales) returns)
)
(define (provincialProfitHelper sales provs returns) 
	(COND 
		((NULL? provs) '())
		(ELSE 
			;(mydisplay (list (car provs) (provinceProfit sales (car provs) returns)) (provincialProfitHelper sales (cdr provs) returns))
			(append (list (list (car provs) (provinceProfit sales (car provs) returns))) (provincialProfitHelper sales (cdr provs) returns))
		)
	)
)
(define (provinceProfit sales prov returns)
	(COND
		((NULL? sales) 0)
		((EQUAL? (cadar (cdddar sales)) prov) 
			(COND
				((checkReturns returns (caar sales)) (provinceProfit (cdr sales) prov returns))
				(ELSE(+ (caddr (caddar sales)) (provinceProfit (cdr sales) prov returns)))
			)
		)
		(ELSE (provinceProfit (cdr sales) prov returns))
	)
)

(mydisplay (provincialProfit SALES RETURNS))














;
;Extra functions from hw2.scm that I also did.
;
(display "Additional functions from hw2.scm:")
(newline)
(display "returns only negatives:")
(newline)

    ; Return a list with only the negatives items
(define (negatives lst)
 (cond
  ((NULL? lst) '()) 
  ((< (car lst) 0) (cons (car lst) (negatives (cdr lst))))
  (ELSE (negatives (cdr lst)))
 )
 )


    (mydisplay (negatives '(-1 1 2 -66 3 0 4 -4 5)))
(display "returns true if two lists have identical structure:")
(newline)

    ; Returns true if the two lists have identical structure.
    ; (struct '(a b c (c a b)) '(1 2 3 (a b c))) -> #t
    ; (struct '(a b c (c (a b) b)) '(1 2 3 (a (a b) c))) -> #f
(define (struct lst1 lst2)
 (COND 
	((AND (NULL? lst1) (NOT (NULL? lst2))) #f)
	((AND (NULL? lst2) (NOT (NULL? lst1))) #f)
	((AND (NULL? lst2) (NULL? lst1)) #t)
  	(ELSE
  		 (COND
    			((OR (list? (car lst1)) (list? (car lst2)))
     				(COND 
      					((AND (NOT (list? (car lst2))) (list? (car lst1)))	
						#f
					)
					((AND (NOT (list? (car lst1))) (list? (car lst2)))	
						 #f
					)
      					(ELSE
						(struct (car lst1) (car lst2))
						 (struct (cdr lst1) (cdr lst2))
					)
     				)
    			)
    		(ELSE
    			 (struct (cdr lst1) (cdr lst2))
   		)	
  		)
  	)
) 
)


    (mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c))))
    (mydisplay (struct '(a b c (c (a b) b)) '(1 2 3 (a (a b) c)))) 
    (mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c) 0)))

(display "returns max and min from list:")
(newline)

    ; Returns a list of two numeric values. The first is the smallest
    ; in the list and the second is the largest in the list. 
    ; lst -- flat, contains numeric values, and length is >= 1.
(define (minAndMax lst)
	(minAndMaxHelper lst -999999999 99999999)	
 )

(define (minAndMaxHelper lst max min)
	(COND 
		((NULL? lst) (list min max))
	(ELSE 
		(COND
			((> (car lst) max)
				(minAndMaxHelper lst (car lst)	min)
			)
			((< (car lst) min)
				(minAndMaxHelper lst max (car lst))
			)
			(ELSE
				(minAndMaxHelper (cdr lst) max min)
			)
		)
	)
	)
)

    (mydisplay (minAndMax '(1 2 -3 4 2)))
    (mydisplay (minAndMax '(1)))

(display "returns a flat list (delete nested elements:")
(newline)

; Returns a list identical to the first, except all nested lists
; are removed:
; (flatten '(a b c)) -> (a b c)
; (flatten '(a (a a) a) -> (a a a a)
; (flatten '((a b) (c (d) e) f) -> (a b c d e f)
;
(define (flatten lst)
	(cond 
		((NULL? lst) '())
		((NULL? (car lst))
			(flatten (cdr lst))
		)
		((list? (car lst))
			(cond 
				((list? (caar lst))
					(append (flatten (car lst))(cdr lst))
				)
				(ELSE
					(cons (caar lst) (flatten  (cons (cdar lst) (cdr lst))))
				)
			)
		)
		(ELSE (cons (car lst) (flatten (cdr lst))))
	)
)
(mydisplay (flatten '(a b c)))
(mydisplay (flatten '(a (a a) a)))
(mydisplay (flatten '((a b) (c (d) e) f)))


(display "returns the 'cross product' of two lists:")
(newline)

; The paramters are two lists. The result should contain the cross product
; between the two lists: 
; The inputs '(1 2) and '(a b c) should return a single list:
; ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
; lst1 & lst2 -- two flat lists.
(define (crossproduct lst1 lst2)
	(cond
		((NULL? lst1) '())
		(ELSE 
			(append (loopAndCons lst2 (car lst1)) (crossproduct (cdr lst1) lst2 ))
		)
	)

)

(define (loopAndCons lst val)
	(cond
		((NULL? lst) '())
		(ELSE 
			(cons (list val (car lst)) (loopAndCons (cdr lst) val))
		)
	)
)
(mydisplay (crossproduct '(1 2) '(a b c)))

,exit
