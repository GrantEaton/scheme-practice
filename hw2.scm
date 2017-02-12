; zipcodes.scm contains all the US zipcodes.
; You should not modify this file. Your code
; should work for other instances of this file.
;(load "zipcodes.scm")
; Helper function

    (define (mydisplay value)
     (display value)
     (newline)
#t
    )

    ; Returns the roots of the quadratic formula, given
    ; ax^2+bx+c=0. Return only real roots. The list will
    ; have 0, 1, or two roots
    (define (quadratic a b c)
     (define root (sqrt(-  (* b b)  (* c (* 4.0 a)))))
     (define lst '())
     (display (rational?  ( / (+ (- b) root ) (* 2.0 a))))
     (cons 3 lst)
     (cond
      ((= a 0) '())
      ((rational?  ( / (+ (- b) root ) (* 2.0 a))) (cons ( / (+ (- b) root ) (* 2.0 a)) lst)) 
      ((not (number? ( / (+ (- b) root ) (* 2.0 a)))) (cons  ( / (+ (- b) root ) (* 2.0 a))lst)) 
      (ELSE '())
     )
     lst
    )
    (mydisplay (quadratic 1 -2 -2))
    (mydisplay (quadratic 0 1 0))
(mydisplay (quadratic 3 4 2))

    ; Return a list with only the negatives items
(define (negatives lst)
 (cond
  ((NULL? lst) '()) 
  ((< (car lst) 0) (cons (car lst) (negatives (cdr lst))))
  (ELSE (negatives (cdr lst)))
 )
 )


    (mydisplay (negatives '(-1 1 2 -66 3 0 4 -4 5)))

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

	    ; Returns all the latitude and longitude of particular zip code.
	    ; Returns the first lat/lon, if multiple entries have same zip code.
	    ; zipcode -- 5 digit integer
    ; zips -- the zipcode DB
    (define (getLatLon zipcode zips)
     (list zipcode (car zips))
    )

(mydisplay (getLatLon 45056 zipcodes))

    ; Returns a list of all the place names common to two states.
    ; placeName -- is the text corresponding to the name of the place
    ; zips -- the zipcode DB
    (define (getCommonPlaces state1 state2 zips)
     (list state1 state2)
    )

    (mydisplay (getCommonPlaces "OH" "MI" zipcodes))

    ; Returns a list of all the place names common to a set of states.
    ; states -- is list of state names
    ; zips -- the zipcode DB
(define (getCommonPlaces2 states zips)
 '("Oxford" "Franklin")
 )

    (mydisplay (getCommonPlaces2 '("OH" "MI" "PA") zipcodes))

    ; Returns the number of zipcode entries for a particular state.
    ; If a zipcode appears multiple times in zipcodes.scm, count one
    ; for each occurance.
    ; state -- state
    ; zips -- zipcode DB
(define (zipCount state zips)
 0
 )

    (mydisplay (zipCount "OH" zipcodes))

    ; Returns the distance between two zip codes.
    ; Use lat/lon. Do some research to compute this.
    ; zip1 & zip2 -- the two zip codes in question.
    ; zips -- zipcode DB
(define (getDistanceBetweenZipCodes zip1 zip2 zips)
 0
 )

(mydisplay (getDistanceBetweenZipCodes 45056 48122 zipcodes))

    ; Some sample predicates
    (define (POS? x) (> x 0))
    (define (NEG? x) (> x 0))
    (define (LARGE? x) (>= (abs x) 10))
(define (SMALL? x) (NOT (LARGE? x)))

    ; Returns a list of items that satisfy a set of predicates.
    ; For example (filterList '(1 2 3 4 100) '(EVEN?)) should return the even numbers (2 4 100)
    ; (filterList '(1 2 3 4 100) '(EVEN? SMALL?)) should return (2 4)
    ; lst -- flat list of items
    ; filters -- list of predicates to apply to the individual elements
(define (filterList lst filters)
 lst
 )

    (mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS?)))
    (mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN?)))
    (mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN? LARGE?)))
