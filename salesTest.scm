; From community.tableau.com

; These are order numbers that were returned. Revenue and profit do not count toward totals.
(define RETURNS '(3))

; Format of indiviudal sales
;(orderNum (orderDate shipDate) (grossSale discount profit unitPrice) (deliveryMethod province) product)
(define SALES '(
 (3 ("10/13/2010" "10/20/2010") (261.54 0.04 -213.25 38.94) ("Regular Air" "Nunavut") "Eldon Base for stackable storage shelf, platinum")
 (293 ("10/1/2012" "10/2/2012") (10123.02 0.07 457.81 208.16) ("Delivery Truck" "Nunavut") "1.7 Cubic Foot Compact Cube Office Refrigerators")
 (293 ("10/1/2012" "10/3/2012") (244.57 0.01 46.71 8.69) ("Regular Air" "Nunavut") "Cardinal Slant-D� Ring Binder, Heavy Gauge Vinyl")
)) 
