;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Vectors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct vector (x y))

;; make-vector-dm : Number Number -> Vector
;; Makes a vector from a direction and magnitude.
(define (make-vector-dm dir mag)
  (make-vector
   (* mag (inexact->exact (cos dir)))
   (* mag (inexact->exact (sin dir)))))

;; vector-magnitude : Vector -> Number
;; Returns the magnitude of the vector
(define (vector-magnitude v)
  (sqrt (+ (sqr (vector-x v))
           (sqr (vector-y v)))))

;; vector-direction : Vector -> Number
;; Returns the direction of the vector in radians.
(define (vector-direction v)
  (atan (vector-x v)
        (vector-y v)))

;; reverse-vector : Vector -> Vector
;; Returns a Vector with magnitude equal to v,
;; but opposite direction.
(define (reverse-vector v)
  (make-vector (- (vector-x v))
               (- (vector-y v))))

;; add-vector : Vector Vector -> Vector
;; Returns the sum of v1 and v2.
(define (add-vector v1 v2)
  (make-vector
   (+ (vector-x v1) (vector-x v2))
   (+ (vector-y v1) (vector-y v2))))

;; sub-vector : Vector Vector -> Vector
;; Returns the sum of v1 and the opposite of v2.
(define (sub-vector v1 v2)
  (make-vector
   (- (vector-x v1) (vector-x v2))
   (- (vector-y v1) (vector-y v2))))
   

;; dot-product : Vector Vector -> Number
;; Returns the dot product of v1 and v2.
(define (dot-product v1 v2)
  (+ (* (vector-x v1) (vector-x v2))
     (* (vector-y v1) (vector-y v2))))

;; cross-product : Vector Vector -> Number
;; Returns the MAGNITUDE of the cross product of v1 and v2.
(define (cross-product v1 v2)
  (- (* (vector-x v1) (vector-y v2))
     (* (vector-y v1) (vector-x v2))))