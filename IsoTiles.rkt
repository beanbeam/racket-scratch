#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define SIDE-LENGTH 30)

(struct tile (x y z color))

(define (tile+scene t s)
  (let ([imgx (+ 300 (* (tile-x t) SIDE-LENGTH)
                 (* -1 (tile-y t) SIDE-LENGTH))]
        [imgy (- 550 (* (tile-x t) 0.5 SIDE-LENGTH)
                 (* (tile-y t) 0.5 SIDE-LENGTH)
                 (* (tile-z t) SIDE-LENGTH))])
    (scene+polygon s
                 (list (make-posn imgx (- imgy (* SIDE-LENGTH 1/2)))
                       (make-posn (+ imgx SIDE-LENGTH) imgy)
                       (make-posn imgx (+ imgy (* SIDE-LENGTH 1/2)))
                       (make-posn (- imgx SIDE-LENGTH) imgy))
                 'solid
                 (tile-color t))))


(define SIZE 15)

(define (num->color-int n)
  (max 0
       (min 255 (inexact->exact (round n)))))

(define (make-color/safe r g b [a 255])
  (make-color (num->color-int r)
              (num->color-int g)
              (num->color-int b)
              (num->color-int a)))

(define (tiles t)
  (foldr append empty
         (build-list
          (sub1 (* 2 SIZE))
          (λ (row) (build-list
                  (if (>= row SIZE)
                      (- (sub1 (* 2 SIZE)) row)
                      (add1 row))
                  (λ (n)
                    (let* ([offset (max 0 (- row (sub1 SIZE)))]
                           [x (+ n offset)]
                           [y (- row n offset)]
                           [z (* (sin (+ (* t 1/10) (* x 1/2)))
                                 (cos (+ (* t 1/10) (* y 1/2))))]
                           [col (+ (* z 128) 127)])
                      (tile x y z
                            (make-color/safe (- col (* 10 y)) (- 255 col) (+ (* 10 x) (/ col 2)))))))))))

(big-bang 0
          [to-draw (λ (n) (foldr tile+scene (empty-scene 600 600) (tiles n)))]
          [on-tick add1])
                       