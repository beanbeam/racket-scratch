#lang racket
(require racket/path)
(require 2htdp/image)
(require 2htdp/universe)

;; CONSTANTS
;; I added that tiny number because an ellipse with width of zero does not draw with rounded ends.
(define  CLOCKWISE (list (+ .015 (* pi -.5)) (* pi .5)))
(define CCLOCKWISE (list (+ .015 (* pi .5)) (* pi 1.5)))

;; CONFIG - YAY!
(define COIN-COLOR (make-color 255 235 77))
(define COIN-SHADOW (make-color 231 213 70))
(define COIN-SIZE 600)
(define SQUARE-AMOUNT .45)
(define RIM-OUTSET .85)

(define FRAME-RATE .007)
(define FRAME-RES   128)
(define ROTN-DIR CLOCKWISE)

;; PRE-CALCULATED STUFF
(define WINDOW-SIZE (* COIN-SIZE 1.1))
(define RIM-WIDTH (+ 1 (ceiling (* .014 COIN-SIZE))))
(define COIN-THICKNESS (/ COIN-SIZE 15))
(define WINDOW-CENTER (/ WINDOW-SIZE 2))

;; Renders a coin rotating back and forth. Rotates clockwise between -pi/2 and pi/2 and counter-clockwise between pi/2 and 3pi/2.
;; (Both of these numbers can be +n*2pi
(define (draw-rotating-coin t)
  (place-image (rectangle (abs (* SQUARE-AMOUNT COIN-SIZE (cos t))) (* COIN-SIZE SQUARE-AMOUNT) 'solid COIN-SHADOW)
               (+ WINDOW-CENTER (* (sin t) COIN-THICKNESS)) WINDOW-CENTER
               (place-image (ellipse (abs (* RIM-OUTSET COIN-SIZE (cos t))) (* RIM-OUTSET COIN-SIZE) 'outline (make-pen COIN-SHADOW RIM-WIDTH 'solid 'round 'round))
                            (+ WINDOW-CENTER (* 1.1 COIN-THICKNESS (sin t))) WINDOW-CENTER
                            (place-image (ellipse (abs (* COIN-SIZE (cos t))) COIN-SIZE 'solid COIN-COLOR)
                                         (+ WINDOW-CENTER (* COIN-THICKNESS (sin t))) WINDOW-CENTER
                                         (place-image (ellipse (abs (* COIN-SIZE (cos t))) COIN-SIZE 'solid COIN-SHADOW)
                                                      (- WINDOW-CENTER (* COIN-THICKNESS (sin t))) WINDOW-CENTER
                                                      (place-image (rectangle (abs (* 2 COIN-THICKNESS (sin t))) COIN-SIZE 'solid COIN-SHADOW)
                                                                   WINDOW-CENTER WINDOW-CENTER
                                                                   (place-image (ellipse (abs (* RIM-OUTSET COIN-SIZE (cos t))) (* RIM-OUTSET COIN-SIZE) 'outline (make-pen COIN-SHADOW RIM-WIDTH 'solid 'round 'round))
                                                                                (+ WINDOW-CENTER (* -1.1 COIN-THICKNESS (sin t))) WINDOW-CENTER
                                                                                (square WINDOW-SIZE 'solid 'white))))))))

;; Render a frame list of length nf between min and max using function df.
(define (build-frame-list min max nf df)
  (local [(define frame-amt (/ (- max min) nf))]
    (build-list nf (λ (n) (df (+ min (* frame-amt n)))))))

;; Export a list of frames to disk at location. Dunno if this will work on all operating systems.
(define (export-image-list im location)
  (local [(define ids (build-list (length im) identity))]
    (for-each (λ (i) (save-image (list-ref im i)
                                 (expand-user-path
                                  (string-append location
                                                 "image"
                                                 (number->string i)
                                                 ".png"))))
              ids)))

;; Pre-render frames to FRAMES, and animate using big-bang
(local [(define FRAMES (build-frame-list (first ROTN-DIR)
                                         (second ROTN-DIR) FRAME-RES draw-rotating-coin))]
  (big-bang 0
            (to-draw (λ (n) (list-ref FRAMES (modulo n (length FRAMES)))))
            (on-tick add1 FRAME-RATE)))