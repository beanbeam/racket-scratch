;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Geom) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; ====== ;;
;; CONFIG ;;
;; ====== ;;

;; WINDOW
(define WIDTH 600)
(define HEIGHT 500)

;; MOVEMENT
(define SPEED 1)
(define FRICTION-MULTIPLIER .2)

(define PLAYER-IMAGE (polygon (list (make-posn -5 0)
                 (make-posn -8.75 6)
                 (make-posn 8.75 0)
                 (make-posn -8.75 -6))
           "outline"
           "white"))


;; ======= ;;
;; STRUCTS ;;
;; ======= ;;
(define-struct world (player enemies bullets pressed))

(define-struct player (x y vx vy))

(define-struct pressed (w a s d up down left right))



;; ======= ;;
;; DRAWING ;;
;; ======= ;;

(define (player->image p)
  (rotate (