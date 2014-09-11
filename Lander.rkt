#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define GRAVITY .5)

(define AIR-RESISTANCE .00)

(define WIDTH 600)
(define HEIGHT 600)

(define ROCKET
  (overlay
   (overlay/offset (overlay (rectangle 20 10 "solid" "darkgray")
                            (rectangle 15 15 "solid" (make-color 70 70 70)))
                   0 9
                   (beside (line 5 -10 (make-pen "darkgray" 3 "solid" "round" "round"))
                           (rectangle 14 0 "solid" "white")
                           (line 5 10 (make-pen "darkgray" 3 "solid" "round" "round"))))
   (rectangle 0 50 "solid" "white")))

#|
(define ROCKET
  (overlay/offset (above (triangle 10 "solid" "gray")
                         (rectangle 10 30 "solid" "gray"))
                  0 10
                  (triangle 20 "solid" "lightgray")))
|#

(define (thrusting-rocket power)
  (overlay/offset
   ROCKET
   0 10
   (overlay/align
    "center" "top"
    (scale/xy 1 (* power 2) (rotate 180 (triangle 5 "solid" "white")))
    (scale/xy 1 (* power 2.5) (rotate 180 (triangle 7 "solid" "yellow")))
    (scale/xy 1 (* power 3) (rotate 180 (triangle 9 "solid" "orange"))))))

(define ROCKET-BOTTOM (- HEIGHT 37))


(struct rocket (x y xv yv t a ls goal))

(define (update w)
  (screen-limit
   (apply-air-resistance
    (apply-gravity
     (apply-friction
      (apply-thrust
       (move w)))))))

(define (move w)
  (struct-copy rocket w
               (x (+ (rocket-x w) (rocket-xv w)))
               (y (+ (rocket-y w) (rocket-yv w)))))

(define (screen-limit w)
  (struct-copy rocket w
               (x (max 12 (min 588 (rocket-x w))))
               (y (min ROCKET-BOTTOM (rocket-y w)))
               (xv (cond [(>= (rocket-x w) 595)
                          (min (rocket-xv w) 0)]
                         [(<= (rocket-x w) 5)
                          (max (rocket-xv w) 0)]
                         [else (rocket-xv w)]))
               (yv (if (>= (rocket-y w) ROCKET-BOTTOM)
                       (min (rocket-yv w) 0)
                       (rocket-yv w)))
               (ls (if (>= (rocket-y w) ROCKET-BOTTOM)
                       (rocket-yv w)
                       0))))
; apply-friction: rocket -> rocket
; Applies friction to the given rocket if it is on the ground.
(define (apply-friction w)
  (struct-copy rocket w
               (xv (if (>= (rocket-y w) ROCKET-BOTTOM)
                       (* 0 (rocket-xv w))
                       (rocket-xv w)))))

(define (apply-thrust w)
  (struct-copy rocket w
               (xv (+ (rocket-xv w) (* (sin (+ pi (rocket-a w))) (rocket-t w))))
               (yv (+ (rocket-yv w) (* (cos (+ pi (rocket-a w))) (rocket-t w))))))

(define (apply-gravity w)
  (struct-copy rocket w
               (yv (+ (rocket-yv w) GRAVITY))))

(define (apply-air-resistance w)
  (struct-copy rocket w
               (yv (* (rocket-yv w) (- 1 AIR-RESISTANCE)))
               (xv (* (rocket-xv w) (- 1 AIR-RESISTANCE)))))

(define (key w key)
  (cond [(key=? key "up")
         (struct-copy rocket w
                      (t 1))]
        [(key=? key "left")
         (struct-copy rocket w
                      (a (+ (rocket-a w) (/ pi 20))))]
        [(key=? key "right")
         (struct-copy rocket w
                      (a (- (rocket-a w) (/ pi 20))))]
        [else w]))

(define (release w key)
  (cond [(key=? key "up")
         (struct-copy rocket w
                      (t 0))]
        [else w]))

(define (draw w)
  (overlay/align 
   "left" "top"
   (text (string-append "Vertical Speed: "
                        (number->string (/ (round(* 10 (rocket-yv w))) 10))) 15 "black")
   (place-image (rectangle 40 5 "solid" "blue")
                (rocket-goal w) HEIGHT
                (place-image/align (rotate (modulo (round (* 180 (/ 1 pi) (rocket-a w))) 360)
                                           (if (> (rocket-t w) 0)
                                               (thrusting-rocket (+ .6 (* (random) .4)))
                                               ROCKET))
                                   (rocket-x w)
                                   (rocket-y w)
                                   "center" "top"
                                   (empty-scene HEIGHT WIDTH)))))

(define (end-screen w)
  (overlay (above (if (> (rocket-ls w) 6)
                      (text "YOU CRASHED!" 25 "red")
                      (if (< (abs (- (rocket-x w) (rocket-goal w))) 15)
                          (if (<= (rocket-ls w) 3)
                              (text "GREAT LANDING!" 25 "forestgreen")
                              (text "GOOD LANDING!" 25 "chartreuse"))
                          (text "YOU MISSED!" 25 "orange")))
                  (text (string-append "Speed: "
                                       (number->string (/ (round(* 10 (rocket-ls w))) 10))) 15 "black"))
           (draw w)))

(define (on-ground? w)
  (> (rocket-ls w) 0))

(big-bang (rocket (/ WIDTH 2) (/ HEIGHT 2) 0 0 0 0 0 (+ 20 (random (- WIDTH 40))))
          (to-draw draw)
          (on-tick update)
          (on-key key)
          (stop-when on-ground? end-screen)
          (on-release release))

