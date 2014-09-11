#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define-struct st (a r))

(define streak (scale/xy .2 2 (radial-star 900 5 30 "solid" "white")))

(define eye-base
  (place-image
   (overlay
    (scale/xy .5 1 (radial-star 800 60 200 "solid" "chartreuse"))
    (scale/xy .75 1 (radial-star 800 100 200 "solid" "aquamarine")))
   200 200
   (circle 200 "solid" "teal")))

(define pupil
  (scale/xy .5 2.5 (radial-star 900 55 65 "solid" "black")))

(define (draw-eye)
  (place-image
   pupil
   200 200
   (foldr (λ (f r) (place-r streak
                            (st-a f) (st-r f)
                            r))
          eye-base
          (build-list
           50
           (λ (n) (make-st
                   (random 360)
                   (+ 50 (random 100))))))))

(define (place-r im1 a r im2)
  (overlay/offset (rotate a im1)
                  (* (sin (* a pi (/ 1 180))) r)
                  (* (cos (* a pi (/ 1 180))) r)
                  im2))