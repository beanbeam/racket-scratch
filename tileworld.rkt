#lang racket

(require racket/draw)

;(module tileworld racket
 ; (provide tile build-world)
  
  (define tile
    (interface ()
      get-color
      can-fall?))
  
  (define basic-tile%
    (class* object% (tile)
      (init color)
      (define tile-color (parse-color color))
      
      (super-new)
      
      (define/public (get-color) tile-color)
      (define/public (can-fall?) true)))

(define (parse-color color)
  (cond [((is-a?/c color%) color) color]
        [(symbol? color) (parse-color (symbol->string color))]
        [(string? color)
         (or (send the-color-database find-color color)
             (raise-arguments-error 'parse-color
                                    "unknown color"
                                    "color" color))]
        [else (raise-arguments-error 'parse-color
                                     "color must be a string, symbol or color% object"
                                     "color" color)]))
  
  (define (build-world width height proc)
    (new world%
         [w width]
         [h height]
         [b (build-vector
             (* width height)
             (λ (n) (proc (remainder n width)
                          (quotient n width))))]))

  (define world%
    (class object%
      (init w h b)
      
      (define width w)
      (define height h)
      (define backing-vector b)
      
      (define bitmap-dirty? #t)
      (define bitmap-context (new bitmap-dc%
                                  [bitmap (make-object bitmap% width height #f #t)]))
      
      (super-new)
      (refresh-bitmap)
      
      (define/public (get-width) width)
      (define/public (get-height) height)
      
      (define/public (get-tile-at x y)
        (cond [(or (>= x width) (< x 0))
               (raise-arguments-error 'get-tile-at
                                      "invalid x coordinate"
                                      "x" x
                                      "width" width)]
              [(or (>= y height) (< y 0))
               (raise-arguments-error 'get-tile-at
                                      "invalid y coordinate"
                                      "y" y
                                      "height" height)]
              [else (vector-ref backing-vector
                                (+ (* width y) x))]))
      
      (define/public (get-bitmap)
        (if bitmap-dirty?
            (begin (refresh-bitmap)
                   bitmap-context)
            bitmap-context))
      
      (define/private (refresh-bitmap)
        (send bitmap-context set-argb-pixels
              0 0 width height
              (foldl (λ (f r)
                       (bytes-append
                        r (if (not f) ;; True if f is #f, representing no block
                              (bytes 0 0 0 0)
                              (let ([tc (send f get-color)])
                                (bytes 255
                                       (send tc red)
                                       (send tc green)
                                       (send tc blue))))))
                     (make-bytes 0)
                     (vector->list backing-vector)))
        (set! bitmap-dirty? #f))))
  
  (define (tile-vector->pixel-string vec)
    (foldl (λ (f r)
             (bytes-append
              r (if (not f) ;; True if f is #f, representing no block
                    (bytes 0 0 0 0)
                    (let ([tc (send f get-color)])
                      (bytes 255
                             (send tc red)
                             (send tc green)
                             (send tc blue))))))
           (make-bytes 0)
           (vector->list vec)))

(define w
  (build-world 200 200
               (λ (x y)
                 (if (= (random 2) 0)
                     (new basic-tile% [color 'red])
                     #f))))