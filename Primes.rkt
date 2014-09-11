;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Primes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Returns true if n is not divisible by every element of l
(define (p-with n l)
  (if (empty? l)
      true
      (and (not (zero? (modulo n (first l))))
           (p-with n (rest l)))))

;; Uses sieve of Eratosthenes to find primes (slowly)
(define (primes-lt n)
  (cond [(<= n 2) (cons 2 empty)]
        [(p-with n (primes-lt (floor (sqrt n))))
         (cons n (primes-lt (- n 2)))]
        [else (primes-lt (sub1 n))]))