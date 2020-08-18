#lang racket

; stream a = nat -> a ∪ nil

; s-const : a -> stream a
(define s-const const)

; s-at : nat a -> stream a
(define (s-at n x) (λ (m) (if (= n m) x null)))

; s-every : nat a -> stream a
(define (s-every n x) (λ (m) (if (zero? (modulo m n)) x null)))

; s-dilate : nat (stream a) -> stream a
(define (s-dilate n s) (λ (m) (if (zero? (modulo m n)) (s (floor (/ m n))) null)))

; s-map : (a -> b) (stream a) -> stream b
(define (s-map f s)
  (λ (n)
    (define sn (s n))
    (if (null? sn) null (f sn))))

; s-ap : (stream (a .. -> b)) (stream a) .. -> stream b
(define (s-ap sf ss)
  (λ (n)
    (if (or (null? sf) (ormap null? ss)) null
        (apply (sf n) (map (λ (s) (s n)) ss)))))

; s-lift : (a .. -> b) (stream a) .. -> stream b
(define (s-lift f . ss)
  (λ (n)
    (if (ormap null? ss) null
        (apply f (map (λ (s) (s n)) ss)))))

; s-take : nat (stream a) -> list a
(define (s-take n s)
  (define (go m)
    (if (< m n) (cons (s m) (go (+ m 1))) '()))
  (go 0))

; s-rand : a -> stream a
(define (s-rand x)
  (define memo (make-hash '()))
  (λ (s)
    (hash-ref memo s
              (λ ()
                (define res (if (zero? (random 2)) x null))
                (hash-set! memo s res)
                res))))

(displayln (s-take 16 (s-lift (λ (x y z) (+ x y z)) (s-const 1) (s-const 2) (s-const 3))))
(displayln (let ([s (s-rand 1)]) (list (s-take 16 s) (s-take 16 s))))
(displayln (s-take 16 (s-rand 1)))

; sounds = stream (list rsound)

(require rsound)
(define frame-rate (default-sample-rate))

; nat sounds -> void
(define (s-play bpm s)
  (define p (make-pstream))
  (define gap (/ (* 60 frame-rate) bpm))
  (define (go n)
    (define sn (s n))
    (define frames (round (* gap n)))
    (unless (null? sn)
      (map (λ (rsound) (pstream-queue p rsound frames)) sn))
    (pstream-queue-callback p (λ () (go (+ n 1))) frames))
  (go 0))

; -> void
(define (wait) (letrec ([loop (λ () (sleep 1000000) (loop))]) (loop)))

(s-play 672
        (s-lift
         (λ ss (filter (λ (s) (not (null? s))) ss))
         (s-every 4 bassdrum)
         (s-dilate 16 (s-rand crash-cymbal))
         (s-rand o-hi-hat)))
(wait)
