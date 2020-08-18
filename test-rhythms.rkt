#lang racket

(require rsound)

(define frame-rate (default-sample-rate))

(define (metronome pstream rsound bpm frames)
  (define gap (round (* frame-rate (/ 60 bpm))))
  (define step-size 100) ; queue 100 beats at a time
  (define (go frames beats)
    (if (zero? beats)
      (pstream-queue-callback pstream (λ () (go frames step-size)) frames)
      (andqueue pstream rsound frames (go (+ frames gap) (- beats 1)))))
  (go frames step-size))

; (metronome (make-pstream) o-hi-hat 120 0)

(define (rhythmic-pattern pstream rsound gap pattern)
  (define step-size 1000) ; queue 100 beats at a time
  (define (go frames beats beat)
    (cond
      [(zero? beats) (pstream-queue-callback pstream (λ () (go frames step-size beat)) frames)]
      [(pattern beat) (pstream-queue pstream rsound frames)
                      (go (+ frames gap) (- beats 1) (+ beat 1))]
      [else (go (+ frames gap) (- beats 1) (+ beat 1))]))
  (go 0 step-size 0))

(define (rand2 n) (not (zero? (random 2))))
(define (every n) (λ (m) (zero? (modulo m n))))

(define (take s n)
  (define (go m)
    (if (< m n) (cons (s m) (go (+ 1 m))) '()))
  (go 0))

;; (rhythmic-pattern (make-pstream) o-hi-hat (round (/ frame-rate 8)) random-rhythm)
;; (letrec ([loop (λ () (sleep 1000000) (loop))]) (loop))

(displayln (take rand2 16))
(displayln (take (every 4) 16))
