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
  (define step-size 10) ; queue 100 beats at a time
  (define (go frames beats beat)
    (cond
      [(zero? beats) (pstream-queue-callback pstream (λ () (go frames step-size beat)) frames)]
      [(pattern beat) (pstream-queue pstream rsound frames)
                      (go (+ frames gap) (- beats 1) (+ beat 1))]
      [else (go (+ frames gap) (- beats 1) (+ beat 1))]))
  (go 0 step-size 0))

(define (rhythm n)
  (or
   (= (modulo n 2) 1)
   (and (< n 20) (= (modulo n 1) 0))))

(define (random-rhythm n)
  (define weight (+ 1 (round (/ (modulo n 100) 16))))
  (println weight)
  (not (zero? (random weight))))

(define (random-pattern)
  (define b0 (zero? (random 2)))
  (define b1 (zero? (random 2)))
  (define b2 (zero? (random 2)))
  (define b3 (zero? (random 2)))
  (define b4 (zero? (random 2)))
  (define b5 (zero? (random 2)))
  (define b6 (zero? (random 2)))
  (define b7 (zero? (random 2)))
  (define b8 (zero? (random 2)))
  (define b9 (zero? (random 2)))
  (define ba (zero? (random 2)))
  (define bb (zero? (random 2)))
  (define bc (zero? (random 2)))
  (define bd (zero? (random 2)))
  (define be (zero? (random 2)))
  (define bf (zero? (random 2)))
  (λ (n)
    (match (modulo n 16)
      [0 b0]
      [1 b1]
      [2 b2]
      [3 b3]
      [4 b4]
      [5 b5]
      [6 b6]
      [7 b7]
      [8 b8]
      [9 b9]
      [10 ba]
      [11 bb]
      [12 bc]
      [13 bd]
      [14 be]
      [15 bf])))

(rhythmic-pattern (make-pstream) o-hi-hat (round (/ frame-rate 8)) random-rhythm)

(letrec ([loop (λ () (sleep 1000000) (loop))]) (loop))
