#lang racket

(require rsound)

(define frame-rate 44100)

(define (tone seconds amplitude frequency)
  (make-tone frequency 0.1 (round (* seconds frame-rate))))

; Equal temperament with A4 = 440
(define (hz name octave)
  (define (name->int name)
    (match name
      ["C" 0]
      ["C#" 1]
      ["Db" 1]
      ["D" 2]
      ["D#" 3]
      ["Eb" 3]
      ["E" 4]
      ["F" 5]
      ["F#" 6]
      ["Gb" 6]
      ["G" 7]
      ["G#" 8]
      ["Ab" 8]
      ["A" 9]
      ["A#" 10]
      ["Bb" 10]
      ["B" 11]))
  (define (convert name octave) (+ (name->int name) (* 12 octave)))
  (define offset (- (convert name octave) (convert "A" 4)))
  (* 440 (expt 2 (/ offset 12))))

(define (note name octave)
  (λ (seconds) (tone seconds 1 (hz name octave))))

(define (rest seconds) (silence (* seconds frame-rate)))

(define (par . fragments) (λ (s) (rs-overlay* (map (λ (f) (f s)) fragments))))

(define (seq . fragments)
  (λ (s)
    (define weighted-fragments
      (map
       (λ (fragment)
         (match fragment
           [(cons _ _) fragment]
           [_ (cons 1 fragment)]))
       fragments))
    (define total-weight (apply + (map car weighted-fragments)))
    (rs-append* (map (λ (wf) ((cdr wf) (* s (/ (car wf) total-weight)))) weighted-fragments))))

(define (run duration fragment)
  (play (fragment duration))
  (sleep duration))

(run
 45
 (par
  ; Soprano
  (seq
   (seq (note "A" 5) (seq (note "B" 5) (note "C#" 6)))
   (seq (note "D#" 6) (seq rest (note "D#" 6)))
   (seq (note "E" 6) (note "B" 5) (note "B" 5) (note "D" 6))
   (seq (cons 3 (note "C#" 6)) (note "B" 5))
   (seq (seq (note "C#" 6) (note "D#" 6)) (note "E" 6) (note "E" 6) (note "D#" 6))
   (seq (note "E" 6) (note "A" 5))
   (seq (seq (note "B" 5) (note "C#" 6)) (note "D#" 6))
   (seq rest (note "D#" 6) (note "E" 6) (note "B" 5))
   (seq (seq (note "B" 5) (note "D" 6)) (note "C#" 6)))
  ; Alto
  (seq
   (seq (note "E" 5) (seq (note "E" 5) (seq (note "D" 5) (note "C#" 5))))
   (seq (note "G#" 5) (seq rest (note "G#" 5)))
   (seq
    (seq (cons 3 (note "G#" 5)) (note "A" 5))
    (seq (note "G#" 5) (note "F#" 5) (note "G#" 5) (note "E" 5)))
   (seq (cons 3 (note "A" 5)) (note "G#" 5))
   (seq
    (seq (note "A" 5) (note "B" 5)) (note "C#" 6) (note "B" 5)
    (seq (seq (note "A" 5) (note "G#" 5)) (note "A" 5)))
   (seq (note "G#" 5) (note "E" 5))
   (seq (seq (note "E" 5) (note "E" 5)) (note "F#" 5))
   (seq
    rest (seq (note "F#" 5) (seq (note "G#" 5) (note "A" 5)))
    (seq (note "G#" 5) (note "F#" 5)) (seq (note "G#" 5) (note "A" 5)))
   (seq (seq (note "B" 5) (note "G#" 5)) (note "A" 5)))
  ; Tenor
  (seq
   (seq (note "C#" 5) (seq (note "B" 4) (note "F#" 5)))
   (seq (note "F#" 5) (seq rest (note "C" 5)))
   (seq (seq (note "C#" 5) (note "D" 5)) (note "E" 5) (note "E" 5) (note "E" 5))
   (seq (cons 3 (note "E" 5)) (note "E" 5))
   (seq (seq (note "E" 5) (note "F#" 5)) (note "G" 5) (note "F#" 5) (note "B" 4))
   (seq (note "B" 4) (note "C#" 5))
   (seq (seq (note "B" 4) (note "A" 4)) (note "A" 4))
   (seq rest (note "B" 4) (note "B" 4) (note "E" 5))
   (seq (seq (note "E" 5) (note "E" 5)) (note "E" 5)))
  ; Bass
  (seq
   (seq (note "A" 4) (seq (note "G#" 4) (note "A#" 4)))
   (seq (note "C" 5) (seq rest (note "G#" 4)))
   (seq
    (note "C#" 5) (seq (note "B" 4) (note "A" 4)) (note "G#" 4) (note "F#" 4)
    (note "E" 4) (note "D" 4) (note "C#" 4) (note "B" 3))
   (seq (cons 3 (note "A" 3)) (note "E" 4))
   (seq (note "A" 4) (note "G#" 4) (note "B" 4) (note "B" 3))
   (seq (note "E" 4) (note "A" 4))
   (seq (seq (note "G#" 4) (note "G" 4)) (note "F#" 4))
   (seq (seq rest (note "B" 3)) (seq (cons 3 (note "E" 4)) (note "F#" 4)))
   (seq (seq (note "G#" 4) (seq (note "F#" 4) (note "E" 4))) (note "A" 4)))))
