#lang racket

(provide
 abc->rsound
 abc-play-file
 abc->wav)

(require 
 (prefix-in rs: rsound)
 "tokenizer.rkt"
 "parser-playback.rkt")

(define (note->rsound n)
  (define 12th-root-of-2 (expt 2 (/ 1 12)))
  (define freq (* (expt 12th-root-of-2 (- n 49)) 440))
  (rs:network ()
              [out (rs:sine-wave freq)]))

(define (chord->rsound notes)
  (rs:signal-+s
   (for/list ([each (in-list notes)])
     (note->rsound each))))

(define (abc->rsound current-note)
  (define current-rsound
    (match current-note
      [(or #f '())
       (rs:silence 1)]
      [(note _ length pitch)
       (rs:signal->rsound (exact-floor (* length 44100)) (note->rsound pitch))]
      [(silence _ length)
       (rs:silence (exact-floor (* length 44100)))]))
  
  (define rest-rsound
    (match current-note
      [(or #f '())
       (rs:silence 1)]
      [(item (list-rest next-note rest-notes) _)
       (set-item-next! current-note rest-notes)
       (abc->rsound next-note)]
      [(item next-note _)
       (abc->rsound next-note)]))
  
  (rs:rs-append*
   (list current-rsound
         (rs:silence 10)
         rest-rsound)))

(define (abc-play-file filename)
  (rs:play
   (abc->rsound
    (abc-parse/playback 
     (call-with-input-file filename abc-lex)))))

(define (abc->wav abc-filename wav-filename)
  (rs:rs-write
   (abc->rsound
    (abc-parse/playback 
     (call-with-input-file abc-filename abc-lex)))
   wav-filename))