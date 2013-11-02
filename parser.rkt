#lang racket

(require 
 "parser-helpers.rkt"
 "tokens.rkt")

(provide 
 abc-parse/playback
 (struct-out item)
 (struct-out note)
 (struct-out chord)
 (struct-out silence))

; item    : Linked list of note, chord, rest; see note below
; note    : Single pitch
; chord   : Multiple notes at once
; silence : Silence, blessed silence

(struct item          (length next) #:transparent #:mutable) 
(struct note     item (pitch)       #:transparent #:mutable)
(struct chord    item (notes)       #:transparent #:mutable)
(struct silence  item ()            #:transparent #:mutable)

; Note on next:
; item: that item will be played next
; list: first the first time, second the second, etc; for measures
;   #f: end of the song

; Current parser state
(define current-key    (make-parameter #f))
(define current-length (make-parameter #f))
(define current-tempo  (make-parameter #f))
(define current-repeat (make-parameter #f))
(define current-item   (make-parameter #f))

; Parse a song into a linked list of notes/chords/rests (see above)
(define (abc-parse/playback tokens)
  ; Reset parameters for this song
  (current-key    (parse-key    "CMaj"))
  (current-length (parse-length "1/8"))
  (current-tempo  (parse-tempo  "120"))
  (current-repeat #f)
  (current-item   #f)
  
  ; Read headers
  (set! tokens
    (let loop ([tokens tokens])
      (match tokens
        ; Parse headers
        [(list-rest (header text) tokens)
         (match-define (list _ key val)
           (regexp-match #px"([A-Z])\\s*\\:\\s*([^\n]*)\n?" text))
         
         (case (string->symbol key)
           [(K) (current-key    (parse-key val))]
           [(L) (current-length (parse-length val))]
           [(Q) (current-tempo  (parse-tempo val))])
         
         (loop tokens)]
        
        ; Skip initial breaks and line breaks
        [(list-rest (or 'break 'line-break) tokens)
         (loop tokens)]
        
        ; Anything else, go on to the song
        [_
         tokens])))
  
  ; Read the song body
  (let loop ([tokens tokens])
    (match tokens
      ; Ran out of tokens, force the end of the song
      ; TODO: fix this
      [(list) (void)]
      ; Ignore bars
      [(list-rest (or 'bar 'double-bar 'double-bar-start 'double-bar-end) tokens)
       (loop tokens)])))

; Parse a note, taking into account the current key
(define (parse-note text)
  (match-define (list _ accidental note octave)
    (regexp-match #px"(^|^^|_|__|=)([A-Ga-g])([',]*)" text))
  
  (define accidental-w/key
    (or accidental
        (let ([note-name (string->symbol (string-upcase note))])
          (cond
            [(member note-name (key-sharps (current-key))) "^"]
            [(member note-name (key-flats  (current-key))) "_"]
            [else                                          ""]))))
  
  (define octave-offset
    (for/sum ([c (in-string octave)])
      (if (eq? c #\') 1 -1)))
  
  (+ 49
     (case (string->symbol accidental-w/key)
       [(|| |=|) 0] [(^) 1] [(^^) 2] [(_) -1] [(__) -2])
     (case (string->symbol note)
       [(c) -21] [(d) -19] [(e) -17] [(f) -16] [(g) -14] [(a) -12] [(b) -10]
       [(C) -9]  [(D) -7]  [(E) -5]  [(F) -4]  [(G) -2]  [(A) 0]   [(B) 2])
     (* 12 octave-offset)))

; Parse a duration, taking into account the current note length and tempo
(define (parse-duration text)
  (match-define (list _ numer slashes denom)
    (regexp-match #px"(\\d*)(/*)(\\d*)" text))

  (* (cond
       [(and (equal? "" numer) (equal? "" denom))
        (/ 1 (* 2 (string-length slashes)))]
       [else
        (/ (or (and numer (string->number numer)) 1)
           (or (and denom (string->number denom)) 1))])
     (current-length)))

; TODO: DEBUG
#;(require "tokenizer.rkt")
#;(abc-parse
 (call-with-input-string "T:test\nEDCD|EEEz|DDDz|EGGz|EDCD|EEEz|EFGF|E4" abc-lex))