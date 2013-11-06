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

; Yup.
(define (snoc ls x) (append ls (list x)))

; item    : Linked list of note, chord, rest; see note below
; note    : Single pitch
; chord   : Multiple notes at once
; silence : Silence, blessed silence

(struct item          (next length) #:transparent #:mutable) 
(struct note     item (pitch)       #:transparent #:mutable)
(struct chord    item (notes)       #:transparent #:mutable)
(struct silence  item ()            #:transparent #:mutable)

; Special item that should have length 0 that represents the start of a song
; This will be removed at the end of processing so the user should never see it

(struct start    item ()            #:transparent #:mutable)

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
  
  ; Special token at the start makes the world go round, remove this later
  (define *start* (start #f 0))
  (current-repeat *start*)
  (current-item *start*)
  
  ; Read the song body
  (let loop ([tokens tokens])
    (match tokens
      ; Ran out of tokens, force the end of the song
      ; Return the first token (which is the 'next' of the special start token)
      [(list)
       (item-next *start*)]
      ; Deal with headers (inline or not)
      [(list-rest (header text) tokens)
         (match-define (list _ key val)
           (regexp-match #px"([A-Z])\\s*\\:\\s*([^\n]*)\n?" text))
         
         (case (string->symbol key)
           [(K) (current-key    (parse-key val))]
           [(L) (current-length (parse-length val))]
           [(Q) (current-tempo  (parse-tempo val))])
         
         (loop tokens)]
      ; Ignore bars
      [(list-rest (or 'bar 'double-bar 'double-bar-start 'double-bar-end) tokens)
       (loop tokens)]
      ; Remember the current item as the start of a repeat block
      [(list-rest 'repeat-start tokens)
       (current-repeat (current-item))
       (loop tokens)]
      ; Add the start of the repeat block to the current item's nexts
      ; Note:The current item is the previous, so use the next
      ; The actual next item will get added as the second option
      [(list-rest 'repeat-end tokens)
       (add-next-to-current-item! (item-next (current-repeat)))
       (loop tokens)]
      ; Start/end repeat pairings do both
      [(list-rest 'repeat-end-start tokens)
       (add-next-to-current-item! (item-next (current-repeat)))
       (current-repeat (current-item))
       (loop tokens)]
      ; Notes with a set duration
      [(list-rest (pitch p) (duration d) tokens)
       (next-item-is! (note #f (parse-duration d) (parse-pitch p)))
       (loop tokens)]
      ; Notes without a duration
      [(list-rest (pitch p) tokens)
       (next-item-is! (note #f (parse-duration "1") (parse-pitch p)))
       (loop tokens)]
      ; Rests with a set duration
      [(list-rest 'rest (duration d) tokens)
       (next-item-is! (silence #f (parse-duration d)))
       (loop tokens)]
      ; Rests with standard duration
      [(list-rest 'rest tokens)
       (next-item-is! (silence #f (parse-duration "1")))
       (loop tokens)])))

; Append a given item to the current item's next value
; If it was #f, this item is a singletone
; If it was a single item, make it a list and add to the end
; If it was a list, add it to the end
; (If the current item is #f, this function does nothing)
(define (add-next-to-current-item! new-next)
  (when (current-item)
    (cond
      [(list? (item-next (current-item)))
       (set-item-next! (current-item) (snoc (item-next (current-item)) new-next))]
      [(item-next (current-item))
       (set-item-next! (current-item) (list (item-next (current-item)) new-next))]
      [else
       (set-item-next! (current-item) new-next)])))

; Add the new item to the current (see above) and update current item
(define (next-item-is! new-next)
  (add-next-to-current-item! new-next)
  (current-item new-next))

; Parse a note, taking into account the current key
(define (parse-pitch text)
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