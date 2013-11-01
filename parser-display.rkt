#lang racket

(require 
 "tokens.rkt")

(provide 
 abc-parse
 (struct-out song)
 (struct-out meter)
 (struct-out key)
 (struct-out staff)
 (struct-out measure)
 (struct-out note))

(define (snoc x ls) (append ls (list x)))

; An ABC song
(struct song 
  (name     ; From the T: header
   headers  ; Any headers not otherwise parsed
   staffs   ; Each staff in printed or instrument in played
   ) #:transparent #:mutable)

; A single staff of music, should represent an entire song
(struct staff
  (which    ; Treble, bass, etc.
   measures ; First measure (they form a linked list)
   ) #:transparent #:mutable)

; A measure is a sequential collection of notes
; Meter and key are originally set by headers but can be reset by inline fields
(struct measure
  (meter    ; Time sig for measure, #f to use song default
   key      ; Key sig for measure, #f to use song default
   notes    ; Ordered list of notes / rests in this measure
   next     ; Measures form a linked list (can be used to implement repeats)
            ; - A measure is simple case, list for first/second/third time through
   ) #:transparent #:mutable)

; Meter / time signature
(struct meter
  (beats    ; How many notes are in a measure
   1-beat   ; Which note is a beat (4 = quarter, so 1/n)
   ) #:transparent #:mutable)

; Key signature
(struct key
  (base     ; Base note for the key (C-B, ex: G)
   type     ; Type of key (major, minor, etc)
   sharps   ; List of notes that are sharp (C-B)
   flats    ; List of flats
   ) #:transparent #:mutable)

; A note has information to either print it or play it
(struct note
  (pitch    ; C-B (starting at middle C), z, or Z (z is a rest)
   duration ; Normalized to whole notes, 1 is whole, 1/4 is quarter, etc.
   accident ; Printed accidental on this note (^, ^^, _, __, =, #f)
   octave   ; 0 is middle C up to B, +-n for other octaves
   1/2steps ; Half steps on a normal 88 key keyboard
   ) #:transparent #:mutable)

; Record initial meter / key so we can add them to the measures
; Also other state variables like note length
(define current-meter  (make-parameter (meter 4 4)))
(define current-key    (make-parameter (key 'C 'Maj 0 0)))
(define current-length (make-parameter 1/8))

; Record the current measure a repeat would go to
(define current-repeat-start (make-parameter #f))

; Parse a lexed series of tokens into an ABC song
(define (abc-parse tokens)
  ; Start with an empty song
  (define new-song 
    (song #f (make-hash) '()))
  
  ; (Re)set parameter defaults
  (current-meter       (meter 4 4))
  (current-key         (key 'C 'Maj 0 0))
  (current-length       1/8)
  (current-repeat-start #f)
  
  ; Read initial headers
  (set! tokens 
    (let loop ([tokens tokens])
      (match tokens
        [(list-rest (header text) tokens)
         (match-define (list _ key val)
           (regexp-match #px"([A-Z])\\:\\s*([^\n]*)\n?" text))
         
         (define skey (string->symbol key))
         (case skey
           ; Title becomes name
           [(T)  (set-song-name! new-song val)]
           ; Set the default note length / meter / key
           [(L)  (current-length (parse-length-header val))]  
           [(M)  (current-meter (parse-meter val))]
           [(K)  (current-key (parse-key val))]
           ; Any other header, just store it in the headers hash
           [else (hash-set! (song-headers new-song) key val)])
         
         ; Process the next header
         (loop (cdr tokens))]
        
        ; Not a header, move on to the music
        [_ 
         tokens])))
  
  ; Parse notes
  (define current-measure (make-parameter (make-default-measure)))
  (set! tokens
    (let loop ([tokens tokens])
      (match tokens
        ; Out of tokens, the song is done
        [(list) (void)]
        ; On a bar, start a new measure
        ; TODO: Treat double bars seperately. (How?)
        [(list-rest (or 'bar 
                        'double-bar 
                        'double-bar-start 
                        'double-bar-end)
                    tokens)
         (define new-measure (make-default-measure))
         (set-measure-next! (current-measure) new-measure)
         (current-measure new-measure)]
        ; Repeats set up a 
        
        
        )))
  
  ; Return the song
  new-song)

; Parse length header information
(define (parse-length-header text)
  (string->number text))
      
; Parse a meter definition
(define (parse-meter text)
  (cond
    [(equal? text "none") (meter #f #f)]
    [(equal? text "C")    (meter 4 4)]
    [(equal? text "C|")   (meter "C|")]
    [else
     (apply meter (map string->number (string-split text "/")))]))

; Parse a key signature
(define (parse-key text)
  (match-define (list note type)
    (map 
     string->symbol
     (regexp-match #px"([A-G][#b]?)(|Maj|m|Min|Mix|Dor|Phr|Lyd|Loc)" text)))
  
  (when (eq? type '||) (set! type 'Maj))
  (when (eq? type 'm)  (set! type 'Min))
  
  (define-values (num-sharps num-flats)
    (case (string->symbol (format "~a~a" note type))
      [(C#Maj, AMin, G#Mix, D#Dor, E#Phr, F#Lyd, B#Loc) (values 7 0)]
      [(F#Maj, DMin, C#Mix, G#Dor, A#Phr, BLyd, E#Loc)  (values 6 0)]
      [(BMaj, GMin, F#Mix, C#Dor, D#Phr, ELyd, A#Loc)   (values 5 0)]
      [(EMaj, CMin, BMix, F#Dor, G#Phr, ALyd, D#Loc)    (values 4 0)]
      [(AMaj, FMin, EMix, BDor, C#Phr, DLyd, G#Loc)     (values 3 0)]
      [(DMaj, Min, AMix, EDor, F#Phr, GLyd, C#Loc)      (values 2 0)]
      [(GMaj, Min, DMix, ADor, BPhr, CLyd, F#Loc)       (values 1 0)]
      [(CMaj, Min, GMix, DDor, EPhr, FLyd, BLoc)        (values 0 0)]
      [(FMaj, Min, CMix, GDor, APhr, BbLyd, ELoc)       (values 0 1)]
      [(BbMaj, Min, FMix, CDor, DPhr, EbLyd, ALoc)      (values 0 2)]
      [(EbMaj, Min, BbMix, FDor, GPhr, AbLyd, DLoc)     (values 0 3)]
      [(AbMaj, Min, EbMix, BbDor, CPhr, DbLyd, GLoc)    (values 0 4)]
      [(DbMaj, BMin, AbMix, EbDor, FPhr, GbLyd, CLoc)   (values 0 5)]
      [(GbMaj, EMin, DbMix, AbDor, BbPhr, CbLyd, FLoc)  (values 0 6)]
      [(CbMaj, AMin, GbMix, DbDor, EbPhr, FbLyd, BbLoc) (values 0 7)]))
  
  (key note 
       type 
       (take '(F C G D A E B) num-sharps)
       (take '(B E A D G C F) num-flats)))

; Make a new default meter using the current parameters
(define (make-default-measure)
  (meter
   (current-meter)
   (current-key)
   '()
   #f))

; TODO: DEBUG
(require "tokenizer.rkt")
(abc-parse
 (call-with-input-string "T:test\nEDCD|EEEz|DDDz|EGGz|EDCD|EEEz|EFGF|E4" abc-lex))