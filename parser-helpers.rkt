#lang racket

(provide (all-defined-out))

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

; Parse tempo information
(define (parse-tempo text)
  (string->number text))

; Parse length header information
(define (parse-length text)
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
  (match-define (list _ note type)
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