#lang racket

(require 
 parser-tools/lex
 (prefix-in : parser-tools/lex-sre)
 "tokens.rkt")

(provide abc-lex)

(define abc-lexer
  (lexer
   ; Header lines / inline headers
   [(:or (:: (:/ "AZ") #\: (:* whitespace) (:* (:~ #\newline)) (:* whitespace) (:? #\newline))
         (:: #\[ (:/ "AZ") #\: (:* whitespace) (:* (:~ #\])) (:* whitespace) #\]))
    (header lexeme)]
   
   ; Pitches
   [(:: (:? (:or "^^" "^" "__" "_" "="))
        (:/ "agAG")
        (:* (:or "'" ",")))
    (pitch lexeme)]
   
   ; Timing information
   [(:or (:: (:+ numeric) (:+ #\/) (:+ numeric))
         (:: (:+ #\/) (:+ numeric))
         (:+ numeric)
         (:+ #\/))
    (duration lexeme)]
   
   ; Bar lines
   ["||" (double-bar)]
   ["[|" (double-bar-start)]
   ["|]" (double-bar-end)]
   ["|:" (repeat-start)]
   [":|" (repeat-end)]
   [(:or ":|:" "::") (repeat-end-start)]
   ["|"  (bar)]
   
   ; Simple tokens
   [#\- (tie)]
   [#\[ (chord-start)] [#\] (chord-end)]
   [#\( (slur-start)]  [#\) (slur-end)]
   [#\{ (grace-start)] [#\} (grace-end)]
   [#\z (rest)]        [#\Z (long-rest)]
   
   ; Text / guitar chords
   [(:: #\" (:* (:or (:: #\\ any-char) (:~ #\"))) #\")
    (text (string-trim lexeme "\""))]
   
   ; Line comments
   [(:: #\% (:* (:~ #\newline)))
    (comment lexeme)]
   
   ; Whitespace breaks notes sharing a bar (\ continues lines, so treat as a normal break)
   [(:or (:: #\\ (:* whitespace))
         (:+ whitespace))
    (abc-lexer input-port)]
   
   ; Newlines break lines in output as well (unless escaped with \, see above)
   [#\newline (linebreak)]))

; Lex a document into a list given a lexer
(define (lex lexer in)
  (for/list ([token (in-port lexer in)]
             #:break (eq? token 'eof))
    token))

; Lex ABC documents
(define abc-lex (curry lex abc-lexer))

 