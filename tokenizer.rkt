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
    (token-header lexeme)]
   
   ; Pitches
   [(:: (:? (:or "^^" "^" "__" "_" "="))
        (:/ "agAG")
        (:* (:or "'" ",")))
    (token-pitch lexeme)]
   
   ; Timing information
   [(:or (:: (:+ numeric) (:+ #\/) (:+ numeric))
         (:: (:+ #\/) (:+ numeric))
         (:+ numeric)
         (:+ #\/))
    (token-duration lexeme)]
   
   ; Bar lines
   ["||" (token-double-bar)]
   ["[|" (token-double-bar-start)]
   ["|]" (token-double-bar-end)]
   ["|:" (token-repeat-start)]
   [":|" (token-repeat-end)]
   [(:or ":|:" "::") (token-repeat-end-start)]
   ["|"  (token-bar)]
   
   ; Simple tokens
   [#\- (token-tie)]
   [#\[ (token-chord-start)] [#\] (token-chord-end)]
   [#\( (token-slur-start)]  [#\) (token-slur-end)]
   [#\{ (token-grace-start)] [#\} (token-grace-end)]
   [#\z (token-rest)]        [#\Z (token-long-rest)]
   
   ; Text / guitar chords
   [(:: #\" (:* (:or (:: #\\ any-char) (:~ #\"))) #\")
    (token-text (string-trim lexeme "\""))]
   
   ; Line comments
   [(:: #\% (:* (:~ #\newline)))
    (token-comment lexeme)]
   
   ; Whitespace breaks notes sharing a bar (\ continues lines, so treat as a normal break)
   [(:or (:: #\\ (:* whitespace))
         (:+ whitespace))
    (abc-lexer input-port)]
   
   ; Newlines break lines in output as well (unless escaped with \, see above)
   [#\newline (token-linebreak)]))

; Lex a document into a list given a lexer
(define (lex lexer in)
  (for/list ([token (in-port lexer in)]
             #:break (eq? token 'eof))
    token))

; Lex ABC documents
(define abc-lex (curry lex abc-lexer))

 