#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens abc 
  (header pitch duration text comment ending))

(define-empty-tokens abc-empty
  (tie
   chord-start chord-end
   slur-start slur-end
   grace-start grace-end
   rest long-rest 
   bar double-bar double-bar-start double-bar-end 
   repeat-start repeat-end repeat-end-start
   break linebreak))

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
   
   ; Additional text
   [(:: #\" (:* (:or (:: #\\ any-char) (:~ #\"))) #\")
    (token-text (string-trim lexeme "\""))]
   
   ; Line comments
   [(:: #\% (:* (:~ #\newline)))
    (token-comment lexeme)]
   
   ; Escaped whitespace (ignore newline)
   [(:: #\\ (:* whitespace)) 
    (abc-lexer input-port)]
   
   ; Break lines on newlines (unless escaped) and break bars on other whitespace
   [#\newline (token-linebreak)]
   [whitespace (token-break)]))

(define (lex lexer in)
  (for/list ([token (in-port lexer in)]
             #:break (eq? token 'eof))
    token))

(call-with-input-file "greensleeves.abc" (curry lex abc-lexer))
;(call-with-input-file "chiming-bells.abc" (curry lex abc-lexer))

 