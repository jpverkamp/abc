#lang racket

(require 
 parser-tools/lex)

(provide 
 (all-defined-out))

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