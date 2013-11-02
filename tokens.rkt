#lang racket

(define-syntax-rule (define-and-provide-tokens name ...)
  (begin
    (begin
      (provide (struct-out name))
      (struct name (value) #:transparent))
    ...))

(define-and-provide-tokens 
  header pitch duration text comment ending)

(define-syntax-rule (define-and-provide-empty-tokens name ...)
  (begin
    (begin
      (provide name)
      (define (name) 'name))
    ...))

(define-and-provide-empty-tokens 
  tie
  chord-start chord-end
  slur-start slur-end
  grace-start grace-end
  rest long-rest 
  bar double-bar double-bar-start double-bar-end 
  repeat-start repeat-end repeat-end-start
  break linebreak)
