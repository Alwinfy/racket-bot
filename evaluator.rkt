#!/usr/bin/env racket
#lang racket

(require racket/sandbox racket/format json)

(define (custom-evaluator)
  (parameterize ((sandbox-eval-limits '(0.1 0.05))
                 (sandbox-error-output #f))
    (make-evaluator 'racket/base)))

(define evaluator (custom-evaluator))

(define (deserialize str)
  (read (open-input-string (string->jsexpr str))))

(define length-cap 1600)

(define (outbox result)
  (display (car result))
  (display " ")
  (display (jsexpr->string (let ((body (~a (cdr result))))
    (if (> (string-length body) length-cap)
      (string-append (substring body 0 (- length-cap 3)) "...")
      body))))
  (newline)
  (flush-output))

(define (err issue)
  (cons "ERR" (exn-message issue)))

(define (main)
  (let ((data (read-line)))
    ;(display data (current-error-port))
    (when (not (eof-object? data))
      (outbox (with-handlers ((exn:fail? err))
        (cons "OK" (evaluator (deserialize data)))))
      (main))))

(main)
      
; vim: ts=2 et
