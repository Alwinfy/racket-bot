#!/usr/bin/env racket
#lang racket

(require racket/sandbox racket/format json)

(define (custom-evaluator)
  (parameterize ((sandbox-eval-limits '(0.1 0.05))
                 (sandbox-error-output #f))
    (make-evaluator 'racket/base)))

(define evaluators (make-hash))

(define (get-evaluator id)
  (unless (hash-has-key? evaluators id)
    (hash-set! evaluators id (custom-evaluator)))
  (hash-ref evaluators id))

(define (deserialize str)
  (let ((expr (string->jsexpr str)))
    (list
      (get-evaluator (hash-ref expr 'id))
      (hash-ref expr 'body)
      (hash-ref expr 'args))))

(define (evaluate evaluator body args)
  (let ((result (evaluator body)))
    (if (procedure? result)
      (evaluator (cons result args))
      result)))

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
        (cons "OK" (apply evaluate (deserialize data)))))
      (main))))

(main)
      
; vim: ts=2 et
