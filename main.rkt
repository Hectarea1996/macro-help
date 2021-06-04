#lang racket

(provide (except-out (all-defined-out)
                     stx-reverse-aux))


; Pair constructors and selectors

(define-syntax (stx-pair? stx)
   (syntax-case stx ()
      [(_ (a . d)) #'#t]
      [(_ x) #'#f]))

(define-syntax (stx-null? stx)
   (syntax-case stx ()
      [(_ ()) #'#t]
      [(_ x) #'#f]))

(define-syntax (stx-cons stx)
   (syntax-case stx ()
      [(_ a b) #'(a . b)]))

(define-syntax (stx-car stx)
   (syntax-case stx ()
      [(_ (a . b)) #'a]))

(define-syntax (stx-cdr stx)
   (syntax-case stx ()
      [(_ (a . b)) #'b]))

(define-syntax (stx-null stx)
   #'())

(define-syntax (stx-list? stx)
   (syntax-case stx ()
      [(_ (rest ...)) #'#t]
      [(_ x) #'#f]))

(define-syntax (stx-list stx)
   (syntax-case stx ()
      [(_ rest ...) #'(rest ...)]))

(define-syntax (stx-list* stx)
   (syntax-case stx ()
      [(_ rest ... last) #'(rest ... . last)]))


; List operations

(define-syntax (stx-append stx)
   (syntax-case stx ()
      [(_ (rest ...) ...) #'(rest ... ...)]))

(define-syntax (stx-reverse-aux stx)
   (syntax-case stx ()
      [(_ () (rest2 ...)) #'(rest2 ...)]
      [(_ (a rest ...) (rest2 ...)) #'(stx-reverse-aux (rest ...) (a rest2 ...))]))

(define-syntax (stx-reverse stx)
   (syntax-case stx ()
      [(_ (rest ...)) #'(stx-reverse-aux (rest ...) ())]
      [(_ x) #'x]))


; List iteration

(define-syntax (stx-map stx)
   (syntax-case stx ()
      [(_ macro (rest ...)) (identifier? #'macro) #'((macro rest) ...)]))

(define-syntax (stx-andmap stx)
   (syntax-case stx (#f)
      [(_ ()) #'#t]
      [(_ (#f rest ...)) #'#f]
      [(_ (arg rest ...)) #'(stx-andmap (rest ...))]))