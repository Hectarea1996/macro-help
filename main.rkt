#lang racket/base

(require (for-syntax racket/base)
         (for-meta 2 racket/base))

(provide (except-out (all-defined-out)
                     stx-reverse-aux))


; Funciones auxiliares
(define-syntax (define-syntax/help stx)
   (syntax-case stx ()
      [(_ (name arg) body ...) #'(begin
                                    (define-for-syntax (name arg)
                                       body ...)
                                    (define-syntax name name))]))

                     
; syntax type
(define-syntax/help (stx-literal? stx)
   (syntax-case stx ()
      [(_ x) (let ([arg (syntax->datum #'x)])
                (not (or (null? arg) (pair? arg)))) #'#t]
      [(_ x) #'#f]))

(define-syntax/help (stx-keyword? stx)
   #`#,(regexp-match? #rx"#:.*" (format "~a" (syntax->datum stx))))



; Pair constructors and selectors
(define-syntax/help (stx-pair? stx)
   (syntax-case stx ()
      [(_ (a . d)) #'#t]
      [(_ x) #'#f]))

(define-syntax/help (stx-null? stx)
   (syntax-case stx ()
      [(_ ()) #'#t]
      [(_ x) #'#f]))

(define-syntax/help (stx-cons stx)
   (syntax-case stx ()
      [(_ a b) #'(a . b)]))

(define-syntax/help (stx-car stx)
   (syntax-case stx ()
      [(_ (a . b)) #'a]))

(define-syntax/help (stx-cdr stx)
   (syntax-case stx ()
      [(_ (a . b)) #'b]))

(define-syntax/help (stx-null stx)
   #'())

(define-syntax/help (stx-list? stx)
   (syntax-case stx ()
      [(_ (rest ...)) #'#t]
      [(_ x) #'#f]))

(define-syntax/help (stx-list stx)
   (syntax-case stx ()
      [(_ rest ...) #'(rest ...)]))

(define-syntax/help (stx-list* stx)
   (syntax-case stx ()
      [(_ rest ... last) #'(rest ... . last)]))


; List operations

(define-syntax/help (stx-length stx)
   (syntax-case stx ()
      [(_ (head rest ...)) #`#,(length (syntax->datum #'(head rest ...)))]))

(define-syntax/help (stx-append stx)
   (syntax-case stx ()
      [(_ (rest ...) ...) #'(rest ... ...)]))

(define-syntax/help (stx-reverse-aux stx)
   (syntax-case stx ()
      [(_ () (rest2 ...)) #'(rest2 ...)]
      [(_ (a rest ...) (rest2 ...)) #'(stx-reverse-aux (rest ...) (a rest2 ...))]))

(define-syntax/help (stx-reverse stx)
   (syntax-case stx ()
      [(_ (rest ...)) #'(stx-reverse-aux (rest ...) ())]
      [(_ x) #'x]))


; List iteration
(define-syntax/help (stx-map stx)
   (syntax-case stx ()
      [(_ macro (rest ...)) (identifier? #'macro) #'((macro rest) ...)]))


; Flow control
(define-syntax/help (stx-if stx)
   (syntax-case stx ()
      [(_ #f _ f-state) #'f-state]
      [(_ _ t-state _) #'t-state]))