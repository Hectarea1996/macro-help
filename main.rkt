#lang racket/base

(require (for-syntax racket/base)
         ;(for-meta 2 racket/base)
         racket/pretty)

(provide (except-out (all-defined-out)))



; Funciones auxiliares
#|(define-syntax (define-syntax/help stx)
   (syntax-case stx ()
      [(_ (name arg) body ...) #'(begin
                                    (define-for-syntax (name arg)
                                       body ...)
                                    (define-syntax name name))]))|#


(define (print-expand syntax-object [n 1])
  (case n
    [(1) (pretty-print (syntax->datum (expand-syntax-once syntax-object)))]
    [else (print-expand (expand-syntax-once syntax-object) (sub1 n))]))


                     
; types
(define stx-identifier? identifier?)

(define (stx-literal? stx)
   (let ([dat (syntax->datum stx)])
      (if (not (or (null? dat) (pair? dat)))
         #t
         #f)))

(define (stx-keyword? stx)
   (if (regexp-match? #rx"#:.*" (format "~a" (syntax->datum stx)))
      #t
      #f))


; Pair constructors and selectors
(define (stx-pair? stx)
   (syntax-case stx ()
      [(_ . _) #t]
      [_ #f]))

(define (stx-null? stx)
   (syntax-case stx ()
      [() #t]
      [_ #f]))

(define (stx-cons stx1 stx2)
   #`(#,stx1 . #,stx2))

(define (stx-car stx)
   (syntax-case stx ()
      [(a . b) #'a]))

(define (stx-cdr stx)
   (syntax-case stx ()
      [(a . b) #'b]))

(define stx-null #'())

(define (stx-list? stx)
   (syntax-case stx ()
      [(rest ...) #t]
      [_ #f]))

(define (stx-list . stxs)
   #`(#,@stxs))

(define (stx-list* stx . stxs)
   (if (null? stxs)
      stx
      (stx-cons stx (apply stx-list* stxs))))


; List operations
(define (stx-length stx)
   (length (syntax->datum stx)))



(define (stx-append . stxs)

   (define (stx-append-2 stx1 stx2)
      (if (stx-null? stx1)
         stx2
         (stx-cons (stx-car stx1) (stx-append-2 (stx-cdr stx1) stx2))))

   (define (stx-append-aux stx . stxs)
      (if (null? stxs)
         stx
         (stx-append-2 stx (apply stx-append-aux stxs))))

   (if (null? stxs)
      stx-null
      (apply stx-append-aux stxs)))



(define (stx-reverse stx)

   (define (stx-reverse-aux stx1 stx2)
      (if (stx-null? stx2)
         stx1
         (stx-reverse-aux (stx-cons (stx-car stx2) stx1) (stx-cdr stx2))))

   (stx-reverse-aux stx-null stx))


; List iteration
(define (stx-map f stx)
   (if (stx-null? stx)
      stx-null
      (stx-cons (f (stx-car stx)) (stx-map f (stx-cdr stx)))))


; List searching
(define (stx-rec-findb v stx)
   (cond 
      [(stx-identifier? stx) (bound-identifier=? v stx)]
      [(stx-pair? stx) (or (stx-rec-findb v (stx-car stx)) (stx-rec-findb v (stx-cdr stx)))]
      [else #f]))
