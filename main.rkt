#lang racket/base


(provide (all-defined-out))



(define (stx-apply f stx-lst)
   (apply f (syntax-e stx-lst)))
                     
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



(define (stx-partition pred stx)
   (let ([result (stx-foldr (lambda (p elem)
                                (if (pred elem)
                                    (stx-cons (stx-cons elem (stx-car p)) (stx-cdr p))
                                    (stx-cons (stx-car p) (stx-cons elem (stx-cdr p))))) (stx-cons stx-null stx-null) stx)])
       (values (stx-car result) (stx-cdr result))))


                  
(define (stx-group-adjacent-by key stx same?)
   (stx-foldr (lambda (init elem)
                  (cond 
                     [(stx-null? init) (stx-list (stx-list elem))]
                     [(same? (key (stx-car (stx-car init))) (key elem))
                        (stx-cons (stx-cons elem (stx-car init)) (stx-cdr init))]
                     [else (stx-cons (stx-list elem) init)])) stx-null stx))


; List iteration
(define (stx-map f stx)
   (if (stx-null? stx)
      stx-null
      (stx-cons (f (stx-car stx)) (stx-map f (stx-cdr stx)))))


(define (stx-foldl f stx-init stx)
   (if (stx-null? stx)
      stx-init
      (stx-foldl f (f stx-init (stx-car stx)) (stx-cdr stx))))


(define (stx-foldr f stx-init stx)
   (if (stx-null? stx)
      stx-init
      (f (stx-foldr f stx-init (stx-cdr stx)) (stx-car stx))))


; List searching
(define (stx-rec-findb v stx)
   (cond 
      [(stx-identifier? stx) (bound-identifier=? v stx)]
      [(stx-pair? stx) (or (stx-rec-findb v (stx-car stx)) (stx-rec-findb v (stx-cdr stx)))]
      [else #f]))


; Additional list functions
(define (stx-takef stx pred)
   (cond 
      [(stx-null? stx) stx]
      [(pred (stx-car stx)) (stx-cons (stx-car stx) (stx-takef (stx-cdr stx) pred))]
      [else stx-null]))


(define (stx-dropf stx pred)
   (cond 
      [(stx-null? stx) stx]
      [(pred (stx-car stx)) (stx-dropf (stx-cdr stx) pred)]
      [else stx]))


(define (stx-splitf-at stx pred)
   (cond 
      [(stx-null? stx) (values stx-null stx-null)]
      [(pred (stx-car stx)) (let-values ([(tk dp) (stx-splitf-at (stx-cdr stx) pred)])
                                (values (stx-cons (stx-car stx) tk) dp))]
      [else (values stx-null stx)]))