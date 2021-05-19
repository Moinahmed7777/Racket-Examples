#lang racket
(provide (all-defined-out))

( define (divisible-by-x? x)
    (lambda(n)
      (if(= 0 (modulo n x))
         #t
         #f)))

(define (function-9 f)
  (f 9))

(define (my-map f l)
  (cond
[(empty? l) empty]
[else (cons (f (first l))
(my-map f (rest l)))]))

(define (pair-up lst1 lst2)
  (cond ((null? lst1) '())
        ((null? lst2) '())
        (else
         (cons (list(first lst1)(first lst2))
               (pair-up (cdr lst1)(cdr lst2))))))


      
(define (is-member? atom lst)
  (if (null? lst)
      #f
      (if (equal? atom (car lst))
          #t
          (is-member? atom (cdr lst)))))

(define (my-flatten lst)
  (cond ((null? lst) '())
        ((pair? lst)
         (append (my-flatten (car lst)) (my-flatten (cdr lst))))
        (else (list lst))))



(define (upper-threshold lst theshold)
  (cond
    ((null? lst) '())
    ((< (car lst) theshold)
     (cons (car lst)
           (upper-threshold (cdr lst) theshold)))
    (else (upper-threshold (cdr lst) theshold))))

(define (my-list-ref list n)
   (if(null? list)   
        (error "Error:Index out of bounds")
        (if (equal? n 0)
          (car list)
          (my-list-ref (cdr list) (- n 1)))))

(define (deep-reverse l)
  (if (list? l)
      (my-reverse (map deep-reverse l))
      l))
(define my-reverse
  (lambda (lst)
    (if (null? lst)
        null
        (append (my-reverse (cdr lst)) (list (car lst))))))



(define (my-sorted? l)
         (if (number? (car l))
            (numCmpr l)
            (stringCmpr l)
            ))
(define (numCmpr l)
         (if (<= (length l) 1)
            #t
            (and (<= (car l) (cadr l)) (numCmpr (cdr l)) )))
(define (stringCmpr l)
         (if (<= (length l) 1)
            #t
            (and (string<=? (car l) (cadr l))(stringCmpr (cdr l)) )))
