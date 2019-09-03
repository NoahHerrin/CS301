; Author: Noah
; Date: 7-29-19
; Class: CSCI 301
;
; Lab 4

#lang racket
; checks if an element is a member of a list
(define member?
  (lambda(e list)
    ; base case list is empty
    (if (null? list)
        #f
        (begin
          ;(displayln list)
           (cond
             [(and (and (list? e) (list? (car list))) (set-equal? e (car list))) #t]
             [(equal? e (car list)) #t]
             [else (member? e (cdr list))])))))

; check if set A is a subset of set B
(define subset?
  (lambda (A B)
    (if (null? A)
        #t
        (begin
          (cond
           [(member? (car A) B) (subset? (cdr A) B)]
           [else #f])))))
            
; check if set's A and B are equal by checking if they are both subsets of eachother
(define set-equal?
  (lambda (A B)
    (begin
      (cond
        [(and (subset? A B) (subset? B A)) #t]
        [else #f]))))

; return a set containing a union of two sets
; the result of the union of two sets should contain all of the elements in each set, without any repititions
(define union
  (lambda (A B)
    (if (null? B)
        A
        (begin
          ;(displayln B)
          (cond
            ; check if first element in B is unique or not
            [(member? (car B) A) (union A (cdr B))]
            [else (cons (car B) (union A (cdr B)))])))))

; return a list that contains all of the elements in list A and B
(define intersect
  (lambda (A B)
    ; base case A and B no longer have any shared elements or A and B are no longer subsets of eachother
    (if (or (null? A) (null? B))
        '()
        (begin
          (cond
            [(member? (car A) B) (cons (car A) (intersect (cdr A) B))]
            [(member? (car B) A) (cons (car B) (intersect A (cdr B)))]
            [else (intersect (cdr A) (cdr B))])))))

; check if for every element a in S there exists a pair (a,a) in L
; additionally all elements in L must also be in S
(define Reflexive?
  (lambda (L S)
    (if (null? S)
        #t
    (begin
      (cond
    [(and (check-for-pairs L S) (valid-symbols? L S)) #t]
    [else #f])))))
       
    
(define check-for-pairs
  (lambda (L S)
    (if (null? S)
        #t
        (begin
          ;(displayln (list (car S) (car S)))
          (cond
            [(member? (list (car S) (car S)) L) (check-for-pairs L (cdr S))]
            [else #f])))))

; checks if set L only contains valid symbols specified in list S
(define valid-symbols?
  (lambda (L S)
    (if (null? L)
        #t
        (begin
          (cond
            [(and (member? (caar L) S) (member? (cadar L) S)) (valid-symbols? (cdr L) S)]
            [else #f])))))
    
                    
(define Symmetric?
  (lambda (L)
    (if (null? L)
        #t
        (begin
          ;(displayln (list (cadar L) (caar L)))
          (cond
            [(picky-member-of (list (cadar L) (caar L)) L)  (Symmetric? (remove-symetrical-elements (car L) L))]
            [else #f])))))
; removes elements that are symetrical i.e. if e was '(a b) remove-symetrical-elements would remove all instances of '(a b) and '(b a)
(define remove-symetrical-elements
  (lambda (e S)
    (if (null? S)
        '()
        (begin
          (cond
            [(or (equal? e (car S)) (equal? (list (cadar S) (caar S)) e)) (remove-symetrical-elements e (cdr S))]
            [else (cons (car S) (remove-symetrical-elements e (cdr S)))])))))

; Assumes e is a pair, and S is a set of pairs. returns true if S contains e in the order that it is specified
(define picky-member-of
  (lambda (e S)
         (if (null? S)
             #f
             (begin
               (cond
                 [(equal? e (car S)) #t]
                 [else (picky-member-of e (cdr S))])))))
; remove all pair's in set B from set A
(define remove-pairs
  (lambda (A B) 
    (if (null? B)
        '()
        (begin
          (cond
            [(member? (car B) A) (remove-pairs A (cdr B))]
            [else (cons (car B) (remove-pairs A (cdr B)))])))))
          
              
; given pair (a b) will find pair (a c) where c != b
(define has-neighbour?
  (lambda (pair S)
    (if (null? S)
        #f
        (begin
          (cond
            ; check if first element in (caar S) is the same as first element in pair
            [(and (and (and (equal? (car pair) (caar S)) (not (equal? (cdr pair) (cadar S)))) (not (Symmetric? (list (car S))))) (not (member? pair (list (car S))))) #t]
            [else (has-neighbour? pair (cdr S))])))))

; precondition: a neighbour exists
; given a pair (a b) and a set will return a pair (a c) where c != b
(define get-neighbour
  (lambda (pair S)
    (if (null? S)
        ; should never happen
        #f
        (begin
          (cond
            ; check if first element in (caar S) is the same as first element in pair
            [(and (not (equal? pair (car S))) (or (member? (car pair) (car S)) (member? (cadr pair) (car S)))) (car S)]
            [else (get-neighbour pair (cdr S))])))))

; takes a pair and returns the element that is not x, assumes x is a member of the pair
(define not-x
  (lambda (x S)
    (if (not (member? x S))
        #f
        (begin
          (cond
            [(equal? (car S) x) (cadr S)]
            [else (car S)])))))
    
; checks if a set is a transitive relation
(define Transitive?
  (lambda (S)
    (if (null? S)
        #t
        (begin
          ;(displayln (list "hello" S))
          ;(displayln (has-neighbour? (car S) S))
          ;(displayln (car S))
          ;(displayln (get-neighbour (car S) S))
          ;(displayln (list (cadar S) (cadr (get-neighbour (car S) S))))
          ;(displayln (member? (list (cadar S) (cdr (get-neighbour (car S) S))) S))
          ;(displayln (list (cadar S) (cadr (get-neighbour (car S) S))))
          ;(displayln (list (cadar S) (not-x (cadar S) (get-neighbour (car S) S))))
          (cond
            ; case 1 (a b) where a = b
            [(Symmetric? (list (car S))) (Transitive? (remove-pairs (list (car S)) S))]
            [(equal? (length S) 2) #f]
            [(not (has-neighbour? (car S) S)) #f]
            ; case 2 (a b) where a != b and                                                              does (b,c) exist in S                                        remove (a,b) (b,c) (a,b)
            [(and (and (not (Symmetric? (list (car S)))) (has-neighbour? (car S) S)) (member? (list (cadar S) (not-x (cadar S) (get-neighbour (car S) S))) S))
             (Transitive? (remove-pairs (append (cons (list (car S) (get-neighbour (car S) S)) S) (list (list (cadar S) (cadr (get-neighbour (car S) S))))) S))]
            ; case 3 (a, b) where a != b but there is no (a c)
            [else #f])))))

            
            
          
  