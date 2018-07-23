#lang racket

; Question 1

(define (sumAbs L)
  (cond
    ((null? L) 0)
    ((number? (car L))(+ (abs (car L))(sumAbs (cdr L)) ))                                                       
    (else ((sumAbs (cdr L))))))


; Question 2

(define (countEven NL)
  (cond
    ((number? NL)
     (if (even? NL) 1 0))
     ((null? NL) 0)
     ((list? NL) (+ (countEven (car NL)) (countEven (cdr NL))))
     (else 0)))


; Question 3

(define (getSymbols L)
  (cond ((null? L) L)
        ((symbol? (car L)) (cons (car L) (getSymbols (cdr L))))
        (else (getSymbols (cdr L)))))


; Question 4

(define (prefix L A)
  (cond
    ((null? L) L)
    ((eq? A (car L)) '())
    (else (cons (car L) (prefix (cdr L) A)))))
        

; Question 5

(define (transform NL)
  (cond
    ((null? NL) NL)
    ((list? NL) (cons (transform (car NL)) (transform (cdr NL))))
    ((not (number? NL)) NL)
    ((< NL 0) -1)
    ((> NL 0) 1)
    (else NL)))
 

; Question 6

(define (map2 F L1 L2)
  (if (null? L1) L1
      (cons (F (car L1) (car L2))
            (map2 F (cdr L1) (cdr L2)))))


; Question 7

(define (children N G)
  (cond
    ((null? G) G)
    ((eq? N (caar G))
     (cons (cadar G) (children N (cdr G))))
    (else (children N (cdr G)))))


; Question 8

(define (descendantsAll N D G)
  (descendantsAllHelp (list N) D G))

(define (descendantsAllHelp List D G)
  (if (zero? D)
      List
      (descendantsAllHelp (union List (childrenList List G)) (- D 1) G)))

(define (childrenList L G)
  (if (null? L) L
      (union (children (car L) G) (childrenList (cdr L) G))))

(define (union Set1 Set2)
  (cond
    ((null? Set1) Set2)
    ((number? (car Set1) Set2) (union (cdr Set1) Set2))
    (else (cons (car set1) (union (cdr Set1) Set2)))))

(define (member? X L)
  (cond
    ((null? L) #f)
    ((eq? X (car L)) #t)
    (else (memeber? X (cdr L)))))
  

; Question 9(a)

(define (descendants1 N D G)
  (if (zero? D)
      (list N)
      (setDiff (descendantsAll N D G) (descendantsAll N (- D 1) G)))
  (descendantsAll N D G))
  
; Question 9(b)

(define (descendants2 N D G)
  (descendants2Help (list N) '() D G))

(define (descendants2Help List1 List2)
  (if (zero? D)
      List1
      (let ((List3 (union List2 List2)))
        (descendants2Help (setDiff (childrenList List1 G) List3) List3 (- D 1) G))))

(define (setDiff Set1 Set2)
  (cond ((null? Set1) Set1)
        ((memeber? (car Set1) Set2) (setDiff (cdr Set1) Set2))
        (else (cons (car Set1) (setDiff (cdr Set1) Set2)))))





                    

