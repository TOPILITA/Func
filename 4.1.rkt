
(define (CreateList count)
 (define (Iteration i prev total)
   (let ((current (+ prev i)))
     (if (= i total) (cons current '())
       (cons current
             (Iteration (+ 1 i) current total)))))
 (Iteration 1 0 count))

(define (Filter predicate list)
 (if (null? list) '()
     (if (predicate (car list))
         (cons (car list) (Filter predicate (cdr list)))
         (Filter predicate (cdr list)))))

(define (Remove predicate list)
 (Filter (lambda (x) (not (predicate x))) list))

(define (CountFullSquares list)
 (cond
   ((null? list) 0)
   ((not (null? (cdr list)))
     (let ((sum (+ (car list) (cadr list))))
       (if (= (sqrt sum) (inexact->exact (round (sqrt sum))))
         (+ 1 (CountFullSquares (cdr list)))
         (CountFullSquares (cdr list)))))
   (else (CountFullSquares (cdr list)))))

(define (main)
 (let ((list (CreateList 10)) (predicate (lambda (x) (if (null? x) #f (= 0 (remainder x 5))))))
   (display list)
   (newline)
   (display (Filter predicate list))
   (newline)
   (display (Remove predicate list))
   (newline)
   (display (CountFullSquares (Remove predicate list)))))

(main)