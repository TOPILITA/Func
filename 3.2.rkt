
(define (sum f term? start stop step)
 (if (term? start stop)
     0
     (+ (f start step) (sum f term? (+ start step) stop step))))

(define (Simpson f start stop step)
 (define (Calculate f x step)
   (* (/ step 6) (+ (f x) (* 4 (f (+ x (/ step 2)))) (f (+ x step))))) 
 (sum (lambda (start step) (Calculate f start step))
      (lambda (start stop) (> start stop))
      start stop step))

(define (LeftRectangles f start stop step)
 (sum (lambda (x step) (* step (f x)))
      (lambda (start stop) (>= start stop))
      start stop step))

(define (RightRectangles f start stop step)
 (sum (lambda (x step) (* step (f x)))
      (lambda (start stop) (> start stop))
      (+ start step) stop step))

(define (MiddleRectangles f start stop step)
 (sum (lambda (x step) (* step (f (+ x (/ step 2)))))
      (lambda (start stop) (> start stop))
      start stop step))

(define (main)

 (let ((func (lambda (x) (/ (* x x) (sqrt (+ 1 (* x x))))))
       (start -0.5)
       (stop 1.3)
       (step 0.0001))
   (display "Метод Сімпсона: ")
   (display (Simpson func start stop step))
   (newline)
   (display "Метод лівих прямокутників: ")
   (display (LeftRectangles func start stop step))
   (newline)
   (display "Метод правих прямокутників: ")
   (display (RightRectangles func start stop step))
   (newline)
   (display "Метод середніх прямокутників: ")
   (display (MiddleRectangles func start stop step))
   (newline)))

(main)