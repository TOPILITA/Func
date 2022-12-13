
(define (IterMethod func start end acc)
 (let ((x (func start)))
   (if (< acc (abs (- x start)))
       (IterMethod func x end acc)
       x)))

(define (SecantMethod func start end acc)
 (let ((xk (- start (/ (* (func start) (- end start)) (- (func end) (func start))))))
   (cond
     ((> acc (abs (- (func xk) (func start))))
      xk)
     ((> 0 (* (func xk) (func start)))
      (SecantMethod func start xk acc))
     (else
      (SecantMethod func xk end acc)))))

(define (main)
 
 (let ((start -1) (end 1) (accuracy 0.00001))
   (display "Метод ітерації: ")
   (display (IterMethod (lambda (x) (asin (/ (- (* x x)) 4))) start end accuracy))
   (newline)
   (display "Метод хорд: ")
   (display (SecantMethod (lambda (x) (+ (* x x) (* 4 (sin x)))) start end accuracy))))

(main)