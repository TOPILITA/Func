
(define (CalcFraction acc)
 (CalcHighIter 0 acc))

(define (CalcHighIter iter acc)
 (let ((prev (+ 1 (/ 1 (+ 1 (CalcIter #t iter)))))
       (next (+ 1 (/ 1 (+ 1 (CalcIter #t (+ iter 1)))))))
   (if (< (abs (- next prev)) acc)
       (and
        (display (string-append "Різниця з " (number->string prev) " = "
                                (number->string (abs (exact->inexact (- next prev))))))
        (newline)
        (display "Значення дробу: ") next)
       (CalcHighIter (+ iter 1) acc))))

(define (CalcIter isEven iter)
 (let ((num (if isEven 2 1)))
 (cond ((equal? 0 iter) (/ 1 num))
       (else (/ 1 (+ num (CalcIter #f (- iter 1))))))))

(define (main)
 (CalcFraction 0.1))

(main)