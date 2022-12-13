
(define (GetComplexReal z) (car z))
(define (GetComplexImag z) (cdr z))
(define (Pow2 x) (* x x))

(define (GetComplexMag z)
 (sqrt (+ (Pow2 (GetComplexReal z)) (Pow2 (GetComplexImag z)))))

(define (GetComplexAng z)
 (atan (GetComplexImag z) (GetComplexReal z)))

(define (GetComplexByRealImag real imag)
 (cons real imag))

(define (GetComplexByMagAng r a)
 (cons (* r (cos a)) (* r (sin a))))

(define (Summarize z1 z2)
(GetComplexByRealImag (+ (GetComplexReal z1) (GetComplexReal z2))
	(+ (GetComplexImag z1) (GetComplexImag z2))))

(define (Multiply z1 z2)
  (cond
	((and (real? z1) (pair? z2))
       (GetComplexByRealImag (* (GetComplexReal z2) z1) (* (GetComplexImag z2) z1)))
	((and (real? z2) (pair? z1))
       (GetComplexByRealImag (* (GetComplexReal z1) z2) (* (GetComplexImag z1) z2)))
	(else
       (GetComplexByMagAng (* (GetComplexMag z1) (GetComplexMag z2))
                          (+ (GetComplexAng z1) (GetComplexAng z2))))))

(define (Divide z1 z2)
       (GetComplexByMagAng (/ (GetComplexMag z1) (GetComplexMag z2))
                          (- (GetComplexAng z1) (GetComplexAng z2))))

(define (main)

 (let ((x-coef 1+2i) (y-coef 3-5i) (res 1-3i))
   (cond
     ((complex? x-coef)
      (set! x-coef (GetComplexByRealImag (real-part x-coef) (imag-part x-coef))))
     ((real? x-coef)
      (set! x-coef (GetComplexByRealImag x-coef 0))))
   (cond
     ((complex? y-coef)
      (set! y-coef (GetComplexByRealImag (real-part y-coef) (imag-part y-coef))))
     ((real? y-coef)
      (set! y-coef (GetComplexByRealImag y-coef 0))))
   (cond
     ((complex? res)
      (set!  res (GetComplexByRealImag (real-part res) (imag-part res))))
     ((real? res)
      (set! res (GetComplexByRealImag res 0))))
  
   (let ((y 5/11))
     (let ((x (Divide (Summarize
                       (GetComplexByRealImag (- (GetComplexReal (Multiply y-coef y)))
                                             (- (GetComplexImag (Multiply y-coef y))))
                       res)
                      x-coef)))
       (cons (/ (+ (- (* 3-5i y)) 1-3i) 1+2i) y)))))

(main)