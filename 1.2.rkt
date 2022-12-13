(define (NumberToChars n)

  (cond ((= (quotient n 10) 0)(display n))

  (else

    (NumberToChars (quotient n 10))

    (display " ")

    (display (- n (* (quotient n 10) 10))))))



(define (main)

  (display "Введіть n: ")

  (let ((n (read)))

    (display "Розділене по цифрам число: ")

    (display (NumberToChars n))
    (newline)))



(main)