(define (CountCombinations n) ;quotient(х) – функція одержання цілої частини від ділення


  (cond

    ((< n 3) 0)

    ((= n 3) 1)

    ((even? n) (* 2 (CountCombinations (/ n 2))))

    ((odd? n) (+ (CountCombinations (quotient n 2))

                 (CountCombinations (+ 1 (quotient n 2)))))))



(define (RecursionDepth n)

  (cond

    ((<= n 3) 1)

    ((even? n) (+ 1 (RecursionDepth (/ n 2))))

    ((odd? n) (+ 1 (max (RecursionDepth (quotient n 2))

                        (RecursionDepth (+ 1 (quotient n 2))))))))



(define (main)

  (display "Введіть n: ")

  (let ((n (read)))

    (display "Способів формування групи: ")

    (display (CountCombinations n))

    (newline)

    (display "Глибина рекурсії: ")

    (display (RecursionDepth n))

    (newline)))



(main)
    

