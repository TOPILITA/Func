; ділення методом Горнера

(define (solve f g)

  ; знаходження наступного коеф. частки

  (define (__solve f Q)

    (if (eq? (length f) 1)

      (list (if (eq? (length Q) (list 0)) 0 Q) f)

      (__solve (append (list (+ (cadr f) (* (cadr g) (- (/ (car f) (car g)))))) (cdr (cdr f))) (append Q (list(/ (car f) (car g)))))  ))

  (__solve f (list))  )



; виведення поліному

(define (print-pol p x)

  (define (__print-pol pr n)

    (if (eq? (length pr) 0)

        ""

        (string-append (if (string=? (__print-pol (cdr pr) (+ n 1)) "") "" (string-append (__print-pol (cdr pr) (+ n 1)) " + ")) (number->string (car pr)) (if (eq? n 0) "" (string-append x "^" (number->string n))))  ))

  (display (__print-pol (reverse p) 0))  )



; знаходження кінцевого результату за допомогою часткових

(define (solve_gorn f g)

  (if (eq? (length (car (solve f g))) 1)

      (list (car (car (solve f g))) (car (car (cdr (solve f g)))))

      (append (solve_gorn (car (solve f g)) g) (cadr (solve f g))) ))



; задання коефіцієнтів функцій f, g та обраування результатів

(define f (list 3 -1 5 6))

(define g (list 2 -4))

(define Q (car (solve f g)))

(define R (cadr (solve f g)))

(define ANS (solve_gorn f g))

(define ax-b (string-append "(" (number->string (car g)) "x+" (number->string (cadr g)) ")"))



; виведення результатів

(display "f(x) = ") (print-pol f "x") (newline)

(display "g(x) = ") (print-pol g "x") (newline)

(display "Q(x) = ") (print-pol Q "x") (newline)

(display "R(x) = ") (print-pol R "x") (newline)

(display "ANS(x) = ") (print-pol ANS ax-b) (newline)