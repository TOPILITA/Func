
; функція перевірки числа на простоту

(define (prime? n)

  (let loop ((d 2))

    (cond ((< n (* d d)) #t)

          ((zero? (modulo n d)) #f)

          (else (loop (+ d 1))))))



; функція створення нового вектору

(define (f v)

  ; прохід по елементам вектору

  (define (__f acc i)

    (if (>= i n)

        ; перетворення списку у вектор для результату

        (list->vector acc)  

        (if (prime? (vector-ref v i))

            (__f (append acc (list(vector-ref v i))) (+ i 1))

            (__f acc (+ i 1))  )))

  (define n (vector-length v))

  (__f (list) 0)  )



; задання початкових даних

(define v (vector 2 7 4 6 11 12 17 18 23))

(define v_prime (f v))



; вивід

(display "Вектор: ") (display v) (newline)

(display "Вектор простих чисел: ") (display v_prime) (newline)

