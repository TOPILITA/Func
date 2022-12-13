

(define (make-queue)

 (define p (cons '() '() ) )

 (cons p p)

)

;Перевірка черги на пустоту

(define (null-queue? q)

 (and

  (eq? (front q) (rear q)) (eq? (car (front q)) '() ))

)

;селектор (доступ) до першого елемента черги

(define (front q)

 (car q)) 

;селектор (доступ) до останнього елемента черги

(define (rear q)

 (cdr q))

;додавання нового елемента в чергу

(define (push q e)

 (define p (cons e '()))

 (if (null-queue? q)

  (begin (set-car! q p)

   (set-cdr! q p)

  )

  (begin

   (set-cdr! (rear q) p)

   (set-cdr! q p)

  ) ) )

;вилучення елемента з черги

(define (pop q)

 (define x 0)

 (if (null-queue? q)

  'Empty    ; виведення повідомдення про пусту чергу

  (if (and (eq? (front q) (rear q))  (eq? '() (cdr (front q)))   )

   (begin

    (set! x (car (front q)))

    (set-car! (front q) '() )

    x ) 

   (begin

    (set! x (car (front q)))

    (set-car! q (cdr (front q)) )

    x ))))



; пошук мінімального та максимального значення у списку

(define (minim lst)

  (cond ((null? (cdr lst)) (car lst))

        ((< (car lst) (minim (cdr lst))) (car lst))

        (else (minim (cdr lst)))) )

(define (maxim lst)

  (cond ((null? (cdr lst)) (car lst))

        ((> (car lst) (maxim (cdr lst))) (car lst))

        (else (maxim (cdr lst)))) )



; вилучити зі списку його мінімальні елементи, усі інші зменшити на дану велечину

(define (filter-processing v)

  (define min-processing 0)

  (define (__filter-processing vv acc)

    (if (eq? (length vv) 0)

        acc

        (if (eq? (car vv) min-processing)

            (__filter-processing (cdr vv) acc)

            (__filter-processing (cdr vv) (append acc (list (- (car vv) min-processing))))  )))

  (if (not(eq? (length v) 0))

      (set! min-processing (minim v)))

    (__filter-processing v (list))  )



; заповнити список обробки із черги до наповнення із k елементів

(define (fill-processing processing waiting k)

    (if (or (eq? (length processing) k) (null-queue? waiting))

        processing

        (fill-processing (append processing (list (pop waiting))) waiting k)  ))



; обробка стану черги за один крок

(define (step-all processing waiting acc k)

  (define min-processing (minim processing))

  (if (null-queue? waiting)

      (+ acc (maxim processing))

      (step-all (fill-processing (filter-processing processing) waiting k) waiting (+ acc min-processing) k)  ))



; функція обчислення результату

(define (solve waiting k)

  (step-all (fill-processing (list) waiting k) waiting 0 k)  )



; задання к-сті кас і самої черги

(define waiting (make-queue))

(define k 3)

(push waiting 1)

(push waiting 2)

(push waiting 3)

(push waiting 2)

(push waiting 3)

(push waiting 2)

(display "Каси: ") (display k) (newline)

(display "Черга: ") (display waiting) (newline)

; результат

(define time (solve waiting k))

(display "Час: ") (display time) (newline)