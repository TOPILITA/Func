
(define (GetNumerator q) (car q))
(define (GetDenominator q) (cdr q))
(define (Rational n d) (cons n d))

(define (GetCommonDenom lst)
 (define (Iter lst common)
   (cond
     ((null? lst) common)
     (else
      (Iter (cdr lst) (lcm common (GetDenominator (car lst)))))))
 (cond
   ((null? lst) 0)
   ((not (list? lst)) "Очікується переданим аргументов бути список!")
   ((= 1 (length lst)) (GetDenominator (car lst)))
   (else (Iter lst 1))))

(define (ToCommonDenom lst)
 (define (CreateNewList oldLst commonDenom)
   (cond
     ((null? oldLst) '())
     (else
      (cons
       (Rational (* (GetNumerator (car oldLst)) (/ commonDenom (GetDenominator (car oldLst)))) commonDenom)
       (CreateNewList (cdr oldLst) commonDenom)))))
 (let ((commonDenom (GetCommonDenom lst)))
   (CreateNewList lst commonDenom)))

(define (RemoveIf predicate lst)
 (define (Filter predicate lst)
   (if (null? lst) '()
       (if (predicate (car lst))
           (cons (car lst) (Filter predicate (cdr lst)))
           (Filter predicate (cdr lst)))))
 (Filter (lambda (x) (not (predicate x))) lst))

(define (SortByDescending lst)
 (define GetMaxByNumer
   (lambda (lst maxRat)
     (cond
       ((null? lst) maxRat)
       ((> (GetNumerator (car lst)) (GetNumerator maxRat))
        (GetMaxByNumer (cdr lst) (car lst)))
       (else
        (GetMaxByNumer (cdr lst) maxRat)))))
 (cond
   ((null? lst) '())
   (else
    (let ((first (GetMaxByNumer lst (car lst))))
      (cons first (SortByDescending (RemoveIf (lambda (x) (equal? x first)) lst)))))))

(define (DisplayList lst)
 (define (DisplayRational x)
   (display (GetNumerator x))
   (display "/")
   (display (GetDenominator x)))
 (map
  (lambda (rat) (DisplayRational rat) (display " "))
  lst)
 (newline))
(define (main)
 (let ((lst (list (Rational 7 20) (Rational 1 5) (Rational 3 10))))
   (display "Створений список: ")
   (DisplayList lst)
   (set! lst (ToCommonDenom lst))
   (set! lst (SortByDescending lst))
   (display "Відсортовані за спаданням: ")
   (DisplayList lst)))

(main)