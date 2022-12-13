#lang racket
; запис рядків у файл input.txt

(let ((port (open-output-file "C://New folder//output.txt")))

(write "Hello world!" port)

(write "It's my first program" port)

(close-output-port port))



; функція добавлення слова на початок рядка

(define (new-str s word)

  (string-append word s))



; введення слова 

(display "Введіть слово: ")

(define uword (read-line))



; читання із попереднього файлу

(define in (open-input-file "C://New folder//input.txt"))

(define s1 (read in))

(define s2 (read in))

(close-input-port in)



; змінна рядків

(display s1) (newline)

(set! s1 (new-str s1 uword))

(display s1)

(display "\n-------------------------------\n")



(display s2) (newline)

(set! s2 (new-str s2 uword))

(display s2)

(display "\n-------------------------------\n")



; запис резульату у файл output.txt

(let ((port (open-output-file "C://New folder//output.txt")))

(write s1 port)

(write s2 port)

(close-output-port port))