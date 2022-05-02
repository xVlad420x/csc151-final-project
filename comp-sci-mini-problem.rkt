#lang racket

(define give-x-spaces
  (lambda (x)
    (give-x-letter x " ")))

(define give-x-letter
  (lambda (x letter)
    (if (zero? x)
        ""
        (string-append letter (give-x-letter (- x 1) letter)))))


(define is-numb-between-zero-and-my-numb-generator?
  (lambda (mynumb)
    (letrec ([my-numb-if-generator (lambda (mynumb spacecount)
                                     (if (zero? (+ 1 mynumb))
                                         (string-append (give-x-spaces (+ 2 spacecount)) "#f" (give-x-letter spacecount ")"))
                                         (string-append (give-x-spaces (+ 2 spacecount)) "(if (equal? " (number->string mynumb) " given-numb) \n"
                                                        (give-x-spaces (+ 3 spacecount)) "#t\n"
                                                        (my-numb-if-generator (- mynumb 1) (+ 1 spacecount)))))])
      (string-append "Create a procedure that returns true if given-numb is an integer between 0 and " (number->string mynumb) "\n\n"
                     "(define is-given-between-zero-and-" (number->string mynumb)"?\n"
                     " (lambda given-numb)\n"
                     (my-numb-if-generator mynumb 0)))))
              
;(displayln (is-numb-between-zero-and-my-numb-generator? 60))
;;;60 shouled be a random number between 5 and 20

(define string-cdr
  (lambda (str)
    (substring str 1 (string-length str))))

(define string-car
  (lambda (str)
    (substring str 0 1)))

(define string-ref
  (lambda (str index)
    (letrec ([string-ref-helper
              (lambda (str index cdr-count)
                (cond
                  [(> (- index 1) (string-length str))
                   (error "index larger than string length")]
                  [(equal? cdr-count index)
                   (substring str 0 1)]
                  [else
                   (string-ref-helper (string-cdr str) index (+ 1 cdr-count))]))])
      (string-ref-helper str index 0))))


;;;Create a procedure that adds str1 between each letter in str2. Str2 is 14 letters long.
(define add-str1-between-str2-generator
  (lambda (str-length)
    (letrec ([add-symbol-between-letters-helper
              (lambda (str-length currentlength currentstring x-spaces)
                (cond
                  [(equal? (- currentlength (+ 1 str-length)) 0)
                   (substring currentstring 0 (- (string-length currentstring) 1))];;base
                  [(equal?(- str-length currentlength) str-length)
                   (add-symbol-between-letters-helper str-length (+ currentlength 1)
                                                      (string-append (give-x-spaces x-spaces) "(string-append " (give-x-letter (- str-length currentlength) "(string-cdr ")
                                                                     "str2" (give-x-letter (+ 1 (- str-length currentlength))  ")") " str1)\n" currentstring)x-spaces)]
                  [else
                   (add-symbol-between-letters-helper str-length (+ currentlength 1)
                                                      (string-append (give-x-spaces x-spaces) "(string-append " "(string-car " (give-x-letter (- str-length currentlength) "(string-cdr ")
                                                                     "str2" (give-x-letter (+ 1 (- str-length currentlength))  ")") " str1)\n" currentstring)x-spaces)]))])
      (string-append "Create a procedure that adds str1 after each letter in str2. Str2 is "
                     (number->string str-length)
                     " letters long\n\n"
                     "(define string-cdr\n"
                     " (lambda (str)\n"
                     "  (substring str 1 (string-length str))))\n\n"
                     "(define string-car\n"
                     " (lambda (str)\n"
                     "  (substring str 0 1)))\n\n"
                     "(define add-str1-between-str2-letters\n"
                     " (lambda (str1 str2)\n"
                     "  (string-append\n"
                     (add-symbol-between-letters-helper (- str-length 1) 0 "" 4)
                     "))"))))

;;;(displayln(add-str1-between-str2-generator 10)) 10 should be a number between 7 and 12


;;; Given a sorted list of grades that is x long. Ex: List. Find the median grade


;;example code
(define find-median-grade
  (lambda (grade-list)
    (car
     (cdr
      (reverse (cdr (reverse
        (cdr
          (reverse (reverse(reverse (reverse(reverse (cdr (reverse
            (cdr
              (reverse (reverse
                (cdr grade-list)))))))))))))))))))

(define give-even-random
  (lambda (num)
    (let ([rando (random num)])
    (if (odd? rando)
        (give-even-random num)
        rando))))
#|
(define find-median-grade-generator
  (lambda (list-length currentlist)
    (cond
      [(and (even? list-length) (equal? (length currentlist) 2))
       "(/ (+ (car grade-list) (cadr grade-list)) 2)"
      [(and (odd? list-length) (equal? (length currentlist) 1))
       "(/ (car grade-list)"
|#
                     
