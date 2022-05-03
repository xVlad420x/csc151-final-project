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
                     " (lambda (given-numb)\n"
                     (my-numb-if-generator mynumb 0)
                     "))"))))
              
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

;;;(car one-elem-list)
;;; (/ (apply + one-elem-list) 2)
(define find-median-grade-generator
  (lambda (grade-list)
    (letrec ([find-median-grade-generator-helper
              (lambda (list-length currentlist spacecount parencount extraspaces)
                (let ([currentlen (length currentlist)]
                      [rando (give-even-random 10)])
                  (cond
                    [(and (even? list-length) (equal? currentlen 2))
                     (string-append "grade-list" (give-x-letter parencount ")"))];;basea
                    [(and (odd? list-length) (equal? currentlen 1))
                     (string-append "grade-list" (give-x-letter parencount ")"))];;baseb
                    [(or
                      (and (odd? list-length) (equal? currentlen 2))
                      (and (even? list-length) (equal? currentlen 3)))
                     (string-append (give-x-spaces extraspaces) (give-x-spaces spacecount) "(cdr " (find-median-grade-generator-helper list-length (cdr currentlist) (+ 1 spacecount)(+ 1 parencount)extraspaces))];;;no \n
                    [(or (and (even? list-length) (odd? currentlen))
                         (and (odd? list-length) (even? currentlen)))
                     (string-append (give-x-spaces extraspaces) (give-x-spaces spacecount) "(cdr\n" (find-median-grade-generator-helper list-length (cdr currentlist) (+ 1 spacecount)(+ 1 parencount) extraspaces))]
                    [(or (and (even? list-length) (even? currentlen))
                         (and (odd? list-length) (odd? currentlen)))
                     (string-append (give-x-spaces extraspaces) (give-x-spaces spacecount) (give-x-letter rando "(reverse ")
                                    "(reverse (cdr (reverse\n" (find-median-grade-generator-helper list-length (drop-right currentlist 1) (+ 1 spacecount)(+ parencount (+ 3 rando)) extraspaces))])))])
      (string-append "Given a sorted list of grades that is " (number->string (length grade-list)) " numbers long. Ex: " (list->a_string grade-list) ". Find the median grade\n\n"
                     "(define median-of-" (number->string (length grade-list)) "-long-grades\n"
                     " (lambda (grade-list)\n"
                     (if (even? (length grade-list))
                         (string-append (give-x-spaces 2) "(/ (apply + \n" (find-median-grade-generator-helper (length grade-list) grade-list 0 0 3) ") 2)")
                         (string-append (give-x-spaces 2) "(car \n" (find-median-grade-generator-helper (length grade-list) grade-list 0 0 3) ")"))
                     "))")
      )))

(define give-grades
  (lambda (list-length min max)
    (build-list list-length (lambda (x) (+ min (random (+ 1 (- max min))))))))

(define list->a_string
  (lambda (lst)
    (letrec ([list->a_string-helper
              (lambda (lst)
                (if (null? lst)
                    ""
                    (string-append (number->string (car lst)) " " (list->a_string-helper (cdr lst)))))])
      (string-append "(list " (substring (list->a_string-helper lst)  0 (- (string-length (list->a_string-helper lst)) 1)) ")"     ))))



;;; (random-list-element lst) -> any?
;;;   lst : list? (nonempty)
;;; Randomly select an element of `lst` (This is SamR's procedure from some project but I probably could've wrote something similar myself)
(define random-list-element
  (lambda (lst)
    (list-ref lst (random (length lst)))))


(define part-c
  (lambda ()
    (find-median-grade-generator (sort (give-grades (+ 6 (random 11)) 50 100) <))))

(define part-b
  (lambda ()
    (add-str1-between-str2-generator (+ 6 (random 7)))))

(define part-a
  (lambda ()
    (is-numb-between-zero-and-my-numb-generator? (+ 6 (random 12)))))
      
(define part-d
  (lambda ()
    (random-list-element (list
     (string-append "Write a regular expression, ab, that matches nonempty words that consist only of the letters\n"
     "lowercase a and lowercase b and that start and end with the same letter.\n\n"
                    "(define ab\n"
                    " (rex-any-of\n"
                    "  (rex-concat (rex-string " "a" ")\n"
                    "         (rex-repeat-0 (rex-char-range " "#\a"  "#\b" "))\n"
                    "         (rex-string " "a" "))\n"
                    "  (rex-concat (rex-string " "b" ")\n" 
                    "         (rex-repeat-0 (rex-char-range " "#\a" "#\b" "))\n"
                    "         (rex-string " "b" "))\n"
                    "  (rex-string " "a" ")\n"
                    "  (rex-string " "b" ")))\n")
     (string-append "Write a tail recursive procedure called replicate that takes inputs n and x.\n"
     "Replicate should create a list with x inside the list n times (so it is n elements long).\n\n"
                    "(define replicate\n"
                    "  (lambda (n x)\n"
                    "    (letrec ([replicate-helper\n"
                    "              (lambda (n x so-far)\n"
                    "                (if (zero? n)\n"
                    "                    so-far\n"
                    "                    (replicate-helper (- n 1) x (cons x so-far))))])\n"
                    "      (replicate-helper n x '()))))\n")
     (string-append "Write a procedure that finds the largest number in a vector\n\n"
                    "(define number-vector-largest/helper\n"
                    " (lambda (vec pos)\n"
                    "   (let ([current (vector-ref vec pos)])\n"
                    "     (if (zero? pos)\n"
                    "         current\n"
                    "         (max current\n"
                    "              (number-vector-largest/helper vec (- pos 1)))))))\n\n"
                    "(define number-vector-largest\n"
                    " (lambda (vec)\n"
                    "   (let ([last (- (vector-length vec) 1)])\n"
                    "     (number-vector-largest/helper vec last))))\n")
     (string-append "Write the append procedure yourself. Append puts two lists together into one.\n\n"
                    "(define append\n"
                    " (lambda (l1 l2)\n"
                    "   (match l1\n"
                    "     ['() l2]\n"
                    "     [(cons x tail) (cons x (append tail l2))])))\n")
     (string-append "Write the length procedure yourself. Length returns a number which corresponds to the length of a list.\n\n"
                    "(define length\n"
                    " (lambda (lst)\n"
                    "   (if (null? lst)\n"
                    "       0\n"
                    "       (+ 1 (length (cdr lst))))))\n")
     (string-append "Write a procedure that sums all the values of a binary tree consisting of only numbers\n\n"
                    "(define binary-tree-sum\n"
                    " (lambda (tree)\n"
                    "   (if (empty-tree? tree)\n"
                    "       0\n"
                    "       (+ (bt/t tree)\n"
                    "          (binary-tree-sum (bt/l tree))\n"
                    "          (binary-tree-sum (bt/r tree))))))\n")
   

                    ))))


(define comp-sci-minigame-helper
  (lambda (answerlst lst2 index)
          (cond
            [(null? answerlst)
                    ""]
            [(equal? (car answerlst) "a")
             (string-append "ANSWER CHOICE " (list-ref lst2 index) ")\n" (part-a) "\n\n\n" (comp-sci-minigame-helper (cdr answerlst) lst2 (+ 1 index)))]
            [(equal? (car answerlst) "b")
             (string-append "ANSWER CHOICE " (list-ref lst2 index) ")\n" (part-b) "\n\n\n" (comp-sci-minigame-helper (cdr answerlst) lst2 (+ 1 index)))]
            [(equal? (car answerlst) "c")
             (string-append "ANSWER CHOICE " (list-ref lst2 index) ")\n" (part-c) "\n\n\n" (comp-sci-minigame-helper (cdr answerlst) lst2 (+ 1 index)))]
            [(equal? (car answerlst) "d")
             (string-append "ANSWER CHOICE " (list-ref lst2 index) ")\n" (part-d) "\n\n\n" (comp-sci-minigame-helper (cdr answerlst) lst2 (+ 1 index)))])))

(define create-random-set-abcd
  (lambda (lst);;;should be (list a b c d)
    
     (if (null? lst)
        null
        (let ([rando-element (random-list-element lst)])
        (cons rando-element (create-random-set-abcd (remove rando-element lst)))))))

(define introtext
  (string-append "Here are 4 procedures that are syntactically correct and accomplish the task listed.\n"
                 "Although all procedures are functional, (in both senses) 3 of them are coded very poorly.\n"
                 "Choose the answer choice with the procedure that wouldn't make SamR cry (the one that is coded in the best style)\n"
                 "Give the answer in this format: e\n\n\n"))
                 

(define comp-sci-game
  (lambda ()
    (let* ([options (list "A" "B" "C" "D")]
           [order (create-random-set-abcd (list "a" "b" "c" "d"))]
           [correct-answer-index (index-of order "d")])
    (displayln (string-append introtext (comp-sci-minigame-helper order options 0)))
    (define player-choice (string-upcase (read-line)))
      (cond
        [(equal? correct-answer-index (index-of options player-choice))
         (displayln "right answer (next level command)")]
        [else
         (displayln "wrong answer (go back a level)")]))))

;;;(comp-sci-game)
