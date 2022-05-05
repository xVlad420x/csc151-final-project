#lang racket
(require csc151)
(require 2htdp/image)
(provide (all-defined-out))
(require rackunit)

;;;returns a list containing a y-height of jump, x min distance of jump and slab length
;;;y should be between 100 and 1000, x should be between 100 and 1000 and slab should be between 1 and 10
(define give-physics-data
  (lambda ()
    (list (- 1000 (random 901))
          (- 1000 (random 901))
          (- 10 (random 10)))))

;;;returns the necessary x-velocities (xvelmin and xvelmax) in order to make it onto the slab
(define hit-slab
  (lambda (lst) ;;;lst containing y,x and slab (from give-physivs-data)
    (let* ([dis-y (car lst)]
           [dis-x (cadr lst)]
           [dis-slab (caddr lst)]
           [flight-time (sqrt (/ dis-y 49/10))]) ;;; y = 0.5*a*t^2, a = 9.8 gravity
      (list (/ dis-x flight-time) ;;; v = d/t
            (/ (+ dis-x dis-slab) flight-time)))))


(define physics-mini-game-helper
  (lambda (lst)
    (string-append "We are standing on a " (number->string (list-ref lst 0)) " meter high ledge.\n"
                   "How fast do we have to travel in m/s in order to clear a " (number->string (list-ref lst 1))
                   " meter long gap in front of us\n" "and land onto a " (number->string (list-ref lst 2))
                   " meter long slab?\n"
                   "Assume gravity is 9.8m/s^2 and give the answer as a number (with or without decimals)\n")))


(define physics-results
  (lambda (num answer skin-color shirt-color pants-color weapon the-story) ;;;num is a numb while answer is a list of 2 elems
    (cond
      [(and
        (<= (floor (car answer)) num)
        (>= (ceiling (cadr answer)) num))
       (displayln "Right answer!\n\n")
       (displayln (list-ref the-story 3))
       (displayln (level-0-win skin-color shirt-color pants-color weapon))
       (chem-game skin-color shirt-color pants-color weapon the-story)]
      [else
       (displayln "YOU'RE DEAD\n\n")
       (displayln (list-ref the-story 2))
       (displayln (level-0-lose skin-color shirt-color pants-color weapon))])))


(define physics-game
  (lambda (skin-color shirt-color pants-color weapon the-story)
    (let* ([options (give-physics-data)]
           [answer (hit-slab options)])
      (displayln (list-ref the-story 1))
      (displayln level-0)
      (displayln (physics-mini-game-helper options))
      (define player-choice (cheat-menu (read-line) skin-color shirt-color pants-color weapon the-story))
      (cond
        [(number? (string->number player-choice))
         (physics-results (string->number player-choice) answer skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "answer")
         (displayln (string-append "The answer is any number between " (number->string (floor (car answer))) " and " (number->string (ceiling (cadr answer))) "\n"))
         (physics-results (numbercheck) answer skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "retry")
         (physics-game skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "teleport")
         (teleport-menu skin-color shirt-color pants-color weapon the-story)]
        [else
         (displayln "Your answer must be a number\n")
         (physics-results (numbercheck) answer skin-color shirt-color pants-color weapon the-story)]))))

;Definitions
(define periodic-table-elements
  (file->lines "elements.txt"))

(define periodic-table-noble-gases
  (list "He - Helium," "Ne - Neon," "Ar - Argon," "Kr - Krypton," "Xe - Xenon," "Rn - Radon,"))

(define periodic-table-elements-without-noble-gases
  (filter  (o not (section member? <> periodic-table-noble-gases)) periodic-table-elements))

;test periodic-table-elements-without-noble-gases

;(test-equal? "brief check to see if elements were removed by looking at length"
;             (length periodic-table-elements-without-noble-gases)
;             (- (length periodic-table-elements)
;                (length periodic-table-noble-gases)))

;RANDOM PROCEDURES

;;; (random-list-element lst) -> any?
;;;   lst : listof any?
;;; Randomly select an element of `lst`
(define random-list-element
  (lambda (lst)
    (list-ref lst (random (length lst)))))


;;; (random-periodic-element) -> any?
;;; Randomly select an element from 'periodic-table-elements-without-noble-gases'
(define random-periodic-element
  (lambda ()
    (random-list-element periodic-table-elements-without-noble-gases)))

#|
The following random procedures produces a random list of 8 elements for the game
|#
;;; (random-periodic-elements) -> any?
;;;  n: integer?
;;; randomly builds a list of n amount of 'periodic-table-elements-without-noble-gases'
(define random-periodic-elements
  (lambda (n)
    (if (zero? n)
        null
        (cons (random-periodic-element) (random-periodic-elements (- n 1))))))

(define fix-duplicate-noble-gases-helper
  (lambda (list-8-elements occurences)
    ;(displayln (car list-8-elements))
    ;(displayln occurences)
    (cond
      [(null? list-8-elements)
       null]
      [(member? (car list-8-elements) periodic-table-elements-without-noble-gases)
       (cons (car list-8-elements) (fix-duplicate-noble-gases-helper (cdr list-8-elements) occurences))]
      [(and (not (member? (car list-8-elements) periodic-table-elements-without-noble-gases))
            (= occurences 0))
       (cons (car list-8-elements) (fix-duplicate-noble-gases-helper (cdr list-8-elements) (+ 1 occurences)))]
      [(and (not (member? (car list-8-elements) periodic-table-elements-without-noble-gases)) (> occurences 0))
       (cons (random-periodic-element) (fix-duplicate-noble-gases-helper (cdr list-8-elements) (+ 1 occurences)))])))

(define fix-duplicate-noble-gases
  (lambda (list-8-elements)
    (fix-duplicate-noble-gases-helper list-8-elements 0)))

          
  
  

;produces a list of 8 random 'periodic-table-elements'
(define 8-random-elements
  (lambda ()
    (random-periodic-elements 8)))

;produces a random noble gas element from 'periodic-table-noble-gases'
(define random-noble-gas
  (lambda ()
    (random-list-element periodic-table-noble-gases)))
  




;;; (replace-all old new lst) -> list?
;;;  old: val?
;;;  new: val?
;;;  lst: list?
;;; takes two values and a list as parameters
;;; and replaces all instances of old by new.
(define replace-all
  (lambda (old new lst)
    (cond
      [(null? lst)
       (list)]
      [(equal? (car lst) old)
       (cons new (replace-all old new (cdr lst)))]
      [else
       (cons (car lst) (replace-all old new (cdr lst)))])))

;;; (8-random-elements-including-noble-gases) -> any?
;;; creates 8-random-elements-including-noble-gases and
;;; replaces a selected-value from that list of 8-random-table-elements
;;; with a random noble gas so that there will always be at least one noble gas
;;; in the question.
(define 8-random-elements-including-noble-gases
  (lambda()
    (let* ([8-random-table-elements (8-random-elements)]
           [selected-val (random-list-element 8-random-table-elements)])
      (replace-all selected-val (random-noble-gas) (fix-duplicate-noble-gases 8-random-table-elements)))))

(define my-list '("Pm - Promethium,"
                  "He - Helium,"
                  "He - Helium,"
                  "He - Helium,"
                  "Ir - Iridium,"
                  "He - Helium,"
                  "La - Lanthanum,"
                  "Og - Oganesson,"))

;tests

;;; (random-experiment rproc val n) -> integer?
;;;    rproc : zero-parameter-procedure?
;;;    val : any?
;;;    n : integer?
;;;    so-far : how many instances rproc equals 'val' 
;;; Runs `rproc` n times and counts how many times it equals `val`
(define random-experiment-helper
  (lambda (rproc val n so-far)
    (cond
      [(zero? n)
       so-far]
      [(equal? (rproc) val)
       (random-experiment-helper rproc val (- n 1) (+ 1 so-far))]
      [else
       (random-experiment-helper rproc val (- n 1) so-far)])))

;;; (random-experiment rproc val n) -> integer?
;;;    rproc : zero-parameter-procedure?
;;;    val : any?
;;;    n : integer?
;;; Runs `rproc` n times and counts how many times it equals `val`
;;; tail recurive.
(define random-experiment
  (lambda (rproc val n)
    (random-experiment-helper rproc val n 0)))

;;; (contains-noble-gas? lst) -> lst
;;; lst: list
;;; checks to see if a list contains an element from 'periodic-table-noble-gases'
(define contains-noble-gas?
  (lambda (lst)
    (= 1 (tally (section member? <> periodic-table-noble-gases) lst))))

(define contains-more-than-1-gas?
  (lambda (lst)
    (= 1 (tally (section member? <> periodic-table-noble-gases) lst))))

;random procedure used for the 'random-experiment test'
(define experiment
  (lambda ()
    (contains-noble-gas? (fix-duplicate-noble-gases (8-random-elements-including-noble-gases)))))

;the question always contains a noble gas
(test-equal? "always contains a noble gas"
             (random-experiment experiment #t 100)
             100)

;the question is given a random list of periodic table elements(8-random-elements-including-noble-gases),
; choose the noble gas to pass the level

;"insert script" options:
;(displayln (8-random-elements-including-noble-gases))
;instructions:


(define grab-correct-noble
  (lambda (8-list)
    (cond
      [(null? 8-list)
       "This shouldn't happen"]
      [(member? (car 8-list) periodic-table-noble-gases)
       (car 8-list)]
      [else
       (grab-correct-noble (cdr 8-list))])))



(define chem-results
  (lambda (player-choice 8-list skin-color shirt-color pants-color weapon the-story) ;;;num is a numb while answer is a list of 2 elems
    (cond
      [(and (member? player-choice 8-list)(member? player-choice periodic-table-noble-gases))
       (displayln "Right answer!\n\n")
       (displayln (list-ref the-story 6))
       (displayln (level-1-win  skin-color shirt-color pants-color weapon))
       (math-game skin-color shirt-color pants-color weapon the-story)]
      [else
       (displayln "Wrong answer!\n\n")
       (displayln (list-ref the-story 5))
       (displayln level-1-lose)
       (physics-game skin-color shirt-color pants-color weapon the-story)])))


(define chem-game
  (lambda (skin-color shirt-color pants-color weapon the-story)
    (let*([8-random-table-elements (8-random-elements)]
          [selected-val (random-list-element 8-random-table-elements)]
          [8-random-elements-including-noble-gases (replace-all selected-val (random-noble-gas) 8-random-table-elements)]
          [8-list (fix-duplicate-noble-gases 8-random-elements-including-noble-gases)])
      (displayln (list-ref the-story 4))
      (displayln level-1)
      (displayln 8-list)
      (displayln "which of the following is a noble gas?")
      (displayln "Type the answer below matching the case and comma. (Ex. O - Oxygen,)")
      (define player-choice (cheat-menu (read-line) skin-color shirt-color pants-color weapon the-story))
      (cond
        [(equal? player-choice "answer")
         (displayln (string-append "The correct answer is " (grab-correct-noble 8-list) "\n"))
         (chem-results (read-line) 8-list skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "retry")
         (chem-game skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "teleport")
         (teleport-menu skin-color shirt-color pants-color weapon the-story)]
        [else
         (chem-results player-choice 8-list skin-color shirt-color pants-color weapon the-story)]))))

(define roulette-slices
  (lambda ()
    (let ([rando (random 13 102)])
      (if (odd? rando)
          rando
          (roulette-slices)))))


;;;0 is considered green ;;;odd numbers are red ;;; even numbers are black

(define give-number-and-opposite-col
  (lambda (num-slices)
    (let ([rando (random 1 num-slices)])
      (list
       rando
       (if (odd? rando)
           "black"
           "red")))))

(define give-spins
  (lambda ()
    (random 1 11)))

  
;;;math-game-calculator
(define math-game-helper
  (lambda (slices question-lst spins)
    (string-append "In our roulette, there are "
                   (number->string slices)
                   " numbers (including zero).\n"
                   "Zero is green, odd numbers are red, and even numbers are black.\n"
                   "Consider the event of landing on the number "
                   (number->string (car question-lst))
                   " OR landing on a "
                   (cadr question-lst)
                   " number\n"
                   "What is the probability of this event happening "
                   (number->string spins)
                   " times in a row?\n"
                   "Give your answer as a fraction or a decimal rounded to the nearest 10,000th")))

(define math-game-calculator
  (lambda (slices spins)
    (let ([chance-once (+ (/ 1 slices) (/ (/ (- slices 1) 2) slices))])
      (expt chance-once spins))))

(define floor-10000th
  (lambda (num)
    (/ (floor (* num 10000)) 10000)))

(define ceiling-10000th
  (lambda (num)
    (/ (ceiling (* num 10000)) 10000)))

(define math-results
  (lambda (num answer skin-color shirt-color pants-color weapon the-story) ;;;num is a numb while answer is a list of 2 elems
    (cond
      [(and (>= (ceiling-10000th answer) num)
            (<= (floor-10000th answer) num))
       (displayln "Right answer!\n\n")
       (displayln (list-ref the-story 9))
       (displayln (level-2-win skin-color shirt-color pants-color weapon))
       (comp-sci-game skin-color shirt-color pants-color weapon the-story)]
      [else
       (displayln "Wrong answer!\n\n")
       (displayln (list-ref the-story 8))
       (displayln (level-2-lose skin-color shirt-color pants-color weapon))
       (chem-game skin-color shirt-color pants-color weapon the-story)])))


(define math-game
  (lambda (skin-color shirt-color pants-color weapon the-story)
    (let* ([slices (roulette-slices)]
           [question-lst (give-number-and-opposite-col slices)]
           [spins (give-spins)]
           [answer (math-game-calculator slices spins)])
      (displayln (list-ref the-story 7))
      (displayln level-2)
      (displayln (math-game-helper slices question-lst spins))
      (define player-choice (cheat-menu (read-line) skin-color shirt-color pants-color weapon the-story))
      (cond
        [(number? (string->number player-choice))
         (math-results (string->number player-choice) answer skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "answer")
         (displayln (string-append "The answer is any number between " (number->string (floor-10000th answer)) " and " (number->string (ceiling-10000th answer)) "\n"))
         (math-results (numbercheck) answer skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "retry")
         (math-game skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "teleport")
         (teleport-menu skin-color shirt-color pants-color weapon the-story)]
        [else
         (displayln "Your answer must be a number\n")
         (math-results (numbercheck) answer skin-color shirt-color pants-color weapon the-story)]))))

;formula = ((1/slices + ((slices - 1)/2)/slices)^times)


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

(define comp-sci-results
  (lambda (player-choice correct-answer-index options skin-color shirt-color pants-color weapon the-story)
    (cond
      [(equal? correct-answer-index (index-of options player-choice))
       (displayln "Right answer!\nYou escaped Noyce!\n")
       (displayln (list-ref the-story 12))
       (displayln (level-3-win skin-color shirt-color pants-color weapon))
       ;(exit)
       ]
      [else
       (displayln "Wrong answer!\n\n")
       (displayln (list-ref the-story 11))
       (displayln level-3-lose)
       (math-game skin-color shirt-color pants-color weapon the-story)])))
                 

(define comp-sci-game
  (lambda (skin-color shirt-color pants-color weapon the-story)
    (let* ([options (list "a" "b" "c" "d")]
           [order (create-random-set-abcd (list "a" "b" "c" "d"))]
           [correct-answer-index (index-of order "d")])
      (displayln (list-ref the-story 10))
      (displayln (level-3 skin-color shirt-color pants-color weapon))
      (displayln (string-append introtext (comp-sci-minigame-helper order options 0)))
      (define player-choice (cheat-menu (string-downcase (read-line)) skin-color shirt-color pants-color weapon the-story))
      (cond
        [(equal? player-choice "answer")
         (displayln (string-append "The correct answer is " (list-ref options correct-answer-index) "\n"))
         (comp-sci-results (string-downcase (read-line)) correct-answer-index options skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "retry")
         (comp-sci-game skin-color shirt-color pants-color weapon the-story)]
        [(equal? player-choice "teleport")
         (teleport-menu skin-color shirt-color pants-color weapon the-story)]
        [else
         (comp-sci-results player-choice correct-answer-index options skin-color shirt-color pants-color weapon the-story)]))))

;;;(comp-sci-game)
;;;image code


(define sword
  (rotate -135 (above (triangle 30 'solid "silver")
                      (rectangle 30 120 'solid "silver")
                      (rectangle 50 20 'solid "black")
                      (rectangle 10 30 'solid "black"))))
(define spear
  (above (triangle 30 'solid "silver")
         (overlay/align 'center 'top (rotate 180 (triangle 30 'solid "silver"))
                        (rectangle 10 200 'solid "beige"))))
(define bow
  (rotate -75 (wedge 60 150 'outline (pen "brown" 8 'solid 'projecting 'round))))

(define choose-your-weapon
  (beside sword spear bow))

(define person
  (lambda (skin-color shirt-color pants-color weapon)
    (let ([base (above (circle 25 'solid skin-color)
                       (above (rectangle 140 20 'solid shirt-color)
                              (rectangle 40 50 'solid shirt-color))
                       (rectangle 40 80 'solid pants-color))])
      (cond
        [(equal? weapon sword)
         (overlay base weapon)]
        [(equal? weapon spear)
         (beside/align 'bottom base weapon)]
        [(equal? weapon bow)
         (beside base weapon)]
        [else
         (error "invalid weapon!")]))))

(define scene-1
  (lambda (skin-color shirt-color pants-color weapon)
    (overlay/align 'center 'bottom (above (scale 0.5 (person skin-color shirt-color pants-color weapon))
                                          (rectangle 500 50 'solid "silver"))
                   (rectangle 500 300 'solid "black"))))

(define level-0
  (overlay (above (rotate 120 (rhombus 90 60 'solid "black"))
                  (rotate -120 (rhombus 90 60 'solid "black"))
                  (rotate 120 (rhombus 90 60 'solid "black"))
                  (rotate -120 (rhombus 90 60 'solid "black")))
           (rectangle 500 300 'solid "silver")))

(define level-0-lose
  (lambda (skin-color shirt-color pants-color weapon)
    (overlay/align 'center 'top (above (text "A" 24 "white")
                                       (text "A" 24 "white")
                                       (text "A" 24 "white")
                                       (text "A" 24 "white")
                                       (text "A" 24 "white")
                                       (text "A" 24 "white")
                                       (text "A" 24 "white")
                                       (text "A" 24 "white")
                                       (scale .25 (person skin-color shirt-color pants-color weapon)))
                   (overlay (rectangle 200 300 'solid "black")
                            (rectangle 500 300 'solid "silver")))))

(define level-0-win
  (let* ([small-gray (circle 10 'solid "gray")]
         [medium-red (circle 15 'solid "red")]
         [large-black (circle 20 'solid "black")]
         [warp-top-left (scale 3 (overlay/align 'left 'top small-gray medium-red large-black))]
         [warp-bottom-left (scale 3 (overlay/align 'left 'bottom small-gray medium-red large-black))]
         [warp-top-right (scale 3 (overlay/align 'right 'top small-gray medium-red large-black))]
         [warp-bottom-right (scale 3 (overlay/align 'right 'bottom small-gray medium-red large-black))])
    (lambda (skin-color shirt-color pants-color weapon)
      (overlay (person skin-color shirt-color pants-color weapon) (overlay/align 'right 'bottom warp-bottom-right
                                                                                 (overlay/align 'right 'top warp-top-right
                                                                                                (overlay/align 'left 'bottom warp-bottom-left
                                                                                                               (overlay/align 'left 'top warp-top-left
                                                                                                                              (rectangle 500 300 'solid "lightpink")))))))))

(define level-1
  (overlay (rectangle 500 300 60 "green")(overlay (scale 2.5 (beside (rectangle 30 40 'solid 'grey)
                                                                     (rectangle 10 40 0 'white)
                                                                     (rectangle 30 40 'solid 'grey)
                                                                     (rectangle 10 40 0 'white)
                                                                     (rectangle 30 40 'solid 'grey)
                                                                     (rectangle 10 40 0 'white)
                                                                     (rectangle 30 40 'solid 'grey)
                                                                     (rectangle 10 40 0 'white)
                                                                     (rectangle 30 40 'solid 'grey)))
                                                  (rectangle 500 300 'solid "beige"))))

(define level-1-lose
  (overlay (overlay (star-polygon 25 30 7 'solid "yellow") (star-polygon 40 30 7 'solid "orange"))
           (above (rectangle 300 200 'solid "brown")
                  (rectangle 500 50 'solid "black"))))
(define level-1-win
  (lambda (skin-color shirt-color pants-color weapon)
    (beside/align 'bottom (rectangle 60 60 'solid "black")
                  (rectangle 60 120 'solid "black")
                  (above (scale .3 (person skin-color shirt-color pants-color weapon))
                         (rectangle 60 180 'solid "black"))
                  (rectangle 60 240 'solid "black")
                  (rectangle 60 300 'solid "black"))))
                  
;;INTRODUCTORY IMAGE FOR LEVEL 2


;;  futuristic space ship

(define floor1-strct (rectangle 100 50 'solid "Gainsboro"))
(define window (beside
                (rectangle 8 30 'solid "Gainsboro")
                (rectangle 10 30 'solid "royal blue")
                (rectangle 8 30 'solid "Gainsboro")))
(define pair-of-windows (beside
                         window window))
(define windows (beside
                 pair-of-windows pair-of-windows))
(define floor1 (overlay windows floor1-strct))
(define building
  (above
   floor1 floor1 floor1 floor1 floor1 floor1 floor1))


(define red-window (beside
                    (rectangle 8 30 'solid "Gainsboro")
                    (rectangle 10 30 'solid "red")
                    (rectangle 8 30 'solid "Gainsboro")))
(define pair-of-red-windows (beside
                             red-window red-window))
(define red-windows (beside
                     pair-of-red-windows pair-of-red-windows))

(define red-floor (overlay red-windows floor1-strct))

(define red-wall (above
                  red-floor red-floor red-floor red-floor red-floor red-floor red-floor))


(define walls
  (above
   (rotate -90 building)
   (overlay
    (radial-star 32 30 40 "outline" "white")
    (rectangle 350 80 'solid "Gainsboro"))
   (rotate -90 red-wall)
   (overlay
    (radial-star 32 30 40 "outline" "white")
    (rectangle 350 80 'solid "Gainsboro"))
   (rotate -90 building)))


(define arm-1
  (lambda (color)
    (rectangle 20 20 'solid color)))



(define arm-2
  (lambda (color)
    (rectangle 15 182 'solid color)))

(define legs
  (beside (rectangle 15 100 'solid "olive")
          (rectangle 20 100 'solid "Gainsboro")
          (rectangle 15 100 'solid "olive")))

(define eyes
  (lambda (color)
    (beside
     (overlay
      (square 10  'solid  color)
      (square 20 'solid "black"))
     (rectangle 15 4 'solid "black")
     (overlay
      (square 10  'solid  color)
      (square 20 'solid "black")))))


(define face-no-mouth
  (lambda (color)
    (overlay
     (eyes color)
     (circle 36 'solid color))))


(define mouth-beard
  (above
   (rectangle 58 8 'solid "Dim Gray")
   (overlay/align 'center 'bottom
                  (rectangle 20 5 'solid "red")
                  (rectangle 45 8 'solid "Dim Gray"))
   (rectangle 35 4 'solid "Dim Gray")))

(define old-man-face-1
  (lambda (color)
    (overlay/align 'center 'bottom
                   mouth-beard
                   (face-no-mouth color))))



(define old-man-face
  (lambda (color)
    (above
     (old-man-face-1 'pink)
     (rectangle 30 4 'solid "Dim Gray")
     (rectangle 25 4 'solid "Dim Gray")
     (rectangle 24 4 'solid "Dim Gray")
     (rectangle 21 4 'solid "Dim Gray")
     (rectangle 18 4 'solid "Dim Gray")
     (rectangle 16 4 'solid "Dim Gray"))))
         
(define old-man-chest
  (above
   (triangle 30 'solid "olive")
   (old-man-face 'pink)
   (overlay/align 'center 'top (above
                                (rectangle 17 4 'solid "Dim Gray")
                                (rectangle 16 4 'solid "Dim Gray")
                                (rectangle 14 4 'solid "Dim Gray"))
                  (rectangle 50 150 'solid 'olive))))


(define old-man-body
  (above
   (beside/align 'center
                 (arm-1 "olive")
                 old-man-chest
                 (arm-1 "olive"))
   legs))

(define old-man
  (beside/align 'top
                old-man-body
                (rectangle 15 162 'solid "white")))

(define level-2
  (overlay/align 'center 'bottom
                 old-man
                 walls))


(define level-2-win
  (lambda (skin-color shirt-color pants-color weapon)
    (beside/align 'bottom old-man (person skin-color shirt-color pants-color weapon)
                  (overlay/align 'center 'bottom (rectangle 200 20 'solid 'blue)
                                 (overlay/align 'center 'top (rectangle 200 20 'solid 'blue)
                                                (overlay (overlay/align 'left 'center (rectangle 10 100 'solid 'black)(rectangle 100 150 'solid 'blue))
                                                         (rectangle 200 300 'solid 'silver)))))))

(define level-2-lose
  (lambda (skin-color shirt-color pants-color weapon)
    (overlay (person skin-color shirt-color pants-color weapon)
             (star-polygon 7 30 7 'solid "white")
             (star-polygon 15 30 7 'solid "skyblue")
             (star-polygon 25 30 7 'solid "blue"))))

(define level-3
  (lambda (skin-color shirt-color pants-color weapon)
    (beside/align 'bottom (scale .5 (person skin-color shirt-color pants-color weapon))
                  (rectangle 100 300 0 "white")
                  (above (overlay (rectangle 300 40 'solid "palegreen")
                                  (rectangle 375 60 'solid "green"))
                         (overlay (rectangle 200 40 'solid "palegreen")
                                  (rectangle 275 60 'solid "green"))
                         (overlay (rectangle 100 40 'solid "palegreen")
                                  (rectangle 150 60 'solid "green"))
                         (overlay (rectangle 40 40 'solid "palegreen")
                                  (rectangle 50 60 'solid "green"))
                         (overlay(rectangle 10 40 'solid "palegreen")
                                 (rectangle 16 60 'solid "green"))))))

(define level-3-win
  (lambda (skin-color shirt-color pants-color weapon)
    (overlay/align 'left 'top (circle 30 'solid 'yellow)
                   (above (scale .1 (person skin-color shirt-color pants-color weapon)) (overlay/align 'center 'top (beside (rectangle 20 75 'solid 'silver)
                                                                                                                            (rectangle 48 75 'solid 'skyblue)
                                                                                                                            (rectangle 20 75 'solid 'silver)
                                                                                                                            (rectangle 48 75 'solid 'skyblue)
                                                                                                                            (rectangle 20 75 'solid 'silver)
                                                                                                                            (rectangle 48 75 'solid 'skyblue)
                                                                                                                            (rectangle 20 75 'solid 'silver)
                                                                                                                            (rectangle 48 75 'solid 'skyblue)
                                                                                                                            (rectangle 20 75 'solid 'silver))(overlay (text "Noyce" 30 'silver) (rectangle 300 200 'solid "brown")))
                          (rectangle 500 50 'solid "black")))))

(define level-3-lose
  (above (overlay/align 'center 'bottom (scale .5 (overlay/align 'center 'top (beside (rectangle 20 75 'solid 'silver)
                                                                                      (rectangle 48 75 'solid 'skyblue)
                                                                                      (rectangle 20 75 'solid 'silver)
                                                                                      (rectangle 48 75 'solid 'skyblue)
                                                                                      (rectangle 20 75 'solid 'silver)
                                                                                      (rectangle 48 75 'solid 'skyblue)
                                                                                      (rectangle 20 75 'solid 'silver)
                                                                                      (rectangle 48 75 'solid 'skyblue)
                                                                                      (rectangle 20 75 'solid 'silver))(overlay (text "Noyce" 30 'silver) (rectangle 300 200 'solid "brown"))))
                        (above (overlay (rectangle 300 40 'solid "palegreen")
                                        (rectangle 375 60 'solid "green"))
                               (overlay (rectangle 200 40 'solid "palegreen")
                                        (rectangle 275 60 'solid "green"))
                               (overlay (rectangle 100 40 'solid "palegreen")
                                        (rectangle 150 60 'solid "green"))
                               (overlay (rectangle 40 40 'solid "palegreen")
                                        (rectangle 50 60 'solid "green"))
                               (overlay(rectangle 10 40 'solid "palegreen")
                                       (rectangle 16 60 'solid "green"))))
         (rectangle 500 50 'solid "black")))
(require csc151)

(define adjectives-old
  (list "inescapable"
        "dark"
        "depressing"
        "downward"
        "unathletic"
        "stupid"
        "athletic"
        "smart"
        "correct"
        "strange"
        "gigantic"
        "cracked"
        "scattered"
        "triggered"
        "uneventful"
        "different"
        "automatic"
        "futuristic"
        "white"
        "old"
        "technological"
        "able"
        "greatest"
        "capable"
        "great"
        "impressed"
        "confused"
        "oppressive"
        "scattered"
        "luckily"
        "swirling"
        "horrible"
        "best"
        "given"
        "upload"
        "angrier"))
        
(define verbs-old
  (list "materialize"
        "escape"
        "begin"
        "continue"
        "clear"
        "land"
        "accelerate"
        "dodge"
        "weave"
        "decide"
        "place"
        "find"
        "sort"
        "add"
        "play"
        "solve"
        "choose"
        "step"
        "warp"
        "realize"
        "hit"
        "step"
        "think"
        "see"))

(define nouns-old
  (list "basement"
        "soul"
        "roof"
        "sword"
        "bow"
        "spear"
        "force"
        "doofus"
        "speed"
        "reality"
        "containers"
        "category"
        "tanker"
        "floor"
        "chasm"
        "climb"
        "stage"
        "quest"
        "technology"
        "world"
        "intelligence"
        "threat"
        "remote"
        "button"
        "lair"
        "vortex"
        "solitude"
        "window"
        "flash"
        "battle"
        "challenge"))
        

(define adjectives-new
  (list "hellish"
"exclusive"
"stale"
"light"
"decent"
"decorous"
"humorous"
"jaded"
"nostalgic"
"halting"
"brief"
"abiding"
"well-made"
"deep"
"marvelous"
"scattered"))

(define verbs-new
  (list "extract"
"imply"
"regain"
"practise"
"suck"
"cancel"
"compensate"
"market"
"express"
"perform"
"melt"
"replace"
"convict"
"in"
"wave"
"fight"))

(define nouns-new
  (list "tongue"
"foundation"
"attention"
"police"
"president"
"organization"
"map"
"knowledge"
"management"
"historian"
"ambition"
"employee"
"perspective"
"difficulty"
"secretary"
"cancer"))

(define nouns
  (list nouns-old nouns-new))

(define verbs
  (list verbs-old verbs-new))

(define adjectives
  (list adjectives-old adjectives-new))

(define all-categories
  (list nouns verbs adjectives))




(define replace-words
  (lambda (old-list new-list words)
    (if (null? old-list)
        words
        (replace-words (cdr old-list) new-list (replace-all (car old-list) (random-list-element new-list) words)))))


(define my-story
  (list "You materialize in the basement of Noyce. You feel like you’ve been saved from somehow falling into an inescapable void that would have consumed youand your soul. You see that all the doors around you are locked and the only way to escape is through the roof. You do not know what you’re doing there, why you’re there, who placed you here, or who you even are, but you begin at level 0: a dark, depressing dungeon known as the physics basement. It’s dangerous to go alone! Take this."
        "You see the floor beneath you shaking and now you are unable to continue forward. In order to keep going, you need to clear a jump from a known height and clear a known distance onto a slab of a known length. Your super weapon allows you to achieve any speed you want, the catch is that you can’t generate any downward force (you can’t jump). How fast do you run in order to land on the 5-meter-long slab?"
        "Congratulations, doofus! You’re either incredibly unathletic, unbelievably stupid, or just plain bad at physics. You must not have traveled at the correct speed to clear the gap, cause you just fell to your doom in the endless darkness below the surface of the earth! You’re dead!"
        "Not bad! You were either athletic enough, smart enough, or good enough at physics to clear the gap, because you figured out the correct speed to travel. As you cross the gap and continue to accelerate, you enter a strange warp tunnel with gigantic atoms flying past you. You dodge and weave through the storm of atoms flying at you, in the hope that you will not be harmed. Suddenly, you find yourself back in reality. You realize you’re on Level 1: the chemistry lab. You ALSO realize there are actual WINDOWS on this floor - giving you a glimpse into what awaits the end of your quest."
        "You notice several cracked containers of different elements scattered across the floor. As you lean down to inspect one, you realize it’s a toxic noble gas which could infect everyone else working in the lab and kill you! Reasoning that all the other containers must contain similarly dangerous chemicals, you decide to help clear up the mess - but the only way to do so is to sort the scattered elements based on their category. Did you pay attention in high school chemistry class?"
        "Uh oh! You placed an element in a category that it doesn’t belong to. Triggered merely by your stupidity, a gas tanker suddenly explodes, quickly obliterating you, the entire floor and the rest of Noyce - but before the explosion can spread and consume the surrounding buildings, time stops. Suddenly, everything starts happening quickly in reverse - the explosion and your death are undone, but so is your trip through the warp tunnel, your clearing of the chasm in the basement, and the earthquake that created that chasm in the first place. You find yourself back in Level 0, exactly where you started. Some mysterious force just saved you from utter, explosive, (the same random element)-fueled oblivion, so be thankful!"
        "You managed to sort them all correctly. Maybe you do have potential! The strange, poisonous cloud that pervaded the floor fades. Nearby you notice a set of stairs and climb it. Surprisingly, the climb up is uneventful - which doesn’t add up, leaving you questioning whether the next stage in your quest will be only a fraction as dangerous as it was before - or exponentially more so. You now find yourself at Level 2: the math department."
        "You are now on the Math Floor. You don’t know why, but each floor looks completely different from the other. The Math Floor looks like a world from the future. Somehow, because of all the fast thinking and the automatic calculations that the inhabitants of that floor could do, the architecture of the place changed to a futuristic white spaceship. An old man in a technological suit says to you:“Hello, our work here is deciphering enigmas, problems that sometimes go hundreds and hundreds of years without being solved. We are detectives, and we have technology from the future. There’s one particular thing about our team: Everyone loves to play a Roulette bet machine. But it involves math. If you solve the following enigma, you might be able to pass it to our teleporter to the Third Floor because you prove yourself to be an enigma solver. In our roulette, there are (random number) numbers. And two colors. What is the probability (in a fraction) of guessing the right number, with the right color, (random number between 1 and 10) times in a row?"
        "Unlucky! The old man pulls out a remote and presses a button. In a flash, you find yourself back on the chemistry floor, with the elements scattered once again and the poisonous cloud once again pervading the air. Luckily you aren’t dead, but you’ve got to re-sort the elements once again if you want to succeed!"
        "Looks like you got lucky! The old man is impressed, and muses that with your good luck, an acceptable level of athleticism, and intelligence, you are indeed capable of defeating the threat upstairs. Confused, you ask the old man about this “threat” he speaks of. He tells you about the Great Rebelsky - an oppressive force that has enslaved everyone in the building and closed off the way out. He takes you to his teleporter - the only thing capable of transporting you to face the threat. He asks you, “Are you ready for the ultimate battle?” You nod, and step through the teleporter, warping up to the third and final floor - the dreaded Computer Lab."
        "You warp into the top floor - right into the lair of the Great Rebelsky! A swirling vortex of code and binary, the Great Rebelsky possesses the power to consume all it sees - and that includes you, now that you’ve disturbed its solitude! As the vortex slowly starts to increase in ferocity, you realize it’s moving slow enough for you to read some of the code that comprises it - and.... oh geez. That is horrible code. Suddenly, a gadget materializes in your hand, courtesy of the old man downstairs. The gadget will give you examples of code and allow you to upload one of them into the Great Rebelsky program. But you need to choose the best code among the given options in order to neutralize the virus and open up the way to the roof. Otherwise, there is no telling what will happen!"
        "You hit the upload button. Looks like you picked the bad code, because the swirling vortex is getting bigger, stronger, and seemingly angrier! It grows increasingly giant, consuming the entire building, and you along with it! But suddenly, a flash of light appears before your eyes, and you find yourself back on the math floor, with no memory of the great battle that just occurred. You see the old man once again and walk towards him, prepared to take on his roulette challenge."
        "You hit the upload button, and the swirling vortex of horrible code surely, but slowly dissipates. As a beam of light blazes through a window, you realize - you won! You step through the window and look out into the campus below you. But as grateful as you are to make it out of the building, you suddenly think to yourself, “How am I gonna get down from here?”"))

(define replace-words-category
  (lambda (category words)
    (replace-words (list-ref category 0) (list-ref category 1) words)))

(define replace-words-all-categories
  (lambda (categories words)
    (if (null? categories)
        words
        (replace-words-all-categories (cdr categories) (replace-words-category (car categories) words)))))

(define my-story-random-list-version
  (let ([x (map string-split my-story)])
    (map (section replace-words-all-categories all-categories <>) x)))

(define my-story-random
  (map (section string-join <>) my-story-random-list-version))
    
                 
(define sequence-list (list "intro" "level-0" "level-0-lose" "level-0-win"
                            "level-1" "level-1-lose" "level-1-win"
                            "level-2" "level-2-lose" "level-2-win"
                            "level-3" "level-3-lose" "level-3-win"))

(define image-or-proc-list
  (list "p" "i" "p" "p"
        "i" "i" "p"
        "i" "p" "p"
        "p" "i" "p"))

;(define story-list
;  (list "intro-placeholder\n" "level-0-text-placeholder\n" "level-0-lose-text-placeholder\n" "level-0-win-text-placeholder\n"
;        "level-1-text-placeholder\n" "level-1-lose-text-placeholder\n" "level-1-win-text-placeholder\n"
;        "level-2-text-placeholder\n" "level-2-lose-text-placeholder\n" "level-2-win-text-placeholder\n"
;        "level-3-text-placeholder\n" "level-3-lose-text-placeholder\n" "level-3-win-text-placeholder\n"))

(define story-list
  my-story-random)
        

(define image/proc-list
  (list scene-1 choose-your-weapon level-0 level-0-lose level-0-win
        level-1 level-1-lose level-1-win
        level-2 level-2-lose level-2-win
        level-3 level-3-lose level-3-win))

(define nightmares-at-noyce
  (lambda ()
    (displayln
     (string-append "Welcome to Nightmares at Noyce!\n"
                    "Choose your main weapon!\n"
                    "Your choices are: bow, spear, and sword"))
    (displayln (list-ref image/proc-list 1))
    (define choice (give-valid-weapon))
    (define my-weapon
      (cond
        [(equal? choice "bow")
         bow]
        [(equal? choice "sword")
         sword]
        [(equal? choice "spear")
         spear]))
    (displayln
     (string-append "You will now customize your character!\n"
                    "Choose your head color\n"))
    (define my-skin (give-valid-color))
    (displayln "Choose your shirt color\n")
    (define my-shirt (give-valid-color))
    (displayln "Choose your pants color\n")
    (define my-pants (give-valid-color))
    (displayln "Choose your story option\n")
    (displayln " a: default story")
    (displayln " b: story with randomized words")
    (define the-story (give-valid-story))
    (displayln (list-ref the-story 0))
    (displayln (scene-1 my-skin my-shirt my-pants my-weapon))
    (physics-game my-skin my-shirt my-pants my-weapon the-story)))


(define give-valid-story
  (lambda ()
    (let ([the-story (read-line)])
      (cond
        [(equal? the-story "a")
         my-story]
        [(equal? the-story "b")
         story-list]
        [else
         (displayln "Please choose a proper story option\n")
         (give-valid-story)]))))

(define give-valid-color
  (lambda ()
    (let ([my-color (read-line)])
      (cond
        [(image-color? my-color)
         my-color]
        [else
         (displayln "Please give a valid color\n")
         (give-valid-color)]))))

(define give-valid-weapon
  (lambda ()
    (let ([my-weapo (read-line)])
      (cond
        [(member? my-weapo (list "bow" "sword" "spear"))
         my-weapo]
        [else
         (displayln "Please enter a valid weapon")
         (give-valid-weapon)]))))

;;;give-answer
;;;redo-question
;;;send-player-to-level
(define cheat-menu
  (lambda (my-answer my-skin my-shirt my-pants my-weapon the-story)
    (cond
      [(equal? (string-downcase my-answer) "cheat")
       (displayln "Cheating Options:\n")
       (displayln (string-append " a: Give answer\n"
                                 " b: Retry question\n"
                                 " c: Open teleport menu\n"))
       (define player-choice (string-downcase(read-line)))
       (cond
         [(equal? player-choice "a")
          "answer"]
         [(equal? player-choice "b")
          "retry"]
         [(equal? player-choice "c")
          "teleport"]
         [else
          (displayln "Choose a proper cheating option\n")
          (cheat-menu "cheat" my-skin my-shirt my-pants my-weapon the-story)])]
      [else
       my-answer])))

(define teleport-menu
  (lambda (my-skin my-shirt my-pants my-weapon the-story)
    (displayln (string-append "Choose what floor you want to teleport to:\n"
                              " level-0\n"
                              " level-1\n"
                              " level-2\n"
                              " level-3\n"))
    (define player-choice (string-downcase(read-line)))
    (cond
      [(equal? player-choice "level-0")
       (displayln "You are now being teleported to level-0\n")
       (physics-game my-skin my-shirt my-pants my-weapon the-story)]
      [(equal? player-choice "level-1")
       (displayln "You are now being teleported to level-1\n")
       (chem-game my-skin my-shirt my-pants my-weapon the-story)]
      [(equal? player-choice "level-2")
       (displayln "You are now being teleported to level-2\n")
       (math-game my-skin my-shirt my-pants my-weapon the-story)]
      [(equal? player-choice "level-3")
       (displayln "You are now being teleported to level-3\n")
       (comp-sci-game my-skin my-shirt my-pants my-weapon the-story)]
      [else
       (displayln "Please choose a proper level!\n")
       (displayln "Level naming scheme ex: level-2\n")
       (teleport-menu my-skin my-shirt my-pants my-weapon the-story)])))

(define numbercheck
  (lambda ()
    (define player-choice (read-line))
    (cond
      [(number? (string->number player-choice))
       (string->number player-choice)]
      [else
       (displayln "Your answer must be a number\n")
       (numbercheck)])))
        
        
