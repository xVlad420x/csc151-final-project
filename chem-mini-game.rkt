#lang racket
(require csc151)
(require rackunit)

;Definitions
(define periodic-table-elements
  (file->lines "elements.txt"))

(define periodic-table-noble-gases
  (list "He - Helium," "Ne - Neon," "Ar - Argon," "Kr - Krypton," "Xe - Xenon," "Rn - Radon,"))

(define periodic-table-elements-without-noble-gases
  (filter  (o not (section member? <> periodic-table-noble-gases)) periodic-table-elements))

;test periodic-table-elements-without-noble-gases

(test-equal? "brief check to see if elements were removed by looking at length"
             (length periodic-table-elements-without-noble-gases)
             (- (length periodic-table-elements)
                (length periodic-table-noble-gases)))

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


(define chem-game
  (lambda ()
    (let*([8-random-table-elements (8-random-elements)]
          [selected-val (random-list-element 8-random-table-elements)]
          [8-random-elements-including-noble-gases (replace-all selected-val (random-noble-gas) 8-random-table-elements)]
          [8-list (fix-duplicate-noble-gases 8-random-elements-including-noble-gases)])
      (displayln 8-list)
      (displayln "which of the following is a noble gas?")
      (displayln "Type the answer below matching the case and comma. (Ex. O - Oxygen,)")
      (define player-choice (read-line))
      (cond
        [(not (member? player-choice 8-list)) 
         (displayln "INVALID OPTION TRY AGAIN")
         (chem-game)]
        [(member? player-choice periodic-table-noble-gases)
         (displayln "right answer (next level command)")]
        [else
         (displayln "wrong answer (go back a level)")]))))

;(define crazy-lst
;  (list (fix-duplicate-noble-gases (8-random-elements-including-noble-gases))
;        (fix-duplicate-noble-gases (8-random-elements-including-noble-gases))
;        (fix-duplicate-noble-gases (8-random-elements-including-noble-gases))
;        (fix-duplicate-noble-gases (8-random-elements-including-noble-gases))
;        (fix-duplicate-noble-gases (8-random-elements-including-noble-gases))
;        (fix-duplicate-noble-gases (8-random-elements-including-noble-gases))
;        (fix-duplicate-noble-gases (8-random-elements-including-noble-gases))
;        (fix-duplicate-noble-gases (8-random-elements-including-noble-gases))))



        

 

