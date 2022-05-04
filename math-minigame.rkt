#lang racket
(require csc151)
(require 2htdp/image)
(provide (all-defined-out))
(require rackunit)

(define roulette-slices
  (lambda ()
    (let ([rando (random 13 102)])
      (if (odd? rando)
          rando
          (roulette-slices)))))

;;; (random-list-element lst) -> any?
;;;   lst : list? (nonempty)
;;; Randomly select an element of `lst` (This is SamR's procedure from some project but I probably could've wrote something similar myself)
(define random-list-element
  (lambda (lst)
    (list-ref lst (random (length lst)))))

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

(define math-game
  (lambda ()
    (let* ([slices (roulette-slices)]
           [question-lst (give-number-and-opposite-col slices)]
           [spins (give-spins)]
           [answer (math-game-calculator slices spins)])
      (displayln answer)
      (displayln (math-game-helper slices question-lst spins))
      (define player-choice (string->number (read-line)))
      (cond
        [(and (>= answer (floor-10000th player-choice))
              (<= answer (ceiling-10000th player-choice)))
         (displayln "right answer (next level command)")]
        [else
         (displayln "wrong answer (go back a level)")]))))

;formula = ((1/slices + ((slices - 1)/2)/slices)^times)
