#lang racket


(require csc151)
(require 2htdp/image)

(provide (all-defined-out))

(require csc151)
(require rackunit)
(require csc151/rex)

;;Mini game for Math Level
;;MP7

;random element every time: number of slots

;; Helper procedure:

(define number
  (lambda ()
  (random 0 315)))

(define roulette
  (lambda (color slot)
     (let* ( [fate (random 0 number)])
      (cond
        [(and
          (equal? fate slot)
          (not (or (and (rex-matches? (rex-string "red")
                                      (symbol->string color))
                        (odd? fate))
                   (and (rex-matches? (rex-string "black")
                                      (symbol->string color))
                        (even? fate)))))
         (+ 0 number)]
        [(and
          (equal? fate slot)
          (or (and (rex-matches? (rex-string "red")
                                 (symbol->string color))
                   (odd? fate))
              (and (rex-matches? (rex-string "black")
                                 (symbol->string color))
                   (even? fate))))
         (+ 2 number)]
        [ (or (and (rex-matches? (rex-string "red")
                                 (symbol->string color))
                   (odd? fate))
              (and (rex-matches? (rex-string "black")
                                 (symbol->string color))
                   (even? fate)))
          2]
        [else
         0]
        ))))

;;Helper procedure to display the number selected

(define question
  (string-append "In our roulette, there are "
                 (number->string (number ))
                 " numbers and two colors (Red and Black). What is the probability (in a fraction) of guessing the right number, with the right color "
                 (number->string (random 1 11)) " times in a row?"))




