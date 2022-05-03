#lang racket
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

;examples
;> (hit-slab '(432 702 5))
;'(74.76412909945518 75.29663714147408)
;> (hit-slab '(100 1000 10))
;'(221.35943621178654 223.57303057390442)
;> (hit-slab (give-physics-data))
;'(62.221311078040834 62.36517538111145)
;> (hit-slab (give-physics-data))
;'(40.98719883405338 41.55844899201929)

(define physics-mini-game-helper
  (lambda (lst)
    (string-append "We are standing on a " (number->string (list-ref lst 0)) " meter high ledge.\n"
                   "How fast do we have to travel in m/s in order to clear a " (number->string (list-ref lst 1))
                   " meter long gap in front of us\n" "and land onto a " (number->string (list-ref lst 2))
                   " meter long slab?\n"
                   "Assume gravity is 9.8m/s^2\n")))


(define physics-game
  (lambda ()
    (let* ([options (give-physics-data)]
           [answer (hit-slab options)])
    (displayln (physics-mini-game-helper options))
    (define player-choice (string->number (read-line)))
      (cond
        [(and
          (<= (floor (car answer)) player-choice)
          (>= (ceiling (cadr answer)) player-choice))
         (displayln "right answer (next level command)")]
        [else
         (displayln "wrong answer (go back a level)")]))))

;;(physics-game)
    
