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


;;IMAGES FOR MATH LEVEL

(define (koch-curve n)
  (cond
    [(zero? n) (square 1 "solid" "blue")]
    [else
     (local [(define smaller (koch-curve (- n 1)))]
       (beside/align "bottom"
                     smaller
                     (rotate 60 smaller)
                     (rotate -60 smaller)
                     smaller))]))


;;Old man (Wolf)
;; Old man face

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
  (lambda (color)
    (above
     (rectangle 58 8 'solid "grey")
     (overlay/align 'center 'bottom
                     (rectangle 20 5 'solid "red")
                     (rectangle 45 8 'solid "grey"))
                     (rectangle 35 4 'solid "grey"))))

    (define old-man-face-1
      (lambda (color)
        (overlay/align 'center 'bottom
                       (mouth-beard color)
                       (face-no-mouth color))))

 
    (define old-man
      (above
         old-man-face-1
         (overlay/align 'center 'top (above(rectangle 30 4 'solid "grey")
         (rectangle 25 4 'solid "grey")
         (rectangle 24 4 'solid "grey")
         (rectangle 21 4 'solid "grey")
         (rectangle 18 4 'solid "grey")
         (rectangle 16 4 'solid "grey"))
                        (rectangle 50 150 'outline 'black))))

      



    
