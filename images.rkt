#lang racket
(require 2htdp/image)

(define person
  (lambda (skin-color shirt-color pants-color)
    (above (circle 25 'solid skin-color)
           (above (rectangle 140 20 'solid shirt-color)
                  (rectangle 40 50 'solid shirt-color))
           (rectangle 40 80 'solid pants-color))))



(define sword
  (above (triangle 30 'solid "silver")
                     (rectangle 30 120 'solid "silver")
                     (rectangle 50 20 'solid "black")
                     (rectangle 10 30 'solid "black")))
(define spear
  (above (triangle 30 'solid "silver")
                     (overlay/align 'center 'top (rotate 180 (triangle 30 'solid "silver"))
                                    (rectangle 10 200 'solid "beige"))))
(define bow
  (rotate -75 (wedge 60 150 'outline (pen "brown" 8 'solid 'projecting 'round))))

(define choose-your-weapon
   (beside sword spear bow))

(define scene-1
  (lambda (skin-color shirt-color pants-color)
  (overlay/align 'center 'bottom (above (scale 0.5 (person skin-color shirt-color pants-color))
                                                (rectangle 500 50 'solid "silver"))
   (rectangle 500 300 'solid "black"))))

(define level-0
  (overlay (above (rotate 120 (rhombus 90 60 'solid "black"))
                  (rotate -120 (rhombus 90 60 'solid "black"))
                  (rotate 120 (rhombus 90 60 'solid "black"))
                  (rotate -120 (rhombus 90 60 'solid "black")))
  (rectangle 500 300 'solid "silver")))

(define level-0-lose
  (lambda (skin-color shirt-color pants-color)
    (overlay/align 'center 'top (above (text "A" 24 "white")
                    (text "A" 24 "white")
                    (text "A" 24 "white")
                    (text "A" 24 "white")
                    (text "A" 24 "white")
                    (text "A" 24 "white")
                    (text "A" 24 "white")
                    (text "A" 24 "white")
              (scale .25 (person skin-color shirt-color pants-color)))
    (overlay (rectangle 200 300 'solid "black")
             (rectangle 500 300 'solid "silver")))))

(define level-0-win
  (lambda (skin-color shirt-color pants-color)
