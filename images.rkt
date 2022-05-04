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
  (let* ([small-gray (circle 10 'solid "gray")]
         [medium-red (circle 15 'solid "red")]
         [large-black (circle 20 'solid "black")]
         [warp-top-left (scale 3 (overlay/align 'left 'top small-gray medium-red large-black))]
         [warp-bottom-left (scale 3 (overlay/align 'left 'bottom small-gray medium-red large-black))]
         [warp-top-right (scale 3 (overlay/align 'right 'top small-gray medium-red large-black))]
         [warp-bottom-right (scale 3 (overlay/align 'right 'bottom small-gray medium-red large-black))])
    (lambda (skin-color shirt-color pants-color)
      (overlay (person skin-color shirt-color pants-color) (overlay/align 'right 'bottom warp-bottom-right
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

(define eyes
  (beside
   (overlay
    (square 10  'solid "brown")
    (square 20 'solid "black"))
   (rectangle 15 4 'solid "black")
   (overlay
    (square 10  'solid "brown")
    (square 20 'solid "black"))))


(define face-no-mouth
  (overlay
   eyes
   (circle 36 'solid "brown")))
(define mouth-beard
  (above
   (rectangle 58 8 'solid "grey")
   (overlay/align 'center 'bottom
                  (rectangle 20 5 'solid "red")
                  (rectangle 45 8 'solid "grey"))
   (rectangle 35 4 'solid "grey")))

(define old-man-face-1
  (overlay/align 'center 'bottom
                 mouth-beard
                 face-no-mouth))



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
    
    
                 
