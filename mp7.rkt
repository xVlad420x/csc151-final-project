#lang racket
(require csc151)
(require rackunit)


;;; (random-list-element lst) -> any?
;;;   lst : listof any?
;;; Randomly select an element of `lst`
(define random-list-element
  (lambda (lst)
    (list-ref lst (random (length lst)))))

(define periodic-table-elements
  (file->lines "elements.txt"))

(define random-periodic-element
  (lambda ()
  (random-list-element periodic-table-elements)))

(define match-element?)
  