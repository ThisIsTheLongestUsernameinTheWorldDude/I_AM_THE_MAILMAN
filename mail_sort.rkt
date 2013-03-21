;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mail_sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A Mail Message is a structure:
; – (make-mail String Number String)
; interp. (make-mail f d m) represents text m sent by
; f, d seconds after the beginning of time
(define-struct mail (f d m))
; List-of-mail -> List-of-mail
; produces a sorted version of alom
(define (mail-sort alom)
  (cond
    [(empty? alom) empty]
    [(cons? alom) (insert (first alom) (mail-sort (rest alom)))]))
 
; Number List-of-numbers -> List-of-numbers
; insert n into the sorted list of mail alom
(define (insert n alom)
  (cond
    [(empty? alom) (cons n empty)]
    [else (if (>= (mail-d n) (mail-d(first alom)))
              (cons n alom)
              (cons (first alom) (insert n (rest alom))))]))