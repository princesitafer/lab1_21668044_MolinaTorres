#lang racket
; Importaciones

;; exportaciones
(provide
 
 attack
 attack?
 ;;get-attack-cost
 ;;get-attack-name
 ;;get-attack-desc
 ;;get-attack-effect
 )





;; .......TDA ATACK........

; Dom: cost (list) X nombre (string) X texto (string) x f (func)
; Rec: atack (list)
; Tipo recursion: no aplica

;constructor
(define (attack cost nombre texto f)
  (list 'attack cost nombre texto f))

;Pertenencia
; Dom: a (cualquiera)
; Rec: boolean
(define (attack? a)
  (and (list? a)
       (not (null? a))
       (eq? (car a) 'attack)
       (= (length a) 5)))

