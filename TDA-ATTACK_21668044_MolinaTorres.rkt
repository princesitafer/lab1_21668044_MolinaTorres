#lang racket
; Importaciones

;; exportaciones
(provide
 
 attack
 attack?
 get-attack-cost
 get-attack-name
 get-attack-desc
 get-attack-effect
 ability
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


;Selectores

; Dom: a (attack)
; Rec: string | func | list | null

(define (get-attack-cost a)
  (if (attack? a) (cadr a) null))


(define (get-attack-name a)
  (if (attack? a) (caddr a) null))


(define (get-attack-desc a)
  (if (attack? a) (cadddr a) null))

(define (get-attack-effect a)
  (if (attack? a) (list-ref a 4) (lambda (g args) g)))



(define (ability nombre texto efecto)
  (attack '() nombre texto efecto))