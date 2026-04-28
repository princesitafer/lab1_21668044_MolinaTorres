#lang racket
(require "TDA-CARD_21668044_MolinaTorres.rkt")
(require "TDA-DECK_21668044_MolinaTorres.rkt") 


; tomar n cartas
(define (take lst n)
  (if (or (= n 0) (null? lst)) '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

; sacar las n cartas anteriores de la baraja
(define (drop lst n)
  (if (or (= n 0) (null? lst)) lst
      (drop (cdr lst) (- n 1))))

; ver si existe al menos 1 poke basico en la mano!
#;(define (tiene-basico? mano)
  (if (null? mano)
      #f
      (if (and (eq? (get-card-type (car mano)) 'pokemon)
               (null? (get-pokemon-evolve-from (car mano))))
          #t
          (tiene-basico? (cdr mano)))))


;func para barajar denuevo si la mano no es valida
;(define (preparar-mazo-valido mazo semilla)
 


;(define (initGame deck1 deck2 semilla)
