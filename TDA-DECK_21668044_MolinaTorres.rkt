#lang racket
(require "TDA-CARD_21668044_MolinaTorres.rkt")

;exportaciones 
(provide
 deck
 deck?
 get-deck-cards
 ;shuffle-deck
 )


;Funciones auxiliares para la pertenencia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DESC: contador de veces que aparece un nombre en una listado de cartas
; Dom: name(string) X cards(list)
; Rec: int

(define (count-name name cards)
  (length (filter (lambda (c) 
                    (equal? (get-card-name c) name)) 
                  cards)))

; DESC: verifiica que no haya + de 4 copias de una cartas pokemon o entrenador
(define (copies-limit cards)
  (if (null? cards)#t
      
      (if (or (equal? (get-card-type (car cards)) 'energy)
              (<= (count-name (get-card-name (car cards)) cards) 4))
          (copies-limit (cdr cards)) #f)))

; DESC: verifica si existe al menos un pokemon BASICO (sin evolucionar)
(define (basic-pokemon? cards)
  (not (null? (filter (lambda (c) 
                        (and (eq? (get-card-type c) 'pokemon)
                             (null? (get-pokemon-evolve-from c)))) 
                      cards))))


; ------ TDA DECK-----

; Dom: cards (list)
; Rec: deck (list)
; Tipo recursion: no aplica


(define (deck . cards)
  (if (and (=(length cards)60)
           (copies-limit cards)
           (basic-pokemon? cards)) cards null))

; pertenencia
(define (deck? d)
  (and (list? d)
       (or (null? d)(list? (car d)))))

;selectores
(define (get-deck-cards d) d)



;; BARAJAR MAZO

; DESC: genera de numeros random para barajar cartas
; Dom: Xn (int)
; Rec: int
(define (randomPuro Xn)
  (modulo (+ (* Xn 1103515245) 12345) 2147483648))

;(define (shuffleDeck d seed)
;  (if (null? d)
;      '()
;     (cons)))