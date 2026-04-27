#lang racket
;;IMPORTACIONES
(require "TDA-ATTACK_21668044_MolinaTorres.rkt")
;;EXPORTACIONES

(provide
 
 card
 card?
 get-card-type
 get-card-name
 get-pokemon-hp
 get-pokemon-attacks
 get-trainer-effect
 get-pokemon-evolve-from
 get-pokemon-hp 
 get-pokemon-type 
 get-pokemon-attacks 
 get-pokemon-is-ex?
 ;get-trainer-type aun sin aplicar
 ;get-trainer-effect aun sin aplicar
 )


(define CARD-TYPE '(pokemon energy trainer))
(define (card-type? t)
  (and (symbol? t)
       (member t CARD-TYPE)))
 
(define ELEMENT-TYPE '(grass fire water lightning psychic fighting darkness metal colorless fairy))
(define (element-type? e)
  (and (symbol? e)
       (member e ELEMENT-TYPE)))
 
(define ENERGY
  '((fire-energy      . fire)
    (water-energy     . water)
    (grass-energy     . grass)
    (lightning-energy . lightning)
    (psychic-energy   . psychic)
    (fighting-energy  . fighting)
    (darkness-energy  . darkness)
    (metal-energy     . metal)
    (fairy-energy     . fairy)
    (colorless-energy . colorless)))



;; .........TDA CARTA...............................

; Dom: tipo (str) X args (. args)
; Rec: card (list)
; Tipo recursiin: no aplica



; ; constructores

; DESC: Construye una carta validando su tipo y la cantidad de args que necesita
; Dom: tipoCarta (symbol) X argumentos variables (. args)
; Rec: card (list)
; Tipo recursión: No aplica
(define (card tipo . args)
  (if (card-type? tipo)
      (cond
        ; caso1:pokemon(10)
        [(eq? tipo 'pokemon)
         (if (and (= (length args) 10) (> (list-ref args 2) 0))
             (cons 'pokemon args)
             (error "la carta pokemon requiere 10 argumentos"))]
        
        ; caso2: energia(1)
        [(eq? tipo 'energy)
         (if (= (length args) 2)
             (cons 'energy args)
             (error "la carta requiere el tipo d energia."))]
        
        ; caso3: trainer(4)
        [(eq? tipo 'trainer)
         (if (= (length args) 4)
             (cons 'trainer args)
             (error "la carta requiere 4 argumentos."))]
        )
      (error "carta no valida")))


;; pertenencia
;;;;;;;;;;;;;

;;DESC: verifica si es carta valida
; Dom: c (cualquiera)
; Rec: bool
; Tipo recursión: no aplica
(define (card? c)
  (and (list? c)
       (not (null? c))
       (if (card-type? (car c)) #t #f)))



;; SELECTORES (CARD)
;;;;;;;;;;;;;;;;;;;;

; Dom: c (card)
; Rec: symbol (pokemon, energy, trainer)
(define (get-card-name c) (cadr c))

; Dom: c (card)
; Rec: string
(define (get-card-type c) (car c))



; Selectores para pokemon
; Dom: c (card)
; Rec: number | null | string |... 
(define (get-pokemon-evolve-from c) (if (eq? (get-card-type c) 'pokemon) (list-ref c 2) (error "no es un pokemon")))
(define (get-pokemon-hp c)          (if (eq? (get-card-type c) 'pokemon) (list-ref c 3) (error "no es un pokemon")))
(define (get-pokemon-type c)        (if (eq? (get-card-type c) 'pokemon) (list-ref c 4) (error "no es un pokemon")))
(define (get-pokemon-attacks c)     (if (eq? (get-card-type c) 'pokemon) (list-ref c 9) (error "no es un pokemon")))
(define (get-pokemon-is-ex? c)      (if (eq? (get-card-type c) 'pokemon) (list-ref c 8) (error "no es un pokemon")))

;Selectores para entrenador
(define (get-trainer-type c)   (if (eq? (get-card-type c) 'trainer) (caddr c) (error "no es un entrenador")))
(define (get-trainer-effect c) (if (eq? (get-card-type c) 'trainer) (list-ref c 4) (error "no es un entrenador")))

