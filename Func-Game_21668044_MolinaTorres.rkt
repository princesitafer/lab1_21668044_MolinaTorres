#lang racket
(require "TDA-CARD_21668044_MolinaTorres.rkt")
(require "TDA-DECK_21668044_MolinaTorres.rkt")
(require "TDA-ATTACK_21668044_MolinaTorres.rkt")

;; FUNCIONES AUXILIARES

; tomar n cartas
(define (take lst n)
  (if (or (= n 0) (null? lst)) '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

; sacar las n cartas anteriores de la baraja
(define (drop lst n)
  (if (or (= n 0) (null? lst)) lst
      (drop (cdr lst) (- n 1))))

; ver si existe al menos 1 poke basico en la mano!
(define (tiene-basico? mano)
  (if (null? mano)
      #f
      (if (and (eq? (get-card-type (car mano)) 'pokemon)
               (or (null? (get-pokemon-evolve-from (car mano)))
                   (equal? (get-pokemon-evolve-from (car mano)) '())
                   (equal? (get-pokemon-evolve-from (car mano)) "")))
          #t
          (tiene-basico? (cdr mano)))))


;func para barajar denuevo si la mano no es valida
(define (preparar-mazo-valido mazo semilla)
  (if (tiene-basico? (take mazo 7))
      mazo
      (preparar-mazo-valido (shuffleDeck mazo (randomPuro semilla)) (randomPuro semilla))))


; ; INICIO DEL JUEGO
(define (lanzar-moneda sem)
  (modulo (randomPuro sem) 2))

(define (armar-estado-juego m1 m2 sem)
  (list 
   ;PLAYERS: nombre X mano(cartas en uso) X deck-restante X premios)
   (list "Player 1" (take m1 7) (drop (drop m1 7) 6) (take (drop m1 7) 6))
   (list "Player 2" (take m2 7) (drop (drop m2 7) 6) (take (drop m2 7) 6))
   sem
   (lanzar-moneda sem)))

(define (initGame deck1 deck2 semilla)
  (armar-estado-juego 
   (preparar-mazo-valido (shuffleDeck deck1 semilla) semilla)
   (preparar-mazo-valido (shuffleDeck deck2 (randomPuro semilla)) (randomPuro semilla))
   semilla))



;CONVERTIR LST-CARDS -> STRING D NOMBRES
(define (listar-nombres cartas acc-nombres)
  (if (null? cartas)
      acc-nombres
      (listar-nombres (cdr cartas) 
                      (string-append acc-nombres "\n\t" (get-card-name (car cartas))))))

(define (printGame game id-jugador-ver)
  (define (aux g step id-ver acc)
    (cond
      [(= step 0)
       (aux g 1 id-ver 
                     (string-append "Turno actual: Jugador " 
                                    (number->string (+ (list-ref g 3) 1)) "\n\n"))]
      
      ;player1
      [(= step 1)
       (aux g 2 id-ver 
            (string-append acc "PLAYER 1: " 
                                    (number->string (length (cadr (car g)))) " cartas en la mano"
                                    (if (= id-ver 1) (listar-nombres (cadr (car g)) "") "") "\n"
                                    "deck: " (number->string (length (caddr (car g)))) " cartas disponibles\n"
                                    "Premios disponibles: " (number->string (length (cadddr (car g)))) "\n\n"))]
      
      ;player 2
      [(= step 2)
       (aux g 3 id-ver 
                     (string-append acc "PLayer 2: " 
                                    (number->string (length (cadr (cadr g)))) " cartas en la mano"
                                    (if (= id-ver 2) (listar-nombres (cadr (cadr g)) "") "") "\n"
                                    "deck: " (number->string (length (caddr (cadr g)))) " cartas disponibles\n"
                                    "Premios disponibles: " (number->string (length (cadddr (cadr g)))) "\n"))]
      
      [else acc]))

  (aux game 0 id-jugador-ver ""))

;; funcion de prueba temporal!!
;(define e1 (card 'energy "Energia Rayo" 'lightning))
;(define atk1 (attack '(lightning) "Trueno" "Hace daño" (lambda (g) g)))
;(define c1 (card 'pokemon "Pikachu" '() 60 'lightning 'fighting 'metal 1 #f '() (list atk1)))

;mazo cn 1 Pikachu y 59 energys
;(define d1 (apply deck (cons c1 (make-list 59 e1))))
;(define g1 (initGame d1 d1 1531312))
;(define g3Str  (printGame  g1 1))

