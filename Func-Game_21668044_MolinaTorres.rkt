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


; ; ------------------------------------------- INICIO DEL JUEGO ----------------------------------------------------

(define (lanzar-moneda sem)
  (modulo (randomPuro sem) 2))

;; DESC: Simula la banca en juego
;; DOM: game(LIST) X cards(list)
;; REC: game(LIST)
(define (armar-estado-juego m1 m2 sem)
  (list 
   ;player 1
   ;(NAME MANO DECK PREMIOS ACTIVOS BANCA DESCARTADOS)
   (list "Player 1" (take m1 7) (drop (drop m1 7) 6) (take (drop m1 7) 6) '() '() '())
   ;player 2
   (list "Player 2" (take m2 7) (drop (drop m2 7) 6) (take (drop m2 7) 6) '() '() '())
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


(define (formatear-pokemons lista-pokes acc)
  (cond 
    [(null? lista-pokes) acc] ; lista nula se retorna el acc
    
    [(null? (car lista-pokes)) (formatear-pokemons (cdr lista-pokes) acc)] ;si el 1er elemento es nulo vamos la sgte

    ;ver pokemon
    [else 
     (formatear-pokemons 
      (cdr lista-pokes)
      (string-append acc "\n\t" 
                     (list-ref (car lista-pokes) 1) "    " ;nombre pokemon
                     (number->string (list-ref (car lista-pokes) 3)) "PS    "  ;vida poke
                     (symbol->string (list-ref (car lista-pokes) 4)) "    " ; tipo de pokemonm 
                     (if (> (length (car lista-pokes)) 11)
                         (string-append "\n\t Energias en juego: " 
                                        (listar-nombres (drop (car lista-pokes) 11) ""))
                         "")))]))

        

(define (printGame game id-jugador-ver)
  (define (aux g step id-ver acc)
    (cond
      [(= step 0)
       (aux g 1 id-ver (string-append "Turno actual -> PLAYER " (number->string (+ (list-ref g 3) 1)) "\n\n"))]
      
      ;pplayer 1
      [(= step 1)
       (aux g 2 id-ver 
            (string-append acc "Jugador 1: " (number->string (length (cadr (car g)))) " cartas en la mano"
                           (if (= id-ver 1) (listar-nombres (cadr (car g)) "") "") "\n"
                           "Mazo: " (number->string (length (caddr (car g)))) " cartas disponibles\n"
                           "Premios disponibles: " (number->string (length (cadddr (car g)))) "\n"
                           "Banca: " (formatear-pokemons (list-ref (car g) 5) "") "\n"
                           "Pokemon activo: " (if (null? (list-ref (car g) 4)) "Ninguno" (formatear-pokemons (list (list-ref (car g) 4)) "")) "\n"
                           "Pila de descarte: " (listar-nombres (list-ref (car g) 6) "") "\n\n"))]
      
      ; player 2
      [(= step 2)
       (aux g 3 id-ver 
            (string-append acc "Jugador 2: " (number->string (length (cadr (cadr g)))) " cartas en la mano"
                           (if (= id-ver 2) (listar-nombres (cadr (cadr g)) "") "") "\n"
                           "Mazo: " (number->string (length (caddr (cadr g)))) " cartas disponibles\n"
                           "Premios disponibles: " (number->string (length (cadddr (cadr g)))) "\n"
                           "Banca: " (formatear-pokemons (list-ref (cadr g) 5) "") "\n"
                           "Pokemon activo: " (if (null? (list-ref (cadr g) 4)) "Ninguno" (formatear-pokemons (list (list-ref (cadr g) 4)) "")) "\n"
                           "Pila de descarte: " (listar-nombres (list-ref (cadr g) 6) "") "\n"))]
      [else acc]))
  (aux game 0 id-jugador-ver ""))


(define (playToBench game carta)
  (define (remover-de-mano mano carta-buscada)
    (cond [(null? mano) '()]
          [(equal? (car mano) carta-buscada) (cdr mano)]
          [else (cons (car mano) (remover-de-mano (cdr mano) carta-buscada))]))

  (define (actualizar-jugador jugador c)
    (if (and (eq? (get-card-type c) 'pokemon) (< (length (list-ref jugador 5)) 5))
        (list (car jugador)                         ;nombre
              (remover-de-mano (cadr jugador) c)    ;nueva mano
              (caddr jugador)                       ;deck
              (cadddr jugador)                      ;premios
              (list-ref jugador 4)                  ;activo
              (cons c (list-ref jugador 5))         ;nueva banca
              (list-ref jugador 6))                 ;descartes
        jugador)) ;caso en que no hay espacio o no es pokemon

  (if (= (list-ref game 3) 0) ;turno player 1
      (list (actualizar-jugador (car game) carta) (cadr game) (caddr game) (cadddr game))
      (list (car game) (actualizar-jugador (cadr game) carta) (caddr game) (cadddr game))))



; Dom: juego (Juego) X cartaDeLaBanca (card)
; REC: Juego
(define (changeActivePokemon game carta-banca)
  (define (remover-de-banca banca c);aux para sacar la carta de la banca de un player
    (cond [(null? banca) '()]
          [(equal? (car banca) c) (cdr banca)]
          [else (cons (car banca) (remover-de-banca (cdr banca) c))]))


  (define (procesar-cambio jugador c)
    (if (member c (list-ref jugador 5)) ;si la carta esta en banca
        (list (car jugador)    ;noimbre
              (cadr jugador)   ;mano
              (caddr jugador)  ;deck
              (cadddr jugador) ; premio
              c                ; nuevo activo

              ;banca
              (if (null? (list-ref jugador 4)) 
                  (remover-de-banca (list-ref jugador 5) c)
                  (cons (list-ref jugador 4) (remover-de-banca (list-ref jugador 5) c)))
              (list-ref jugador 6)) jugador)) 

  ;se cambian datos nuevos de cada player
  (list (procesar-cambio (car game) carta-banca)   ;player 1
        (procesar-cambio (cadr game) carta-banca)  ;player 2
        (list-ref game 2)                          ;seed
        (list-ref game 3)))                        ;turno


; Dom: juego (Juego) X cartaPokemon (card) X cartaEnergia (card)
; REC: Juego
#;(define (useEnergyCard game pokemon-destino carta-energia)
  ;verificacion si es elturno y si la carta es energia
  (define turno (list-ref game 3))
  (define jugador-actual (if (= turno 0) (car game) (cadr game)))
  (define mano (cadr jugador-actual)))



;; funcion de prueba temporal!!
#;((define e1 (card 'energy "Energia Rayo" 'lightning))
(define atk1 (attack '(lightning) "Trueno" "Hace daño" (lambda (g) g)))
(define c1 (card 'pokemon "Pikachu" '() 60 'lightning 'fighting 'metal 1 #f '() (list atk1)))

(define d1 (apply deck (cons c1 (make-list 59 e1))))
(define g1 (initGame d1 d1 1531312))

;pikachu en la banca
(define mi-pika (list-ref (cadr (car g1)) 5)) 
(define g2 (playToBench g1 mi-pika))

(displayln "--- PIKACHU EN BANCAA---")
(displayln (printGame g2 1))


(define turno (list-ref g2 3))

(define jugador-con-el-turno (if (= turno 0) (car g2) (cadr g2)))
(define pika-en-banca (car (list-ref jugador-con-el-turno 5)))

;pasar pikachu a activo
(define g3 (changeActivePokemon g2 pika-en-banca)) 
(displayln "---PIKACHU ACTIVADO EN JUEGO!!----")
(displayln (printGame g3 1)))

