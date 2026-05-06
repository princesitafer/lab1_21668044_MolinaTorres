#lang racket
(require "TDA-CARD_21668044_MolinaTorres.rkt")
(require "TDA-DECK_21668044_MolinaTorres.rkt")
(require "TDA-ATTACK_21668044_MolinaTorres.rkt")

;; FUNCIONES AUXILIARES

;DESC: tomar n cartas de la baraja
;DOM: cards (list) X number-cards (int)
;REC: list
;RECURSIÓN: NATURAL
(define (take lst n)
  (if (or (= n 0) (null? lst)) '()  ; CASO BASE: BARAJA VACIA o ya sacar todas las necesarias (n cartas).
      (cons (car lst) (take (cdr lst) (- n 1))))) ;toma la 1ra carta de la baraja
                                                  ;llamado recursivo a sacar n-1 cartas más

; DESC: sacar las n cartas anteriores de la baraja (disminuye su cantidad)
; DOM: cards (list) X number-cards (int) ;
; REC: list
; RECURSIÓN: COLA
(define (drop lst n)
  (if (or (= n 0) (null? lst)) lst    ; caso base: barajavacía o la cantidad (n) ya se quitaron
      (drop (cdr lst) (- n 1))))      ; llamado recursivo con n-1 (saco carta de 1 en 1)

; DESC: ver si existe al menos 1 poke basico en la mano!
; DOM: mano (list)
; REC: bool
; RECURSIÓN: COLA
(define (tiene-basico? mano)
  (if (null? mano) #f       ; caso base: ver la mano y que ya no queden cartas
      
      (if (and (eq? (get-card-type (car mano)) 'pokemon)               ; si quedan cartas reviso si es tipo pokemon Y
               (or (null? (get-pokemon-evolve-from (car mano)))        ; vigila que no tenga pre-evoluciones (debe ser básico)
                   (equal? (get-pokemon-evolve-from (car mano)) '())   ; que en pre-evoluciones no sea una lista nula
                   (equal? (get-pokemon-evolve-from (car mano)) "")))  ; que en pre-evoluciones no sea una lista con vacío
          #t   ; si no se cumple nada de lo anterior es pq es básico!!
          (tiene-basico? (cdr mano))))) 


; DESC: func para barajar denuevo si la mano no es valida
; DOM: mazo (list) x semilla (int)
; REC: mazo (list)
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
 
   ;(NAME MANO DECK PREMIOS ACTIVOS BANCA DESCARTADOS)
   ;player 1
   (list "Player 1" (take m1 7)              ; toma las primeras 7 cartas del jugador 1
         (drop (drop m1 7)                   ; se salta las 7 cartas
               6)                            ; se salta otras 6 cartas!! (premios)
                                             ; para poder ver la baraja disponible en juego
         
         (take (drop m1 7)                   ;se salta las primeras 7 cartas con el drop
               6)                            ; toma las prox 6 cartas (premios)
         '()   ; pokemones activos(vacio)     
         '()   ; banca (vacio)
         '())  ; descartes (vacio)
   
   ;player 2
   (list "Player 2" (take m2 7) (drop (drop m2 7) 6) (take (drop m2 7) 6) '() '() '())
   sem
   (lanzar-moneda sem)))

; DESC: Inicializa el juego... Prepara mazos de ambos jugadores para que sean válidos y arma TDA-GAME.
; DOM: deck1 (list) X deck2 (list) X semilla (int)
; REC: list
(define (initGame deck1 deck2 semilla)
  (armar-estado-juego 
   (preparar-mazo-valido (shuffleDeck deck1 semilla) semilla)
   (preparar-mazo-valido (shuffleDeck deck2 (randomPuro semilla)) (randomPuro semilla))
   semilla))



; DESC: CONVERTIR LST-CARDS -> STRING D NOMBRES
; DOM: lst-cards (lista) X acc (string)
; REC: string
; RECURSIÓN: COLA
(define (listar-nombres cartas acc-nombres)
  (if (null? cartas)
      acc-nombres
      (listar-nombres (cdr cartas) 
                      (string-append acc-nombres "\n\t" (get-card-name (car cartas))))))

; DESC: LST-CARDS-POKEMON -> string formateado con estadisticas
;       base y añade sus energias utilizadas.
; DOM: lista-pokes (list) X acc (string) 
; REC: acc (string)
; RECURSIÓN: NATURAL
(define (formatear-pokemons lista-pokes acc)
  (cond 
    [(null? lista-pokes) acc] ; lista nula se retorna el acc
    
    [(null? (car lista-pokes)) (formatear-pokemons (cdr lista-pokes) acc)] ;si el 1er elemento es nulo vamos la sgte

    ;ver pokemon
    [else 
     (formatear-pokemons 
      (cdr lista-pokes)
      (string-append acc "\n\t" 
                     (list-ref (car lista-pokes) 1) "    "                     ;nombre pokemon
                     (number->string (list-ref (car lista-pokes) 3)) "PS    "  ;vida poke
                     (symbol->string (list-ref (car lista-pokes) 4)) "    "    ; tipo de pokemonm 
                     (if (> (length (car lista-pokes)) 11) ; si el largo de list-pokes es pq hay energias en juego
                         (string-append "\n\t Energias en juego: " 
                                        (listar-nombres (drop (car lista-pokes) 11) ""))
                         "")))]))                          ; si no hay energias en juego, se imprime "vacio"

        
; DESC: muestra un string con el estado actual de la
;       partida (manos, mazos, premios, banca, pokemones-activo y descartes) 
;       muestra las cartas en mano de quien es el turno solo al propio jugador.
; DOM: TDA-GAME (list) X player (int)
; REC: acc (string)
; RECURSION: COLA
(define (printGame game player)
  (define (aux g turno visor acc) ; g -> estado actual de la partida
                                  ; visor es a quien se le revela datos de cartas
    (cond
      [(= turno 0)
       (aux g 1 visor (string-append "Turno actual -> PLAYER "
                                     (number->string (+ (list-ref g 3) 1)) "\n\n"))] ; saca el int turno de TDA-GAME.
                                                                                     ; +1 YA QUE NO PUEDE SER PLAYER 0, SINO 1.
      
      ;pplayer 1
      [(= turno 1)
       (aux g 2 visor 
            (string-append acc "Jugador 1: " (number->string (length (cadr (car g)))) " cartas en la mano"
                           (if (= visor 1) (listar-nombres (cadr (car g)) "") "") "\n"
                           "Mazo: " (number->string (length (caddr (car g)))) " cartas disponibles\n"
                           "Premios disponibles: " (number->string (length (cadddr (car g)))) "\n"
                           "Banca: " (formatear-pokemons (list-ref (car g) 5) "") "\n"
                           "Pokemon activo: " (if (null? (list-ref (car g) 4)) "Ninguno" (formatear-pokemons (list (list-ref (car g) 4)) "")) "\n"
                           "Pila de descarte: " (listar-nombres (list-ref (car g) 6) "") "\n\n"))]
      
      ; player 2
      [(= turno 2)
       (aux g 3 visor 
            (string-append acc "Jugador 2: " (number->string (length (cadr (cadr g)))) " cartas en la mano"
                           (if (= visor 2) (listar-nombres (cadr (cadr g)) "") "") "\n"
                           "Mazo: " (number->string (length (caddr (cadr g)))) " cartas disponibles\n"
                           "Premios disponibles: " (number->string (length (cadddr (cadr g)))) "\n"
                           "Banca: " (formatear-pokemons (list-ref (cadr g) 5) "") "\n"
                           "Pokemon activo: " (if (null? (list-ref (cadr g) 4)) "Ninguno" (formatear-pokemons (list (list-ref (cadr g) 4)) "")) "\n"
                           "Pila de descarte: " (listar-nombres (list-ref (cadr g) 6) "") "\n"))]
      [else acc]))
  (aux game 0 player ""))


; DESC: Mueve una carta-pokemon desde la mano del player que tiene el turno
;       hacia su banca, tambien verifica si este mov es válido (su banca tenga espacio)
;       Retorna el estado del juego actualizado.
; DOM: TDA-GAME (list) X TDA-CARD(list)   
; REC: TDA-GAME (list)          (Nuevo TDA Game modificado)
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


; DESC: cambia un pokemon en banca por el pokemon activo  de un jugador, si no hay poke activo,
;        mueve la carta de la banca a activo
; DOM: TDA-GAME (list) X TDA-CARD(list)   
; REC: TDA-GAME (list)          
(define (changeActivePokemon game carta-banca)
  (define (remover-de-banca banca c) ;aux para sacar la carta de la banca de un player
    (cond [(null? banca) '()]
          [(equal? (car banca) c) (cdr banca)]
          [else (cons (car banca) (remover-de-banca (cdr banca) c))]))

  (define (procesar-cambio jugador c) ; IDENTIFICA A QUE JUGADOR LE PERTENECE LA CARTA
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
        (list-ref game 2)                          ;semilla
        (list-ref game 3)))                        ;turno


;; DESC: jugador actual roba la primera carta de su baraja y la añade a su mano 
;       si el mazo está vacío -> el juego termina y gana el oponente
; DOM: TDA-GAME (list)
; REC: TDA-GAME (list) | string 
(define (drawCardFromDeck game)
  (define (aux jugador)
    (list (car jugador)          ;nombre
          (append (list-ref jugador 1) (list (car (list-ref jugador 2)))) ;mano nueva
          (cdr (list-ref jugador 2))    ;mazo
          (list-ref jugador 3)          ;premios
          (list-ref jugador 4)          ;activos
          (list-ref jugador 5)          ;banca
          (list-ref jugador 6)))        ;descarte

  (cond 
    ;si es jugador 1 se queda sin cartas
    [(and (= (list-ref game 3) 0) (null? (list-ref (car game) 2))) 
     "OPONENTE GANA EL JUEGO"]
    ;si el jugador 2 se queda sin cartas
    [(and (= (list-ref game 3) 1) (null? (list-ref (cadr game) 2))) 
     "OPONENTE GANA EL JUEGO"]

    ; robo normal de la primera carta de la baraja.
    [(= (list-ref game 3) 0)   ; si es el turno del jugador 1
     (list (aux (car game))    ; extrae al jugador 1 y le aplica aux (sacar una carta de la baraja)
           (cadr game)         ; deja tal cual al jugador 2
           (list-ref game 2)   ; toma la semilla identica al juego
           (list-ref game 3))] ; copia el num de turno
    
    [else                    ; si es turno del jugador 2
     (list (car game) (aux (cadr game)) (list-ref game 2) (list-ref game 3))]))


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
(displayln (printGame g3 1))


(define g4 (drawCardFromDeck g3))
(displayln "---carta sacada del mazo---")
(displayln (printGame g4 1)))