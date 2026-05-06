#lang racket
(require "TDA-CARD_21668044_MolinaTorres.rkt")
(require "TDA-DECK_21668044_MolinaTorres.rkt")
(require "TDA-ATTACK_21668044_MolinaTorres.rkt")

(provide
 initGame
 printGame
 playToBench
 changeActivePokemon
 drawCardFromDeck
 useEnergyCard
 evolvePokemon
 useTrainerCard
 ;usePokemonAbility
 ;usePokemonAttack
 )

;; ------------------ FUNCIONES AUXILIARES--------------------

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
                   (equal? (get-pokemon-evolve-from (car mano)) "")    ; que en pre-evoluciones no sea una lista con vacío
                   (equal? (get-pokemon-evolve-from (car mano)) #f)))           
          #t   ; si no se cumple nada de lo anterior es pq es básico!!
          (tiene-basico? (cdr mano))))) 

; DESC: func para barajar denuevo si la mano no es valida
; DOM: mazo (list) x semilla (int)
; REC: mazo (list)
(define (preparar-mazo-valido mazo semilla)
  (if (tiene-basico? (take mazo 7))
      mazo
      (preparar-mazo-valido (shuffleDeck mazo (randomPuro semilla)) (randomPuro semilla))))


; DESC: elimina la carta especificada de la mano del jugador.
; DOM: list (mano) X list (carta)
; REC: list
(define (quitar-de-mano mano carta)
  (remove carta mano))

; DESC: verifica si la carta de evolución es compatible con el pokemon en juego
; DOM: list (mano) X list (carta)
; REC: list
(define (es-compatible? viejo nueva)
  (equal? (get-card-name viejo) (get-pokemon-evolve-from nueva)))


; DESC: eemplaza el pokemon en la banca manteniendo sus energias(si es que tiene)
;DOM: banca (list) x viejo (list) x nueva (list)
;REC: banca(list)
(define (reemplazar-en-banca banca viejo nueva)
  (cond [(null? banca) '()]         ; banca nula
        [(equal? (car banca) viejo) ;revisa si el pokemon a evolucionar es el 1ro de la lista

         ;si lo encuentra:
         (cons (append nueva ;toma la carta de la evolución (nueva)
                       (drop viejo 11)) ;ignora los 11 elementos solo para considerar las energias en juego
               (cdr banca))]            ;une el pokemon evolucionado con el resto de la banca

        ;si no era el pokemon: sigue buscando (llamado recursivo)
        [else (cons (car banca) (reemplazar-en-banca (cdr banca) viejo nueva))]))


; DESC: Confirma si el pokemon puede evolucionar (tiene pre-evolucion en juego) y tiene al menos
;       un turno en juego
; DOM: viejo (list) X nueva (list)
; REC: bool
(define (puede-evolucionar? viejo nueva)
  (and (es-compatible? viejo nueva) ;revisa si es complatible
       (> (length viejo) 11)        ;Revisa que la lista sea > 11 antes de pedir el indice 11
       (>= (list-ref viejo 11) 1))); Al menos tenga 1 turno ya jugado.

; DESC: CONVERTIR LST-CARDS -> STRING D NOMBRES
; DOM: lst-cards (lista) X acc (string)
; REC: string
; RECURSIÓN: COLA
(define (listar-nombres cartas acc)
  (cond [(null? cartas) acc]
        [(null? (car cartas)) (listar-nombres (cdr cartas) acc)]
        [else (listar-nombres (cdr cartas) ;llamado recursivo con cdr cartas (siguiente pokemon)
                              (string-append acc "\n\t  - " (list-ref (car cartas) 1)))]))


; DESC:
; DOM: 
; REC: 
; RECURSIÓN: 
(define (formatear-individual p)
  (string-append
   "\n\t> " (list-ref p 1) " [" (symbol->string (list-ref p 4)) "]"
   " | PS: " (number->string (list-ref p 3))
   " | " (if (or (null? (list-ref p 2)) (equal? (list-ref p 2) "") (equal? (list-ref p 2) #f)) "Basico" "Evolucionado")
   "\n\t  Debilidad : " (if (null? (list-ref p 5)) "Ninguna" (symbol->string (list-ref p 5)))
   " | Resistencia : " (if (null? (list-ref p 6)) "Ninguna" (symbol->string (list-ref p 6)))
   
   ;calcula energias en juego (salta 11 indicess los cuales son stats y el turno)
   "\n\t  Energias en juego (" (if (> (length p) 11) (number->string (length (drop p 12))) "0") "):"
   (if (> (length p) 11) (listar-nombres (drop p 12) "") " Ninguna")
   "\n"))


; DESC: LST-CARDS-POKEMON -> string formateado con estadisticas base y añade sus energias utilizadas
; DOM: lista-pokes (list) X acc (string) 
; REC: acc (string)
; RECURSIÓN: NATURAL
(define (formatear-pokemons lista-pokes acc)
  (cond [(null? lista-pokes) acc]
        [(null? (car lista-pokes)) (formatear-pokemons (cdr lista-pokes) acc)] ;si el 1er elemento es nulo vamos la sgte
        ;ver pokemon
        [else (formatear-pokemons (cdr lista-pokes)
                                  (string-append acc (formatear-individual (car lista-pokes))))]))


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

(define (pasar-turno g)
  (list (car g) (cadr g) (list-ref g 2) (if (= (list-ref g 3) 0) 1 0)))

(define (buscar-ataque ataques nombre)
  (cond [(null? ataques) #f]
        [(equal? (get-attack-name (car ataques)) nombre) (car ataques)]
        [else (buscar-ataque (cdr ataques) nombre)]))


; ; ------------------------------------------- INICIO DEL JUEGO ----------------------------------------------------


; DESC: Inicializa el juego... Prepara mazos de ambos jugadores para que sean válidos y arma TDA-GAME.
; DOM: deck1 (list) X deck2 (list) X semilla (int+)
; REC: list
(define (initGame deck1 deck2 semilla)
  (armar-estado-juego 
   (preparar-mazo-valido (shuffleDeck deck1 semilla) semilla)
   (preparar-mazo-valido (shuffleDeck deck2 (randomPuro semilla)) (randomPuro semilla))
   semilla))


; DESC: muestra un string con el estado actual de la
;       partida (manos, mazos, premios, banca, pokemones-activo y descartes) 
;       muestra las cartas en mano de quien es el turno solo al propio jugador.
; DOM: TDA-GAME (list) X player (int)
; REC: acc (string)
; RECURSION: COLA
(define (printGame game player-turno)
  (define (aux g fase turno acc)
    (cond
      ; FASE 0: Encabezado del juego
      [(= fase 0)
       (aux g 1 turno (string-append acc
                                     ">>>>>>>>>>>>>>> TURNO ACTUAL: JUGADOR "                       ; saca el int turno de TDA-GAME.
                                     (number->string (+ (list-ref g 3) 1))" <<<<<<<<<<<<<<" "\n"))] ; +1 YA QUE NO PUEDE SER PLAYER 0, SINO 1.

      ; FASE 1: Datos del Jugador 1
      [(= fase 1)
       (aux g 2 turno
            (string-append acc
             "\n--- " (car (list-ref g 0)) " ---\n"
             "Cartas en la mano: " (number->string (length (list-ref (list-ref g 0) 1))) " cartas"
             
             ; muestra las cartas del jugador 1 si es su turno
             (if (= turno 1)
                 (string-append "\n\tTus cartas son:" (listar-nombres (list-ref (list-ref g 0) 1) ""))
                 "") ;si no es su turno no agrega nada
             
             "\n\nMazo: " (number->string (length (list-ref (list-ref g 0) 2))) " cartas disponibles"
             "\nPremios: " (number->string (length (list-ref (list-ref g 0) 3))) " cartas disponibles"
             "\nActivo: " (if (null? (list-ref (list-ref g 0) 4)) "Ninguno" (formatear-individual (list-ref (list-ref g 0) 4)))
             "\nBanca: " (if (null? (list-ref (list-ref g 0) 5)) "Vacia\n" (formatear-pokemons (list-ref (list-ref g 0) 5) ""))
             "Pila de descarte: " (if (null? (list-ref (list-ref g 0) 6)) "Vacia\n" (string-append (listar-nombres (list-ref (list-ref g 0) 6) "") "\n"))))]

      ;player 2
      [(= fase 2)
       (aux g 3 turno
            (string-append acc
             "\n--- " (car (list-ref g 1)) " ---\n"
             "Cartas en la mano: " (number->string (length (list-ref (list-ref g 1) 1))) " cartas"
             

             ; muestra las cartas del jugador 2 si es su turno.
             (if (= turno 2)
                 (string-append "\n\tTus cartas son:" (listar-nombres (list-ref (list-ref g 1) 1) ""))
                 "");si no es su turno no agrega nada
             
             "\n\nMazo: " (number->string (length (list-ref (list-ref g 1) 2))) " cartas disponibles"
             "\nPremios: " (number->string (length (list-ref (list-ref g 1) 3))) " cartas disponibles"
             "\nActivo: " (if (null? (list-ref (list-ref g 1) 4)) "Ninguno" (formatear-individual (list-ref (list-ref g 1) 4)))
             "\nBanca: " (if (null? (list-ref (list-ref g 1) 5)) "Vacía\n" (formatear-pokemons (list-ref (list-ref g 1) 5) ""))
             "Pila de descarte: " (if (null? (list-ref (list-ref g 1) 6)) "Vacia\n" (string-append (listar-nombres (list-ref (list-ref g 1) 6) "") "\n"))))]

      [else acc])) ;retorna el string
  (aux game 0 player-turno ""))



; DESC: Mueve una carta-pokemon desde la mano del player que tiene el turno
;       hacia su banca, tambien verifica si este mov es válido (su banca tenga espacio)
;       Retorna el estado del juego actualizado.
; DOM: TDA-GAME (list) X TDA-CARD(list)   
; REC: TDA-GAME (list)          (Nuevo TDA Game modificado)
(define (playToBench game carta)
  (define (actualizar-jugador jugador c)
    (if (and (eq? (get-card-type c) 'pokemon) (< (length (list-ref jugador 5)) 5))
        (list (car jugador)                         ;nombre
              (quitar-de-mano (cadr jugador) c)    ;nueva mano
              (caddr jugador)                       ;deck
              (cadddr jugador)                      ;premios
              (list-ref jugador 4)                  ;activo
              (cons (if (= (length c) 11) (append c '(0)) c) (list-ref jugador 5)) ;nueva banca
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
          [(equal? (get-card-name (car banca)) c) (car banca)]
          [else (remover-de-banca (cdr banca) c)]))

  (define (procesar-cambio jugador c) ; IDENTIFICA A QUE JUGADOR LE PERTENECE LA CARTA
    (if c ;si la carta esta en banca
        (list (car jugador)    ;noimbre
              (cadr jugador)   ;mano
              (caddr jugador)  ;deck
              (cadddr jugador) ; premio
              c                ; nuevo activo

              ;banca
              (if (null? (list-ref jugador 4)) 
                  (remover-de-banca (list-ref jugador 5)c)
                  (cons (list-ref jugador 4) (remover-de-banca (list-ref jugador 5) c)))
              (list-ref jugador 6)) jugador)) 

  ;se cambian datos nuevos de cada player
  (list (procesar-cambio (car game) (remover-de-banca (list-ref (car game) 5) (get-card-name carta-banca))) ;player 1
        (procesar-cambio (cadr game)(remover-de-banca (list-ref (cadr game) 5) (get-card-name carta-banca)))  ;player 2
        (list-ref game 2)                          ;semilla
        (list-ref game 3)))                        ;turno

 
;; DESC: jugador actual roba la primera carta de su baraja y la añade a su mano 
;       si el mazo está vacío -> el juego termina y gana el oponente
; DOM: TDA-GAME (list)
; REC: TDA-GAME (list) | string 
(define (drawCardFromDeck game)
  (define (aux jugador)
    (list (car jugador)          ;nombre

          ;mano nueva
          (append (list-ref jugador 1) ;mano actual (cartas que ya tiene)
                  (list (car (list-ref jugador 2)))) ;añade la primera carta de la baraja (deck)
          
          (cdr (list-ref jugador 2))    ;baraja
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




; DESC: le asigna una energia a un pokemon en juego (banca o activo)
;       LA carta de energia se elimina de la mano y se añade al final de la lista de pokemon
; Dom: TDA-GAME (list) X cartaPokemon (list) X cartaEnergia (list)
; REC: TDA-GAME (list)
(define (useEnergyCard game pokemon-destino carta-energia)
  (define turno (list-ref game 3))

  ;-------------FUNCIONES AUXILIARES INTERNAS------------------------------------------------

  ; DESC: busca el pokemon y le asigna energia al final d su lista
  ; DOM: lista-poke (list) X poke-dest (list) X enrgia (list card)
  ; REC: list
  ; RECURSION: NATURAL
  
  (define (adjuntar lista-pokes poke-dest energia)
    (cond [(null? lista-pokes) '()]                ; verifica que haya pokemon en la lista
       
          [(equal? (car lista-pokes) poke-dest)    ;busca pokemon 
           (cons (append (car lista-pokes) (list energia)) (cdr lista-pokes))] ;le asigna la energia al final
          ;y mantiene el resto de la banca

          ;en caso de no encontrar la carta, sigue buscandola (llamado recursivo)
          [else (cons (car lista-pokes) (adjuntar (cdr lista-pokes) poke-dest energia))]))

  ;;;;;;;;;;;;;;;;;;;;;;;;

  ; DESC: recibe datos y arma la lista final del jugador con datos actualizados
  ; DOM: jugador (list)X nueva-mano (list) X activo-actualizado(list) X banca-actualizada (LIST)
  ; REC: jugador (LIST) 
  (define (reconstruir-jugador jugador nueva-mano activo-actualizado banca-actualizada)
    (list (car jugador)             ; nombre
          nueva-mano                ; mano(sin la energia usada)
          (list-ref jugador 2)      ; deck
          (list-ref jugador 3)      ; premios
          activo-actualizado        ; pokemon activo 
          banca-actualizada         ; banca actualizada
          (list-ref jugador 6)))    ; descartess
  ;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;------------------------------------------------------------------------------------------

  ; DESC: ve la posicion del pokemon a quien se le asignará la energia y se le aplica
  ; DOM: jugador(list) X poke-dest(list card) x e (list card).
  ; REC: jugador (list)
  (define (procesar-energia jugador poke-dest e)
    (cond 
      ;caso 1 pokemon destino esta activo
      [(equal? (list-ref jugador 4) poke-dest) ;list-ref jugador 4 = activo
       (reconstruir-jugador jugador 
                            (quitar-de-mano (list-ref jugador 1) e) ;nueva mano
                            (append (list-ref jugador 4) (list e))   ;activo + energia
                            (list-ref jugador 5))]                   ;banca(sin cambio)

      ;caso 2 pokemon destino esta en banca
      [(member poke-dest (list-ref jugador 5)) ;list-ref jugador 4 = enbanca
       (reconstruir-jugador jugador 
                            (quitar-de-mano (list-ref jugador 1) e) ;nueva mano
                            (list-ref jugador 4)                     ;activo (sin cambio)
                            (adjuntar (list-ref jugador 5) poke-dest e))] ;banca + energia

      [else jugador])) ;si no lo encuentra no hace nada (devuelve el jugador como estaba)

  ;RECONSTRUIR EL TDA-GAME
  (if (= turno 0) ;ver si es el turno 1
      ; si es turno del player 1
      (list (procesar-energia (car game) pokemon-destino carta-energia) ;llama la fn para que ponga la energia y quite de la mano
            (cadr game)       ; JUGADOR 2 (NO HAY CAMBIOS)
            (list-ref game 2) ;SEMILLA (NO HAY CAMBIOS)
            turno)            ; TURNO (NO HAY CAMBIOS)

      ;PLAYER 2
      (list (car game)  ; JUGADOR 1 (NO HAY CAMBIOS)
            (procesar-energia (cadr game) pokemon-destino carta-energia) ;FN para ponerle la energia al pokemon y sacarla de su mano
            (list-ref game 2) ;SEMILLA (NO HAY CAMBIOS)
            turno)))          ; TURNO (NO HAY CAMBIOS)



; DESC: Evoluciona algun pokemon que esté en el juego (activo o banca)usando cartA en mano (su evolución)
;      Tambien verifica compatibilidad y que el pokemon lleve al menos 1 turno en juego (indice 11)
; DOM: TDA-GAME (list) X pokemon-a-evolucionar (list card) X carta-evolucion (list card)
; REC: TDA-GAME (list)
(define (evolvePokemon game viejo nueva)
  (define (actualizar-jugador pj) ;cambios del jugador
    
    (if (not (puede-evolucionar? viejo nueva)) pj ;si no puede evolucionar, se retorna el jugador sin cambios
        
        (if (equal? viejo (list-ref pj 4))    ;el pokemon a evolucionar está activo       
            (list (car pj)                      ;nombre            
                  (remove nueva (list-ref pj 1)) ;elimina 1 carta d evolucion de la mano
                  (caddr pj)                     ;baraja           
                  (cadddr pj)                    ;premios           
                  (append nueva (drop viejo 11)) ;pokemon acctivo nuevo(mantiene energías)   
                  (list-ref pj 5)                ;banca   
                  (list-ref pj 6))               ;descarte 
            
            (if (member viejo (list-ref pj 5))  ;pokemon a evolucionar está en la banca
                (list (car pj)                  ;nombre                
                      (remove nueva (list-ref pj 1)) ;elimina 1 carta d evolucion de la mano
                      (caddr pj)                    ;baraja            
                      (cadddr pj)                   ;premios            
                      (list-ref pj 4)               ;pokemon activo   
                      (map (lambda (p) 
                             (if (equal? p viejo)    ;mira si el pokemon viejo es el evolucionará 
                                 (append nueva (drop viejo 11)) ;carta de energía en juego!! 
                                 p))  
                           (list-ref pj 5))         ;banca  
                      (list-ref pj 6))              ;descarte  
                pj)))) ;si no encuentra turno -> no hace nada
  
 ;;aplica cambios al jugador que tiene el turno
  (if (= (list-ref game 3) 0)  ; pregunta si es el turno 0 = jugador 1 
      (list (actualizar-jugador (list-ref game 0)) (list-ref game 1) (list-ref game 2) 0) ;JUGADOR 1
      (list (list-ref game 0) (actualizar-jugador (list-ref game 1)) (list-ref game 2) 1))) ;JUGADOR 2



;; DESC: aplica el efecto de una carta de entrenador y la descarta (mueve a descarte)
; DOM: juego (list) X trainer-card (list) X args (list)
;REC: juego (list)
(define (useTrainerCard game trainer-card args)
  (define juego-post-efecto ((get-trainer-effect trainer-card) game args)) ; extraer el efecto de la carta de entrenador

  ;actualiza mano y descarte (agrega la mano entrenador utilizada)
  (define (procesar-descarte pj)
    (list (car pj)                                ;nombre
          (quitar-de-mano (cadr pj) trainer-card) ;mano (sin el entrenador)
          (caddr pj)                              ;mazo
          (cadddr pj)                             ;premios
          (list-ref pj 4)                         ;activos
          (list-ref pj 5)                         ;banca
          (cons trainer-card (list-ref pj 6))))   ;pila descarte

  ;actualizar el tda juego
  (if (= (list-ref juego-post-efecto 3) 0) ;verifica si es turno del player 0 -> jugador1
      ;playeer 1
      (list (procesar-descarte (car juego-post-efecto)) ;actualiza jugador 1
            (cadr juego-post-efecto)                   ;jugador 2 no cambia
            (list-ref juego-post-efecto 2)             ; semilla
            (list-ref juego-post-efecto 3))            ; Turno
      
      ;player 2
      (list (car juego-post-efecto)                    ;jugador 1 no cambia
            (procesar-descarte (cadr juego-post-efecto)) ;actualiza jugador 2
            (list-ref juego-post-efecto 2)
            (list-ref juego-post-efecto 3))))


; DESC: ejecuta el efecto que genera una habilidad de un pokemon (tiene que estar en juegop)
; DOM: juego (list) X pokemon-card (list) X args (list)
; REC: juego (list)
;(define (usePokemonAbility game pokemon-card args)
; (define habilidad (get-pokemon-ability pokemon-card)) ;extraer la habilidad del pokemon


