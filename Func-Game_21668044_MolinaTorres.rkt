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
 usePokemonAbility
 usePokemonAttack
 lanzar-moneda
 coincide-nombre?
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
      (cadr (list (displayln "MAZOS INVALIDOS, EL CRUPIER ESTÁ BARAJANDO NUEVAMENTE...") 
                  (preparar-mazo-valido (shuffleDeck mazo (randomPuro semilla)) 
                                        (randomPuro semilla))))))
      ;preparar-mazo-valido (shuffleDeck mazo (randomPuro semilla)) (randomPuro semilla))))

;; DESC: jugador actual roba la primera carta de su baraja y la añade a su mano 
;       si el mazo está vacío -> el juego termina y gana el oponente
; DOM: TDA-GAME (list)
; REC: TDA-GAME (list) | string 
(define (drawCardFromDeck game)
  (define turno (list-ref game 3));TURNO
  (define sem (list-ref game 2));SEMILLA
  (define nueva-sem (randomPuro sem))
  
  ;funcion para que el jugador robe carta de su baraja
  ;se descarte del mazo y actualice tda game
  (define (preparar-jugador jugador id)
    (procesar-estados 
     (list (car jugador) ;nombre
           (append (list-ref jugador 1);mano actual (cartas que ya tiene)
                   (list (car (list-ref jugador 2)))) ;añade la primera carta de la baraja (deck)
           
           (cdr (list-ref jugador 2)) ;baraja -1 (carta extraida)
           (list-ref jugador 3) ;premios
           (list-ref jugador 4) ;activos
           (list-ref jugador 5) ;banca
           (list-ref jugador 6));descarte
     turno id nueva-sem))

  (cond
    ;si es jugador 1 se queda sin cartas en la baraja
    [(and (= turno 0) (null? (list-ref (car game) 2))) "OPONENTE GANA EL JUEGO"]
    
     ;si el jugador 2 se queda sin cartas en la baraja
    [(and (= turno 1) (null? (list-ref (cadr game) 2))) "OPONENTE GANA EL JUEGO"]

    ;actualiza jugador 1
    [(= turno 0)
     (list (preparar-jugador (car game) 0) (cadr game) nueva-sem turno)]
    
    [else; jugador 2
     (list (car game) (preparar-jugador (cadr game) 1) nueva-sem turno)]))
  
; DESC: elimina la carta especificada de la mano del jugador.
; DOM: list (mano) X list (carta)
; REC: list
(define (quitar-de-mano mano carta)
  (remove carta mano))

; DESC: CONVERTIR LST-CARDS -> STRING D NOMBRES
; DOM: lst-cards (lista) X acc (string)
; REC: string
; RECURSIÓN: COLA
(define (listar-nombres cartas acc)
  (cond [(null? cartas) acc]
        [(null? (car cartas)) (listar-nombres (cdr cartas) acc)]
        [else (listar-nombres (cdr cartas) ;llamado recursivo con cdr cartas (siguiente pokemon)
                              (string-append acc "\n\t  - " (list-ref (car cartas) 1)))]))

; DESC: convierte la lista de cada pokemon en un string considerando sus stats (hp, nombre, tipo, debilidad)
;       Y tambien considera las energias que tiene asignada cada pokemon
; DOM: p (list)
; REC: string
(define (formatear-individual p)
  (define hp-max (list-ref p 3))
  (define dano-recibido (if (> (length p) 12) (list-ref p 12) 0))
  (define hp-actual (- hp-max dano-recibido))
  (string-append
   "\n\t> " (list-ref p 1) " [" (symbol->string (list-ref p 4)) "]"
   " | PS: " (number->string hp-actual) " / " (number->string hp-max) ; vida actual (dañado)/vida base (original de la carta)
   " | " (if (or (null? (list-ref p 2)) (equal? (list-ref p 2) "") (equal? (list-ref p 2) #f)) "Basico" "Evolucion")
   "\n\t  Debilidad : " (if (null? (list-ref p 5)) "Ninguna" (symbol->string (list-ref p 5)))
   " | Resistencia : " (if (null? (list-ref p 6)) "Ninguna" (symbol->string (list-ref p 6)))
   
   ;calcula energias en juego (salta 11 indicess los cuales son stats y el turno)
   "\n\t  Energias en juego (" (if (> (length p) 13) (number->string (length (drop p 13))) "0") "):"
   (if (> (length p) 13) (listar-nombres (drop p 13) "") " Ninguna")
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

;cara 0
;cruz 1
(define (lanzar-moneda sem)
  (modulo (randomPuro sem) 2))

;; DESC: Simula la banca en juego
;; DOM: game(LIST) X cards(list)
;; REC: game(LIST)
(define (armar-estado-juego m1 m2 sem)
  (list 
 
   ;(NAME MANO DECK PREMIOS ACTIVOS BANCA DESCARTADOS)
   ;player 1
   (list "JUGADOR 1" (take m1 7)              ; toma las primeras 7 cartas del jugador 1
         (drop (drop m1 7)                   ; se salta las 7 cartas
               6)                            ; se salta otras 6 cartas!! (premios)
         ; para poder ver la baraja disponible en juego
         
         (take (drop m1 7)                   ;se salta las primeras 7 cartas con el drop
               6)                            ; toma las prox 6 cartas (premios)
         '()   ; pokemones activos(vacio)     
         '()   ; banca (vacio)
         '())  ; descartes (vacio)
   
   ;player 2
   (list "JUGADOR 2" (take m2 7) (drop (drop m2 7) 6) (take (drop m2 7) 6) '() '() '())
   sem
   (lanzar-moneda sem)))

(define (pasar-turno game)
  (define turno-act (list-ref game 3))
  (define sem (list-ref game 2))
  (define nueva-sem (+ sem 701)) ;nueva semilla, ya que al lanzar la moneda con la misma semilla
                                 ; el resultado siempre será el mismo. 701 es un numero x, puede ser
                                 ; cualquiera (exceptuando 0)
  (list (car game) 
        (cadr game) 
        nueva-sem
        (if (= turno-act 0) 1 0) ; cambia el turno 0 <-> 1 
        #f))
  

;busca ataque valido (que no sea nulo) y/o nombre coincida con el TDA ATTACK (get)
(define (buscar-ataque ataques nombre)
  (cond [(null? ataques) #f]
        [(equal? (get-attack-name (car ataques)) nombre) (car ataques)]
        [else (buscar-ataque (cdr ataques) nombre)]))


;DESC: funcion de ayuda para habilidades/ataques los cuales ignoren debilidades o resistencias
;DOM: pj (list) X dano(int)
;REC: list
(define (daniar-activo pj dano)
  (if (null? (list-ref pj 4)) pj
      (list (car pj) (cadr pj) (caddr pj) (cadddr pj)
            (append (take (list-ref pj 4) 12) 
                    (list (+ (list-ref (list-ref pj 4) 12) dano)) 
                    (drop (list-ref pj 4) 13))
            (list-ref pj 5) (list-ref pj 6))))

; DESC: quita en el nombre del pokemon activo el estado que le afectaba (paralizado, dormido, confundido)
; DOM: pj (list) x sufijo (string)
; REC: list
(define (desmarcar-activo pj sufijo)
  (if (null? (list-ref pj 4)) 
      pj
      (list (car pj)  ;nombre
            (cadr pj) ; mano
            (caddr pj) ;mazo
            (cadddr pj);premios
            ;rehace al pokemon activo pero le quita el sufijo (efecto, tal que paralizado, dormido, confundido)
            (append (list (car (list-ref pj 4)) 
                          (string-replace (list-ref (list-ref pj 4) 1)
                                          sufijo ""))  ;cambia el sufijo por un vacio
                    (drop (list-ref pj 4) 2))
            (list-ref pj 5) 
            (list-ref pj 6))))

(define (aplicar-veneno pj)
  (if (and (not (null? (list-ref pj 4))) (string-contains? (list-ref (list-ref pj 4) 1) "[envenenado]"))
      (daniar-activo pj 10) ;si esta envenenado le resta -10HP CADA TURNO
      pj))

(define (despertar-dormido pj sem)
  (if (and (not (null? (list-ref pj 4))) (string-contains? (list-ref (list-ref pj 4) 1) "[dormido]"))
      (if (= (modulo (randomPuro sem) 2) 0) ;cara: despierta
          (desmarcar-activo pj " [dormido]")
          pj) ;cruz: sigue durmiendo (no puede atacar)
      pj))

(define (curar-paralisis pj turno-actual mi-id)
  (if (and (not (null? (list-ref pj 4))) (string-contains? (list-ref (list-ref pj 4) 1) "[paralizado]") (= turno-actual mi-id))
      (desmarcar-activo pj " [paralizado]") ;quita paralizado (se cura al sgte turno del jugador dueño del pokemon afectado)
      pj))

;aplica todas las funciones en cadena!
(define (procesar-estados pj turno-actual mi-id sem)
  (curar-paralisis
   (despertar-dormido
    (aplicar-veneno pj) sem) turno-actual mi-id))

; DESC: extrae daño base de unacarta
(define (obtener-dano-base desc)
  (define palabras (string-split desc))
  (if (and (not (null? palabras)) (equal? (car palabras) "Inflige"))
      (if (number? (string->number (cadr palabras)))
          (string->number (cadr palabras))
          0)
      0))

; DESC:verifica si el nombre buscado es igual que el de la carta actual (pokemon dormido) funcion para evitar bug!! ya que no detectaba a un pokemon si tenia un efecto.
;      REVISA LETRA POR LETRA
; DOM: nombre-completo (string) X nombre-buscado (string)
; REC: bool
;RECURSION: NATURAL
(define (coincide-nombre? nombre-completo nombre-buscado)
  (define (comparar-chars lst-c lst-b)
    (cond [(null? lst-b) #t] ;si terminamos de buscar y coincide -> TRUE
          [(null? lst-c) #f] ;si terminó de buscar y no lo encontró, fallaron letras -> FALSE
          
          [(char=? (car lst-c) (car lst-b)) ; si la letra actual coincide, llamado recurviso con la siguiente
           (comparar-chars (cdr lst-c) (cdr lst-b))]
          [else #f]))
  (comparar-chars (string->list nombre-completo) (string->list nombre-buscado)))

(define (puede-retirarse? p-act)
  (if (null? p-act) #t 
      (if (or (string-contains? (list-ref p-act 1) "[dormido]")
              (string-contains? (list-ref p-act 1) "[paralizado]"))
          #f 
          (if (>= (length (drop p-act 13)) (list-ref p-act 7))
              #t #f))))

;Funcion para ver si es compatible ambos pokemon apra hacer posible la evolucion
; REC: BOOL
(define (compatible? viejo nueva)
  (equal? (get-card-name viejo) ;nombre del pokemon a evolucionar
          (get-pokemon-evolve-from nueva))) ;nombre pre-evolucion
;DEBEN COINCIDIR PARA RETORNAR TRUE


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
      ;Encabezado del juego: Inicio, ningun jugador tiene pokemons activos, ambos tienen misma cantidad de premios, baraja, etc.
      [(= fase 0)
       (aux g 1 turno
            (string-append acc
                           ;mensaje de Inicio (solo si nadie tiene pokemon activo aun, se ejecuta)
                           (if (and (null? (list-ref (car g) 4)) (null? (list-ref (cadr g) 4)))
                               (string-append "========================================================\n"
                                              "BUENA SUERTE (LANZANDO MONEDA...)\n"
                                              "COMIENZA EL  " 
                                              (if (= (list-ref g 3) 0) (car (list-ref g 0)) (car (list-ref g 1))) "\n"
                                              "========================================================\n")
                               "")
                           ; Indicador de Turno Actual (Se muestra siempre)
                           "\n>>>>>>>>>>>>>>> TURNO DE: " 
                           (if (= turno 1) (car (list-ref g 0)) (car (list-ref g 1)))
                           " <<<<<<<<<<<<<<\n"))]

      ;Datos del jugador 1: cartas, repmios, descartes. (EN ESTE TURNO NO PUEDE VERSE LAS CARTAS EN MANO DEL JUGADOR 2)
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

      ;player 2 (EN ESTE TURNO NO PUEDE VERSE LAS CARTAS EN MANO DEL JUGADOR 1)
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
; REC: TDA-GAME (list)        
(define (playToBench game carta)
  (define (actualizar-jugador jugador c)
    (if (and (eq? (get-card-type c) 'pokemon) ;verifica que la carta sea tipo pokemon
             (< (length (list-ref jugador 5)) 5)  ; verifica haya espacio en la banca
             (member c (cadr jugador))) ;verifica que la carta esté en la mano
        
        (list (car jugador)                         ;nombre
              (quitar-de-mano (cadr jugador) c)    ;nueva mano
              (caddr jugador)                       ;deck
              (cadddr jugador)                      ;premios
              (list-ref jugador 4)                  ;activo
              (cons (if (= (length c) 11) (append c '(0 0)) c) (list-ref jugador 5)) ;nueva banca
              (list-ref jugador 6))                 ;descartes
        jugador)) ;caso en que no hay espacio o no es pokemon

  (if (= (list-ref game 3) 0) ;turno player 1
      (list (actualizar-jugador (car game) carta) (cadr game) (caddr game) (cadddr game))
      (list (car game) (actualizar-jugador (cadr game) carta) (caddr game) (cadddr game))))


; DESC: Cambia un pokemon en banca por el activo, cobrando costo de retirada.
;       mueve la carta de la banca a activo
;       Manda la energía cobrada al descarte y limpia los estados.
; DOM: TDA-GAME (list) X TDA-CARD(list)   
; REC: TDA-GAME (list)
(define (changeActivePokemon game carta-banca)
  
  (define (remover-de-banca banca nombre) ;aux para sacar la carta de la banca de un player
    (cond [(null? banca) #f] ;caso base: banca vacia
          [(equal? (get-card-name (car banca)) nombre) (car banca)] ; si encuentra la carta la retorna
          [else (remover-de-banca (cdr banca) nombre)])) ; llamado recursivo para seguir buscandola

  ;crea una nueva lista de la banca sin la carta que se va de banca
  (define (filtrar-banca banca nombre)  
    (cond [(null? banca) '()] ;banca vacia
          [(equal? (get-card-name (car banca)) nombre) (cdr banca)] ;si encuentra retorna el resto
          [else (cons (car banca) (filtrar-banca (cdr banca) nombre))])) ;;llamado recursivo


  ;extrae las energias de costo para llevarlas a la pila de descarte
  (define (obtener-energias-pagadas p-act)
    (drop (drop p-act 13) (- (length (drop p-act 13)) (list-ref p-act 7))))

  
  (define (procesar-retirada p-act) ;fn para retornar al pokemon sin la energia descartada y sin efectos(dormido, paralizado, etc)

    (define p-sin-estados ; POKEMON SIN NINGUN ESTADO 
      (append (list (car p-act) ;NOMBRE
                    (string-replace
                     (string-replace
                      (string-replace
                       (string-replace (list-ref p-act 1) ;se reemplaza cada estado por un vacío, ej: Spoink [dormido] -> Spoink                          
                                       " [dormido]" "") " [confundido]" "") " [paralizado]" "") " [envenenado]" ""))
              (drop p-act 2))) ;resto de datos del pokemon para unir con el append
              
    ;junta datos del pokemon(0 a 12)con las energias asignadas que le queden luego del retiro
    (append (take p-sin-estados 13)  
            (take (drop p-sin-estados 13) (- (length (drop p-sin-estados 13)) (list-ref p-sin-estados 7)))))

  ;cambio para jugador especifico
  (define (procesar-cambio jugador)
     ;busca si el jugador tiene la carta en banca
    (define carta-encontrada (remover-de-banca (list-ref jugador 5) (get-card-name carta-banca)))
    
    (if (and carta-encontrada (puede-retirarse? (list-ref jugador 4))) ;si la encuentra y puede retirarse hace el cambio
        (list (car jugador) ;nombre
              (cadr jugador) ;mano
              (caddr jugador) ;mazo
              (cadddr jugador) ;premios 
              carta-encontrada ;carta que sacamos de la banca (el nuevo pokemon activo)
              
              ;si no hay pokemonn activo
              (if (null? (list-ref jugador 4)) 
                  (filtrar-banca (list-ref jugador 5);banca actual
                                 (get-card-name carta-banca));activo a sacar
                  
                  ;si habia un pokemon activo, le quita la energia y limpia (estados) para llevarlo a banca
                  (cons (procesar-retirada (list-ref jugador 4)) 
                        (filtrar-banca (list-ref jugador 5) (get-card-name carta-banca))))
                        
              ;actualizar pila de descarte (añade las energias que se descartaron)
              (if (null? (list-ref jugador 4)) ; SI NO HAY pokemon activo
                  (list-ref jugador 6)         ; SE MANTIENEN LOS DESCARTES
                  
                 ;añadir a la pila de descarte
                  (append (obtener-energias-pagadas (list-ref jugador 4))
                          (list-ref jugador 6)))) 
        jugador))  ;si no se puede retirar, no hay cambio
  
;aplica el cambio al jugador que tiene el turno
  (list (if (= (list-ref game 3) 0) (procesar-cambio (car game)) (car game))
        (if (= (list-ref game 3) 1) (procesar-cambio (cadr game)) (cadr game))
        (list-ref game 2)
        (list-ref game 3)))


; DESC: le asigna una energia a un pokemon en juego (banca o activo)
;       LA carta de energia se elimina de la mano y se añade al final de la lista de pokemon
; Dom: TDA-GAME (list) X cartaPokemon (list) X cartaEnergia (list)
; REC: TDA-GAME (list)
(define (useEnergyCard game pokemon-destino carta-energia)
  (define turno (list-ref game 3)) ;turno (0 o 1)
  (define nombre-dest (get-card-name pokemon-destino)) ;nombre del pokemon a buscar

  (define (adjuntar banca e) ;busca al pokemon y le asigna la energia
    (cond [(null? banca) '()]
          [(coincide-nombre? (get-card-name (car banca)) nombre-dest) ;si lo encuentra, une al pokemon con la energia
           (cons (append (car banca) (list e)) (cdr banca))]

          ;llamado recursivo hasta encontrar al pokemon
          [else (cons (car banca) (adjuntar (cdr banca) e))]))

  ;actualiza al jugador con la eenergia puesta y -1 carta en la mano
  (define (procesar-energia jugador e)
    (cond [(and (not (null? (list-ref jugador 4))) ;verifica que pokemon activo no sea nulo
                (coincide-nombre? (get-card-name (list-ref jugador 4)) nombre-dest)) ; busca que el nombre coincida
                                                                        ;con pokemon activo
           ;pokemon-destino activo
           (list (car jugador) ;nombre
                 (remove e (cadr jugador));quita la energia de la mano
                 (caddr jugador);mazo
                 (cadddr jugador);premios
                 (append (list-ref jugador 4) (list e)) ;le asigna la energia
                 (list-ref jugador 5) (list-ref jugador 6))] ;banca se mantiene (premios)
          [else
           ;si el destino no estaba activo, está en banca
           (list (car jugador)             ;nombre
                 (remove e (cadr jugador)) ;quita energia de la mano
                 (caddr jugador)
                 (cadddr jugador)        
                 (list-ref jugador 4)      ;mantiene los activos
                 (adjuntar (list-ref jugador 5) e) ;modifica la banca
                 (list-ref jugador 6))]))


  ;verifica que no se haya usada una energia en este turno
  (if (or (< (length game) 5) (not (list-ref game 4)))

      ;aplica procesar-energia al jugador quien tiene el turno
      (if (= turno 0)
          (list (procesar-energia (car game) carta-energia) 
                (cadr game) 
                (list-ref game 2) 
                turno 
                #t) ;ya se usó energia en el turno
          
          (list (car game) 
                (procesar-energia (cadr game) carta-energia) 
                (list-ref game 2) 
                turno 
                #t)) ;ya se usó energia en el turno
      
      ;si no es valido, retorna el juego sin cambios
      game))
 

; DESC: Evoluciona algun pokemon que esté en el juego (activo o banca)usando cartA en mano (su evolución)
;      Tambien verifica compatibilidad y que el pokemon lleve al menos 1 turno en juego (indice 11)
; DOM: TDA-GAME (list) X pokemon-a-evolucionar (list card) X carta-evolucion (list card)
; REC: TDA-GAME (list)
(define (evolvePokemon game viejo nueva)
  (define turno (list-ref game 3)) ; ve de quien es el turno
  (define nombre-viejo (get-card-name viejo)); nombre del pokemon a evolucionar

  (define (evolucionar-banca banca); busca al pokemon en banca para reemplazarlo
    (cond [(null? banca) '()]
          
          [(and(equal? (get-card-name (car banca)) nombre-viejo)
               (>= (list-ref (car banca)11)0))                ;une la lista de carta nueva,
                                                             ; pero mantiene turnos, daños, energia
           (cons (append nueva (drop (car banca) 11)) (cdr banca))]

          ;caso de no encontrar al pokemonm sigue buscando.
          [else (cons (car banca) (evolucionar-banca (cdr banca)))]))

  ;actualiza jugadores
  (define (actualizar-jugador pj)
    (define banca-intento (evolucionar-banca (list-ref pj 5))) ;intento de evolucionar banca
    (cond
      [(and (not (null? (list-ref pj 4))) ;pokemon activo
            (equal? (get-card-name (list-ref pj 4)) nombre-viejo) ;verificar si es el pokemon que se desea evolucionar
            (>= (list-ref (list-ref pj 4 )11)0)) ;contador de turno
           
       (list (car pj) ;nombre
             (remove nueva (cadr pj)) ;quita la carta evolucion de la mano
             (caddr pj) ;mazo
             (cadddr pj);premios 
             (append nueva ;junta la carta nueva con datos de la carta vieja (turnos, energias)
                     (drop (list-ref pj 4) 11))
             (list-ref pj 5) ;premios
             (list-ref pj 6))] ;pila descarte
          
      [else ;funcion para priorizar evoluciones al activo (si esta el mismo )
       (define banca-intento (evolucionar-banca (list-ref pj 5)))
       (if (not (equal? banca-intento (list-ref pj 5))) ; intentar evolucionar pokemon de la banca
           (list (car pj) 
                 (remove nueva (cadr pj)) ;quita carta de la mano
                 (caddr pj) 
                 (cadddr pj) 
                 (list-ref pj 4) 
                 banca-intento ;banca actualizada
                 (list-ref pj 6))
           pj)]))

  
  (if (and (compatible? viejo nueva) ;revisa si son evolucion y pre-evoluciones compatibles
           (member nueva (list-ref (if (= turno 0) (car game) (cadr game)) 1))) ;evita duplicados si quiere evolucionar con algo que está en banca
      (if (= turno 0) ;aplica el cambio dependiendo de quien tiene el turno
          (list (actualizar-jugador (car game)) (cadr game) (list-ref game 2) turno)
          (list (car game) (actualizar-jugador (cadr game)) (list-ref game 2) turno))
      game))


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
(define (usePokemonAbility game pokemon-card args)
  (define habilidad (get-pokemon-ability pokemon-card)) ;extrae la lista con info de la habilidad
  
  (if (or (null? habilidad) (not habilidad))
      game ;si no tiene habilidad, retorna el juego solamente (no hay cambios)
      
      ;si encuentra la habilidad:
      ((list-ref habilidad 4) ;busca la funcion lambda a ejecutar
       game args))) 


; DESC: ejecuta un ataque, validando energias calculando el daño, debilidades, premios y ver si gana o no
;        (tambien cambios turnos)
; DOM: juego (list) X pokemon (list) X attack-name (string/null) X args (list)
; REC: juego (list) | string
(define (usePokemonAttack game pokemon attack-name args)
  (define turno (list-ref game 3));turno
  (define atacante (if (= turno 0) (car game) (cadr game))) ;quien atacará
  (define p-activo (list-ref atacante 4)) ;pokemon activo del defensor (quien recibira el daño)
  (define semilla (list-ref game 2))

  (define (intentar-ataque nombre-p atk-buscado)
    (cond 
      ;Si el pokemon esta dormido o paralizado (lo contienen en su nombre) no pueden atacar
      [(or (string-contains? nombre-p "[dormido]") 
           (string-contains? nombre-p "[paralizado]"))
       (pasar-turno game)]
       
      ;Si el pokemon esta confundido, usa lanzar-moneda
      ;cara (0) ataca o Cruz (1) se hace daño el mismo
      
      [(string-contains? nombre-p "[confundido]")
       (if (= (modulo (randomPuro semilla) 2) 0) ;cara
           (ejecucion game p-activo atk-buscado args) ;sigue normal
           (pasar-turno (impacto game p-activo p-activo 30)))] ;se hace daño
           
      ;si no tiene ningun efecto, sigue el juego con normalidad
      [else 
       (ejecucion game p-activo atk-buscado args)]))
  
  (if (or (null? attack-name) (equal? attack-name "") (null? p-activo))
      (pasar-turno game) ;si no encuentra el nombre del ataque o no hay pokemon activo, pasa turno

      ;si no ejecuta la proxima funcion (aplicar el efecto)
      (intentar-ataque (list-ref p-activo 1) (buscar-ataque (get-pokemon-attacks p-activo) attack-name))))
   ;   (ejecucion game p-activo (buscar-ataque (get-pokemon-attacks p-activo) attack-name) args)))

;prepara todo para ejecutar el daño (valida si es posible atacar)
(define (ejecucion game p-atk atk args)
  (if (and atk (>= (length (drop p-atk 13))          ;compara si tiene energias suficientes (mayor o igual )
                   (length (get-attack-cost atk))))  ; al coste del ataque

      ;modifica el juego y se lo pasa a la siguiente funcion la calcula daños
      (danio-combate ((get-attack-effect atk) game args) p-atk atk)
      (pasar-turno game))) ; si falla el ataque, pasa turno

;calcula daño base
(define (danio-combate game-mod p-atk atk)
  (define turno (list-ref game-mod 3))
  (define defensor (if (= turno 0) (cadr game-mod) (car game-mod))) 
  (define p-def (list-ref defensor 4)) ;pokemon defensor (quien recibirá el ataque)
  
  (if (null? p-def);ve si hay pokemon activo
      (pasar-turno game-mod) ;pasa turno si no hay pokemon activo
      
      ;SI TIENE POKEMON ACTIVO: se hace llamado a la funcion que calcula el daño que recibirá
      (multiplicadores game-mod p-atk p-def (obtener-dano-base (get-attack-desc atk)))))

;funcion para aplicar debilidad y resistencias (ampliar daño o reducirlo)
(define (multiplicadores game-mod p-atk p-def d-base)
  (impacto game-mod p-atk p-def
      (max 0 ;daño minimo debe ser 0 (para evitar bugs/curarlo)
           (if (equal? (get-pokemon-type p-atk) (list-ref p-def 6)) ;pregunta si tiene resistencia o no vs el atacante
                 (- (if (equal? (get-pokemon-type p-atk) ; si tiene resistencia
                                (list-ref p-def 5)) (* d-base 2) ;daño base *2
                                                    d-base) 30)  ;baja 30puntos la resistencia

                 ;sii no tiene resistencia
                 (if (equal? (get-pokemon-type p-atk) (list-ref p-def 5)) (* d-base 2) d-base)))))

;fn que procesa el daño y ve si mata al pokemon o no
(define (impacto game-mod p-atk p-def dano)
  (define p-def-danado (append (take p-def 12) ;toma stats del pokewmon
                               (list (+ (list-ref p-def 12) dano)) ;añade el daño acumulado del pokemon
                               (drop p-def 13))) ; enegias que tiene el pokemon
  
  (if (>= (list-ref p-def-danado 12) (list-ref p-def 3)) ; compara el daño acumulado y el HP

      ;SI ES VERDADERA. EL POKEMON MUERE (se descarta)
      (descartes-y-premios game-mod p-def p-def-danado)
      
      ;pasa turno y actualiza el daño (menos hp del pokemon)
      (pasar-turno (actualizar-juego-danio game-mod p-def-danado))))

;Si el pokemon sobrevive hay que actualizar el tda game y su hp
(define (actualizar-juego-danio game p-def-nuevo)
  (define turno (list-ref game 3)) ;turno
  (define (act-defensor def)
    
    (list (car def);nombre
          (cadr def) ;mano
          (caddr def);mazo
          (cadddr def);premios
          p-def-nuevo ;pokemon activo (actualizado)
          (list-ref def 5) ;banca
          (list-ref def 6))) ;descarte
  
  (if (= turno 0);aplica el cambio dependiendo de quien tiene el turno
      (list (car game) (act-defensor (cadr game)) (list-ref game 2) turno)
      (list (act-defensor (car game)) (cadr game) (list-ref game 2) turno)))

(define (descartes-y-premios game-mod p-muerto p-muerto-danado)
  (define turno (list-ref game-mod 3))
  (define cant-premios (if (get-pokemon-is-ex? p-muerto) 2 1)) ;si el pokemon es EX, se lleva 2 premios
                                                               ;SI NO, 1.
  
  (define (matar-defensor def) ;reconstruye al defensor (hacerlo nuevo sin el pokemon activo)
 
    (define pokemon-limpio (take p-muerto-danado 13)) ;stats del pokemon 
    (define energias-botadas (drop p-muerto-danado 13)) ;energias asignadas del pokemon
    (define banca-actual (list-ref def 5)) ;banca


    (if (null? banca-actual)
        ;si no hay banca ni tiene pokemon activo, pierde la partida
        (list (car def) (cadr def) (caddr def) (cadddr def)
              '() ; pokemon activo vacio
              '() ;banca vacia
              (append energias-botadas (cons pokemon-limpio (list-ref def 6))))
        
        (list (car def);nombre
              (cadr def);mano
              (caddr def);mazo
              (cadddr def);premios
              (car banca-actual) ;el primero de la banca sube a Activo 
              (cdr banca-actual) ;resto de la banca se mantiene
              (append energias-botadas (cons pokemon-limpio (list-ref def 6))))));se le agrega el pokemon a la pila d descartes

  
  (define (premiar-atacante atk cant) ;reconstruye al atacante (añade premios)
    (list (car atk) ;nombre
          (append (cadr atk) (take (list-ref atk 3) cant)) ;suma premio a la mano
          (caddr atk) ;mazo
          (drop (list-ref atk 3) cant) ;borra el premio ganado de la pila
          (list-ref atk 4);pokemon activa
          (list-ref atk 5);banca
          (list-ref atk 6)));descarte
  
  (define despues-muerte
    (if (= turno 0);aplica el cambio dependiendo de quien tiene el turno
        (list (premiar-atacante (car game-mod) cant-premios) (matar-defensor (cadr game-mod)) (list-ref game-mod 2) turno)
        (list (matar-defensor (car game-mod)) (premiar-atacante (cadr game-mod) cant-premios) (list-ref game-mod 2) turno)))
  
  (define atk-final (if (= turno 0) (car despues-muerte) (cadr despues-muerte))) ;extrae al atacante despues de muerte de un pokemon
  (define def-final (if (= turno 0) (cadr despues-muerte) (car despues-muerte))) ;defensor despues de muerte de su pokemon


  (cond [(null? (list-ref atk-final 3))  ;revisa premios si al atacante no le quedan gana la partida
         ;imprime mensaje y tda game.
         (display "\n====================================\n")
         (display "  GANADOR POR PREMIOS!!\n")
         (display "======================================\n")
         despues-muerte]

        ;; CASO DE QUE NO QUEDEN POKEMONS ACTIVOS NI EN BANCA
        [(and (null? (list-ref def-final 4)) (null? (list-ref def-final 5))) 
         ;imprime mensaje y tda game.
         (display "\n====================================\n")
         (display " GANADOR RIVAL SIN POKEMON!!\n")
         (display "======================================\n")
         despues-muerte]

        [else (pasar-turno despues-muerte)])) ;sigue la partida (pasan turno)