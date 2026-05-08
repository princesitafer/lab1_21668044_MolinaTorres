#lang racket
(require "Func-Game_21668044_MolinaTorres.rkt")

(provide
 robar-n
 accion-dano-variable
 accion-superpocion 
 accion-mover-energia
 accion-sin-efecto
 accion-dormir
 accion-paralizar
 accion-envenenar
 accion-dano-variable
 accion-mover-energia
 accion-robar-carta
 accion-pokeball
 accion-cambio
 )

;DESC: Aplica estado para un pokemon (lo añade a su nombre)
;DOM: pj (list) x sufijo(string)
;REC: list
(define (marcar-activo pj sufijo)
  (if (null? (list-ref pj 4)) pj
      (list (car pj) (cadr pj) (caddr pj) (cadddr pj) ;mantiene nombre, mano mazo y premio
            (append (list (car (list-ref pj 4))                       ;une el nombre original + sufijo
                          (string-append (list-ref (list-ref pj 4) 1) sufijo)) ;(envenenado, dormido)
                    (drop (list-ref pj 4) 2))    ; resto de datos de la carta
            (list-ref pj 5) (list-ref pj 6))))   ; mantiene banca y descarte

;DESC: identifica al defensor (segun el turno) y le aplica el estado a su pokemon
;DOM: juego (list) X sufijo (string)
;REC: list
(define (aplicar-estado-rival juego sufijo)
  (define turno (list-ref juego 3)) ;turno (0 o 1)
  (if (= turno 0) ; si es turno del player 1, se le aplica a player 2 (y viceversa.)     
      (list (car juego) (marcar-activo (cadr juego) sufijo) (list-ref juego 2) turno)
      (list (marcar-activo (car juego) sufijo) (cadr juego) (list-ref juego 2) turno)))


;DESC: vacia la mano del jugador
;DOM: pj (list)
;REC: list
(define (vaciar-mano-pj pj)
  (list (car pj) ;nombre
        '()      ;mano (vacia)
        (caddr pj) ;mazo
        (cadddr pj) ;premios
        (list-ref pj 4) ;activos
        (list-ref pj 5) ;banca
        (list-ref pj 6))) ;descarte


;DESC: descarta la mano para el atacante (quien tiene el turno)
;DOM: juego (list)
;REC: juego (list)
(define (accion-descartar-mano juego)
  (define turno (list-ref juego 3)) ;turno
  (if (= turno 0)  ; si es el turno de player 1, le vacia su mano y mantiene el otro jugador (o viceversa)
      (list (vaciar-mano-pj (car juego)) (cadr juego) (list-ref juego 2) turno)
      (list (car juego) (vaciar-mano-pj (cadr juego)) (list-ref juego 2) turno)))

;DESC: robar n cartas de manera recursiva del mazo, las cuales las añade a su mano
;DOM: juego (list) X n (int)
;REC: juego (list)
;RECURSION: natural
(define (robar-n juego n)
  (if (or (= n 0) (string? juego)) juego (robar-n (drawCardFromDeck juego) (- n 1))))

;DESC: cura al pokemon (reduce daño acumulado del pokemon para posterior quitar energias en juego)
;DOM: p (list) x energia-obj(list)
;REC: list
(define (curar-poke p energia-obj)
  (append (take p 12) 
          (list (if (< (list-ref p 12) 60) 0 (- (list-ref p 12) 60))) 
          (remove energia-obj (drop p 13))))

;DESC: busca en la banca un pokemon por nombre para curarle (reducir su daño acumulado)
;DOM: banca(list) X nombre-obj(string) X energia-obj (list)
;REC: list
;RECURSION: de cola
(define (curar-banca banca nombre-obj energia-obj)
  (cond [(null? banca) '()]
        [(equal? (list-ref (car banca) 1) nombre-obj) (cons (curar-poke (car banca) energia-obj) (cdr banca))]
        [else (cons (car banca) (curar-banca (cdr banca) nombre-obj energia-obj))]))

;DESC: funcion la cual controla a que pokemon curar: si uno de la banca o uno que está activo
;DOM: pj (list)X nombre-obj(string) X energia-obj(list)
;REC: list
(define (curar-pj pj nombre-obj energia-obj)
  (cond [(and (not (null? (list-ref pj 4))) (equal? (list-ref (list-ref pj 4) 1) nombre-obj))
         (list (car pj) (cadr pj) (caddr pj) (cadddr pj) (curar-poke (list-ref pj 4) energia-obj) (list-ref pj 5) (list-ref pj 6))]
        [else (list (car pj) (cadr pj) (caddr pj) (cadddr pj) (list-ref pj 4) (curar-banca (list-ref pj 5) nombre-obj energia-obj) (list-ref pj 6))]))

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

;DESC: Mueve la energía de un pokemon a otro dentro de la banca
;DOM: banca (list)
;REC: banca (list)
(define (mover-energia-banca banca)
  (if (and (>= (length banca) 2) (> (length (car banca)) 13))
      (cons (take (car banca) (- (length (car banca)) 1))
            (cons (append (cadr banca) (list (list-ref (car banca) (- (length (car banca)) 1))))
                  (cddr banca)))
      banca))

;DESC: funcion la cual procesa el estado del jugador luego de mover la energia (apoya la funcion anterior!)
;DOM: pj (lista)
;REC: list
(define (procesar-mover-pj pj)
  (list (car pj) (cadr pj) (caddr pj) (cadddr pj) (list-ref pj 4) (mover-energia-banca (list-ref pj 5)) (list-ref pj 6)))


;IMPLEMENTACION
(define accion-sin-efecto 
  (lambda (juego args) juego))

(define accion-dormir 
  (lambda (juego args) (aplicar-estado-rival juego " [dormido]")))

(define accion-paralizar 
  (lambda (juego args) (aplicar-estado-rival juego " [paralizado]")))

(define accion-envenenar 
  (lambda (juego args) (aplicar-estado-rival juego " [envenenado]")))

(define accion-dano-variable 
  (lambda (juego args)
    (define turno (list-ref juego 3))
    (define dano (* 50 (lanzar-moneda (if (null? args) 0 (car args)))))
    (if (= turno 0)
        (list (car juego)
              (daniar-activo (cadr juego) dano) (list-ref juego 2) turno)
        (list (daniar-activo (car juego) dano)
              (cadr juego) (list-ref juego 2) turno))))

(define accion-mover-energia 
  (lambda (juego args)
    (define turno (list-ref juego 3))
    (if (= turno 0)
        (list (procesar-mover-pj (car juego)) (cadr juego) (list-ref juego 2) turno)
        (list (car juego) (procesar-mover-pj (cadr juego)) (list-ref juego 2) turno))))

(define accion-robar-carta 
  (lambda (juego args) (drawCardFromDeck juego)))

(define accion-superpocion 
  (lambda (juego args)
    (define turno (list-ref juego 3)) ;turno
    (define nombre-poke (list-ref (car args) 1)) ;nombre del pokemon a curar
    (define energia-poke (cadr args))            ;energia del pokemon a quitar
    
    (define (curar p)
      (append (take p 12) (list (if (< (list-ref p 12) 60) ; mira el contador de daño acumulado
                                    ; si >60 = daño acumulado (0)
                                    ; si es mayor el daño acumulado solo resta 60
                                    0 (- (list-ref p 12) 60)))
              (remove energia-poke (drop p 13)))) ;elimina la energia (energia-poke)
      
    (define (recorre-banca b) ;busqueda en banca
      (cond [(null? b) '()]
            [(coincide-nombre? (list-ref (car b) 1) nombre-poke) ;verifica si es el pokemon (aun con un efecto, como dormido)
             (cons (curar (car b)) (cdr b))]  ;si encuentra el pokemon, lo cura!
            [else (cons (car b) (recorre-banca (cdr b)))]))
            
    (define (act-poke pj) ;revisa si el pokemon que va a curar es el activo
      (cond [(and (not (null? (list-ref pj 4)))
                  (coincide-nombre? (list-ref (list-ref pj 4) 1) nombre-poke)) 
             (list (car pj) (cadr pj) (caddr pj) (cadddr pj) 
                   (curar (list-ref pj 4)) (list-ref pj 5) (list-ref pj 6))] ;si es el pokemon activo, lo cura ahi mismo!!

            ;si no, se asume que el pokemon está en banca
            [else (list (car pj) (cadr pj) (caddr pj) (cadddr pj) (list-ref pj 4) (recorre-banca (list-ref pj 5)) (list-ref pj 6))]))
            
    (if (= turno 0) ;le aplica cambios al jugador que tiene el turno
        (list (act-poke (car juego)) (cadr juego) (list-ref juego 2) turno)
        (list (car juego) (act-poke (cadr juego)) (list-ref juego 2) turno))))


(define accion-pokeball 
  (lambda (juego args)
    (define turno (list-ref juego 3))
    
    ;revisa si una carta es un pokemon basico
    (define (es-poke-basico? c)
      (and (equal? (car c) 'pokemon)
           (or (null? (list-ref c 2)) ;verifica si tiene pre evolucion o no (indice 2)
               (equal? (list-ref c 2) '()) 
               (equal? (list-ref c 2) #f) 
               (equal? (list-ref c 2) ""))))
               
    ;recorre el mazo con recursion hasta encontrar el primer basico
    (define (buscar-basico mazo)
      (cond [(null? mazo) #f] ;caso base: no hay basico en el mazo
            [(es-poke-basico? (car mazo)) (car mazo)] ;si lo encuentra retorna la carta
            [else (buscar-basico (cdr mazo))])) ;llamado recursivo para seguir buscando
            
    ;revisa el mazo del jugador y une todo al localizar la carta
    (define (procesar-jugador pj)
      (define carta-encontrada
        (buscar-basico (list-ref pj 2))) ; indice -> mazo
      
      (if carta-encontrada
          ;si encontro un pokemon basico, reconstruye el juego
          (list (car pj)                                     ;nombre
                (append (cadr pj) (list carta-encontrada))   ;mano (pokemon añadido al final) 
                (remove carta-encontrada (caddr pj))         ;mazo (se saca la carta pokemon)
                (cadddr pj)                                  ;premios
                (list-ref pj 4)                              ;activo
                (list-ref pj 5)                              ;banca
                (list-ref pj 6))                            ;descarte
                
                pj)) ;si el mazo no tenia basico, no cambia nada en el juego
          

      (if (= turno 0) ;le aplica cambios al jugador que tiene el turno
          (list (procesar-jugador (car juego)) (cadr juego) (list-ref juego 2) turno)
          (list (car juego) (procesar-jugador (cadr juego)) (list-ref juego 2) turno))))

  (define accion-cambio 
    (lambda (juego args) (robar-n (accion-descartar-mano juego) 7)))