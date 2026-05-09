#lang racket
(require "MAIN_21668044_MolinaTorres.rkt")

;; ============================================================
;; RF02 - ATAQUES
;; ============================================================


(define a1 (attack '(psychic) "Pequeña Rabieta" "Inflige 20 de daño" accion-sin-efecto))
(define a2 (attack '(psychic colorless) "Pesadilla" "Lanza 1 moneda. Si sale cara, el Pokémon Activo rival queda Dormido" accion-dormir))
(define a3 (attack '(psychic psychic) "Bola Sombra" "Inflige 50 de daño" accion-sin-efecto))
(define a4 (attack '(psychic psychic colorless) "Golpe Psíquico" "Inflige 80 de daño" accion-sin-efecto))
(define a5 (attack '(fire) "Ascuas" "Inflige 30 de daño" accion-sin-efecto))
(define a6 (attack '(fire colorless) "Lanzallamas" "Inflige 70 de daño. Descarta 1 energía de fuego de este Pokémon" accion-sin-efecto))
(define a7 (attack '(fire fire colorless) "Llamarada" "Inflige 120 de daño" accion-sin-efecto))
(define a8 (attack '(colorless) "Arañazo" "Inflige 10 de daño" accion-sin-efecto))
(define a9 (attack '(lightning) "Impactrueno" "Inflige 20 de daño" accion-sin-efecto))
(define a10 (attack '(lightning colorless) "Electrobola" "Inflige 50 de daño" accion-sin-efecto))
(define a11 (attack '(lightning lightning colorless) "Trueno" "Inflige daño variable según monedas" accion-dano-variable))
(define a12 (attack '(water) "Burbuja" "Lanza 1 moneda. Si sale cara, el Pokémon Activo rival queda Paralizado" accion-paralizar))
(define a13 (attack '(water colorless) "Pistola Agua" "Inflige 40 de daño" accion-sin-efecto))
(define a14 (attack '(water water colorless) "Hidrobomba" "Inflige 90 de daño" accion-sin-efecto))
(define a15 (attack '(colorless colorless) "Placaje Tóxico" "Inflige 20 de daño y envenena al rival" accion-envenenar))
(define a16 (attack '(fire) "Llama Menor" "Inflige 20 de daño" accion-sin-efecto))
(define a17 (attack '(fire fire) "Giro Fuego" "Inflige 60 de daño" accion-sin-efecto))
(define a18 (attack '(grass colorless) "Picotazo Venenoso" "Inflige 30 de daño y envenena" accion-envenenar))
(define a19 (attack '(grass grass colorless) "Tierra Viva" "Inflige 90 de daño" accion-sin-efecto))
(define a20 (attack '(psychic colorless) "Explosión Mística" "Inflige 150 de daño" accion-sin-efecto))

;; ============================================================
;; RF02b - HABILIDADES
;; ============================================================


(define hab1 (ability "Traspaso de Energía" "Mueve 1 energía de un Pokémon de tu banca a otro" accion-mover-energia))
(define hab2 (ability "Pescar" "Roba 1 carta adicional del mazo" accion-robar-carta))


;; ============================================================
;; RF03 - CARTAS
;; ============================================================


;; Mazo 1: Psíquico / hada
(define c1 (card 'pokemon "Spoink" null 50 'psychic 'darkness 'fighting 1 #f null (list a1 a2)))
(define c2 (card 'pokemon "Mew" null 60 'psychic 'darkness null 0 #f hab1 (list a8)))
(define c3 (card 'pokemon "Fidough" null 60 'fairy 'metal null 1 #f null (list a4 a5)))
(define c4 (card 'pokemon "Grumpig" "Spoink" 100 'psychic 'darkness 'fighting 2 #f null (list a2 a3)))
(define c5 (card 'pokemon "Dachsbun" "Fidough" 90 'fairy 'metal null 2 #f hab2 (list a5 a6)))
(define c6 (card 'pokemon "Colagrito EX" null 190 'psychic 'psychic null 1 #f null (list a1 a8)))

(define ce-psi (card 'energy "Energía Psíquica" 'psychic))
(define ce-fairy (card 'energy "Energía Hada" 'fairy))


(define ct1 (card 'trainer "Superpoción" "objeto" "Cura 60 PS a 1 Pokémon y descarta 1 energía" accion-superpocion))
(define ct2 (card 'trainer "Pokeball" "objeto" "Busca 1 Pokémon básico en tu mazo" accion-pokeball))
(define ct3 (card 'trainer "Profesora Iono" "partidario" "Descarta tu mano y roba 3 cartas" accion-cambio))


;; Mazo 2: planta / fuego
(define c7 (card 'pokemon "Vulpix" null 60 'fire 'water null 1 #f null (list a16)))
(define c8 (card 'pokemon "Nidorina" null 90 'grass 'fire null 2 #f null (list a18))) 
(define c9 (card 'pokemon "Scyther" null 80 'grass 'fire 'fighting 1 #f hab1 (list a18)))
(define c10 (card 'pokemon "Ninetales" "Vulpix" 110 'fire 'water null 1 #f null (list a16 a17)))
(define c11 (card 'pokemon "Nidoqueen" "Nidorina" 170 'grass 'fire null 3 #f null (list a18 a19)))
(define c12 (card 'pokemon "Gardevoir EX" null 190 'psychic 'metal null 2 #t null (list a20)))

(define ce-grass (card 'energy "Energía Planta" 'grass))
(define ce-fire (card 'energy "Energía Fuego" 'fire))


(define ct4 (card 'trainer "Cambio" "objeto" "Intercambia tu Pokémon Activo sin pagar retirada" accion-cambio))
(define ct5 (card 'trainer "Superpoción" "objeto" "Cura 60 PS a 1 Pokémon y descarta 1 energía" accion-superpocion))
(define ct6 (card 'trainer "Profesora Encina" "partidario" "Descarta tu mano y roba 7 cartas" accion-cambio))


;; ============================================================
;; RF04 - MAZOS
;; d1: 4+3+4+4+3+2+3+3+2+16+16 = 60 cartas
;; d2: 4+3+4+3+4+2+3+3+2+16+16 = 60 cartas
;; ============================================================


(define d1 (deck
           c1 c1 c1 c1 c4 c4 c4 c2 c2 c2 c2 c3 c3 c3 c3 c5 c5 c5 c6 c6
           ct1 ct1 ct1 ct2 ct2 ct2 ct3 ct3
           ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi
           ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi
           ce-fairy ce-fairy ce-fairy ce-fairy ce-fairy ce-fairy ce-fairy ce-fairy
           ce-fairy ce-fairy ce-fairy ce-fairy ce-fairy ce-fairy ce-fairy ce-fairy))


(define d2 (deck
           c7 c7 c7 c7 c10 c10 c10 c8 c8 c8 c8 c11 c11 c11 c9 c9 c9 c9 c12 c12
           ct4 ct4 ct4 ct5 ct5 ct5 ct6 ct6
           ce-grass ce-grass ce-grass ce-grass ce-grass ce-grass ce-grass ce-grass
           ce-grass ce-grass ce-grass ce-grass ce-grass ce-grass ce-grass ce-grass
           ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire
           ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire))


;; ============================================================
;; RF06 - INICIO DEL JUEGO

;; ============================================================


(define g0 (initGame d1 d2 1111))  ;comienza jugador 1
(define g0_1 (initGame d1 d2 7222222222222)) ;comienza jugador 2
(define g0_2 (initGame d1 d2 64126)) ; mazos sin un pokemon basico (shuffledeck en uso)


(displayln (printGame g0 1)) ;; aqui debe cambiarse quien comienza 1 o 2
(display "\n")

;; ============================================================
;; TURNO 1 
;; J1 roba carta dos veces, pone Spoink como activo, pone a otro spoink en la
;; banca, intenta evolucionar a Spoink con una la carta spoink.
;; (la evolucion no debe cumplirse, ya que es el 1er turno y no coincide pre-evolucion)
;; (el daño tampoco se cumple, no tiene a nadie para dañar)
;; ============================================================


(display "===== TURNO 1 =====\n")
(define g1 (drawCardFromDeck g0))
(define g2 (drawCardFromDeck g1)) 
(define g3 (playToBench g2 c1))
(define g4 (changeActivePokemon g3 c1))
(define g5 (playToBench g4 c1))
(define g6 (evolvePokemon g5 c1 c1))
(define g7 (useEnergyCard g6 c1 ce-psi))
(define g8 (usePokemonAttack g7 c1 null '())) ;pasa turno

(displayln (printGame g8 1)) 
(display "\n")


;; ============================================================
;; TURNO 2
;; J2 roba carta, pone Vulpix como activo, adjunta 1 energía
;; fuego y ataca a spoink, spoink queda 30/50 HP.
;; ============================================================


(display "===== TURNO 2  =====\n")
(define g9 (drawCardFromDeck g8))  
(define g10 (playToBench g9 c7))  
(define g11 (changeActivePokemon g10 c7))  
(define g12 (useEnergyCard g11 c7 ce-fire))
(define g13 (usePokemonAttack g12 c7 "Llama Menor" '()))

(displayln (printGame g13 2))
(display "\n")


;; ============================================================
;; TURNO 3 
;; J1 SZACA CARTA, adjunta 2da energía psíquica
;; a spoink. iNTENTA PONER UNA SEGUNDA ENERGÍA
;; (TAN SOLO SE PUEDE PONER UNA ENERGIA POR TURNO), POR LO QUE NO
;; PASA NADA.
;; Luego Spoink ataca con Pesadilla. Daño base 0; VULPIX puede
;; quedar Dormido según semilla 1 (accion-dormir)
;; ============================================================


(display "===== TURNO 3 =====\n")
(define g14 (drawCardFromDeck g13))  ; RF10 drawCardFromDeck
(define g15 (useEnergyCard g14 c1 ce-psi))  ; RF11 useEnergyCard
(define g16 (useEnergyCard g15 c1 ce-psi))
(define g17 (usePokemonAttack g16 c1 "Pesadilla" '(1)))  ; RF15 usePokemonAttack
(displayln (printGame g17 1))
(display "\n")


;; ============================================================
;; TURNO 4 
;; J2 roba carta, pone Nidorina en la banca, adjunta energía
;; fuego a Vulpix y ataca con Llama Menor.
;; 20 daño a Spoink -> Spoink queda con 10/50 HP
;; ============================================================


(display "===== TURNO 4 =====\n")
(define g18 (drawCardFromDeck g17))
(define g19 (playToBench g18 c8))               
(define g20 (useEnergyCard g19 c7 ce-fire))     ;energía fuego a vulpix
(define g21 (usePokemonAttack g20 c7 "Llama Menor" '()))  ;20 daño
(displayln (printGame g21 2))
(display "\n")


;; ============================================================
;; TURNO 5 - Jugador 1
;;J1 roba carta y evoluciona Spoink a Grumpig (SI ES QUE ESTÁ EN LA MANO)
;; Se le asigna una energia psíquica.
;; USA TRAINER CARD (iono)

;; ============================================================


(display "===== TURNO 5  =====\n")
(define g22 (drawCardFromDeck g21))
(define g23 (evolvePokemon g22 c1 c4))        
(define g24 (playToBench g23 c3))                
(define g25 (useEnergyCard g24 c1 ce-psi))       
(define g26 (useTrainerCard g25 ct3 '()))   ;DESCARTA MANO Y ROBA 3 CARTAS

(define g27 (usePokemonAttack g26 c1 "Pequeña Rabieta" '())) ;PASA TURNO

(displayln (printGame g27 1))
(display "\n")


;; ============================================================
;; TURNO 6 - Jugador 2
;; se pone scyther en la banca y se pone energia planta a nidorina en banca
;; vulpix ataca con 
;; Llama Menor: 20 daño a grumpig (40/100)
;;============================================================


(display "===== TURNO 6  =====\n")
(define g28 (drawCardFromDeck g27)) 
(define g29 (playToBench g28 c9))              
(define g30 (useEnergyCard g29 c8 ce-grass))    
(define g31 (usePokemonAttack g30 c7 "Llama Menor" '()))  ; 20 daño
(displayln (printGame g31 2))
(display "\n")

;; ============================================================
;; TURNO 7 - Jugador 
;;  JUGADOR USA POKEBALL (EXTRAE UN POKEMON BASICO DE SU BARAJA A SU MANO)
;; JUGADOR PONE ACTIVO A FIDOUGH
;; SE DESCARTAN 2 ENERGIAS POR RETIRAR A GRUMPIG
;; LUEGO DE LA RETIRADA, ATACA FIDOUGH pero el ataque necesita 3 energias
;; no sucede nada.
;;============================================================


(display "===== TURNO 7  =====\n")
(define g32 (playToBench g31 c3))    
(define g33 (changeActivePokemon g32 c3))
(define g34 (drawCardFromDeck g33))         
(define g35 (useTrainerCard g34 ct2 '()))   
(define g36 (useEnergyCard g35 c1 ce-psi))  
(define g37 (changeActivePokemon g36 c3))
(define g38 (useEnergyCard g37 c3 ce-fairy)) 
(define g39 (usePokemonAttack g38 c3 "Golpe Psíquico" '()))

(displayln (printGame g39 1))
(display "\n")


;; ============================================================
;; TURNO 8 - Jugador 2
;; J1 roba carta
;; usa superpocion en vulpix por lo que se cura 60 PS y  descarta 1 energia fuego
;; vulpix NO SE CURA NADA, YA QUE TENIA TODA LA VIDA
;; adjunta una 2da energía planta a nidorina en banca
;; vulpix ataca con llama menor: 20 daño a Fidough
;; fidough queda con 40/60 PS
;; ============================================================
(display "===== TURNO 8 =====\n")
(define g40 (drawCardFromDeck g39))
(define g41 (useTrainerCard g40 ct5 (list c7 ce-fire)))  ;superpocion
(define g42 (useEnergyCard g41 c8 ce-grass))             ;2da energia nidorina
(define g43 (usePokemonAttack g42 c7 "Llama Menor" '())) ; 20 daño 
(displayln (printGame g43 2))
(display "\n")

;; ============================================================
;; TURNO 9 - Jugador 1
;; roba carta
;; pone  a mew en la banca
;; adjunta energia hada a fidough, pero sus ataques necesitan
;; energia psiquica o fuego por lo que no puede atacar...
;; pasa turno sin hacer daño
;; ============================================================


(display "===== TURNO 9 =====\n")
(define g44 (drawCardFromDeck g43))
(define g45 (playToBench g44 c2))               ;mew a la banca
(define g46 (useEnergyCard g45 c3 ce-fairy))    ;2da energia a fidoug
(define g47 (usePokemonAttack g46 c3 null '())) ; pasa turno
(displayln (printGame g47 1))
(display "\n")


;; ============================================================
;; TURNO 10  J2
;; roba carta y juega otro scyther a banca
;; utiliza la trainercard de profesora Encina (descarta la mano y
;; roba 7 cartas) Y ATACA a FIDOUGH (Queda 20/100 hp)
;; ============================================================

(display "===== TURNO 10 =====\n")
(define g48 (drawCardFromDeck g47))
(define g49 (playToBench g48 c9)) 
(define g50 (useTrainerCard g49 ct6 '())) 
(define g51 (usePokemonAttack g50 c7 "Llama Menor" '())) 
(displayln (printGame g51 2))
(display "\n")


;; ============================================================
;; TURNO 11 
; J1 roba carta
;; baja un nuevo spoink de su mano a la banca y le 
;; asigna energia hada a fidough en banca
;; intenta atacar con fidough, vulpix enemigo en banca queda 30/60 ps
;; ============================================================

(display "===== TURNO 11 =====\n")
(define g52 (drawCardFromDeck g51))                  
(define g53 (playToBench g52 c1))                    
(define g54 (useEnergyCard g53 c3 ce-fairy))         
(define g55 (usePokemonAttack g54 c3 "Ascuas" '())) ;;30 daño
(displayln (printGame g55 1))
(display "\n")



;; ============================================================
;; TURNO 12 - Jugador 2
;; Roba carta y pone a vulpix nuevo en su banca.
;; le pone una energia fuego al vulpix activo
;; ataca a fidough y lo mata (se descarta), por lo que entra
;; un spoink enemigo a estar activo.
;; gana 1 premio
;; ============================================================

(display "===== TURNO 12 =====\n")
(define g56 (drawCardFromDeck g55))
(define g57 (playToBench g56 c7))
(define g58 (useEnergyCard g57 c7 ce-fire))
(define g59 (usePokemonAttack g58 c7 "Llama Menor" '()))

(displayln (printGame g59 2))
(display "\n")


;; ============================================================
;; TURNO 13 - Jugador 1
;; J1 roba carta
;; evoluciona a spoink activo -> pasa a ser grumpig
;; usa energia hada y se la asigna a grumpig activo
;; GRUMPig intenta atacar, le faltan energias por lo tanto el
;; ataque falla, pasa el turno
;; ============================================================


(display "===== TURNO 13 =====\n")

(define g60 (drawCardFromDeck g59)) 
(define g61 (evolvePokemon g60 c1 c4))
(define g62 (useEnergyCard g61 c4 ce-fairy))
(define g63 (usePokemonAttack g62 c4 "Bola Sombra" '()))

(displayln (printGame g63 1))
(display "\n")

;; ============================================================
;; TURNO 14 
;; roba carta
;; evoluciona a vulpix activo, se convierte en ninetales
;; usa energia fuego en ninetales y ataca.
;; grumpig queda 80/100ps
;;  ============================================================


(display "===== TURNO 14 =====\n")
(define g64 (drawCardFromDeck g63))
(define g65 (evolvePokemon g64 c7 c10))
(define g66 (useEnergyCard g65 c10 ce-fire))
(define g67 (usePokemonAttack g66 c10 "Llama Menor" '())) ;;20 daño
(displayln (printGame g67 2))
(display "\n")



;; ============================================================
;; TURNO 15 - Jugador 1
;; roba carta
;; le asigna una energia hada a grumpig activo
;; grumpig ataca, golpe psiquico pero le faltan energias
;; falla el ataque.
;; pasa turno
;; ============================================================


(display "===== TURNO 15 =====\n")
(define g68 (drawCardFromDeck g67))
(define g69 (useEnergyCard g68 c4 ce-fairy)) 
(define g70 (usePokemonAttack g69 c4 "Golpe Psíquico" '())) 
(displayln (printGame g70 1))
(display "\n")


;; ============================================================
;; TURNO 16 - Jugador 2
;; roba carta y le asigna una tercera carta a ninetales.
;; ninetales atacam
;; grumpig queda con 20/100 ps.
;; ============================================================

(display "===== TURNO 16 =====\n")
(define g71 (drawCardFromDeck g70)) 
(define g72 (useEnergyCard g71 c10 ce-fire)) 
(define g73 (usePokemonAttack g72 c10 "Giro Fuego" '())) 

(displayln (printGame g73 2))
(display "\n")

;; ============================================================
;;  TURNO 17 - Jugador 1
;; ROBA 5 CARTAS
;; AL ROBAR, SACA ENERGIA PSIQUICA, SE LA ASIGNA A GRUMPIG.
;; UTILIZA POKEBALL
;; Y ATACA CON BOLA SOMBRA.
;; ataque falla, PASA TURNO
;; ============================================================


(display "===== TURNO 17  =====\n")

(define g74 (drawCardFromDeck g73)) 
(define g75 (drawCardFromDeck g74))
(define g76 (drawCardFromDeck g75))
(define g77 (drawCardFromDeck g76))
(define g78 (drawCardFromDeck g77))

(define g79 (useTrainerCard g78 ct2 '()))
(define g80 (useEnergyCard g79 c4 ce-psi))
(define g81 (usePokemonAttack g80 c4 "Bola Sombra" '())) 

(displayln (printGame g81 1))
(display "\n")


;; ============================================================
;;  TURNO 18 - Jugador 2
;; ROBA 1 CARTA
;; UTILIZA SUPERPOCION. QUEDA CON 90/110 PS Y SE LE DESCARTA UNA ENERGIA DE FUEGO
;; ataca con llama menor a grumpig. MATA A GRUMPIG Y ENTRA EN COMBATE MEW.
;; GANA +1 PREMIO.
;; PREMIOS RESTANTES: 4
;; ============================================================

(display "===== TURNO 18  =====\n")

(define g82 (drawCardFromDeck g81))
(define g83 (useTrainerCard g82 ct5 (list c10 ce-fire)))
(define g84 (usePokemonAttack g83 c10 "Llama Menor" '())) 
(displayln (printGame g84 2))
(display "\n")


;; ============================================================
;;  TURNO 19 - Jugador 1
;; ROBA 1 CARTa
;; mew ocupa su habilidad "Traspaso de Energía"
;; pasa turno
;; ============================================================
(display "===== TURNO 19  =====\n")

(define g85 (drawCardFromDeck g84)) 
(define g86 (usePokemonAbility g85 c2 '())) 
(define g87 (usePokemonAttack g86 c2 null '())) 

(displayln (printGame g87 1))
(display "\n")


;; ============================================================
;;  TURNO 20 - Jugador 2
;;ROBA 1 CARTA Y EVOLUCIONA A NIDORINA A NIDOQUEEN (EN BANCA)
;; LE ASIGNA UNA NUEVA ENERGIA ENERGIA A NINETALES Y ATACA A
;; MEW CON GIRO FUEGO.
;; MATA A MEW, GANA +1 PREMIO Y ELL GRUMPIG ENEMIGO PASA A ACTIVO.
;; ============================================================
(display "===== TURNO 20 =====\n")

(define g88 (drawCardFromDeck g87)) 
(define g89 (evolvePokemon g88 c8 c11)) 
(define g90 (useEnergyCard g89 c10 ce-fire)) 

(define g91 (usePokemonAttack g90 c10 "Giro Fuego" '())) 
(displayln (printGame g91 2))
(display "\n")


;; ============================================================
;;  TURNO 21 - Jugador 2
;;ROBA 1 CARTA Y ASIGNA ENERGIA SIQUICA A GRUMPIG EN JUEGO.
;; INTENTA ATACAR PERO NO TIENE ENERGIA.
;; FALLA ATAQUE, PASA TURNO.
;; ============================================================
(display "===== TURNO 21 =====\n")

(define g92 (drawCardFromDeck g91))
(define g93 (useEnergyCard g92 c4 ce-psi))

(define g94 (usePokemonAttack g93 c4 "Bola Sombra" '()))
(displayln (printGame g94 1))
(display "\n")


;; ============================================================
;;  TURNO 21 - Jugador 2
;;ROBA 1 CARTA Y NINETALES ATACA A GRUMPIG, LO MATE
;; GANA PREMIO Y ENTRA SPOINK AL COMBATE.
;; ============================================================
(display "===== TURNO 22 =====\n")

(define g95 (drawCardFromDeck g94))

(define g96 (usePokemonAttack g95 c10 "Giro Fuego" '()))

(displayln (printGame g96 2))
(display "\n")

;; ============================================================
;;  TURNO 23 - Jugador 1
;; ROBA 1 CARTA
;; SPOINK ESTÁ ACTIVO CON 2 ENERGIAS SIQUICAS
;; ATACA A NINETALES, NINETALES QUEDA 70/110PS
;; ============================================================
(display "===== TURNO 23 - Jugador 1 =====\n")
(define g97 (drawCardFromDeck g96))
(define g98 (usePokemonAttack g97 c1 "Golpe Psíquico" '()))

(displayln (printGame g98 1))
(display "\n")

;; ============================================================
;;  TURNO 24 - Jugador 2 
;; ROBA 2 CARTAS
;; Ninetales ataca con giro fuego a spoink (que tiene 50 HP)
;; mata a spoink
;; JUGADOR 1 NO TIENE POKEMON ACTIVO, NI EN BANCA.
;; JUGADOR 1 PIERDE.
;; ============================================================
(display "===== TURNO 24 =====\n")

(define g99 (drawCardFromDeck g98))
(define g100 (drawCardFromDeck g99))
(define g101 (usePokemonAttack g100 c10 "Giro Fuego" '()))
(displayln (printGame g101 2))
(display "\n")

(display "===== ESTADO FINAL =====\n")
(display "--- Vista Jugador 1 ---\n")
(displayln (printGame g101 1))
(display "\n--- Vista Jugador 2 ---\n")
(displayln (printGame g101 2))
(display "\n===== FIN DEL SCRIPT DE EJECUCIÓN =====\n")