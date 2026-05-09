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


;; ============================================================
;; RF02b - HABILIDADES
;; ============================================================


(define hab1 (ability "Traspaso de Energía" "Mueve 1 energía de un Pokémon de tu banca a otro" accion-mover-energia))
(define hab2 (ability "Pescar" "Roba 1 carta adicional del mazo" accion-robar-carta))


;; ============================================================
;; RF03 - CARTAS
;; ============================================================


;; Mazo 1: Psíquico / Fuego
(define c1 (card 'pokemon "Gastly" null 40 'psychic 'darkness 'fighting 1 #f null (list a1 a2)))
(define c2 (card 'pokemon "Abra" null 30 'psychic 'darkness null 0 #f hab1 (list a8)))
(define c3 (card 'pokemon "Charmander" null 50 'fire 'water null 1 #f null (list a5 a8)))
(define c4 (card 'pokemon "Haunter" "Gastly" 70 'psychic 'darkness 'fighting 1 #f null (list a2 a3)))
(define c5 (card 'pokemon "Charmeleon" "Charmander" 80 'fire 'water null 1 #f null (list a5 a6)))
(define c6 (card 'pokemon "Mewtwo EX" null 170 'psychic 'darkness null 2 #t null (list a3 a4)))


(define ce-psi (card 'energy "Energía Psíquica" 'psychic))
(define ce-fire (card 'energy "Energía de Fuego" 'fire))


(define ct1 (card 'trainer "Superpoción" "objeto" "Cura 60 PS a 1 Pokémon y descarta 1 energía" accion-superpocion))
(define ct2 (card 'trainer "Pokeball" "objeto" "Busca 1 Pokémon básico en tu mazo" accion-pokeball))
(define ct3 (card 'trainer "Profesora Encina" "partidario" "Descarta tu mano y roba 7 cartas" accion-cambio))


;; Mazo 2: Eléctrico / Agua
(define c7 (card 'pokemon "Pikachu" null 60 'lightning 'fighting null 1 #f null (list a9 a10)))
(define c8 (card 'pokemon "Squirtle" null 50 'water 'lightning null 1 #f null (list a12 a13)))
(define c9 (card 'pokemon "Magnemite" null 40 'lightning 'fighting 'metal 1 #f hab2 (list a9)))
(define c10 (card 'pokemon "Raichu" "Pikachu" 100 'lightning 'fighting null 1 #f null (list a10 a11)))
(define c11 (card 'pokemon "Wartortle" "Squirtle" 70 'water 'lightning null 1 #f null (list a13 a14)))
(define c12 (card 'pokemon "Pikachu EX" null 190 'lightning 'fighting null 2 #t null (list a10 a15)))


(define ce-elec (card 'energy "Energía Eléctrica" 'lightning))
(define ce-water (card 'energy "Energía de Agua" 'water))


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
           ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire
           ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire))


(define d2 (deck
           c7 c7 c7 c7 c10 c10 c10 c8 c8 c8 c8 c11 c11 c11 c9 c9 c9 c9 c12 c12
           ct4 ct4 ct4 ct5 ct5 ct5 ct6 ct6
           ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec
           ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec
           ce-water ce-water ce-water ce-water ce-water ce-water ce-water ce-water
           ce-water ce-water ce-water ce-water ce-water ce-water ce-water ce-water))


;; ============================================================
;; RF06 - INICIO DEL JUEGO
;; Entrada: d1 (Deck), d2 (Deck), 1531312 (semilla int+)
;; La semilla 1531312 se usa para barajar ambos mazos de forma
;; determinista y lanzar la moneda que decide quién comienza.
;; Con la misma semilla el resultado siempre es idéntico.
;; Salida: Juego con mazos barajados, 7 cartas en mano por
;; jugador, 6 cartas de premio por jugador y turno asignado.
;; ============================================================


(define g0 (initGame d1 d2 1531312)) ;; Supongamos este ejemplo provoca que parta jugador 1
(define g0_1 (initGame d1 d2 123)) ;; Supongamos este ejemplo provoca que parta jugador 2
(define g0_2 (initGame d1 d2 3451)) ;; Supongamos este ejemplo provoca que la mano se tenga que sacar 2 veces para el jugador 1


(display "===== ESTADO INICIAL =====\n")
(displayln (printGame g0_1 1))
(display "\n")

; haz que cada recursión sea infinita
;; ============================================================
;; TURNO 1 - Jugador 1
;; J1 roba carta, pone Gastly como activo, adjunta 1 energía
;; psíquica y pasa turno sin atacar.
;; ============================================================


(display "===== TURNO 1 - Jugador 1 =====\n")
(define g1 (drawCardFromDeck g0_1))  ; RF10 drawCardFromDeck
(define g2 (playToBench g1 c1))  ; RF08 playToBench
(define g3 (changeActivePokemon g2 c1))  ; RF09 changeActivePokemon
(define g4 (useEnergyCard g3 c1 ce-psi))  ; RF11 useEnergyCard
(define g5 (usePokemonAttack g4 c1 null '()))  ; RF15 usePokemonAttack
(displayln (printGame g5 1)) 
(display "\n")


;; ============================================================
;; TURNO 2 - Jugador 2
;; J2 roba carta, pone Pikachu como activo, adjunta 1 energía
;; eléctrica y pasa turno sin atacar.
;; ============================================================


(display "===== TURNO 2 - Jugador 2 =====\n")
(define g6 (drawCardFromDeck g5))  ; RF10 drawCardFromDeck
(define g7 (playToBench g6 c7))  ; RF08 playToBench
(define g8 (changeActivePokemon g7 c7))  ; RF09 changeActivePokemon
(define g9 (useEnergyCard g8 c7 ce-elec))  ; RF11 useEnergyCard
(define g10 (usePokemonAttack g9 c7 null '()))  ; RF15 usePokemonAttack
(displayln (printGame g10 2))
(display "\n")

;; ============================================================
;; TURNO 3 - Jugador 1
;; J1 pone Charmander en la banca, adjunta 2da energía psíquica
;; a Gastly y ataca con Pesadilla. Daño base 0; Pikachu puede
;; quedar Dormido según semilla 777 (accion-dormir).
;; ============================================================


(display "===== TURNO 3 - Jugador 1 =====\n")
(define g11 (drawCardFromDeck g10))  ; RF10 drawCardFromDeck
(define g12 (playToBench g11 c3))  ; RF08 playToBench
(define g13 (useEnergyCard g12 c1 ce-psi))  ; RF11 useEnergyCard
(define g14 (usePokemonAttack g13 c1 "Pesadilla" '(777)))  ; RF15 usePokemonAttack
(displayln (printGame g14 1))
(display "\n")


;; ============================================================
;; TURNO 4 - Jugador 2
;; J2 pone Squirtle en la banca, adjunta 2da energía eléctrica
;; a Pikachu y ataca con Impactrueno. 20 daño a Gastly (neutral).
;; Gastly queda con 20/40 PS.
;; ============================================================


(display "===== TURNO 4 - Jugador 2 =====\n")
(define g15 (drawCardFromDeck g14))  ; RF10 drawCardFromDeck
(define g16 (playToBench g15 c8))  ; RF08 playToBench
(define g17 (useEnergyCard g16 c7 ce-elec))  ; RF11 useEnergyCard
(define g18 (usePokemonAttack g17 c7 "Impactrueno" '()))  ; RF15 usePokemonAttack
(displayln (printGame g18 2))
(display "\n")


;; ============================================================
;; TURNO 5 - Jugador 1
;; J1 evoluciona Gastly a Haunter (lleva 4 turnos en juego),
;; adjunta energía de fuego a Charmander (banca) y ataca con
;; Bola Sombra. Haunter hereda energías y daño de Gastly:
;; queda con 50/70 PS. Pikachu recibe 50 daño → 10/60 PS.
;; ============================================================


(display "===== TURNO 5 - Jugador 1 =====\n")
(define g19 (drawCardFromDeck g18))  ; RF10 drawCardFromDeck
(define g20 (evolvePokemon g19 c1 c4))  ; RF12 evolvePokemon
(define g21 (useEnergyCard g20 c3 ce-fire))  ; RF11 useEnergyCard
(define g22 (usePokemonAttack g21 c4 "Bola Sombra" '()))  ; RF15 usePokemonAttack
(displayln (printGame g22 1))
(display "\n")


;; ============================================================
;; TURNO 6 - Jugador 2
;; J2 usa Superpoción en Pikachu (cura 60 PS → 60/60, descarta
;; 1 energía eléctrica), pone Magnemite en la banca, activa la
;; habilidad "Pescar" de Magnemite (roba 1 carta, accion-robar-carta),
;; adjunta 1 energía eléctrica a Pikachu (banca, para preparar
;; la evolución a Raichu) y ataca con Impactrueno.
;; 20 daño a Haunter (neutral) → Haunter queda con 30/70 PS.
;; ============================================================


(display "===== TURNO 6 - Jugador 2 =====\n")
(define g23 (drawCardFromDeck g22))  ; RF10 drawCardFromDeck
(define g24 (useTrainerCard g23 ct5 (list c7 ce-elec))) ; RF13 useTrainerCard
(define g24b (playToBench g24 c9))  ; RF08 playToBench
(define g25 (usePokemonAbility g24b c9 '()))  ; RF14 usePokemonAbility
(define g26 (useEnergyCard g25 c7 ce-elec))  ; RF11 useEnergyCard
(define g27 (usePokemonAttack g26 c7 "Impactrueno" '()))  ; RF15 usePokemonAttack
(displayln (printGame g27 2))
(display "\n")


;; ============================================================
;; TURNO 7 - Jugador 1
;; J1 retira a Haunter pagando 1 energía psíquica como costo de
;; retirada, pone Charmander como activo, adjunta 2da energía de
;; fuego a Charmander y ataca con Ascuas. 30 daño a Squirtle
;; (neutral) → Squirtle queda con 20/50 PS.
;; ============================================================


(display "===== TURNO 7 - Jugador 1 =====\n")
(define g28 (drawCardFromDeck g27))  ; RF10 drawCardFromDeck
(define g29 (changeActivePokemon g28 c3))  ; RF09 changeActivePokemon
(define g30 (useEnergyCard g29 c3 ce-fire))  ; RF11 useEnergyCard
(define g31 (usePokemonAttack g30 c3 "Ascuas" '()))  ; RF15 usePokemonAttack
(displayln (printGame g31 1))
(display "\n")


;; ============================================================
;; TURNO 8 - Jugador 2
;; J2 retira a Pikachu pagando 1 energía eléctrica como costo de
;; retirada, pone Squirtle como activo, adjunta 1 energía de agua
;; a Squirtle y ataca con Burbuja. Daño base 0; Charmander puede
;; quedar Paralizado según semilla 555 (accion-paralizar).
;; Charmander tiene debilidad agua, pero el daño base es 0.
;; ============================================================


(display "===== TURNO 8 - Jugador 2 =====\n")
(define g32 (drawCardFromDeck g31))  ; RF10 drawCardFromDeck
(define g33 (changeActivePokemon g32 c8))  ; RF09 changeActivePokemon
(define g34 (useEnergyCard g33 c8 ce-water))  ; RF11 useEnergyCard
(define g35 (usePokemonAttack g34 c8 "Burbuja" '(555)))  ; RF15 usePokemonAttack
(displayln (printGame g35 2))
(display "\n")


;; ============================================================
;; TURNO 9 - Jugador 1
;; J1 pone Abra en la banca, usa Pokeball (accion-pokeball) para buscar
;; un Pokémon básico del mazo, adjunta energía psíquica a Abra
;; (necesaria para la habilidad en T11) y ataca con Arañazo.
;; Arañazo costo (colorless): Charmander [fire,fire] cubre 
;; 10 daño a Squirtle (neutral) → Squirtle queda con 10/50 PS.
;; ============================================================


(display "===== TURNO 9 - Jugador 1 =====\n")
(define g36 (drawCardFromDeck g35))  ; RF10 drawCardFromDeck
(define g37 (playToBench g36 c2))  ; RF08 playToBench
(define g38 (useTrainerCard g37 ct2 '()))  ; RF13 useTrainerCard
(define g39 (useEnergyCard g38 c2 ce-psi))  ; RF11 useEnergyCard
(define g40 (usePokemonAttack g39 c3 "Arañazo" '()))  ; RF15 usePokemonAttack
(displayln (printGame g40 1))
(display "\n")


;; ============================================================
;; TURNO 10 - Jugador 2
;; J2 usa Profesora Encina (partidario, accion-cambio): descarta su
;; mano y roba 7 cartas nuevas, adjunta 1 energía de agua a
;; Squirtle y ataca con Pistola Agua.
;; Pistola Agua costo (water colorless): Squirtle [water,water] 
;; 40 daño a Charmander. Charmander tiene debilidad agua:
;; daño final = 40 × 2 - 0 = 80 → Charmander KO (50-80=-30 PS).
;; J1 toma 1 carta de premio (Charmander no es EX).
;; ============================================================

(display "===== TURNO 10 - Jugador 2 — KO DE CHARMANDER =====\n")
(define g41 (drawCardFromDeck g40))  ; RF10 drawCardFromDeck
(define g42 (useTrainerCard g41 ct3 '()))  ; RF13 useTrainerCard
(define g43 (useEnergyCard g42 c8 ce-water))  ; RF11 useEnergyCard
(define g44 (usePokemonAttack g43 c8 "Pistola Agua" '()))  ; RF15 usePokemonAttack
(displayln (printGame g44 2))
(display "\n")


;; ============================================================
;; TURNO 11 - Jugador 1
;; Charmander fue KO → J1 pone Haunter al frente sin costo de
;; retirada. Pone Mewtwo EX en la banca, adjunta 2da energía
;; psíquica a Haunter y activa la habilidad "Traspaso de Energía"
;; de Abra (banca): mueve la energía psíquica de Abra a Mewtwo EX
;; (ambos en banca, accion-mover-energia).
;; Haunter ataca con Bola Sombra: costo (psychic psychic) 
;; 50 daño a Squirtle (neutral) → Squirtle KO (10-50=-40 PS).
;; J2 toma 1 carta de premio (Squirtle no es EX).
;; ============================================================


(display "===== TURNO 11 - Jugador 1 — KO DE SQUIRTLE =====\n")
(define g45 (changeActivePokemon g44 c4))  ; RF09 changeActivePokemon
(define g46 (drawCardFromDeck g45))  ; RF10 drawCardFromDeck
(define g47 (playToBench g46 c6))  ; RF08 playToBench
(define g48 (useEnergyCard g47 c4 ce-psi))  ; RF11 useEnergyCard
(define g49 (usePokemonAbility g48 c2 '()))  ; RF14 usePokemonAbility
(define g50 (usePokemonAttack g49 c4 "Bola Sombra" '()))  ; RF15 usePokemonAttack
(displayln (printGame g50 1))
(display "\n")


;; ============================================================
;; TURNO 12 - Jugador 2
;; Squirtle fue KO → J2 pone Pikachu EX al frente sin costo de
;; retirada. Adjunta 2da energía eléctrica a Pikachu EX y ataca
;; con Placaje Tóxico (accion-envenenar).
;; Costo (colorless colorless): Pikachu EX [lightning,lightning] 
;; lightning cubre colorless × 2.
;; 20 daño a Haunter (neutral) → Haunter queda con 10/70 PS.
;; Haunter queda Envenenado.
;; ============================================================


(display "===== TURNO 12 - Jugador 2 =====\n")
(define g51 (changeActivePokemon g50 c12))  ; RF09 changeActivePokemon
(define g52 (drawCardFromDeck g51))  ; RF10 drawCardFromDeck
(define g53 (useEnergyCard g52 c12 ce-elec))  ; RF11 useEnergyCard
(define g54 (usePokemonAttack g53 c12 "Placaje Tóxico" '()))  ; RF15 usePokemonAttack
(displayln (printGame g54 2))
(display "\n")


;; ============================================================
;; TURNO 13 - Jugador 1
;; J1 retira Haunter pagando 1 energía psíquica, pone Mewtwo EX
;; al frente (trae [psychic] de hab1 en T11), evoluciona Pikachu
;; (banca J2, lleva desde T2) a Raichu: Raichu hereda [lightning]
;; adjuntada en T6. Adjunta 2da energía psíquica a Mewtwo EX y
;; ataca con Bola Sombra: costo (psychic psychic) 
;; 50 daño a Pikachu EX (neutral) → Pikachu EX queda con 140/190 PS.
;; ============================================================


(display "===== TURNO 13 - Jugador 1 =====\n")
(define g55 (changeActivePokemon g54 c6))  ; RF09 changeActivePokemon
(define g56 (drawCardFromDeck g55))  ; RF10 drawCardFromDeck
(define g57 (evolvePokemon g56 c7 c10))  ; RF12 evolvePokemon
(define g58 (useEnergyCard g57 c6 ce-psi))  ; RF11 useEnergyCard
(define g59 (usePokemonAttack g58 c6 "Bola Sombra" '()))  ; RF15 usePokemonAttack
(displayln (printGame g59 1))
(display "\n")


;; ============================================================
;; TURNO 14 - Jugador 2
;; J2 adjunta 2da energía eléctrica a Raichu (banca, ahora tiene
;; [lightning,lightning]) y ataca con Pikachu EX usando Electrobola.
;; Costo (lightning colorless): Pikachu EX [lightning,lightning] 
;; 50 daño a Mewtwo EX (neutral) → Mewtwo EX queda con 120/170 PS.
;; ============================================================


(display "===== TURNO 14 - Jugador 2 =====\n")
(define g60 (drawCardFromDeck g59))  ; RF10 drawCardFromDeck
(define g61 (useEnergyCard g60 c10 ce-elec))  ; RF11 useEnergyCard
(define g62 (usePokemonAttack g61 c12 "Electrobola" '()))  ; RF15 usePokemonAttack
(displayln (printGame g62 2))
(display "\n")


;; ============================================================
;; TURNO 15 - Jugador 1
;; J1 adjunta 3ra energía psíquica a Mewtwo EX y ataca con Bola
;; Sombra nuevamente: costo (psychic psychic) 
;; 50 daño a Pikachu EX (neutral) → Pikachu EX queda con 90/190 PS.
;; ============================================================


(display "===== TURNO 15 - Jugador 1 =====\n")
(define g63 (drawCardFromDeck g62))  ; RF10 drawCardFromDeck
(define g64 (useEnergyCard g63 c6 ce-psi))  ; RF11 useEnergyCard
(define g65 (usePokemonAttack g64 c6 "Bola Sombra" '()))  ; RF15 usePokemonAttack
(displayln (printGame g65 1))
(display "\n")


;; ============================================================
;; TURNO 16 - Jugador 2
;; J2 adjunta 3ra energía eléctrica a Raichu (banca):
;; Raichu queda con [lightning,lightning,lightning].
;; Luego retira Pikachu EX pagando 2 energías como costo de
;; retirada y pone Raichu como activo.
;; Raichu ataca con Trueno (accion-dano-variable): semilla 444.
;; Costo (lightning lightning colorless): [lightning,lightning,lightning] 
;; El daño es variable según las monedas simuladas con semilla 444.
;; ============================================================


(display "===== TURNO 16 - Jugador 2 =====\n")
(define g66 (drawCardFromDeck g65))  ; RF10 drawCardFromDeck
(define g67 (useEnergyCard g66 c10 ce-elec))  ; RF11 useEnergyCard
(define g68 (changeActivePokemon g67 c10))  ; RF09 changeActivePokemon
(define g69 (usePokemonAttack g68 c10 "Trueno" '(444)))  ; RF15 usePokemonAttack
(displayln (printGame g69 2))
(display "\n")


;; ============================================================
;; ESTADO FINAL
;; ============================================================


(display "===== ESTADO FINAL =====\n")
(display "--- Vista Jugador 1 ---\n")
(displayln (printGame g69 1))
(display "\n--- Vista Jugador 2 ---\n")
(displayln (printGame g69 2))
(display "\n===== FIN DEL SCRIPT DE EJECUCIÓN =====\n")
