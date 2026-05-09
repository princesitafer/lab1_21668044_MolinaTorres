#lang racket
;; Importaciones

(require "TDA-CARD_21668044_MolinaTorres.rkt") ;RF02
(require "TDA-ATTACK_21668044_MolinaTorres.rkt") ;RF03
(require "TDA-DECK_21668044_MolinaTorres.rkt")   ;RF04
(require "Func-Game_21668044_MolinaTorres.rkt")
(require "acciones_21668044_MolinaTorres.rkt")



(provide

shuffleDeck          ;RF05
initGame             ;RF06
printGame            ;RF07
playToBench          ;RF08
changeActivePokemon  ;RF09
drawCardFromDeck     ;RF10
useEnergyCard        ;RF11
evolvePokemon        ;RF12
useTrainerCard       ;RF13
usePokemonAbility    ;RF14
usePokemonAttack     ;RF15

;--- TDA CARD
card
 card?
 get-card-type
 get-card-name
 get-pokemon-hp
 get-pokemon-evolve-from
 get-pokemon-type 
 get-pokemon-attacks 
 get-pokemon-is-ex?
 get-pokemon-ability
 get-trainer-type 
 get-trainer-effect

 ;-----TDA ATTACK
  attack
 attack?
 get-attack-cost
 get-attack-name
 get-attack-desc
 get-attack-effect
 ability

 ;------ TDA DECK
  deck
 deck?
 get-deck-cards
 shuffleDeck
 randomPuro

 lanzar-moneda
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
 coincide-nombre?
 )