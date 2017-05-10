namespace sharpCombatTracker

module DiceRoller =
    let roller = System.Random()
    let rec roll sides dice = 
        match dice with
        | 1 -> roller.Next(1, sides + 1)
        | _ -> roller.Next(1, sides + 1) + roll sides (dice - 1)
    let rollInitiative () = roll 20 1

type CombatantType = | Player = 1 | Monster = 2
type Status = | Alive = 1 | Dead = 2
type Name = Name of string
type Modifier = Modifier of int
type Initiative = {Modifier:Modifier ; Initiative:int option ; IsSet: bool}

//Combatant record type
type Combatant = {Name:Name; HP:int; MaxHP:int; Initiative:Initiative ; Type:CombatantType; State:Status}

module Combatant =
    open System
//Active Patterns
    let (|IsAlive|IsDead|) health = if health > 0 then IsAlive else IsDead

    //Functions!
    let takeDamage combatant dam =
        let {HP=currentHP} = combatant
        let health = currentHP - dam
        match health with
        | IsAlive -> {combatant with HP = health}
        | IsDead -> {combatant with HP = health; State=Status.Dead}

    let healDamage combatant heal =
        let {HP=currentHP; MaxHP=maxHp} = combatant
        let totalHealth = Math.Min (currentHP + heal, maxHp)
        {combatant with HP=totalHealth}

    let rollInit combatant =
        let {Modifier = Modifier(modifier)} = combatant.Initiative
        let initiativeRoll = DiceRoller.rollInitiative() + modifier
        let newInitiative =  {Modifier = Modifier(modifier); Initiative = Some(initiativeRoll); IsSet = true}
        {combatant with Initiative = newInitiative}

//Sample Player
(*
    let chauncy = {Name = Name "Chauncy"; HP = 3; MaxHP = 3; 
                Initiative = {Modifier = Modifier(5); Initiative = None; IsSet=false};
                Type = CombatantType.Player; State = Status.Alive}
*)

//Simple initiative interactive test
(*
    let initiativeTest count =
        List.init count (fun _ -> DiceRoller.rollInitiative())
    initiativeTest 1000 |> List.filter (fun x -> x > 20 || x < 1)
    //Should return an empty list as nothing will be above 20 or below 1 based on random's API
    //Tests more for implementation of rolling a d20 than random's utility
*)