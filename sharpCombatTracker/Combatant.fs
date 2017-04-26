type CombatantType = | Player = 1 | Monster = 2
type Status = | Alive = 1 | Dead=2
type Name = Name of string
type Modifier = Modifier of int

//Combatant record type
type Combatant = {Name:Name; InitiativeMod:Modifier; HP:int; MaxHP:int; 
                    Type:CombatantType; State:Status}

//Active Patterns
let (|IsAlive|_|) health = if health > 0 then Some IsAlive else None
let (|IsDead|_|) health = if health <= 0 then Some IsDead else None

//Functions!
let takeDamage combatant dam =
    let {HP=currentHP} = combatant
    let health = currentHP - dam
    let newCombatant = 
        match health with
        | IsAlive -> {combatant with HP = health}
        | IsDead -> {combatant with HP = health; State=Status.Dead}
    newCombatant

let healDamage combatant heal =
    let {HP=currentHP; MaxHP=maxHp} = combatant
    let totalHealth = if currentHP + heal > maxHp then maxHp 
                      else currentHP + heal
    let newCombatant = {combatant with HP=totalHealth}
    newCombatant

//Sample Player
(*
    let chauncy = {Name = Name "Chauncy"; InitiativeMod = Modifier 5; HP = 3; MaxHP = 3;
                   Type = CombatantType.Player; State = Status.Alive}
*)