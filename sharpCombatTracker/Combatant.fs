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
let takeDamage state dam =
    let {HP=currentHP} = state
    let health = currentHP - dam
    let newState = 
        match health with
        | IsAlive -> {state with HP = health}
        | IsDead -> {state with HP = health; State=Status.Dead}
        | _ -> state
    newState

let healDamage state heal =
    let {HP=currentHP; MaxHP=maxHp} = state
    let totalHealth = if currentHP + heal > maxHp then maxHp 
                      else currentHP + heal
    let newState = {state with HP=totalHealth}
    newState

//Sample Player
(*
    let chauncy = {Name = Name "Chauncy"; InitiativeMod = Modifier 5; HP = 3; MaxHP = 3;
                   Type = CombatantType.Player; State = Status.Alive}
*)