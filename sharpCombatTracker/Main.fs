namespace sharpCombatTracker

module FlowHelpers =
    type World = { 
        combatants : Combatant list 
        }
    
    type Actions =
        | AddCombatant of Combatant
        | RemoveCombatant of Combatant
        | DamageCombatant of (Combatant * int)
        | HealCombatant of (Combatant * int)
        | Quit

    let removeCombatant combatants (combatant:Combatant) = 
        List.filter (fun t -> t <> combatant) combatants

    //Combatant list -> Combatant -> int -> Combatant list
    let healCombatant combatants combatant amount =
        let localCombatant = List.find (fun t -> t = combatant ) combatants
        let healedCombatant = Combatant.healDamage localCombatant amount
        healedCombatant :: removeCombatant combatants combatant

    let damageCombatant combatants combatant amount =
        let localCombatant = List.find (fun t -> t = combatant ) combatants
        let damagedCombatant = Combatant.takeDamage localCombatant amount
        damagedCombatant :: removeCombatant combatants combatant


    let flow world action  =
        let {combatants = lCombatants} = world
        match action with
        | AddCombatant    (combatant)          -> {world with combatants = combatant :: lCombatants }
        | RemoveCombatant (combatant)          -> {world with combatants = (removeCombatant lCombatants combatant)}
        | DamageCombatant (combatant, damage)  -> {world with combatants = (damageCombatant lCombatants combatant damage)}
        | HealCombatant   (combatant, healing) -> {world with combatants = (healCombatant lCombatants combatant healing)}
        | _                                    -> world 

module Main = 
    
    open System
    open FlowHelpers
    // open Gtk

    [<EntryPoint>]
    let Main(args) = 
        let testCombatant =  {Name = Name "Chauncy"; HP = 3; MaxHP = 3; 
                                Initiative = {Modifier = Modifier(5); Initiative = None; IsSet=false};
                                Type = CombatantType.Player; State = Status.Alive}
        printfn "%A" testCombatant
        // Application.Init()
        // let win = new MainWindow.MyWindow()
        // win.Show()
        // Application.Run()
        0
    