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

    ///Takes a world, performs an action, and returns the new state of the world.
    let flow world action  =
        let {combatants = lCombatants} = world
        match action with
        | AddCombatant    (combatant)          -> {world with combatants = combatant :: lCombatants }
        | RemoveCombatant (combatant)          -> {world with combatants = (removeCombatant lCombatants combatant)}
        | DamageCombatant (combatant, damage)  -> {world with combatants = (damageCombatant lCombatants combatant damage)}
        | HealCombatant   (combatant, healing) -> {world with combatants = (healCombatant lCombatants combatant healing)}
        | _                                    -> world 

    let createCombatant name curHP maxHP modifier cType =
        {Name = name; HP = curHP; MaxHP = maxHP;
         Initiative = {Modifier = modifier; Initiative = None; IsSet = false};
         Type = cType; State = Status.Alive}

    let parseCombatantType cType =
        System.Enum.Parse(typeof<CombatantType>, cType)
        :?> CombatantType //Downcast needed for parsing. Returns obj normally

