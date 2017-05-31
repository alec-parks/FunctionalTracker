namespace sharpCombatTracker

module Main = 
    
    open System
    open FlowHelpers
    // open Gtk

    //This be ugly... //TODO
    let characterPrompt () =
        printfn "Name?"
        let cName = Name ( Console.ReadLine() )
        printfn "Max HP?"
        let success,givenHP = Int64.TryParse (Console.ReadLine())
        let mHP = HP (int(givenHP))
        printfn "Initiative Modifier?"
        let sFlag, givenMod = Int64.TryParse (Console.ReadLine())
        let initMod = Modifier (int(givenMod))
        printfn "Combatant type?"
        let cType = Console.ReadLine()
        createCombatant cName mHP mHP initMod (parseCombatantType cType)

    let rec inputLoop world = 
        printfn "Please make a selection"
        let choice = Console.ReadLine()
        match choice with
        | "Quit" -> world
        | "quit" -> world
        | "Add"  -> inputLoop (flow world (AddCombatant (characterPrompt())))
        | _ -> inputLoop world

    [<EntryPoint>]
    let Main(args) =

        let thisWorld = { combatants = [] }
        //Run it!
        let newWorld = inputLoop thisWorld
        //Verify that I got what I wanted!
        let {combatants = lcombatants} = newWorld
        lcombatants |> List.iter (fun c -> printfn "%A" c)

        // Application.Init()
        // let win = new MainWindow.MyWindow()
        // win.Show()
        // Application.Run()
        0
    