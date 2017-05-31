namespace sharpCombatTracker

module Main = 
    
    open System
    open FlowHelpers
    // open Gtk

    [<EntryPoint>]
    let Main(args) =

        let testCombatant = createCombatant (Name("Chauncy")) (HP(3)) (HP(3)) (Modifier(5)) CombatantType.Player
        
        printfn "%A" testCombatant
        // Application.Init()
        // let win = new MainWindow.MyWindow()
        // win.Show()
        // Application.Run()
        0
    