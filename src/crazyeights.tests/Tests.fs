module Tests

open System
open Xunit
open Swensen.Unquote
open CrazyEights

let (=>) (events: Event list) (cmd: Command) : Event list =
    events 
        |> List.fold evolve initialState
        |> decide cmd

[<Fact>]
let ``Game not started should start`` () =
    [ ] 
        => StartGame { Players = players 4 }
        =! [ GameStarted { Players = players 4 } ]

[<Fact>]
let ``It's not fun to play alone`` () =
    tryPlayers 1 =! Error TooFewPlayers

[<Fact>]
let ``Only one start`` () = 
    raises<GameAlreadyStarted>
        <@
            [
                GameStarted { Players = players 4}
            ] => StartGame { Players = players 4 } 
        @>