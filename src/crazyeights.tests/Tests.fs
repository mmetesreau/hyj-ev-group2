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
        => StartGame { Players = players 4; FirstCard = Three $ Club  }
        =! [ GameStarted { Players = players 4; FirstCard = Three $ Club  } ]

[<Fact>]
let ``It's not fun to play alone`` () =
    tryPlayers 1 =! Error TooFewPlayers

[<Fact>]
let ``Only one start`` () = 
    raises<GameAlreadyStarted>
        <@
            [
                GameStarted { Players = players 4; FirstCard = Three $ Club }
            ] => StartGame { Players = players 4; FirstCard = Three $ Club  } 
        @>

[<Fact>]
let ``Play card with same rank`` () = 
   [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
        => Play { Card = Three $ Diamond }
        =! [ CardPlayed { Card = Three $ Diamond } ]

[<Fact>]
let ``Play card with same suit`` () = 
   [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
        => Play { Card = Four $ Club  }
        =! [ CardPlayed { Card = Four $ Club } ]

[<Fact>]
let ``Play card with neither same rank nor same suit`` () = 
    [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
    => Play { Card = Four $ Diamond  }
    =! [ WrongCardPlayed { Card = Four $ Diamond } ]
