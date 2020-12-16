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
        => Play { Card = Three $ Diamond; Player = Player 1 }
        =! [ CardPlayed { Card = Three $ Diamond; Player = Player 1 } ]

[<Fact>]
let ``Play card with same suit`` () = 
   [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
        => Play { Card = Four $ Club; Player = Player 1  }
        =! [ CardPlayed { Card = Four $ Club; Player = Player 1 } ]

[<Fact>]
let ``Play card with neither same rank nor same suit`` () = 
    [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
    => Play { Card = Four $ Diamond; Player = Player 1  }
    =! [ WrongCardPlayed { Card = Four $ Diamond; Player = Player 1 } ]

[<Fact>]
let ``Play valid card wrong player`` () = 
    [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
    => Play { Card = Four $ Diamond; Player = Player 2  }
    =! [ WrongPlayerPlayed { Card = Four $ Diamond; Player = Player 2 } ]

[<Fact>]
let ``Player 2 after player 1`` () = 
    [ 
        GameStarted { Players = players 4; FirstCard = Three $ Club }
        CardPlayed { Card = Four $ Diamond; Player = Player 1   }
    ]
    => Play { Card = Four $ Club; Player = Player 2  }
    =! [ CardPlayed { Card = Four $ Club; Player = Player 2  } ]

[<Fact>]
let ``Player 0 after player 4`` () = 
    [ 
        GameStarted { Players = players 4; FirstCard = Three $ Club }
        CardPlayed { Card = Four $ Diamond; Player = Player 1   }
        CardPlayed { Card = Five $ Diamond; Player = Player 2   }
        CardPlayed { Card = Five $ Club; Player = Player 3   }
    ]
    => Play { Card = Ace $ Club; Player = Player 0  }
    =! [ CardPlayed { Card = Ace $ Club; Player = Player 0  } ]