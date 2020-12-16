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
let ``Player 2 plays card with same rank`` () = 
   [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
        => Play { Card = Three $ Diamond; Player = Player 1 }
        =! [ CardPlayed { Card = Three $ Diamond; Player = Player 1; Effect = Next } ]

[<Fact>]
let ``Player 2 plays card with same suit`` () = 
   [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
        => Play { Card = Four $ Club; Player = Player 1  }
        =! [ CardPlayed { Card = Four $ Club; Player = Player 1; Effect = Next } ]

[<Fact>]
let ``Player 2 plays card different suit and rank`` () = 
    [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
    => Play { Card = Four $ Diamond; Player = Player 1  }
    =! [ WrongCardPlayed { Card = Four $ Diamond; Player = Player 1; Effect = Next } ]

[<Fact>]
let ``Wrong player plays a card`` () = 
    [ GameStarted { Players = players 4; FirstCard = Three $ Club } ]
    => Play { Card = Four $ Diamond; Player = Player 2  }
    =! [ WrongPlayerPlayed { Card = Four $ Diamond; Player = Player 2; Effect = Next } ]

[<Fact>]
let ``Player 3 plays after Player 2`` () = 
    [ 
        GameStarted { Players = players 4; FirstCard = Three $ Club }
        CardPlayed { Card = Four $ Diamond; Player = Player 1; Effect = Next   }
    ]
    => Play { Card = Four $ Club; Player = Player 2  }
    =! [ CardPlayed { Card = Four $ Club; Player = Player 2; Effect = Next  } ]

[<Fact>]
let ``Player 1 plays after Player 4`` () = 
    [ 
        GameStarted { Players = players 4; FirstCard = Three $ Club }
        CardPlayed { Card = Four $ Diamond; Player = Player 1; Effect = Next   }
        CardPlayed { Card = Five $ Diamond; Player = Player 2; Effect = Next   }
        CardPlayed { Card = Five $ Club; Player = Player 3; Effect = Next   }
    ]
    => Play { Card = Ace $ Club; Player = Player 0  }
    =! [ CardPlayed { Card = Ace $ Club; Player = Player 0 ; Effect = Next } ]

[<Fact>]
let ``Player 4 plays after Player 2 played a Seven`` () = 
    [ 
        GameStarted { Players = players 4; FirstCard = Three $ Club }
        CardPlayed { Card = Seven $ Diamond; Player = Player 1; Effect = Skip   }
    ]
    => Play { Card = Ace $ Diamond; Player = Player 3  }
    =! [ CardPlayed { Card = Ace $ Diamond; Player = Player 3; Effect = Next  } ]

[<Fact>]
let ``Wrong Players plays after Player 2 played a Seven`` () = 
    [ 
        GameStarted { Players = players 4; FirstCard = Three $ Club }
        CardPlayed { Card = Seven $ Diamond; Player = Player 1; Effect = Skip   }
    ]
    => Play { Card = Ace $ Diamond; Player = Player 2  }
    =! [ WrongPlayerPlayed { Card = Ace $ Diamond; Player = Player 2; Effect = Next  } ]

[<Fact>]
let ``Player 1 plays after Player 2 played a Jack`` () = 
    [ 
        GameStarted { Players = players 4; FirstCard = Three $ Club }
        CardPlayed { Card = Jack $ Diamond; Player = Player 1; Effect = Back  }
        CardPlayed { Card = Ace $ Diamond; Player = Player 0; Effect = Next  }
    ]
    => Play { Card = Six $ Diamond; Player = Player 3  }
    =! [ CardPlayed { Card = Six $ Diamond; Player = Player 3; Effect = Next  } ]
    
[<Fact>]
let ``Wrong Player plays after Player 2 played a Jack`` () = 
    [ 
        GameStarted { Players = players 4; FirstCard = Three $ Club }
        CardPlayed { Card = Jack $ Diamond; Player = Player 1; Effect = Back  }
    ]
    => Play { Card = Six $ Diamond; Player = Player 3  }
    =! [ WrongPlayerPlayed { Card = Six $ Diamond; Player = Player 3; Effect = Next  } ]

[<Fact>]
let ``Any Players can interrupt with same card`` () = 
    [ 
        GameStarted { Players = players 4; FirstCard = Three $ Club }
        CardPlayed { Card = Six $ Diamond; Player = Player 1; Effect = Next   }
    ]
    => Play { Card = Six $ Diamond; Player = Player 3  }
    =! [ CardPlayed { Card = Six $ Diamond; Player = Player 3; Effect = Interrupt  } ]