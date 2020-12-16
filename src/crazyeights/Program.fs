// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
module CrazyEights

open System

type Rank = 
    | Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Height
    | Nine
    | Ten
    | Jack
    | Queen
    | King

type Suit = Club | Spade | Diamond | Heart

type Card = {
    Rank: Rank
    Suit: Suit }

let ($) rank suit = { Rank = rank; Suit = suit }

[<Struct>]
type Players = private Players of int

[<Struct>]
type Player = Player of int

exception TooFewPlayers
exception GameAlreadyStarted

let tryPlayers n = 
    if n < 2 then
        Error TooFewPlayers
    else
        Ok (Players n)

let players n = 
    match tryPlayers n with
    | Ok p -> p
    | Error e -> raise e
        
type Command = 
    | StartGame of StartGame
    | Play of Play
and StartGame = 
    { Players: Players 
      FirstCard: Card }
and Play = 
    {   Player: Player
        Card : Card }

type Event = 
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed
    | WrongCardPlayed of CardPlayed
    | WrongPlayerPlayed of CardPlayed
and GameStarted = 
    { Players: Players
      FirstCard: Card  }
and CardPlayed = {
    Player: Player
    Card: Card }

type State = 
    | NotStarted
    | Started of Started
and Started = {
    TopCard: Card
    Player: Player
}

let initialState = NotStarted

let decide (cmd: Command) (state: State) : Event list = 
    match state, cmd with
    | Started s, StartGame _ -> 
        raise GameAlreadyStarted
    | NotStarted, StartGame c ->
        [ GameStarted { Players = c.Players; FirstCard = c.FirstCard }]
    | NotStarted, Play c -> []
    | Started s, Play c when c.Player <> s.Player -> 
        [WrongPlayerPlayed { Card = c.Card; Player = c.Player } ]
    | Started s, Play c when  c.Card.Rank  <> s.TopCard.Rank && c.Card.Suit <> s.TopCard.Suit -> 
        [WrongCardPlayed { Card = c.Card; Player = c.Player } ]
    | Started s, Play c -> 
        [ CardPlayed { Card = c.Card; Player = c.Player} ]

let evolve (state: State) (event: Event) : State = 
    match event with
    | GameStarted e -> Started { TopCard = e.FirstCard; Player = Player 1 }
    | CardPlayed e -> Started { TopCard = e.Card; Player = e.Player }

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code