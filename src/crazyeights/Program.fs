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
    { Card : Card }

type Event = 
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed
    | WrongCardPlayed of CardPlayed
and GameStarted = 
    { Players: Players
      FirstCard: Card  }
and CardPlayed = {
    Card: Card }

type State = 
    | NotStarted
    | Started of Started
and Started = {
    TopCard: Card
}

let initialState = NotStarted

let decide (cmd: Command) (state: State) : Event list = 
    match state, cmd with
    | NotStarted, StartGame c ->
        [ GameStarted { Players = c.Players; FirstCard = c.FirstCard }]
    | Started s, StartGame _ -> raise GameAlreadyStarted
    | NotStarted, Play c -> []
    | Started s, Play c when s.TopCard.Rank = c.Card.Rank || s.TopCard.Suit = c.Card.Suit -> 
        [ CardPlayed { Card = c.Card } ]
    | Started s, Play c -> [WrongCardPlayed { Card = c.Card } ]

let evolve (state: State) (event: Event) : State = 
    match event with
    | GameStarted e -> Started { TopCard = e.FirstCard }
    | CardPlayed e -> Started { TopCard = e.Card }

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code