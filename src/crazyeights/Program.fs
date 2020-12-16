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

type Effect = 
    | Skip
    | Back
    | Interrupt
    | Next

let effect card = 
    match card.Rank with
    | Seven -> Skip
    | Jack -> Back
    | _ -> Next

[<Struct>]
type Players = private Players of int

[<Struct>]
type Player = Player of int

type Direction = Righ | Left

type Table = {
    Direction: Direction
    Players: Players
    Player: Player
}

module Table = 
    let flip table = 
        match table.Direction with
        | Left -> { table with Direction = Righ }
        | Righ -> { table with Direction = Left }

    let start players = {
        Direction = Left
        Players = players
        Player = Player 0
    }

    let next table = 
        let (Players n) = table.Players
        let (Player p) = table.Player
        let next = 
            if table.Direction = Left then  (p + 1) % n
            else if p - 1 < 0 then n-1 else p-1
        { table with Player = Player next }
    
    let back = flip >> next  

    let skip = next >> next  

exception TooFewPlayers
exception GameAlreadyStarted
exception GameNotStarted

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
    Card: Card 
    Effect: Effect }

type State = 
    | NotStarted
    | Started of Started
and Started = {
    TopCard: Card
    Table: Table
}

let initialState = NotStarted

let decide (cmd: Command) (state: State) : Event list = 
    match state, cmd with
    | Started s, StartGame _ -> 
        raise GameAlreadyStarted
    | NotStarted, Play c -> 
        raise GameNotStarted
    | NotStarted, StartGame c ->
        [ GameStarted { Players = c.Players; FirstCard = c.FirstCard }]
    | Started s, Play c when c.Card = s.TopCard -> 
        [CardPlayed { Card = c.Card; Player = c.Player; Effect = Interrupt } ]
    | Started s, Play c when c.Player <> s.Table.Player -> 
        [WrongPlayerPlayed { Card = c.Card; Player = c.Player; Effect = effect c.Card } ]
    | Started s, Play c when  c.Card.Rank  <> s.TopCard.Rank && c.Card.Suit <> s.TopCard.Suit -> 
        [WrongCardPlayed { Card = c.Card; Player = c.Player; Effect = effect c.Card } ]
    | Started s, Play c -> 
        [ CardPlayed { Card = c.Card; Player = c.Player; Effect = effect c.Card} ]

let evolve (state: State) (event: Event) : State = 
    match state, event with
    | NotStarted, GameStarted e -> 
        Started { TopCard = e.FirstCard; Table = e.Players |>  Table.start |> Table.next}
    | Started s, CardPlayed e -> 
        let nextTable = 
            match e.Effect with
            | Next -> Table.next s.Table 
            | Skip -> Table.skip s.Table 
            | Back -> Table.back s.Table 
            | Interrupt -> s.Table

        Started { s with TopCard = e.Card; Table = nextTable; }

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code