// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
module CrazyEights

open System

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
and StartGame = 
    { Players: Players }

type Event = 
    | GameStarted of GameStarted
and GameStarted = 
    { Players: Players }

type State = 
    | NotStarted
    | Started

let initialState = NotStarted

let decide (cmd: Command) (state: State) : Event list = 
    match state, cmd with
    | NotStarted, StartGame c ->
        [ GameStarted { Players = c.Players }]
    | Started, StartGame _ -> raise GameAlreadyStarted

let evolve (state: State) (event: Event) : State = 
    match event with
    | GameStarted _ -> Started

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code