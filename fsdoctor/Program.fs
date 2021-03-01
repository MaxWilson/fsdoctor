// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

#if INTERACTIVE
#r "nuget: FSharp.Compiler.Service, 39.0.0"
#endif
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Interactive.Shell

open System
open System.IO
open System.Text

module String =
    let join (sep:string) (input: string seq) = System.String.Join(sep, input)
type FSIResult =
    | Ok of string option
    | CompileError of string
    | ThrownException of Exception
type Fsi() =
    // Initialize output and input streams
    let sbOut = new StringBuilder()
    let sbErr = new StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)

    // Build command line arguments & start FSI session
    let argv = [| "C:\\placeholder" |] // dummy argument for file location
    let allArgs = Array.append argv [|"--noninteractive"|]

    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)

    /// Evaluate expression & return the result
    let evalExpression text =
      match fsiSession.EvalExpressionNonThrowing(text) with
      | Choice1Of2 (Some value), diag when value.ReflectionValue <> null -> sprintf "%A" value.ReflectionValue |> Some |> Ok
      | Choice1Of2 (_), diag -> None |> Ok
      | Choice2Of2 (:? FsiCompilationException as exn), diag -> (diag.[0].ToString()) |> CompileError
      | Choice2Of2 exn, diag -> ThrownException exn
    member _.eval = evalExpression

let fsi = Lazy<Fsi>()

let generateComment txt =
    match fsi.Value.eval txt with
    | Ok (Some txt) -> $" // evaluates to {txt}"
    | Ok (None) -> $" // evaluates to nothing"
    | CompileError msg -> $" // won't compile"
    | ThrownException exn -> $" // throws exception"

let autoComplete txt = txt

// Define a function to construct a message to print
let getLine() =
    let backspace (accum: string) =
        let accum' = (accum.Substring(0, max 0 (accum.Length-1)))
        accum'
    let append (txt: string) (accum: string) =
        Console.Write txt
        accum + txt
    let rec recur (accum: string list) =
        let c = Console.ReadKey(true)
        let print (next: string) =
            let struct (x, y) = Console.GetCursorPosition()
            Console.SetCursorPosition(0, y)
            printf "%s" next
            if next.Length < x then
                // need to delete
                printf "%s" (String.replicate (x - next.Length) " ")
                Console.SetCursorPosition(next.Length, y)
        let undo() =
            match accum with
            | [_] | [] as lst -> recur lst
            | _::t ->
                print t.Head
                recur t
        let fst = accum.Head
        let printAndRecur next =
            print next
            recur ([next]@accum)
        match c.Key with
        | ConsoleKey.Enter ->
            fst
        | ConsoleKey.Escape ->
            undo()
        | ConsoleKey.Backspace | ConsoleKey.Z when (c.Modifiers &&& ConsoleModifiers.Control |> int) <> 0 ->
            undo()
        | ConsoleKey.Backspace ->
            fst
            |> backspace
            |> printAndRecur
        | ConsoleKey.Tab ->
            fst
            |> autoComplete
            |> printAndRecur
        | _ ->
            let k = c.KeyChar
            (fst + k.ToString())
            |> printAndRecur
    recur [""]

[<EntryPoint>]
let main argv =
    printfn "Initializing fsi.exe..."
    fsi.Value.eval "1+1" |> ignore // make sure it's awake
    printfn "Enter an expression"
    let mutable resp = (getLine())
    while resp <> "q" && resp <> "quit" do
        printfn "%s" (generateComment resp)
        resp <- getLine()
    0 // return an integer exit code