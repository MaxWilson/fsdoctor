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

type Eval() =
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
      | Choice2Of2 (:? FsiCompilationException as exn), diag -> sprintf "Compile error: %s" (diag.[0].ToString()) |> Error
      | Choice2Of2 exn, diag -> sprintf "Exception: %A Diagnostic: %A" exn diag |> Error

    evalExpression """2+1+"""

// Define a function to construct a message to print
let getLine() =
    let backspace (accum: string) =
        let accum' = (accum.Substring(0, max 0 (accum.Length-1)))
        let struct (x, y) = Console.GetCursorPosition()
        Console.SetCursorPosition(0, y)
        Console.Write ((accum') + " \b")
        accum'
    let append (txt: string) (accum: string) =
        Console.Write txt
        accum + txt

    let rec recur (accum: string) =
        let c = Console.ReadKey()
        match c.Key with
        | ConsoleKey.Enter ->
            Console.WriteLine ""
            accum
        | ConsoleKey.Backspace ->
            accum
            |> backspace
            |> recur
        | ConsoleKey.Tab ->
            (accum + "\t")
            |> backspace
            |> append (accum.Trim())
            |> recur
        | _ ->
            let k = c.KeyChar
            (accum + k.ToString())
            |> recur
    recur ""

[<EntryPoint>]
let main argv =
    printfn "Enter data"
    printfn "Hello world %A" (getLine())

    0 // return an integer exit code