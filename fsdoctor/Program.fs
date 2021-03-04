// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#load "Packrat.fs"
#r "nuget: FSharp.Compiler.Service, 39.0.0"
#r "nuget: System.Xml.XDocument, 4.3.0"
#r "nuget: System.Xml.XPath"

#endif
open Wilson.Packrat
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Interactive.Shell

open System
open System.IO
open System.Text
open System.Xml.Linq
open System.Xml.XPath

module String =
    let join (sep:string) (input: string seq) = System.String.Join(sep, input)

#nowarn "40" // We're not doing anything crazy like calling higher-order arguments during ctor execution, don't need the warning
module Parse =
    let (|WSStr|_|) input = (|OWS|) >> (|Str|_|) input
    let rec (|OptionalParens|_|) = pack <| function
        | WSStr "(" (OptionalParens((), WSStr ")" (OptionalParens((), rest)))) -> Some((), rest)
        | WSStr "(" (OptionalParens(v, WSStr ")" rest)) -> Some(v, rest)
        | CharsExcept (Set.ofList ['('; ')']) (txt, rest) -> Some((), rest)
        | _ -> None
    let rec (|FunctionApplications|_|) = pack <| function
        | WSStr "(" (OptionalParens((), WSStr ")" (FunctionApplications((), rest)))) -> Some((), rest)
        | WSStr "(" (OptionalParens(v, WSStr ")" rest)) -> Some(v, rest)
        | CharsExcept (Set.ofList ['('; ')'; ',']) (txt, rest) -> Some((), rest)
        | _ -> None
    let rec (|Term|_|) = pack <| function
        | WSStr "(" (OptionalParens(v, WSStr ")" (_, endIx))) & (ctx, beginIx)
        | FunctionApplications(v, (_, endIx)) & (ctx, beginIx) ->
            let arg = ctx.input.Substring(beginIx, endIx - beginIx).Trim()
            Some(arg, (ctx, endIx))
        | _ -> None

    let rec (|ArgList|_|) =
        let (|Arg|_|) = function
            | Term(txt, (OWS (Term(txt2, rest)))) ->
                Some($"{txt} {txt2}", rest)
            | Term(txt, rest) -> Some(txt, rest)
            //| CharsExcept (Set.ofList [','; '(']) (arg, rest) -> Some(arg, rest)
            | _ -> None
        pack <| function
            | Arg(txt, OWS(Char(',', ArgList(lst, rest)))) -> Some(txt::lst, rest)
            | Arg(txt, rest) -> Some([txt], rest)
            | _ -> None
    let identifierChars = (Set.ofList ['.'; '_'] |> Set.union alphanumeric)
    let (|Ident|_|) = (|Chars|_|) identifierChars
    let rec (|DocCommentLine|_|) =
        let set = (Set.ofSeq ['\r';'\n'])
        function
        | WSStr "///" (Any (comment, rest)) ->
            Some(comment, rest)
        | _ -> None
    let rec (|BlankLine|_|) =
        let set = (Set.ofSeq ['\r';'\n'])
        function
            | OWS (End as rest) ->
                Some((), rest)
            | _ -> None
    let rec (|Module|_|) = function
        | WSStr "module" (Ident (name, rest)) ->
            Some(name, rest)
        | _ -> None
    let rec (|FunctionDeclaration|_|) = function
            | WSStr "val" (OWS (Optional "inline" (Ident (name, (Any (_, rest)))))) ->
                Some(name, rest)
            | _ -> None
    let rec (|FunctionDefinition|_|) =
        let (|Prefix|_|) = function
            | WSStr "let" (OWS (Optional "inline" (Ident (name, rest)))) ->
                Some(name, rest)
            | _ -> None
        let (|MaybeTypeDecl|) =
            let chars = identifierChars |> Set.union (Set.ofList ['>'; '<'; '['; ']'; ' '; '\''])
            function
            | WSStr ":" (Chars chars (_, rest))
            | rest -> rest
        let (|Param|_|) = function
            | WSStr "(" (Ident (name, MaybeTypeDecl (WSStr ")" rest)))
            | Ident(name, rest) -> Some(name, rest)
            | _ -> None
        let rec (|Params|_|) = pack <| function
            | Param(name, WSStr "," (Params(more, rest))) -> Some(name::more, rest)
            | Param(name, rest) -> Some([name], rest)
            | _ -> None
        function
        | Prefix(name, Params(parameters, MaybeTypeDecl(WSStr "=" (OWS rest)))) ->
            Some({| name = name; parameters = parameters |}, rest)
        | _ -> None

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

type DocumentationContext = {
    apiName: string
    }

let initMsg = " <<<Initializing fsi.exe...>>>"
let fsi = Lazy<Fsi>(fun () ->
    printf "%s" initMsg
    let v = new Fsi()
    v.eval "1+1" |> ignore // make sure it's awake
    v
    )

let generateComment txt =
    match fsi.Value.eval txt with
    | Ok (Some txt) -> $" // evaluates to {txt}"
    | Ok (None) -> $" // evaluates to nothing"
    | CompileError msg -> $" // won't compile"
    | ThrownException exn -> $" // throws exception"

let autoComplete (ctx: DocumentationContext) (txt: string) =
    let txt = txt.Trim()
    match txt |> ParseArgs.Init with
    | Parse.ArgList(args, End) ->
        match args with
        | [arg] -> $"{arg} |> {ctx.apiName}"
        | [arg1; arg2] -> $"({arg1}, {arg2}) ||> {ctx.apiName}"
        | [arg1; arg2; arg3] -> $"({arg1}, {arg2}, {arg3}) |||> {ctx.apiName}"
        | _ -> txt
    | _ -> txt

type Command =
    | Eval of string
    | Quit
// Define a function to construct a message to print
let getLine (ctx: DocumentationContext) =
    let backspace (accum: string) =
        let accum' = (accum.Substring(0, max 0 (accum.Length-1)))
        accum'
    let eraseWord (accum: string) =
        let accum = accum.Trim()
        match accum.LastIndexOf(' ') with
        | -1 -> ""
        | n -> accum.Substring(0, n).Trim()
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
            match fst with
            | "q" | "quit"  -> Quit
            | v when String.IsNullOrWhiteSpace v -> Quit
            | _ ->
                fst |> Eval
        | ConsoleKey.Escape ->
            undo()
        | ConsoleKey.Backspace when (c.Modifiers &&& ConsoleModifiers.Control |> int) <> 0 ->
            fst
            |> eraseWord
            |> printAndRecur
        | ConsoleKey.Z when (c.Modifiers &&& ConsoleModifiers.Control |> int) <> 0 ->
            undo()
        | ConsoleKey.Backspace ->
            fst
            |> backspace
            |> printAndRecur
        | ConsoleKey.Tab ->
            fst
            |> autoComplete ctx
            |> printAndRecur
        | _ ->
            let k = c.KeyChar
            (fst + k.ToString())
            |> printAndRecur
    recur [""]

let rec examples prompt =
    match prompt() with
    | Quit -> []
    | Eval expr ->
        // remember position in case we need to wipe out init message from lazy init, during generateComment
        let struct (x, y) = Console.GetCursorPosition()

        let comment = generateComment expr

        if Console.GetCursorPosition() <> (x,y) then
            let struct (x', y') = Console.GetCursorPosition()
            Console.SetCursorPosition(x, y)
            Console.Write (String.replicate (initMsg.Length) " ")
            Console.SetCursorPosition(x, y)
        printfn "%s" comment
        (expr, comment)::(examples prompt)


let scanFile filePath =
    let(|Regex|_|) pattern input =
        match RegularExpressions.Regex.Match(input, pattern, RegularExpressions.RegexOptions.IgnoreCase) with
        | m when m.Success ->
            m.Groups |> Seq.cast<RegularExpressions.Group> |> Seq.skip 1 |> Seq.map (fun g -> g.Value) |> List.ofSeq |> Some
        | _ -> None

    //let filePath = @"d:\code\fsharp-core-docs\fsharp\src\fsharp\FSharp.Core\option.fs"
    let lines = IO.File.ReadAllLines filePath
    let modules =
        lines |> Array.zip [|1..lines.Length|]
        |> Array.choose (function (n, Regex "module\s+([^ ^=]+)\s*=" [moduleName]) -> Some (n, moduleName) | _ -> None)
    for (startLineIx, module') in modules do
        //let startLineIx, module' = 8, "Option"
        let indent (ix:int) = lines.[ix].ToCharArray() |> Array.takeWhile (Char.IsWhiteSpace) |> Array.length
        let startIndent = indent startLineIx
        // let x = 10
        let endLineIx =
            [for x in startLineIx .. (lines.Length - 1) do
                        if indent x > startIndent || match lines.[x] with Regex "\s+(//.*)?" [_optionalcomment] -> true | _ -> false
                            then yield x]
                    @[lines.Length - 1]
                    |> List.last
        let moduleLines = lines.[startLineIx..endLineIx]
        printfn "Module: %s" module'
        let rec recur comments lines =
            match lines with
            | [] -> ()
            | line::rest ->
                match line |> ParseArgs.Init with
                | Parse.DocCommentLine(comment, End) ->
                    recur (comments@[comment]) rest
                | Parse.FunctionDeclaration(funcName, _) ->
                    let addRoot elements= $"<doc>{elements}</doc>"
                    use reader = new StringReader (comments |> Seq.map (fun (s: string) -> s.Trim()) |> String.join "\n" |> addRoot)
                    let xmlDoc = reader |> System.Xml.Linq.XDocument.Load
                    let examples =
                        xmlDoc.XPathSelectElements("//example/code")
                        |> Seq.map (fun x -> x.Value)
                    printfn "%s" funcName
                    for example in examples do
                        printfn "%s" example
                    printfn ""
                    //printfn "%s" (comments |> Seq.map (fun s -> s.Trim()) |> String.join "\n" |> addRoot)
                    recur [] rest
                | Parse.BlankLine((), End)
                | _ ->
                    recur comments rest
        recur [] (moduleLines |> List.ofArray)

[<EntryPoint>]
let main argv =
    let rec api prompt =
        match prompt() with
        | None -> ()
        | Some v when String.IsNullOrWhiteSpace v -> api prompt
        | Some apiName ->
            let ctx = { apiName = apiName }
            let examples = examples (fun _ -> getLine ctx)
            api prompt
    let apiPrompt() =
        printfn "Enter an API name"
        match Console.ReadLine() with
        | "q" | "quit" -> None
        | v -> Some v
    match argv with
    | [|fileName|] ->
        scanFile fileName
        0
    | _ ->
        printfn "Usage: fsdoctor.exe <fsiFilePath>"
        -1
