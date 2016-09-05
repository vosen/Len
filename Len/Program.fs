module Program

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp
open Microsoft.FSharp.Compiler
open System.Text

module top =
    let bar = printfn "bar"

type File with
    static member ReadAllLinesWithEndings(file: string) =
        let fullText = File.ReadAllBytes(file)
        let lines = ResizeArray()
        let mutable start = 0
        while start > -1 && start < fullText.Length - 1 do
            let oldStart = start
            start <- Array.IndexOf(fullText, 0x0Auy, start)
            if start = -1 then
                lines.Add(Encoding.UTF8.GetString(fullText, oldStart, fullText.Length - oldStart))
            else
                start <- start + 1
                lines.Add(Encoding.UTF8.GetString(fullText, oldStart, (start - oldStart)))
        lines

type ChangesWriter() = 
    let mutable allChanges = ResizeArray()
    static let applyChanges (file: string) (changes: (int * Range.range * string) seq) =
        let source = File.ReadAllLinesWithEndings(file)
        for offset, pos, text in changes do
            let fullOffset = offset + pos.StartColumn
            let linesRange = source.GetRange(pos.StartLine, source.Count - pos.StartLine).ToArray()
            let relLineIdx, endDistance = linesRange
                                          |> Seq.scan (fun offset line -> offset - line.Length) fullOffset
                                          |> Seq.indexed
                                          |> Seq.find (snd >> (>) 0)
            let line = source.[pos.StartLine + relLineIdx]
            source.[pos.StartLine + relLineIdx] <- line.Insert(line.Length + endDistance, text)
    member t.Push(pos: Range.range, text: string) =
        allChanges.Add((pos, text))
    member t.Apply() =
        let changesByFiles = allChanges
                             |> Seq.groupBy (fun (pos, _) -> pos.FileName)
                             |> Map.ofSeq
        for KeyValue(file, changes) in changesByFiles do
            changes
            |> Seq.sortBy (fun (pos,_) -> (pos.StartLine, pos.StartColumn))
            |> Seq.mapFold (fun offset (pos, text) -> (offset, pos, text), offset + text.Length) 0
            |> fst
            |> applyChanges file

let rec traverse (project: FSharpCheckProjectResults) (writer: ChangesWriter) (decl: FSharpImplementationFileDeclaration) = 
    match decl with 
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) ->
        if e.IsNamespace || e.IsFSharpModule then
            subDecls |> List.iter (traverse project writer)
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(x, args, e) ->
        if args = [] && not x.IsProperty && not x.IsMember && not x.IsEvent   then
            let uses = project.GetUsesOfSymbol(x) |> Async.RunSynchronously
            for symbolUse in uses do
                if not symbolUse.IsFromDefinition then
                    writer.Push(symbolUse.RangeAlternate.EndRange, ".Value")
    | FSharpImplementationFileDeclaration.InitAction(_) -> ()

[<EntryPoint>]
let main argv =
    let x = top.bar
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let options = ProjectCracker.GetProjectOptionsFromProjectFile(@"D:\Users\vosen\Documents\Visual Studio 2015\Projects\Len\Len\Len.fsproj")
    let project = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    let writer = ChangesWriter()
    for d in project.AssemblyContents.ImplementationFiles |> List.collect (fun f -> f.Declarations) do 
        traverse project writer d
    writer.Apply()
    0
