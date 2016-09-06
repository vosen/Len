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

[<CustomComparison>]
[<StructuralEquality>]
type Change =
    | NewLine of Range.Line0 * string
    | Insert of Range.Pos01 * string
    member t.Column =
        match t with
        | NewLine _ -> -1
        | Insert((_, col), _) -> col
    member t.Line =
        match t with
        | NewLine(line, _) -> line
        | Insert((line, _), _) -> line
    member t.Text =
        match t with
        | NewLine(_, text) -> text
        | Insert(_, text) -> text
    interface System.IComparable with
        member x.CompareTo(y) =
            match y with
            | :? Change as y ->
                let linesDiff = x.Line.CompareTo(y.Line)
                if linesDiff <> 0 then
                    linesDiff
                else
                    x.Column.CompareTo(y.Column)
            | _ -> failwith "wrong type"

type ChangesWriter() = 
    let mutable allChanges = ResizeArray()
    static let applyChanges (file: string) (changes: Change seq) =
        let source = ResizeArray(File.ReadAllLines(file))
        let mutable lineOffset = 0
        for line, changes in changes |> Seq.groupBy (fun c -> c.Line) do
            let lines, inserts = changes
                                 |> Seq.toArray
                                 |> Array.partition (function | Change.NewLine _ -> true | Change.Insert _ -> false)
            let mutable columnOffset = 0
            for line in lines do
                source.Insert(line.Line + lineOffset, line.Text)
                lineOffset <- lineOffset + 1
            for insert in inserts do
                let inserted = source.[insert.Line + lineOffset].Insert(insert.Column, insert.Text)
                source.[insert.Line + lineOffset] <-inserted
                columnOffset <- insert.Text.Length
        File.WriteAllLines(file, source)
    member t.Push(file: string, change: Change) =
        allChanges.Add((file, change))
    member t.Apply() =
        let changesByFiles = allChanges
                             |> Seq.groupBy (fun (file, _) -> file)
                             |> Map.ofSeq
        for KeyValue(file, changes) in changesByFiles do
            changes
            |> Seq.map snd
            |> Seq.sort
            |> applyChanges file

let rec isLazy (typ: FSharpType) =
    if typ.IsAbbreviation then
        isLazy typ.AbbreviatedType
    else
        let tyDef = typ.TypeDefinition
        tyDef.AccessPath = "System" && tyDef.DisplayName = "Lazy"

let rec traverse (project: FSharpCheckProjectResults) (writer: ChangesWriter) (decl: FSharpImplementationFileDeclaration) = 
    match decl with 
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) ->
        if e.IsNamespace || e.IsFSharpModule then
            subDecls |> List.iter (traverse project writer)
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(x, args, e) ->
        if args = [] && not x.IsProperty && not x.IsMember && not x.IsEvent && not (isLazy x.FullType) then
            let uses = project.GetUsesOfSymbol(x) |> Async.RunSynchronously
            for symbolUse in uses do
                if not symbolUse.IsFromDefinition then
                    writer.Push(symbolUse.FileName, Change.Insert(Range.Pos.toZ symbolUse.RangeAlternate.End, ".Value"))
    | FSharpImplementationFileDeclaration.InitAction(_) -> ()

[<EntryPoint>]
let main argv =
    let x = top.bar
    let checker = FSharpChecker.Create(projectCacheSize = 128, keepAssemblyContents = true)
    let options = ProjectCracker.GetProjectOptionsFromProjectFile(@"D:\Users\vosen\Documents\Visual Studio 2015\Projects\Len\Len\Len.fsproj")
    let project = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    let writer = ChangesWriter()
    for d in project.AssemblyContents.ImplementationFiles |> List.collect (fun f -> f.Declarations) do 
        traverse project writer d
    writer.Apply()
    0
