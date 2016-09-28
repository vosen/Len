﻿module Len.Program

open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open System.Collections.Generic

type LetBinding = { RhsExpr: Range.range; ReturnInfo: Range.range option; }

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

type ChangesWriter(writer: (string * string) -> unit) = 
    let mutable allChanges = ResizeArray()
    let applyChanges (file: string) (changes: Change seq) =
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
                let inserted = source.[insert.Line + lineOffset].Insert(insert.Column + columnOffset, insert.Text)
                source.[insert.Line + lineOffset] <-inserted
                columnOffset <- columnOffset + insert.Text.Length
        writer(file, String.concat System.Environment.NewLine source)
    member t.Changes : IReadOnlyList<string * Change> = allChanges :> IReadOnlyList<string * Change>
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
    else if typ.HasTypeDefinition then
        let tyDef = typ.TypeDefinition
        tyDef.AccessPath = "System" && tyDef.DisplayName = "Lazy"
    else
        false

let pushLazyDefChange (writer: ChangesWriter) (range: Range.range) =
    let file = range.FileName
    let startR = Range.Pos.toZ range.Start
    let endR = Range.Pos.toZ range.End
    writer.Push(file, Change.Insert(startR, "lazy("))
    for line in (fst startR)+1..(fst endR) do
        writer.Push(file, Change.Insert((line, 0), "     "))
    writer.Push(file, Change.Insert(endR, ")"))

let pushRetInfoChange (writer: ChangesWriter) (retTange: Range.range) =
    let file = retTange.FileName
    let startR = Range.Pos.toZ retTange.Start
    let endR = Range.Pos.toZ retTange.End
    writer.Push(file, Change.Insert(startR, "Lazy<"))
    writer.Push(file, Change.Insert(endR, ">"))

let pushLazyUseChange (writer: ChangesWriter) (symbolUse: FSharpSymbolUse) =
    writer.Push(symbolUse.FileName, Change.Insert(Range.Pos.toZ symbolUse.RangeAlternate.End, ".Value"))

// TASTs miss some stuff so we have to reparse everything
let traverseForSynBinding (ast: FSharpParseFileResults) (range: Range.range) : SynBinding =
    let rec traverseDecl = function
        | SynModuleDecl.Let (_, bindings, body) ->
            match bindings with 
            | [ binding ] -> if binding.RangeOfHeadPat = range then Some(binding) else None
            | _ -> failwithf "Unexpected multiple let bindings expressions at %A" body // we don't really use this
        | SynModuleDecl.NestedModule(_, _, decls, _, _) -> traverseDecls decls
        | _ -> None
    and traverseDecls (decls: SynModuleDecls) =
        List.map traverseDecl decls
        |> List.tryFind Option.isSome
        |> Option.bind id
    let traverseModuleOrNamespace (modul: SynModuleOrNamespace) =
        let (SynModuleOrNamespace(_, _, _, decls, _, _, _, _)) = modul
        traverseDecls decls

    match ast.ParseTree.Value with
    | ParsedInput.ImplFile implFile ->
        let (ParsedImplFileInput(_, _, _, _, _, modules, _)) = implFile
        List.pick traverseModuleOrNamespace modules
    | ParsedInput.SigFile _ -> failwith "Signature files not implemented"

let findLetBinding (checker: FSharpChecker) (options: FSharpProjectOptions) (range: Range.range) =
    let path = range.FileName
    let tree = checker.ParseFileInProject(path, File.ReadAllText(path), options) |> Async.RunSynchronously
    match traverseForSynBinding tree range with 
    | (SynBinding.Binding(_,_,_,_,_,_,_,_,retInfo,expr,_,_)) ->
        let retInfoRange = retInfo |> Option.map (fun r -> match r with | SynBindingReturnInfo.SynBindingReturnInfo(_,range,_) -> range)
        { RhsExpr = expr.Range; ReturnInfo = retInfoRange }

let rec traverse (checker: FSharpChecker) (options: FSharpProjectOptions) (project: FSharpCheckProjectResults) (writer: ChangesWriter) (decl: FSharpImplementationFileDeclaration) = 
    match decl with 
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) ->
        if e.IsNamespace || e.IsFSharpModule then
            subDecls |> List.iter (traverse checker options project writer)
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(x, args, expr) ->
        if args = [] && not x.IsProperty && not x.IsMember && not x.IsEvent && not (isLazy x.FullType) then
            let { RhsExpr = rhsRange; ReturnInfo = retRange } = findLetBinding checker options x.DeclarationLocation
            pushLazyDefChange writer rhsRange
            retRange |> Option.iter (pushRetInfoChange writer)
            let uses = project.GetUsesOfSymbol(x) |> Async.RunSynchronously
            for symbolUse in uses do
                if not symbolUse.IsFromDefinition then
                    pushLazyUseChange writer symbolUse
    | FSharpImplementationFileDeclaration.InitAction(_) -> ()

[<EntryPoint>]
let main argv =
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let options = ProjectCracker.GetProjectOptionsFromProjectFile(@"..\..\Len.fsproj")
    let project = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    let writer = ChangesWriter(File.WriteAllText)
    for d in project.AssemblyContents.ImplementationFiles |> List.collect (fun f -> f.Declarations) do 
        traverse checker options project writer d
    writer.Apply()
    0
