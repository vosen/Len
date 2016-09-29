module Tests

open System
open System.IO
open NUnit.Framework
open Len.Program
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Text

let testScript (name: string) =
    let input = sprintf @".\Input\%s" name
    let output = sprintf @".\Output\%s" name
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let content = File.ReadAllText(input) // this is fairly dumb, but fsc service requires files to physically exist
    let options = checker.GetProjectOptionsFromScript(input, content) |> Async.RunSynchronously
    let project = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    let mutable result = content
    let writer = ChangesWriter(fun (_, text) -> result <- text)
    for d in project.AssemblyContents.ImplementationFiles |> List.collect (fun f -> f.Declarations) do 
        traverse checker options project writer d
    writer.Apply()
    Assert.AreEqual(File.ReadAllText(output), result)

[<Test>]
let Simple() = testScript "Simple.fsx"

[<Test>]
let Record() = testScript "Record.fsx"

[<Test>]
let Complex() = testScript "Complex.fsx"

[<Test>]
let Internal() = testScript "Internal.fsx"

[<Test>]
let Pattern() = testScript "Pattern.fsx"

[<Test>]
let MutuallyRecursive() = testScript "MutuallyRecursive.fsx"

[<Test>]
let Record2() = testScript "Record2.fsx"