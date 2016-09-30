module Tests

open System
open System.Reflection
open System.IO
open NUnit.Framework
open Len.Program
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Text

module Script = 
    let test (name: string) =
        let input = Path.Combine(Assembly.GetExecutingAssembly().Location, @"..\Input", name)
        let output = Path.Combine(Assembly.GetExecutingAssembly().Location, @"..\Output", name)
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let content = File.ReadAllText(input) // this is fairly dumb, but fsc service requires files to physically exist
        let options = checker.GetProjectOptionsFromScript(input, content) |> Async.RunSynchronously
        let project = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
        let mutable result = content
        let writer = ChangesWriter(fun (_, text) -> result <- text)
        for d in project.AssemblyContents.ImplementationFiles |> List.collect (fun f -> f.Declarations) do 
            traverse checker options project [| project |] writer d
        writer.Apply()
        Assert.AreEqual(File.ReadAllText(output), result)

    [<Test>]
    let Simple() = test "Simple.fsx"

    [<Test>]
    let Record() = test "Record.fsx"

    [<Test>]
    let Complex() = test "Complex.fsx"

    [<Test>]
    let Internal() = test "Internal.fsx"

    [<Test>]
    let Pattern() = test "Pattern.fsx"

    [<Test>]
    let MutuallyRecursive() = test "MutuallyRecursive.fsx"

    [<Test>]
    let Record2() = test "Record2.fsx"

    [<Test>]
    let StaticCall() = test "StaticCall.fsx"

module Project =
    open Microsoft.FSharp.Compiler.SourceCodeServices

    module Map =
        let replace (key: 'a) (value: 'b) (map: Map<'a,'b>) =
            match Map.tryFind key map with
            | Some _ -> Map.add key value map
            | None -> raise <| ArgumentException(sprintf "Key %A does not already exist" key)

    let projPath (inputDir: string) (name: string) =
        Path.Combine(sprintf "%s.%s" inputDir name, sprintf "%s.fsproj" name)

    let crackProj (checker: FSharpChecker) (path: string) =
        let path = Path.GetFullPath(path)
        let options = ProjectCracker.GetProjectOptionsFromProjectFile(path) 
        options, checker.ParseAndCheckProject(options) |> Async.RunSynchronously

    let buildInMemoryFileSystem (basePath: string) (dirs: string[]) : Map<string, string> =
        dirs
        |> Array.collect(fun name -> DirectoryInfo(sprintf "%s.%s" basePath name).GetFiles(sprintf @"*.fs" ))
        |> Array.map (fun file -> (file.FullName.ToLowerInvariant(), File.ReadAllText(file.FullName)))
        |> Map.ofArray

    let test (basePath: string) (libPaths: string[]) =
        let input = Path.Combine(Assembly.GetExecutingAssembly().Location, @"..\Input")
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let baseOpts, baseProj = crackProj checker (projPath input basePath)
        let _, refProjs = (libPaths |> Array.map (projPath input >> crackProj checker) |> Array.unzip)
        let mutable fs = buildInMemoryFileSystem input (Array.append libPaths [| basePath |])
        let writer = ChangesWriter(fun (path, content) -> fs <- Map.replace (path.ToLowerInvariant()) content fs)

        for d in baseProj.AssemblyContents.ImplementationFiles |> List.collect (fun f -> f.Declarations) do 
            traverse checker baseOpts baseProj (Array.append refProjs [| baseProj |]) writer d
        writer.Apply()

        for KeyValue(file, content) in fs do
            Assert.AreEqual(File.ReadAllText(file.Replace("input", "output")), content)

    [<Test>]
    let Library() = test "Base" [| "Library" |]