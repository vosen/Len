module Foo =
    let qwe, (asd: double[]) =
        [| 
            "a", 1.
            "b", 2.
            "c", 3.
        |] |> Array.unzip

    let qwe2, (asd2: double[]) =
        [| 
            "d", 1.
            "e", 2.
            "f", 3.
        |] |> Array.unzip