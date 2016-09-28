namespace Foo

open System

type Rec = { Value: int }

module internal Bar =
    let internal baz : Lazy<Rec> =
        lazy(let x = 10
             {
                 Value = 1
             })