namespace Foo

open System

type Rec = { Value: int }

module internal Bar =
    let internal baz : Lazy<Rec> =
        lazy({
                 Value = 1
             })