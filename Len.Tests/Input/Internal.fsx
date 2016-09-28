namespace Foo

open System

type Rec = { Value: int }

module internal Bar =
    let internal baz : Rec =
        {
            Value = 1
        }