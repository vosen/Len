namespace Foo

open System

type Rec = { Value: int }

module internal Bar =
    let internal baz : Rec =
        let x = 10
        {
            Value = 1
        }