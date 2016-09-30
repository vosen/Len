namespace Len

open System

module public Base =
    let public Foo : DateTime =
        let time = DateTime.UtcNow
        time