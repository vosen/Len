namespace Len

open System

module public Base =
    let public Foo : Lazy<DateTime> =
        lazy(let time = DateTime.UtcNow
             time)