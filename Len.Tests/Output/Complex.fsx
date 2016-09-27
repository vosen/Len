type Rec = { Value: int }

let data: Lazy<Rec[]> =
    lazy(let key : Rec = { Value = 2 }
         [| key; key |])