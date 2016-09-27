type Rec = { Value: int }

let data: Rec[] =
    let key : Rec = { Value = 2 }
    [| key; key |]