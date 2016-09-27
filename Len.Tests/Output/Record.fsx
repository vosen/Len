type Rec = { Value: int }

let entry : Lazy<Rec> =
        lazy({
                 Value = 1
             })