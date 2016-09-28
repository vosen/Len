type Rec = { Value: int }

let entry1 : Rec =
        {
            Value = 1
        }

let entry2 : Lazy<Rec> =
        lazy({
                 Value = 2
             }
             {
                 Value = 1
             })