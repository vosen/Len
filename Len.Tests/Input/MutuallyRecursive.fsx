
let rec private foo
    (arg1: int)
    =
    bar arg1

and private bar
    (arg1: int)
    =
    foo arg1

let eps : double = 1E-20