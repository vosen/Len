type Item = { Val1: int; Val2: int; Val3: int; Val4: int; Val5: int }

module Foo =
    module Bar =
        let item1 : Item =
            {
                Val1= 1
                Val2= 2
                Val3= 3
                Val4= 4
                Val5= 5
            }

        let item2 : Item =
            { item1 with
                Val4= 8
                Val5= 10
            }