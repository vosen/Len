# Len
As you may not know fsc compiles such declarations
```
module Foo =
    let x = SlowFunction()
    let y = FastFunction()
```
to something roughly equivalent to this C# code:
```
static class Foo
{
    static Foo()
    {
        x = SlowFunction();
        y = FastFunction();
    }
    
    static int x;
    static int y;
}
```
Which is fine, unless you have giant modules with masive amount of let bindings (e.g. data for tests),
which might make everything slower than necessary.

This project converts the code at the beginning to
```
module Foo =
    let x = lazy(SlowFunction())
    let y = lazy(FastFunction())
```
possibly winning some performance.
