# Brainfmap
Functional ["Brainf*ck"](https://en.wikipedia.org/wiki/Brainfuck) VM/interpreter utilizing monadic state.

(requires [mtl](http://hackage.haskell.org/package/mtl))

### Features
* 8-bit "wrap-around" data memory cells
* Infinite amount of data memory cells in the "right" direction
* [Turing complete](https://en.wikipedia.org/wiki/Brainfuck#Array_size)

### Demos and Usage
Todo.
For now, running main interprets and prints the output of the included example program.
```
++++++++++[>+>+++>+++++++>++++++++++<<<<-]>>>----.>++++++++++++++.-----------------.++++++++.+++++.--------.+++++++.------------.+++++++++++++++.

> B    66
> r    114
> a    97
> i    105
> n    110
> f    102
> m    109
> a    97
> p    112
```


### Future Work
* Live console REPL and read from user specified filepath
* Configurable output and input modes, or allow for other representations of the outputted bytes
* Null char to enable string printing
* Reverse Haskell-to-Brainfuck transpiling
* [Malbolge](https://en.wikipedia.org/wiki/Malbolge) support.

