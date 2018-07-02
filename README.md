# Brainfmap
Functional ["Brainf*ck"](https://en.wikipedia.org/wiki/Brainfuck) interpreter utilizing monadic state in less than 70 lines of Haskell

(Although requires [mtl](http://hackage.haskell.org/package/mtl) and imports [Data.Word](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Word.html), so true code size is probably unremarkable 😉)

### Features
* 8-bit "wrap-around" data memory cells
* Infinite amount of cells in the right direction
* [Therefore, Turing complete](https://en.wikipedia.org/wiki/Brainfuck#Array_size)

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

### Why StateT?
Fun, mostly. I wanted to learn more about both haskell abstractions and compiler/language theory so I started small. 
The brainfuck machine model is inherently stateful so I figured it would be suitable to simulate it with a state transformer.
That said, knowing Haskell, it is probably possible to write a smaller and more efficient implementation with some more functional-styled monadic magic or by just passing the memory around with good old pattern matching.
