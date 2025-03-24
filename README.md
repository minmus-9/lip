# LISP In Python

"OH NO! Another &^%ing LISP written in Python??!"

This one is different from the others I've studied in that it:
- Has been written in continuation-passing-style (CPS) and uses
trampolines throughout (see refs) so it won't blow up the Python
runtime stack,
- Is usable enough to get through ``Structure and Interpretation of
Computer Programs'' (SICP aka the Wizard Book, see the references
below),
- Has tail-call support, and
- Has first-class continuations with unlimited extent.

```
(define (loop f)
    (define c (call/cc (lambda (cc) cc)))
    (f)
    (c c))

(define (loop f)
    (f)
    (loop f))

(define (! n)
    (define (iter product k)
        (if (< k 2)  product  (iter (* product k) (- k 1))))
    (iter 1 n))
```

It weighs in at ~1650 Python SLOC and ~550 LISP SLOC (for the standard
library). This may seem like a lot, but there's a lot in there -- see
"The Language" below.

If you think of something *actually useful* to do with a
lisp-embedded-in-python, please let me know.

## Running the Code

Use
```
./lisp.py -
```
to run the REPL and
```
./lisp.py examples/factorial.lisp
```
to run code from a file. Finally,
```
./lisp.py file1 file2 ... fileN -
```
loads the specified files and then enters the REPL.

Note that `lisp.lisp` is automatically loaded by `lisp.py`.

## The Language

The core language is pretty much complete I think:

|Special Form|Description|
|--------------------------|-----------------------------|
|`(begin e1 e2 ...)`|evaluate the expressions in order and return the value of the last one|
|`(cond ((p c) ...)`|return the `Consequent` for the `Predicate` that returns true|
|`(define sym value)`|bind `value` to `sym` in the current environment|
|`(define (sym args) body)`|bind `(lambda (args) body)` to `sym` in the current environment|
|`(if p c a)`|if `Predicate` then `Consequent` else `Alternative`|
|`(lambda (args) body)`|create a function|
|`(quasiquote x)`|aka \`, begin quasiquoted form|
|`(quote obj)`|aka `'`, returns obj unevaluated|
|`(set! sym value)`|redefine the innermost definition of `sym`|
|`(special sym proc)`|define a special form|
|`(special (sym args) body)`|define a special form as `(lambda (args) body)`|
|`(trap obj)`|returns a list containing a success-flag and a result or error message|
|`(unquote x)`|aka `,` unquote x|
|`(unquote-splicing x)`|aka `,@` unquote and splice in x|

Quasi-quotation is supported; see `lisp.lisp` for some examples. There is no
macro system in this LISP, just quasiquote.

|Primitive|Description (see the source)|
|--------------------------|------------------------------|
|`()`|the empty list aka false|
|`#t`|true singleton|
|`(apply proc args)`|call `proc` with `args`|
|`(atom? obj)`|return true if obj is an atom: `()` `#t` or symbol|
|`(call/cc (lambda (cc) body))`|also `call-with-current-continuation`|
|`(call/cc)`|fast sugar for `(call/cc (lambda (cc) cc))`|
|`(car list)`|head of list|
|`(cdr list)`|tail of list|
|`(cons obj1 obj2)`|create a pair or prepend `obj1` to list `obj2`|
|`(/ n1 n2)`|return `n1 / n2`|
|`(eq? x y)`|return true if the atoms `x` and `y` are the same|
|`(equal? n1 n2)`|return `#t` if `n1` and `n2` are equal else `()`|
|`(error obj)`|raise `lcore.error` with `obj`|
|`(eval obj)`|evaluate `obj`|
|`(eval obj n_up)`|evaluate `obj` up `n_up` namespaces|
|`(exit obj)`|raise `SystemExit` with the given `obj`|
|`(< n1 n2)`|return `#t` if `n1` < `n2` else `()`|
|`(* n1 n2)`|return `n1 * n2`|
|`(nand n1 n2)`|return `~(n1 & n2)`|
|`(null? x)`|return `#t` if x is `()` else `()`|
|`(print ...)`|print a list of objects space-separated followed by a newline|
|`(range start stop step)`|same as the python function but return a list|
|`(set-car! pair value)`|set the car of a pair|
|`(set-cdr! pair value)`|set the cdr of a pair|
|`(- n1 n2)`|return `n1 - n2`|
|`(type obj)`|return a symbol representing the type of `obj`|

You'll note that `+` is not in the list. It is implemented in the standard
library in terms of subtraction. `nand` is used to create all of the usual
basic bitwise ops. There's no predefined I/O either since it isn't clear
what is wanted there, but this code is very easy to extend. Also, see the
next section on the FFI interface.

The `lisp.lisp` standard library defines a bunch of procedures in LISP:

|Name|Description|
|----|----|
|`(+ x y)`|return `x + y`|
|`(% n d)`|return `n` mod `d`|
|`(& x y)`|bitwise and|
|`(\| x y)`|bitwise or|
|`(^ x y)`|bitwise exclusive-or|
|`(~ x)`|invert bits of `x`|
|`(>= x y)`|return `#t` if `x >= y` else `()`|
|`(<= x y)`|return `#t` if `x <= y` else `()`|
|`(> x y)`|return `#t` if `x > y` else `()`|
|`(abs x)`|absolute value|
|`(and ...)`|logical-and or args|
|`(assert expr)`|raise an error unless `expr` is true|
|`(caddr)...(caddddr)`|extract initial elements from a list|
|`(copysign x y)`|return `\|x\|` with sign of `y`|
|`(fold-left f x0 list)`|see SICP p.158-65|
|`(fold-right f x0 list)`|see SICP p.158-65|
|`(for f start stop step)`|call `(f i)` for each `i` in the given range|
|`(foreach f list)`|call `f` for each element of `list` discarding the results|
|`(ftranspose f lists)`|similar to `map` but over the transpose of `lists`|
|`(gcd x y)`|greatest common divisor of `x` and `y`|
|`(iter-func f x0 n)`|compose `f` with itself `n` times|
|`(join list ...)`|concatenate lists|
|`(last list)`|return the last element of `list`|
|`(length list)`|return the length of `list`|
|`(let ((var value) ...) body)`|same as `((lambda (vars) body) values)`|
|`(let* ((var value ...) body)`|same but each `value` can access preceding `vars`|
|`(letrec ((var value) ...) body)`|same but all vars are pre-declared|
|`(list . args)`|create a list from args|
|`(loop f)`|infinite loop calling `(f)`|
|`(loop-with-break f)`|infinite loop calling `(f break)`, use `(break)` to terminate loop|
|`(lshift x n)`|bitwise left shift `x` by `n` bits|
|`(map f . lists)`|return list of `(f x y ...)` for each `x,y,...` in `(map1 car lists)` over `lists`|
|`(map1 f list)`|return list of `(f x)` for each `x` in `list`|
|`(not x)`|logical negation of `x`|
|`(or ...)`|logical-or of args|
|`(pair? x)`|return `#t` if `x` is a pair else `()`|
|`(queue)`|create a queue, see `lisp.lisp`|
|`(reverse list)`|return a reversed copy of `list`|
|`(rshift x n)`|bitwise right shift `x` by `n` bits|
|`(table compare-proc)`|create an associative table, see `lisp.lisp`|
|`(timeit proc reps)`|call `(f i)` for `i=0..reps-1` and report timing info|
|`(transpose lists)`|equivalent to `(ftranspose (lambda (x) x) lists)`|
|`(while f)`|loop on `(f)` while `(f)` returns true|

## FFI

Rather than adding everything under the sun as a built-in (I'm thinking of
the large number of functions in the `math` module, specifically), I chose
to create a Foreign Function Interface (FFI) to Python to ease incorporating
additional things into this LISP-ish doodad. With this interface, Python gets
to work with native Python lists instead of LISP lists; values are converted
back and forth automatically.

The net result is that the `math` module interface looks like
```
(math symbol args...)
```
so `sin(x)` can be obtained with
```
(math 'sin x)
```
where `(math)` is something close to (sans error checking):
```
@ffi("math")
def op_ffi_math(args):
    import math
    sym = args.pop(0)
    return getattr(math, str(sym))(*args)
```
which gets you the whole `math` module at once. See the "ffi" section of
`lisp.py` for the whole scoop. There are interfaces to `math`, `random`,
and `time` so far, along with some odds and ends like `(shuffle)` that
require separate treatment. The basic support for FFI lives in `lcore.py`
and is trampolined as well -- so potentially recursive LISP<->Python
data structure conversions won't blow up the python stack. Just keep in
mind that this makes it slow! For example, `(range 0 100000 1)` takes
about 25ms (`range` is implemented as an optimized primitive for
benchmarking); meanwhile,
```
@ffi("range")
def op_ffi_range(args):
    start, stop, step = args
    return list(range(start, stop, step))
```
takes over 1250ms.

## The Files

The evaluator lives in 2 files: `lcore.py` and `lisp.py`. The runtime
engine lives in `lcore.py` and is where the real action happens in
terms of trampolines, CPS, etc. The file `lisp.py` implements all of
the special forms, primitives, LISP-Python Foreign Function
Interface (FFI), etc. on top of `lcore.py`. A useful LISP runtime is
in the file `lisp.lisp` and defines things like `let/let*/letrec` and
whatnot.

Please pardon my goofy Pythonic LISP coding style. I'm new to LISP and
haven't quite hit the groove yet.

## Code Overview

Aside from the CPS thing, the code in `lisp.py` is fairly
straightforward: each operator receives an `lcore.Context` instance
that contains the interpreter's execution state (registers, stack,
symbol table, and global environment) and returns a continuation. In
the python realm, a continuation is just a python function that is
called from `Context.trampoline()`. The trampoline is really a means
of implementing `goto` for languages that don't have `goto`. Like
Python or C. CPS is goto-driven programming.

The code in `lcore.py` is fairly optimized and is filled with
unidiomatic and somewhat bizarre constructs including gems like
```
try:
    _ = proc.__call__
    [do something with callable proc]
except AttributeError:
    pass
```
instead of
```
if callable(proc):
    [do something with callable proc]
```
and
```
if x.__class__ is list:
    [do something]
```
instead of
```
if isinstance(x, list):
    [do something]
```

What's happening here is the elimination of Python function/method
calls *at all costs*; in particular, at the cost of readability :D
Function calls are so expensive that eliminating them can give you
a 100% speedup (CPython 3.10.12 Pop-OS! 22.04 LTS on a System76
i7-based Meerkat in low power mode).

Pairs are represented as 2-lists so `cons(x, y)` is `[x, y]`. This,
or its equivalent, is about the only thing that works with the
mutators `set-car!` and `set-cdr!`. In particular, using regular
Python lists as LISP lists breaks when you get to `set-cdr!`.

The runtime stack is also implemented as a LISP linked list of pairs.
This is almost twice as fast as using the `list.append()` and
`lisp.pop()` methods (pronounced *function calls*). You get the
idea. This choice makes continuations *cheap*. If you use a regular
list for the stack, you have to slice the whole thing to create or
call a continuation.

The `Context` class provides `.push()` and `.pop()` methods but
`lcore.py` doesn't use them internally. The `leval()` family of
functions inlines all of the stack operations for speed; this code
needs all the help it can get, speed-wise. You'll see things like
```
ctx.s = [x, ctx.s]  ## push(x)
```
and
```
ret, ctx.s = ctx.s
return ret  ## pop()
```
all over the place in `lcore.py`.

Much of the code in `lisp.py` is more traditional and idiomatic. It
uses `.push()`, `.pop()`, `car()`, `cdr()`, and so on to enhance
clarity and let you focus on *what* is going on instead of *how*
it's happening. The `unary()` and `binary()` helper functions are
optimized a bit because they're used so much. Certain other "hot
spots" have been optimized based on profiling data.

Passing circular data structures into the core will definitely cause
infinite loops. Fixing this in general would have a grave performance
impact and so it hasn't been done.

The Python GC is the LISP GC and so any LISP circular references
should eventually get cleaned up.

Internal exceptions (wrong #args etc) generate Python exceptions
that are handled in the usual Python way; LISP exception handling
is very basic (see `trap`).

## API

XXX TODO

See the `__all__` portion of `lcore.py`. The `lisp.py` file is a
demo of the low-level stuff.

## License

This code is licensed under the GPLv3:

```
lip - lisp in python
       https://github.com/minmus-9/liplip
Copyright (C) 2025  Mark Hays (github:minmus-9)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```

The file LICENSE contains a copy of the full GPL text.

## References

Here are some lisp-related refs that *heavily* influenced this code:

- https://web.mit.edu/6.001/6.037/sicp.pdf
- https://buildyourownlisp.com
- https://www.hashcollision.org/hkn/python/pyscheme/
- https://norvig.com/lispy.html
- https://norvig.com/lispy2.html
- https://github.com/rain-1/single_cream
- https://github.com/Robert-van-Engelen/tinylisp
- https://dl.acm.org/doi/pdf/10.1145/317636.317779
- https://en.wikipedia.org/wiki/Continuation-passing_style
- https://blog.veitheller.de/Lets_Build_a_Quasiquoter.html
- https://paulgraham.com/rootsoflisp.html
- https://www-formal.stanford.edu/jmc/index.html
- https://www-formal.stanford.edu/jmc/recursive.pdf
- https://legacy.cs.indiana.edu/~dyb/papers/3imp.pdf
- https://www.cs.cmu.edu/~dst/LispBook/book.pdf
