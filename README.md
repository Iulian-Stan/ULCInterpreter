# ULCInterpreter

## Substitution-based evaluation

This is the implementation of the evaluation rules for the untyped lambda calculus (ULC), 
using both normal-order and applicative-order semantics. It is based on ther previously 
created [ULCParser](https://github.com/Iulian-Stan/ULCParser).

Additional remarks should be made:

* The **values** are functions.
* The **functional normal form** is employed i.e., the evaluation of an expression stops when it is brought to the functional form, even if its body still contains beta-redexes.

Note that the rules above designate **//small-step//** evaluation semantics, meaning that a **single** evaluation step is performed. 
In contrast, **//big-step//** evaluation refers to the continued evaluation of an expression until either its **value** is obtained, 
or the evaluation **cannot** proceed any further, according to the specified rules.

### Example 

The same input is used as in the previous application:

```
true=\x.\y.x 
false=\x.\y.y 

not=\x.((x false) true)
(not true) 
(not false)

and=\x.\y.((x y) false)
((and true) true)
((and true) false)
((and false) true)
((and false) false)

or=\x.\y.((x true) y)
((or true) true)
((or true) false)
((or false) true)
((or false) false)

nil=\x.true
null=\l.(l \x.\y.false)
cons=\x.\y.\z.((z x) y)
car=\l.(l true)
cdr=\l.(l false)
if=\p.\then.\else.((p then) else)

fix=\f.(\x.(f (x x)) \x.(f (x x)))
(fix fun)

append=\a.\b.((fix \r.\a.(((if (null a)) b) 
                          ((cons (car a)) (r (cdr a))))) a)
la=((cons x) nil)
lb=((cons y) ((cons z) nil))
lc=((append la) lb) 
(car lc)
(car (cdr lc))
(car (cdr (cdr lc)))
```

Since the evaluation of an expression must take into account the previous definitions in the input program, 
a so-called **//global context//** is passed as an argument and returned from the evaluation functions. 
It simply stores bindings of variables to expressions.

### Running the aplpication

This is a simple aplpication that can be run using [Hugs 98](https://www.haskell.org/hugs/pages/downloading.htm) 
that provides an almost complete implementation of Haskell 98. Although it is no longer in development it is lightweiht 
and satisfies all the needs for running this application.

After launching **Hugs 98** you should run two commands:
 1. ``:load interpreter.hs``
 2. ``interpretProgram "program.in"``

It will display the evaluation of the provided program.
