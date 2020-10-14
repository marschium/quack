# Quack

## quack quack quack

A small toy language created so I could learn a bit about creating languages.

Compiles to bytecode-ish and runs on the quack VM. Proper bytecode saving/loading coming soon™.

Has a handful of builtin functions to produce basic maths and boolean logic. Has rust bindings so that you can your own!

Each program must have a main function that is the entry point.

## Types

Supports 3 types

* bool
* number
* struct - something that holds bools and numbers

## Examples

The following..
```
struct vector { x: num, y: num }

fn print_plus_one(b: num) {
    c = add(b, 1)
    dump(c)
} 

fn main() {
    print_plus_one(7)
    a = new vector { 7, 8 }
    dump(a.x)
    dump(add(a.x, a.y))

    d = 1
    if lt(d, 2) {
        dump(1)
    }
}
```
prints...
```
Some(Num(8.0))
Some(Num(7.0))
Some(Num(15.0))
Some(Num(1.0))
```

Take a look in [./examples](examples)