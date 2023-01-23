
 - missing syntactical features:

    + array literals
        - may actually need better valarray impl. current one is completely linear, and as basic as it gets.

    + hashmap literals
        - same as with arrays, current impl is pretty basic. gets the job done, but probably not efficiently.

    + actual implementation of pseudo-classes for builtin types
        - both `String` and `Number` are *declared*, but are functionally blank spaces, with
          no meaningful way to add (or remove) anything to/from them, scripting-wise.

 - missing VM features:
    + inefficient object creation "system".
      which is to say, there is no system. no cacheing, or pooling (except for strings).
      everything is created and forwarded as-is, which is really inefficient.

 - missing operators that need implementing:
    + `^` (binary xor)
    + `~` (binary not)
    + `|` (binary or)
    + `&` (binary and)
    + `<<` (shift left)
    + `>>` (shift right)
    + `++` (infix/prefix increase)
    + `--` (infix/prefix decrease)
    + `**` (power of)

