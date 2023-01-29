
a deeply convergent fork of <https://github.com/fairywreath/BytecodeInterpreter>

a compiler, and bytecode virtual machine for a tiny, object-aware language.

## features

- small: includes all the bits to make it not useless, without being bloaty.

- combined fixed integer/floating point integer arithmetic: stores fixed/floating point separately, but converts between either seamlessly, without depending on double-precision floating point only.

- handwritten parser makes adding new functionality straightforward.

- clean API: little to no macros, clean code, clean API, with the intention of making extending the language as easy as possible.

- builtin support for mutable strings and arrays. more types can be added very easily.

- no thirdparty dependencies: just needs a C compiler.

- familiar, javscript-like syntax.

## what's not implemented (yet)

- in-place arithmetics (`++`, `--`, `+=`, `-=`, etc). but should be trivial to implement.

- no error recovery. any error is fatal! would require adding state jumping via `setjmp`, which is less trivial.

- probably not as fast as it could be: underperforms when compiled with no optimizations, but acceptable speed when compiled with full optimizations.

## what it looks like

binary trees:

```

function clock()
{
    return 0;
}

class Tree
{
    init(item, depth)
    {
        this.item = item;
        this.depth = depth;
        if (depth > 0)
        {
            var item2 = item + item;
            depth = depth - 1;
            this.left = Tree(item2 - 1, depth);
            this.right = Tree(item2, depth);
        }
        else
        {
            this.left = null;
            this.right = null;
        }
    }

    check()
    {
        if(this.left == null)
        {
            return this.item;
        }
        var a = this.item;
        var b = this.left.check();
        var c = this.right.check();
        return a + b - c;
    }
}

var mindepth = 4;
var maxdepth = 14;
var stretchdepth = maxdepth + 1;

var start = clock();

var dep =  Tree(0, stretchdepth).check();
println("stretch tree of depth:", stretchdepth, " check:",dep);

var longlivedtree = Tree(0, maxdepth);


var iterations = 1;
var d = 0;
while (d < maxdepth)
{
    iterations = iterations * 2;
    d = d + 1;
}

var depth = mindepth;
while (depth < stretchdepth)
{
    var check = 0;
    var i = 1;
    while (i <= iterations)
    {
        var t1 = Tree(i, depth).check();
        var t2 = Tree(-i, depth).check();
        check = check + t1 + t2;
        i = i + 1;
    }
    println("num trees:", iterations * 2, ", depth:", depth, ", check:", check);
    iterations = iterations / 4;
    depth = depth + 2;
}

println("long lived tree of depth:", maxdepth, ", check:", longlivedtree.check(), ", elapsed:", clock() - start);
```

or take a look at the `*.fei` files.


## Language Syntax

### Comparators and Logical Operators
> The '==' comparison operator can be replaced with the keywords 'is' or 'equals'
```
40 > 41;       // false
194.5 <= 992.2;   // true

var string assigned "isString";
string == "isString";         // true
string is "notString";        // false

false = !true;    // true
```

### Control Flow
#### If Statements
> 'else if' can be replaced with the 'elf' keyword.
```
if condition then
{
    println("then statement");
}
else if condition then
{
  println("else if statement");
}
else
{
  println("else statement");
}
```
#### While and For Loops
> Fei inherits while and for loops from C.
```
while condition
{
  println("while statement");
}

for (var i = 0; i < 10; i = i + 1)
{
  println(i);
}
```
#### Do While and Repeat Until Loops
> Fei also supports do while and repeat until loops. 'do while' loops until the expression is false. 'repeat 'until' loops until the expression is true.
```
var i assigned 0;

do
{
  println(i);
  i= i + 1;
} while i < 10;
 
repeat
{
  println(i);
  i = i - 1;
} until i equals 0;
```

#### Switch Statements
> Switch cases automatically breaks if the condition is met.
```
switch variable
{
  case 0:
  {
      // execute statment
  }
   case 1:
  {
      // execute statment
  }
  default:
    // default statmenet
}
```

#### Break and Continue Statements
> All loops support break and continue statements.
```
while condition
{
    if breakCondition then break;
    if continueCondition then continue;
}
```

### Functions
```
function product(a, b)
{
    return a * b;
}

function printHello()
{
    print "Hello";
}

var prod = product(12, 23);      // 276
printHello();                    // prints "Hello"
```

### Classes
> Fei has a Python-like class system. Constructors are initialize with the 'init' keyword and class members use the 'this' keyword. 
> The keyword 'from ' used to declare inheritance. The 'super' keyword is used for superclasses.

```
class ParentClass
{
    init(name)
    {
        this.name = name;       // assign name to class member 'name'
    }
    
    printName()                 // class methods are initialized without the 'function' keyword
    {
        print this.name;        
    }
}

ParentClass parent("My name");
parent.printName();             // prints "My name"

class ChildClass from ParentClass
{
    init(name, age)
    {
        super.init(name);       // parent constructor
        this.age = age;
    }
    
    printAge()
    {
        print this.age;
    }
}

ChildClass child("Your name", 17);
child.printName();             // prints "Your name"
child.printAge();              // prints 17

```



