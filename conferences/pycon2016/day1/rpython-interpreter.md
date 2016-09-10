RPython Interpreter
==================

Interpreter
-----------

1. Parses your source code (convert to bytecode)
    * Lexing into a list of tokens
    * Parsing into an AST
    * Generate bytecode

Bytecode
--------

Code:

    def foo():
        a = 2
        b = 3
        return a + b

Output (dis library for disassembling in Python):

    LOAD_CONST
    STORE_FAST
    LOAD_CONST
    STORE_FAST
    LOAD_FAST
    LOAD_FAST
    BINARY_ADD
    RETURN_VALUE

Function Call: Same as running the full program! Fetches bytecode and creates
stack.

JIT
---

* On-demand machine code generation
* Very complicated

RPython
-------

* Subset of Python
* rlib, no access to standard python libraries
* Compiles down to C (comes with JIT and GC)
* Ideal of writing of interpreters

* Can be executed/tested like any other python program (don't need to compile
  to run)

Type System
-----------

Very similar to what we made in class, static type analysis of normal Python
code.

asserts are hints to compiler about types

JIT Elidable
------------

Memoization but at the compiler level.

    @jit.elidable
    def lookup(name):
        return namespace(name)


Working with RPython
--------------------

1. Write valid Python (easy)
2. Modify it until it's valid RPython (take lots of time)

Biggest Issues
--------------

No documentation and very little knowledge about errors
Late stage translation error takes a long time to debug

Lessons Learned
---------------

Some PHP specific features
Function calls are expensive

Basic RPython
-------------

Need an entry point

    def entry_point(argv):
        filename = argv[0]
        source = filename.read()
        ast = source_to_ast(source)
        bc = ast_to_bytecode(ast)

        interpreter = Interpreter()
        interpreter.run(bc)

