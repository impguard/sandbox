### datatypes

datatype: type constructor and data constructors

```
data Bool = False | True
```

Defines a type and how one constructs that type, along with data.

### polymorphism

definition: poly (many) morph (form) => made of many forms

three types of polymorphism: concrete, parametric, constrained

##### concrete

Something like `Int -> Bool` where the types are fully constrained to a
particular set of types.

Broadest set of operations can be performed since the type is narrowed down
concretely.

##### parametric

parametricity: the behaviour of a function with respect to the types of its
arguments is uniform

Something like `a -> b -> c` where the types are not constrained at all.
They're simply type variables and can be any type.

However, the types are limited to what operations can be performed on them.

##### contrained

Something like `Num a :: a -> b` where the types are partially constrained in
that the type must be a "subclass" or "subtype" of Num. In Haskell, the
terminology  is that `a` is constrained to the `Num` typeclass.

A broad set of operations can be performed on this type since it's been
narrowed down.

### expression problem

http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt

"The goal is to define a datatype by cases, where one can add new cases to the
datatype and new functions over the datatype, without recompiling existing
code, and while retaining static type safety." - Philip Wadler

Using Java as an OO example, this means I want to be able to create new
"subclasses" of a particular data type easily. I want to also add new functions
of that data type, without recompiling existing code.

In Java, it's easy to "add new cases" by extending a superclass without
recompiling.  However, adding new functions over the data type requires going
into the original code and recompiling.

In Haskell, it's easy to "add new functions over the datatype" by saying that a
particular datatype now implements a new typeclass. But you can't really add
new subclasses of a particular data type easily.

### binding

variable is bound when it is linked to another object. A bound variable cannot
be bound to another object.

### normal form

normal form (NF)
: the expression is fully evaluated
weak head normal form (WHNF)
: expression is evaluated as far as is necessary to reach
  a data constructor (or lambda)

```
(1, 1) -- WHNF & NF
(1, 1 + 1) -- WHNF
\x -> x * 10 -- WHNF & NF (cannot be reduced until argument is passed)
"Papu" ++ "chon" -- Neither (not evaluated but no data constructor?)
```

### spine

definition
: the structure that glues a collection of objects together
: the structure of the collection that isn't the values

```
-- For the list [1, 2, 3]

1 :
  (2 :
     (3 : --|
         []))

-- Each ':' is a  cons operator that represents the "spine"
```

### catamorphisms

definition: cata (down or against) morph (form) => deconstructing data

One way to look at it is catamorphisms break down the structure of a
collection. If spines define the stuff that isn't the values in a collection, a
catamorphism breaks down the spine.

Easiest example is the "reduce" operator which is a weaker form of a "fold".
This basically breaks down a list (though it can build a new list if it likes).

### algebraic datatypes

One can think of a type as an enumeration of constructors that have zero or
more arguments (makes sense).

One typically models the problem domain before worrying about the computations
to solve the problem. Being able to explore the problem domain and build a
strong model for it is key before you write a single bit of computational code.

```
-- data declaration
data [] a = [] | a : [a]
data Bool = True | False
```

`constants` are type and data constructors that take no arguments. Above,
`True` and `False` are both value constants and `Bool` is a type constant since
it's a constructor that takes no arguments.

### algebra

definition: one more operations and the set that they operate over.

In Haskell, typeclasses are a big deal because they focus on allowing a user to
identify common patterns or operations and then create a "typeclass" to
generalize the set of types these operations operate over.

### monoid

definition: a binary associative operation with an identity

So something is a monoid if it supports a binary associative operation...so an
operation that takes two arguments (think addition or subtraction) that is
associative. This means that I can swap the arguments around.

It must also have an "identity" which I presume is a value that when operated
with any other value in the data type (with the binary associative operation)
returns the other operation.

Common examples: summation, multiplication

Laws that a monoid follows:

```
mappend mempty x = x
mappend x mempty = x

mappend x y = mappend y x
mappend x (mappend y z) = mappend (mappend x y) z
```

Thinking of monoid as a way of "combining" values together makes sense.
However, it might be better to think of it as finding a "summary" for the set.
For example, numbers are monoidal under max, but that doesn't really combine
anything.
