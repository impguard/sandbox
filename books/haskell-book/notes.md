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

type constant
: type constructor with no arguments
: a concrete type that can't be parameterized
type constructor
: a parameterizable type
value constants
: a constant value that that doesn't need further arguments
value constructor
: a value that needs further arguments to fully flush out the value
nullary
: a value or type constructor is nullary if it takes no arguments

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

### semigroup

Semigroups is a superset of monoids in that every monoid must be a semigroup
but every semigroup is not necessary a monoid. Semigroups don't support the
mempty property. The clearest example of a semigroup is a NonEmpty list.

You can "mappend" two NonEmpty lists but there's no "identity" since, by
definition, a non empty list must be... non empty.

### functors

Functors are a category of data types that support the ability to "map" a
function or apply a function over the values it contains while leaving the
structure intact.

Saying the above in different words...the key thing about functors is that it
leaves the structure untouched and applies the function to the value it
contains.

### eta-reducing

Dropping the abstraction over a function. An eta reduction would be moving from
`\x -> abs x` to `abs`. An eta abstraction is going the opposite direction.

### pragmas

An easy way to understand pragmas is to correlate to what Babel does for
Javascript and its "plugins". At the end of the day, a compiler takes a
language and...

1. tokenizes it
2. parses it to an AST
3. compiles it down to machine code

During each stage, you could theoretically build a "plugin system" that, after
potentially the default behaviour of the compiler", does some additional stuff
while maintaining that the output of the stage remains the same.

To be honest, this plugin system probably takes the form of having "functions"
that can be run after each stage, should the plugin writer choose so.

For example, the plugin can...

1. add some more tokens to be considered when tokenizing
2. adjust the parser to recognize how to leverage those tokens and create
   additional AST nodes.
3. handle the new or modified AST nodes to create different code.

Alternatively, a plugin can choose to only affect some of the stages rather
than all.  For example, an optimization plugin can choose to adjust how AST
nodes get converted to actual code to better optimize the final output.

More concretely, let's imagine how Babel might handle the "ES5" vs. "ES6"
plugins. My thoughts are that Babel exposes a few functions that these plugins
can choose to override to control how it handles compiling ES6 code to
Javascript. Each plugin allows for a different set of behaviour (for example,
supporting import syntax or const/let or foreach etc.) and handles how to lex,
parse, and compile code that looks like ES5 or ES6. In fact, the plugin system
might be so advanced that each plugin can further be customized via options to
target specific outputs, so while the generic ES6 plugin has a general strategy
for adjusting how one tokenizes or parses or compiles code, adjusting an option
to target Node vs. Chrome might cause it to output very different code.

In Haskell, these language "pragmas" are similar. Under the hood, the compiler
probably supports "plugins" that adjust how it chooses to lex, parse, and
compile the code. However, unlike Babel which uses a simple JSON file to
configure which plugins are enabled and their associated options, Haskell
chooses to have comment blocks of a specific format be the switches for certain
plugins/features.

These features end up being compiler specific since they're very much
essentially toggles for the compiler to compile slightly differently, and you
need to really understand what the option is doing to compile your code
differently.

Back to concrete examples, you could imagine that the compiler doesn't support
a fancy string syntax sugar and one can't just write "hi" for a string and must
write ['h', 'i']. That's painful but maybe it makes sense. Then, you turn a
pragma on and the compiler will now properly lex "hi" into a token, and perhaps
the parser can convert the "hi" token into an array so the compiler doesn't
need to worry about a "string" node in the AST.

I can imagine this makes compilers an order of magnitude more complicated,
however.  Now you have to deal with understanding how to customize your
compiler behaviour and make sure that different feature flags are compatible
with one another. You wouldn't want one pragma supporting "hi" in one way and
another pragma supporting it in another way.

### applicatives

They are monoidal functors.

Right now, it seems like they're just like functors except the function that
they "lift" is already lifted into the functorial structure.

Similarly, the function it applies is applied over the structure of a functor
(or applicative) and on the values themselves. However, the function is already
lifted into the applicative/functor and isn't just a free function.

Take a look at the structure of the apply operation that defines an
applicative:

```
f (a -> b) -> f a -> f b
```

At face value when you've been using `fmap` a bunch it looks like all it does
is it allows my one function to be lifted into the functorial structure. But
that's more interesting than it sounds. For example....

```
[(*3), (*2)] <*> [1, 2, 3]
```

Now my "one function" is lifted into the structure, so it's not really "one
function". If anything, you can say it's one function type being lifted into
the functorial structure.

So each function applies to my inputs, but I need a way to combine the results!
So I smash them together, and this is where the **monoidal** part of **monoidal
functors** comes from.

A better example is the applicative instance for tuples:

```
("Woo", (+1)) <*> ("Hoo!", 0) -- ("Woo Hoo!", 1)
```

So the key thing here is your structure is being monoidally combined, which is
why the tuple applicative instance requires the first parameter be a monoid.

The reason the monoidal part of something like a list isn't super clear is
because the way you combine lists monoidally is by...creating a bigger list. It
is concatenation but you don't see it clearly. With something like a Maybe
value the way you monoidally combine is by keeping either Nothing or Just so it
also doesn't "seem monoidal".

Or maybe, a better way to word the above is that combining the structures is
transparent since there's no type parameter associated with them so it's
inherent in how you would implement the applicative apply function.

#### interesting bits

One main interesting part of the applicative is the monoidal portion since it
allows for multiple types of applicatives for any given type.

A clear example is for a list, the applicative instance for a list could either
concatenate the results or zip them together. Seems like implementation wise
people choose to ignore this for lists, opting for creating one-off functions
like zipLists to perform the applicative behaviour.

Another example of why applicatives can be more interesting is the interaction
between `Either` and `Validation`. By default, the applicative instance for
`Either` simply short circuits once one error is found, but the `Validation` is
an alternative that concatenates all the errors it finds.

### monad


