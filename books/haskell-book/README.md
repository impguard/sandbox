haskell-book
============

Exercises and activities for the [haskell book](http://haskellbook.com/).

Getting Started
---------------

These various projects leverage Docker and Bake as dependencies. Everything
else will be installed in the docker container.

```
bake init   # Initialize by pulling down the Haskell container
bake shell  # Builds and runs the container before shelling into the environment
```

How to run
----------

For chapters without a stack project, run the repl and load a file to test out
various functions.

For chapters with a stack project, change to the chapter directory and run:

```
cd ch16
stack build
stack exec ch16
```

Notes
-----

The first few chapters were skipped because they were simple and very much related
to the "learn-you-a-haskell" book I read prior.
