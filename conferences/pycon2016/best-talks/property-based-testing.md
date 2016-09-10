Property Based Testing (Matt Bachmann)
======================================

Testing is HARD

* Code (with all of its maintenance/performance requirements)
* Isolation
* Fixtures
* Indirect Value

Property Based Testing
----------------------

Describe thee arguments
Describe the results
Have the computer try to prove you wrong

Normally:

    def test_empty():
        assert quicksort([]) == []

    def test_sorted():
        assert quicksort([1, 2, 3]) == [1, 2, 3]

    def test_unsorted():
        assert quicksort([1, 3, 2]) == [1, 2, 3]

I want to generate this by defining properties of my function.

Hypothesis <- library similar to quick check.

Pattern 1
---------

The code should not explode!

* Pass everything in, code should not explode.

Pattern 2
---------

??

Pattern 3
---------

Testing Oracle - Leave it alone

    @given(integers, strings)
    def test_against_legacy(arg1, arg2)
        assert(
            new_hotness(arg1, arg2)
            ==
            legacy_code(arg1, arg2)
        )

Test efficient solution identical to brute force solution

Pattern 4
---------

Stateful Testing

* Define a state
* What operations can happen in what conditions
* How operations affect the state
* Check if it works

Essentially creating a search space and exploring
