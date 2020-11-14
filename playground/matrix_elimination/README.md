# Matrix Elimination

Very naive first attempt at implementing elimination to find the inverse of a
matrix.

Results: pretty straightforward, but my naive method produces a lot of
redundant code.

Theoretically elimination can be done down the matrix, then by flipping the
matrix, repeated. The results would be a list of transformations I can perform
on the identity. Again, can be simplified by performing it down the identity,
flipping it, and performing it again. Flipping it once more to produce the
result.

The entire problem could be simplified with better matrix base objects. My
implementations produced a "Vector" and a "Matrix". However, it's probably
easier implementing just the matrix and converting all the transformations into
matrix operations. Hence, creating an "elimination matrix". Then I could simply
derive the inverse by observing what the elimination matrix is at the end.

I didn't handle row swapping. Swapping rows makes things slightly harder since
I would have to remember which row was which. The naive implementation simply
tries all permutations of the matrix until one elimination step succeeds. I'm
curious if I can simply swap as I go? My gut says I can because if I end up
in some elimination step where I can't swap the remaining rows, it must mean
something about the how the matrices can or can't be combined:

But worse case that makes the algorithm a n^2 algorithm I think.
