# Checksums (Wikipedia)

- small piece of data derived from a block of data to validate its integrity
- checksum function => produces checksum from block of data
- good function => large changes to value when data changes

problem stmt: if checksum of blob matches previous checksum, what's the percent
chance that blob is not the same as before

- similar problem space as that of hashes, fingerprints, cryptographic hash
  functions. However, design goals are different for each function.
- check digits + parity bits are special cases of checksums useful for small
  blob of data.
- error correcting codes are based on special checksums that can detect and
  recover from errors.

# Error Detection

http://www-users.york.ac.uk/~dajp1/Introductions/GSW_Error_Detection.pdf

#### Parity byte/word

Data into a fixed number of n bits, then computes XOR of all words. Result is
appended onto the message. Receiver simply breaks up message into words, XOR it
all, and if it's all 0, then great! Otherwise, sad!

If the whole packet/frame is L bits, the above method is essentially breaking
up the message into one parity bit for a block of size L/n, for a total of n
parity bits.

Another way to word it is to break data into a fixed number of n bits and add a
parity bit to each block. This is equivalent to the above but worded
differently. In this case, for a packet of L bits, there will be L/n parity
bits. It's essentially putting the parity vertically or horizontally if you lay
out the bits.

1 0 0 -> 1
0 0 0 -> 0
1 1 1 -> 1

0 1 1

In one example, the parity bits are added to each row, in the other example,
the parity bits are added to each column. Same algorithm, framed two different
ways.

This algorithm isn't very good, because its lower bound is basically no better
than the original error rate. AKA, if I have a 0.1% chance of erroring, this
algorithm also has a similar chance of erroring when checking for errors, so
the algorithm does no better than just not checking at all.

#### Checksums

Adding all numbers up and taking their sum modulo 2^N (essentially taking least
significant N bits of a the number).
