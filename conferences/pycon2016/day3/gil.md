Gilectomy (Larry Hastings)
--------------------------

Multithreading
CPython Internals
cf. "Python's Infamous GIL"

1992 - GIL created!

Ramifications
-------------

I/O Bound - OK
CPU Bound - Not OK

Free Threading Patch
--------------------

1999 - GIL removed
* no api changes
* globals into struct
* single mutex around incr/decr
* 4x-7x slower

"Inside Look at the GIL Removal PAtch of Lore"

Gilectomy
---------

4 Technical Considerations:

1. Reference Counting
    * Multiple threads holding a var, you run into a race condition with the
      count
2. Globals and Statics
    * Per thread globals need tobe separate
    * Shared singletons need to be connected
3. C Ext Parallelism and Reentrancy
4. Atomicity
    * If you have multiple threads, you can break list appending or object mutation

3 Political Considerations:

1. Don't hurt single-threaded performance
2. Don't break C Extension
3. Don't make it too complicated
    * CPython is easy to work on, don't compromise this with multicore support

Tracing Garbage Collection
--------------------------

* About as fast (YAY)
* Break all the C extensions (NO)
* More complicated API (NO)

Software Transactional Memory
-----------------------------

* Extremely fast (YAY)
* Break every C extension (NO)
* Also incredibly complicated (NO)

Reference Counting
------------------

* Won't break things (atomic incr/decr)
    - atomic incr/decr an integer

Globals and Statics
-------------------

* Per Thread
    - PyThreadState (already done)
* shared singletons
    - ???

Atomicity
---------

Locks everywhere!

* All mutable objects locked
    - str (has mutable fields under the hood)
* Userspace locks

Two Builds
----------

Two builds of Python, with/without the GIL
* Complicating source code

New API
-------

Enforce new best practices

Don't make it too complicated
-----------------------------

:( but we give it up

How to remove the gil
---------------------

0. Atomic incr and decr
1. pick lock
2. lock dictobject
3. lock listobject
4. lock 10 freelists (global scope)
5. disable gc, track and untrack macros
6. remove the gil
7. use tls thread state
8. fix tests

Language Summit Benchmarks
--------------------------

3.5x slower wall time (look at clock)
25x slower cpu time

gilectomy's official benchmark - fibonacci

Why is it so slow?
------------------

* Lock Contention
* Synchronization and cache misses
    - Atomic incr/decr blows away cache lines over and over
* --private--

Where do we go from here?
-------------------------

* Reference Counting
    - Buffered reference counting
        - Separate thread tracks reference counts
    - Immortal objects
        - Reference count never changes
    - Coalesced Reference Counting
* Thread private locking
    - Unsynchronized fast until another thread wants to talk to you
* garbage collection
    - stop the world
    - buffer the garbage collection
* Auto-lock C-extension ???

Final Thoughts
--------------

'A journey of a thousand miles begins with a single step'
