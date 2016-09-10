Gevent
======

Download photos from FB:

    def download_photos(user):
        conn = open_connection(user)
        photos = get_photos(conn)
        save(photos)

    def download_photos(users):
        for user in users:
            download_photos(user)

How to make this fast?
- twisted, threading, multiprocessing, green_thread?

Multiprocessing
---------------

Spawn a new process for each user, program is concurrent, overhead of processes
makes it unscalable.

Multithreading
--------------

Similar to multiprocessing, but writing correct code is hard. Debugging is
hard.  GIL problems.

Event Driven Programming (Twisted, etc.)
----------------------------------------

Complicated, callback hell (initialize, register callbacks, etc.)

Green Threads
-------------

User space => OS does not manage
Cooperatively Scheduled => Threads yield to each other
Lightweight

Semantics are clear and similar to threads, but with all the advantages.

    gr1 = greenlet(print_red)
    gr2 = greenlet(print_blue)
    gr1.switch()

* A wrapper around yields/coroutines
* Allows functions to yield control flow to others
* Written in C, runs in the C stack

Building Block 1
----------------

    gr1 = greenlet(print_red)
    # Uses stack slicing (assembly level) to manipulate stack pointers/frames
    # and create the yield functionality.

Building Block 2
----------------

`libev` allows you to register event_handler callbacks

* Simply an event loop

In summary

* Creating a Greenlet creates a greenlet (essentially a coroutine)
* Joining with the Hub, the main greenlet, that runs the event loop
* Cooperative scheduling works by monkey patching standard library
    * Therefore, your standard functions automatically yield on certain
      some defined io operations.

cons:
* no parallelism
* non-cooperative code will block the process
* compute-bound greenlets will block
* monkey-patching has interesting import implications

pros:
* excellent for i/o bound applications
