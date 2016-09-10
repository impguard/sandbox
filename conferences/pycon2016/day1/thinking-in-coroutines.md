Thinking in Coroutines (ŁUKASZ LANGA)
=====================================

Asynchronously serve requests.
Event loop that calls callbacks (one event loop).

No busy looping, uses select (OS level) under the hood.

Instead of plain old blocking function, use coroutines to avoid slow function
blocking everything.

Coroutine = instance of a coroutine function

Tasks
=====

Tasks will call one step of a coroutine

* Until one yield...

Libraries
=========

aiohttp - asyncio http client
aiopg + aiomysql accesing DB's with asyncio
uvloop for speed

Executors
=========

Lets you use processes/threads with asyncio

Random Advice
=============

* Use py3.5 (less complicated and better syntax)
* Write unit tests - easier to write
* Set up gc debugging and logging, set up loop debugging
* Prefer ProcessPool executors (No GIL)
* Read the docs/read the source

