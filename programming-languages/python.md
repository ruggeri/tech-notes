* Multiple inheritance; seems to work like mixins mostly.
* Methods are bound: `f = obj.method; f()` calls `method` on
  `obj`. Weird, seems to be treated differently than other
  properties. JS seems more consistent here...
* Most constructs are based on interfaces.
    * `with` for resource teardown uses `__enter__` and `__exit__`
      methods on an object.
    * `for` uses an `Iterator`, which implements `__next__` until
      `StopIteration` raised.
* Generator functions: these return an iterator when called. Just like
  in JS.
* Coroutines: just like in JS. Can use `async def`/`await` to
  return/wait upon futures.
    * `asyncio` library gives you an event loop where you can schedule
      your futures.
    * The event loop must be subscribing to events indicating
      completion.
    * Can turn a generator into a coroutine using the
      `@asyncio.coroutine` decorator. Just like spawn in JS.
    * To do that, you need to use `yield from` for inner async
      calls. This ensures that you will wait through the entire inner
      call before continuing the outer work.
    * `yield*` does the same in JS. I think we didn't need this if
      everyone agreed to use `spawn` for inner calls; in which case
      you return out a regular promise. Whatever.
* Python also has the `threading` module, but of course you have the
  GIL.
    * `multiprocessing` is a module which starts other processes for
      parallelism. You can use it to call other Python
      functions. Python gives you some kind of IPC like pipes, but
      it's janky.
    * In the `concurrent` library I believe there's a threadpool
      mechanism.
    * But I'm not entirely sure why asyncio is preferred to threading.
    * Well, I think Python does spawn system threads, so you have to
      pay that cost. Even despite the GIL.
