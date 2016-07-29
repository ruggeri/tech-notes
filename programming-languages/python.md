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
    * There's a question of "inner" async calls. For instance, if you
      have an async function that itself calls async functions.
        * In my JS spawn example, we would write `yield
          spawn(innerAsyncCall)`.
        * A way to avoid using `spawn` except at the outermost level
          is to say `yield* innerAsyncCall`.
        * `yield*` means "splice in this generator"'s values into my
          generator.
        * This is desirable to avoid writing `spawn`-specific code;
          now async code is written generator style, and any library
          can run it.
    * In Python, the analogue to `yield*` is `yield from`.
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
* Python has `*args` and `**kwargs`.
    * Any argument can be passed by name.
    * You have to pass all positional arguments first, then specify
      some arguments by name.
    * You can use defaults.
* Debugging: `pdb`
    * You can call `pdb.set_trace()` wherever you like!
* You use pyenv to manage Python versions.
    * `pip` is the default for installation of libraries.
    * You can `pip freeze > requirements.txt` to dump something like a
      Gemfile. You can `pip install -r requirements.txt` to do a
      `bundle install`.
    * virtualenv is like gemsets from RVM. I think `bundle exec` is
      the cleaner solution, but I don't know that an analogue exists
      for Python.
* You can use `import imp; imp.reload("library_name")` to force
  reloading of that library. This is not transitive, however...
