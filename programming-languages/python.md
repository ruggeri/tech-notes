- Multiple inheritance; seems to work like mixins mostly.
- Methods are bound: `f = obj.method; f()` calls `method` on
  `obj`. Weird, seems to be treated differently than other
  properties. JS seems more consistent here...
- Most constructs are based on interfaces.
  - `with` for resource teardown uses `__enter__` and `__exit__`
    methods on an object.
  - `for` uses an `Iterator`, which implements `__next__` until
    `StopIteration` raised.

# Generator Functions

- These return an iterator when called. Just like in JS.
- A function is a generator if it has the `yield` keyword in it.
- If it does, calling the function doesn't start it right away.
- Instead, calling the function returns a generator _object_. You call
  `next` on this to produce a value.
- The generator object can be passed to `next`. It will keep running the
  generator until the end of the generator function, at which point
  `next` raises `StopIteration`.
- Generator functions are very useful for creating iterators. `for` will
  iterator over a generator object.
- `yield from abc()` is mostly equivalent to `for x in abc(): yield x`.
  - In JS we call this `yield*`.
- But remember that we can _pass back in_ values to a generator.

```python
def my_fun(n):
  for i in range(n):
    x = yield i
    print(f"inside: {x}")

  return "DONE"

g = my_fun(3) # doesn't begin running generator function

next(g) # returns 0
g.send("a") # sends in "a", which gets printed, returns 1
g.send("b") # sends in "b", which gets printed, returns 2

try :
  g.send("c") # sends in "c", which gets printed, but generator ends...
except StopIteration as e:
  print(e.value) # prints returned value "DONE"
```

- You must start with `next`, not `g.send`. You can keep using `next`,
  in which case equivalent to `g.send(None)`.
  - Returned value is ignored by `for` iteration. But it is available as
    the `value` field of the `StopIteration` exception.
- You could use these generators to implement something like
  async/await, as I did with my JavaScript `spawn` implementation.
  - However, Python added `async` and `await` keywords just like
    JavaScript did.
  - By having their own `async`/`await` syntax, you can make this more
    ergonomic for the task of coroutines, rather than overload old
    generator syntax which is more ergonomic for iterators.
  - But I don't think you actually add _functionality_. Everything was
    _doable_ with generators.
  - Python calls the value of a `async def f(): pass` a "coroutine
    object", but the _concept_ of a coroutine is implementable just from
    these generators.

# `asyncio` and coroutines

- The module is called `asyncio`. The Python language construct is
  called a "coroutine": use `async def` to define a coroutine, and
  `await` to await its completion
- Basically, `async def` will define a function that, when called, will
  return a coroutine object. A coroutine can be `await`ed.
- To schedule a coroutine object on an event loop, you use
  `asyncio.run`. You might do something like:

```python
import asyncio

async def main():
  # blah blah blah

coro = main()
asyncio.run(coro)
```

- Note that calling `asyncio.run` launches the event loop. You can only
  one run at a time (per thread).
- To schedule concurrent work on the same event loop, use
  `asyncio.create_task`:

```python
import asyncio

async def more_work():
  await asyncio.sleep(1)
  return 42

async def main():
  # If you `await more_work()`, then you won't proceed to other main
  # work until more_work is done.
  t = asyncio.create_task(more_work())

  # blah blah blah, do some concurrent work

  result = await t
  print(result)

asyncio.run(main())
```

- The event loop is designed to allow lightweight concurrent IO waiting.
  This can help when there are lots of sockets, and mostly a lot of
  waiting.
  - Of course, the event loop will be blocked if you do any blocking
    operation. So don't do that!
- Can turn a generator into a coroutine using the
  `@asyncio.coroutine` decorator. Just like spawn in JS.

# `threading` and `multiprocessing`

- Python also has the `threading` module, but of course you have the
  GIL.
- `threading` is fairly low-level. Threading launches native threads
  that are scheduled 1:1.
- `multiprocessing` is a module which starts other processes for
  parallelism. You can use it to call other Python functions. Python
  gives you some kind of IPC like pipes, but it can feel janky.
- In the `concurrent` library I believe there's a threadpool mechanism.
  It creates futures for you that you can wait for. It's just making it
  easier to launch a thread to perform a set of tasks?
  - But be careful not to block a thread pool thread with IO!
  - If you have many more threads in the pool than cores, and blocking
    IO happens quite seldom, you may still have high CPU utilization
    since the other threads can keep working. But since IO is so much
    slower than CPU, doing proportion of IO work will probably mean
    _all_ the threads will always be doing IO work almost all of the
    time, resulting in very low CPU utilization.
  - You can have starvation/deadlock if threads in the pool block on
    futures that are also scheduled on the pool, and if there are not
    enough native threads backing the pool. Best for naively
    parallelizable tasks.
- So Python has all the main concurrency ideas: multiprocessing,
  threading, and asyncio. Asyncio is the newest, and I think plays the
  nicest with many long connections that mostly just wait around on IO.

# Other

- Python has `*args` and `**kwargs`.
  - Any argument can be passed by name.
  - You have to pass all positional arguments first, then specify
    some arguments by name.
  - You can use defaults.
- Debugging: `pdb`
  - You can call `pdb.set_trace()` wherever you like!
- You use pyenv to manage Python versions.
  - `pip` is the default for installation of libraries.
  - You can `pip freeze > requirements.txt` to dump something like a
    Gemfile. You can `pip install -r requirements.txt` to do a
    `bundle install`.
  - virtualenv is like gemsets from RVM. I think `bundle exec` is
    the cleaner solution, but I don't know that an analogue exists
    for Python.
- You can use `import imp; imp.reload("library_name")` to force
  reloading of that library. This is not transitive, however...
