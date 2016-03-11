## With Test-And-Set

With shared memory and test-and-set, it's easy to implement a
mutex. You can busy wait, checking a flag to see if the process is
free. Then you do a CAS to try to set the flag to true; if you
succeed, you get the mutex.

To ensure fairness, you can keep a queue of requests on the lock.

Presumably there would be a way to tell the kernel you didn't get the
lock, so that you don't have to busy wait.

## Dekker's Algorithm

Mutex for two processes:

```
wants_to_enter[ME] = false
wants_to_enter[THEM] = false
turn = 0

def acquire():
  wants_to_enter[ME] = true
  while wants_to_enter[THEM] {
    if turn != ME {
      wants_to_enter[ME] = false;
      while turn != ME {
        // Busy wait.
      }
      wants_to_enter[ME] = true;
    }
  }

  // CRITICAL SECTION

  turn = THEM
  wants_to_enter[ME] = false
```

The idea is that if you want to enter, and they don't want to, you get
to go even if its not your turn. Not both of you can barge this way,
since (at least) one of you should see the other wants to enter, and
that would make them check for who's turn it is.

If both of you want to enter simultaneously, it will be someone's
turn, they will enter.

If someone is waiting for the other to finish, eventually, that person
will give up their turn to you (when they're done with the CS).

There is a constant bound on the time for *someone* to acquire the
mutex; it doesn't impose arbitrary delay. There is fairness; a process
can't get starved. No special hardware ops are necessary, though
memory barriers are required (of course). This technique only really
works for two processes, though (since we need to "hand-off" the turn
to the "other" process).

## Peterson's Algorithm

```
wants_to_enter = { false, false }
turn = 0

def acquire():
  wants_to_enter[ME] = true;
  turn = THEM
  while (wants_to_enter[THEM] && turn == THEM) {
    // BUSY WAIT
  }

  wants_to_enter[ME] = false;
```

This is definitely simpler than Dekker. In Dekker, you have a notion
of turn taking; if there's no one around, you can use the lock when
its not your turn, but if someone else is waiting, you have to wait.

I see this as the "No, after you!" algorithm. Except, even when no one
is there, you still say "No, after you!". You make clear you want to
enter, than defer to your opponent. The last one to say "after you"
wins.

But Peterson is still starvation-free. That's because later arrivals
to grabbing the lock defer to those that got it first. And it still
appears to have bounded waiting.

## Peterson's Algorithm, N Processes

```
levels = { 0 ... 0 }
last_to_enter = { -1 ... -1 }
def acquire():
  for level in [0..(N-1)]:
    levels[my_proc_id] = level
    last_to_enter[level] = my_proc_id
  loop_start:
    while (last_to_enter[level] == proc_id)
      for proc_id in PROC_IDS - [my_proc_id]:
        if levels[proc_id] >= level:
          wait
          break loop_start
      // LOCK ACQUIRED
      return

def release():
  levels[my_proc_id] = -1
```

The idea is that there are `N-1` "waiting rooms". To advance, you have
to be at the head of your class; there can't be anyone in this waiting
room or any future waiting room. But of course, many people might
enter a waiting room successfully. We need to allow them to
proceed. For that reason, we say that everyone but the last person can
proceed; that reduces the number of people advancing by one each
room. So the last room can only have one person.

Why not choose the first person gets to proceed? Well, there is only
one "first person" to enter the room ever. If what you mean is "first
person currently in the room", that is complicated information to
track without a queue...

Here we have no starvation again. I read that waiting is not bounded,
however.

## Lamport's Bakery Algorithm

```
entering = { false...false }
ticket = { 0...0 }
def acquire():
  entering[pid] = true;
  // NB: Some people will get same ticket number!
  ticket[pid] = max(ticket) + 1;
  entering[pid] = false;

  for other_pid in OTHER_PIDS:
    while entering[other_pid]: wait
    while (ticket[other_pid] > ticket[pid] ||
           (ticket[other_pid] == ticket[pid] && other_pid > pid)): wait

  // ENTER CRITICAL SECTION
```

Again, you can't have starvation like this.

## TODO

* Not clear how the busy-waiting of these algorithms is avoided. It
  definitely seems they don't always have to constantly busy wait and
  can be woken up only when appropriate.
* But I think that the CAS wait queue seems the most important; the
  kernel could clearly take the head off the queue when the mutext is
  released.
