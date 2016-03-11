## Deadlock

* We'll have to abort and restart one. We could try a timeout.
* Can do cycle detection in a waits-for graph.
* We could try to request locks in the same order, but that is not
  really possible to know up front, right?
* Could also do timestamp detection; not related to MVCC.
    * You start with a timestamp, as you acquire locks, you leave your
      timestamp.
    * If you see a lock from someone with an older timestamp, you die
      and restart.
    * If you see someone with a younger timestamp, you wait.
    * You may abort some transactions prematurely, but you don't have
      to run any global cycle detection.
* An alternative is *wound-wait*:
    * If you see a transaction that you'd normally wait for, you only
      wait a short time, and if you can't still get the lock, you kill
      that transaction.
    * If you see a transaction you'd normally die, you'd just wait.
    * This leads to probably less senseless aborting on a lock that
      won't be released soon.
* Wound-wait seems quite practical.
* They talk about how to run a system where transactions can be days
  long. They talk about "compensating transactions", which sound like
  they basically invert those actions taken thus far.
    * I think a good example is banks, where you can start a transfer,
      but if the account is later found to not exist, you can return
      the funds.
    * I don't know, their coverage is pretty weak here and I'm not
      that interested...
