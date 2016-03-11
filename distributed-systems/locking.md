If locks have an expiration timer, then they might expire without the
clien knowing. This can happen, for instance, if the client goes to
sleep because of a GC pause, because of paging in the VM, network
packets being delayed, SIGSTOP.

The client can try to check that they have the lock before doing the
action, but that is meaningless, because any of these interruptions
can happen at any time.

The answer is for the service being locked to check a token number
issued by the lock service. The idea is that the lock service just
increments this lock number, and the service to which locking applies
should reject lower valued transaction numbers. So if your lock times
out and someone else performs an operation, then your write can be
rejected.
