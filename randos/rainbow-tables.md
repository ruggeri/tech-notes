So you want to crack passwords.

If you use even just a global salt, you'll stop precomputation
attacks; the attacker has to actually generate new hashes to attack
you. But as the number of your users grows, the probability of a hit
is increasing too. So it's better to have an individual salt; that way
the attacker needs to break one password at a time. (Also, as a minor
benefit, that way he can't see which users have the same password).

Of course, you also want an adaptible hashing algorithm like
bcrypt. You basically want to slow down the attacker's hash rate. This
is often done by iteratively hashing and rehashing the result. This
also allows you to dynamically make the hash harder at some later
date (since you just apply more hashing).

Bcrypt typically stores the salt and the number of rounds right in the
"digest".

Let's say you didn't use a salt. Now you're open to precomputation
attacks. For instance, someone can hash an entire dictionary worth of
common passwords.

A *rainbow table* is a refinement of that technique. The memory cost
of a dictionary table mapping passwords to salts is `O(kn)`, where `k`
is the number of passwords you've hashed, and `n` is the hash length.

To reduce this size, what you do is you create "hash
chains". Basically, you start with a possible password, you hash it,
then you map that hash back into password space, and then repeat. You
repeat this many times, ending with a final hash output. You record
the input password and final hash output.

To do a preimage lookup, you can't just lookup the password at the
start of a chain ending with the hash. The start of the chain *isn't*
the preimage. But if you get a hit, you can hope that the preimage is
the last passphrase in the chain! So you re-generate the chain from
the starting point.

What if your passphrase was generated in the middle of the chain? To
find this, when looking up a hash, you will repeat the process of
chaining, each time trying to find a match.

How to map hashes back to passwords? You don't want to take the
literal hash; that would be a brute force search. So your projection
function should try to map the hash onto a dictionary; maybe you use
it as an index to a dictionary file...

This is a time-space tradeoff.

That's just *password hash chains*. The main problem is that chains
can *merge*; if you hit on a hash which any other chain hits, then
your chains are identical from that point.

The solution is to use a different reduction function at each step of
the chain. So for two chains to merge, they need to hit at the same
hash *at the same position*.

This means your rainbow table wastes even less space. It is, however,
slower to do the lookup.
