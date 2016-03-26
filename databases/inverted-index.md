* So you have words that point to list of document ids.
    * Typically sorted ascending. Allows good compression by
      difference encoding.
    * Typically just do an intersection.
    * For a conjunctive query, you might start with the least common
      term.
    * This might actually be fast if you combined unrolled linked list
      with a skip list.
    * In fact, I think Postgres GIN index uses a "postings tree",
      which is just a b-tree. This probably improves intersection.
* Typically we want to ignore stop words and do stemming.
* To rank, you may take the greatest number of matches.
    * A common measure is *tf-idf*: *term-frequency inverse-document
      frequency*.
    * This is basically: how often does the phrase occur in the
      document versus how often does it appear *overall*.
    * There are some ways to tweak this. It's often done logscale, and
      we often control for document size.
* You can do `+word1 +word2` by intersecting results in memory.
* For phrase search, I think it's typical to record `word: (doc id,
  position)`.
    * Then you search all the words and intersect the results in
      memory, but making sure words appear in the correct order.
    * I think having an index with bigrams isn't (that) commonly used,
      even though it sounds like that could quickly decrease documents
      under consideration. I do see some indication this is done.
    * For instance, if someone search `"the who"`, should I get all
      documents with `"the"` and `"who"` in it?
    * I guess a bigram index is a lot bigger, but not `n**2` bigger
      likely...
    * For multiword phrases, they often create "shingles", which are
      series of overlapping ngrams. You can intserect these.
    * It sounds like bigram actually *is* a common proposal. As an
      optimization sometimes they just index commonly used phrases.
* Fuzzy Matching
    * Typical unit is Levenshtein distance (maybe augmented with
      transpositions).
    * The naive approach is that when you insert `abcd`, insert
      `_bcd`, `a_cd`, etc. That's all the distance one items.
    * That only gives you an edit distance of one, though.
    * I guess the number of entries would be proportional to the
      square of the length of the word even with two entries.
    * So that'd be storage size, and lookups!
    * Doesn't sound practical!
* Ngrams
    * You split into bigrams or trigrams, you ask whether a certain
      threshold of ngrams match. E.g., 80%.
    * This gives you some locality, and also some specificity (a
      trigram will occur in relatively few words).
    * The typical approach is use the trigrams to narrow down to some
      candidates, then compute edit distance on each.
    * This sounds like a pretty common approach.
* Whatever you do, you'd only have to build it for the words.
    * That shouldn't be *too* much space. You're not talking ngrams to
      documents.
    * Then you use this to search the index for documents.
    * Note that this can be used for spelling correction, also!
* Github Autocomplete: Usernames
    * I think this generates/searches ngrams to find possible
      candidates.
    * I think it orders by longest common substring before it starts
      looking at string-edit distance.
        * And maybe the *number* of these ngram matches!
    * That should bias it toward early characters being very
      important, since the `^` can match (several times for ngrams!
    * It definitely looks for partial matches inside the word.
    * It doesn't seem terribly biased by what letters come at the
      beginning.
    * If I randomly shuffle `w1zeman1p`, does it give me that as a
      suggestion?
        * No! So while bigrams are important, maybe it's not
          looking up unigrams.
        * Which could make sense, since then every individual letter
          would recall almost all words.
    * OTOH, if I type `kvg`, I get `PinkKitten: Eline van der Gaast`.
        * It doesn't feel like there are any bigrams here!
        * Maybe it's a special case for when there are three
          letters. It could try to find matches for this set of
          unigrams.
        * I don't know... There are so many subsets!
* Google Autocomplete: Phrases!
    * Github does a lot of work trying to match inside the word. Your
      query is essentially a single word.
    * Google does a lot more assuming that you want to *complete* your
      query.
    * That makes Google much more likely to use a trie/DAWG.
    * But of course your prefix can be mispelled!
    * I think that Google probably does spell correction, but *on the
      prefix*. It does that in the usual ngram, string-edit distance
      way.
    * Then it completes it with the trie.

Source: http://nlp.stanford.edu/IR-book/html/htmledition/irbook.html
Source: http://books.ithunder.org/Inverted.pdf (Inverted files for text search engines, Zobel, Moffat)
Source: http://www.milkdom.com/uni/Sem05/a4md/docs/Managing%20Gigabytes%3B%20Witten,%20Noffat,%20Bell.pdf
Source: http://www.postgresql.org/docs/9.3/static/gin-implementation.html
