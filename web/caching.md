* Asset Caching
    * Use fingerprints, live forever.
* Page Caching
    * Serves a page, but tells user not to re-request again for a while.
* Action Caching
    * Has Rails cache so it doesn't have to re-render.
    * Requires sweepers or some way to bust the cachye.
* Fragment Caching
    * For a dynamic page that consists of parts that can be cached.
    * Rails tries to be convenient: you can use a model or relation as
      a cache key; that way whenever the model (or any model in a
      subset) is updated, that fragment will be busted and regenerated
      next time.
    * Note that, for the most part, this *does not* avoid queries.
    * But it can. E.g., you can do a query to find the max updated at
      in a set, and then only fetch that set if it changed.
* Query Caching
    * Can extract from SQL and store in Redis/memcached or somesuch
    * Typically you set the expiry time.
    * Really only helps if you are doing *query logic*. Storing an
      entire table won't help, since data transfer dominates.
* Conditional GET
    * User can supply `If-None-Match`, uploading the previously given
      Etag. Can just issue a 304 Not Modified if still the same.
    * Rails typically uses the hash of the rendered page as the
      etag. That means it typically generates the whole fucking
      thing. But if you use the `stale?(@model)`, it will compare the
      etag to the hash of the model's `cache_key`. *And* it will set
      the etag appropriately for next time!
    * Also could use `If-Modified-Since`.
    * There isn't a clear advantage to either
      mode. `If-Modified-Since` makes a lot of sense for files
      (`mtime`) or whether all the necessary models have an
      `updated_at` column.
    * OTOH, models may have been updated but the generated page is the
      same, in which case you can still return a 304 based on an etag
      hash of the page.
