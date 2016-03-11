# JSON vs HSTORE vs JSONB

Basically, JSON stores a string, but let's you use operators to
query. It is not indexable, I think.

HSTORE is faster, because it's kv-pairs. However, it just stores
strings as values and can't do nested data.

JSONB is like a cross between HSTORE and JSON. It can do nesting,
arrays, etc.

Note: with JSON/JSONB, the `field->'key'` returns a JSON/JSONB, while
`field->'key'` returns a TEXT. So you must do explicit casts, which is
annoying.

# Lateral Joins

We know about correlated subqueries. But a joined table producing
expression can't reference other tables. For instance, consider the
failures of this query which tries to find the oldest owner of a cat.

```
-- Can't use MAX to find the human that maximizes a quantity...
SELECT cats.*, humans.*
FROM cats
JOIN humans ON cats.id = humans.cat_id
-- WHERE won't help; only filters one human at a time
GROUP BY cats.id
-- HAVING won't help; decides whether to keep the cat.
```

One solution is:

```
SELECT cats.*, humans.*
FROM cats
JOIN humans ON cats.id = humans.cat_id
JOIN (SELECT cat_id, MAX(age) AS age
      FROM humans
      GROUP BY humans.cat_id)
     AS cat_group_info ON cats.id = cat_group_info.cat_id
WHERE humans.age >= cat_group_info.age
GROUP BY cats.id
```

This does a subquery to find the max age for each cat. We then join
the humans and then throw out those under the max age. This actually
isn't 100% correct, because I think selecting humans like this is
going to give an error. We should probably returning a single human
id; otherwise this query returns an arbitrary human of the max age.

There are fancier ways to do it with more sophisticated
`EXISTS`/`ANY`/`IN` magic:

```
SELECT cats.*, humans.*
FROM cats
JOIN humans ON cats.id = humans.cat_id
WHERE NOT EXISTS (SELECT 1
                  FROM humans AS humans2
                  WHERE humans2.cat_id = cats.id AND humans1.age < humans2.age)
```

This way is a correlated subquery and may not be well
understood.

Lateral joins allows us to write a join that references previously
joined tables:

```
SELECT cats.*, humans.*
FROM cats
JOIN LATERAL (SELECT *
              FROM humans
              WHERE humans.cat_id = cats.id
              ORDER BY humans.age DESC
              LIMIT 1) ON true
```

This is desirable, because it can reference cats like a where could,
but while where filters rows one-at-a-time, the lateral join lets us
pick a subset (or, in this case, one) row to join against cats **based
on aggregate information**.

Lateral joins also allow "expanding" rows:

```
SELECT *
FROM cats
JOIN LATERAL json_array_elements(cats.json_favorite_ice_creams) on true
```

# CTEs

???

# Array Columns

???

# ENUMs

???
