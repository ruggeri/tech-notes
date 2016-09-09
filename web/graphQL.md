## GraphQL

* GraphQL is an alternative to REST.
    * Allows the client to define queries.
    * Allows you to specify fields you want.
    * Allows you to have nested queries.
    * You can pass hashes that are parameters for the server.

```
{
  user(id: 3500401) {
    id,
    name,
    isViewerFriend,
    profilePicture(size: 50)  {
      uri,
      width,
      height
    }
  }
}
```

Okay, on the server-side, you specify a "schema", which defines what
queries are available. So there would be a `"user"` query, which would
specify that you need a string `id` argument, and then you write a
`resolve` method, which is what would need to actually do the SQL
query. The library that lets you define these queries is
`graphql-js`. `graphql-js` also specifies a way to strongly type the
results of queries.

A library called `express-graphql` will take your schema and return a
handler so you can easily setup a GraphQL endpoint.

So you define these types via `new GraphQLObjectType()`. You specify
fields. Some of these fields will have `resolve` methods (presumably
if a join needs to be performed to populate that
information). Presumably you can even require arguments for some of
these resolved fields!

At the end, you use `new GraphQLObjectType` to define a "root
query". Here every field has a resolve method, so that the query can
be performed.

Here's an example:

    https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

The code that gives actually does the fetching lives here:

    https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsData.js

Note that GraphQL lets you do neat things like:

```
{
  luke: human(id: 1000) {
    name
    homePlanet
  }
  leia: human(id: 9999) {
    name
    homePlanet
  }
}
```

The GraphQL handler will fetch both objects.

To avoid the repetition of `name\n homePlanet`, we can factor this out
into a *fragment*:

```
{
  luke: human(id: 1000) {
    ...HumanFragment
  }
  leia: human(id: 9999) {
    ...HumanFragment
  }
}

fragment HumanFragment on Human {
  name
  homePlanet
}
```

Note that the app developer needs to write the methods to do the
resolve, but the work of translating a request to a set of queries is
done by the `graphql-js` library.

`graphql-js` will also enforce query validity.

You can also define mutations. They work basically the same as
queries. Again, it works just via the `resolve` method. You write a
mutation like so:

```
mutation {
  likeStory(storyID: 12345) {
    story {
      likeCount
    }
  }
}
```

When you define the query, you can give a GraphQLObjectType for the
query namespace, and another for the mutation space.

Queries can have variables, and the client submits the query and the
variable values. This is like a prepared statements in SQL. The idea
is to avoid unnecessary string-building on the client, I guess.

* They have some basic libraries to support Express.
* They have a type system.
* There also seems to be a Rails library.
* It does seem to make querying with parameters, nesting, more
  formalized and less adhoc.
* Queries can perform mutations, but you just give the mutation a
  name; the language doesn't specify what a mutation does.
    * That's kinda annoying.
    * But I guess you can just follow some obvious conventions.

## Relay

You create a React Component, but then you wrap it with
`Relay.createContainer`. Normally you specify a `fragment`, which is
the properties of the given object you need.

A parent component/container will specify another fragment, embedding
nested fragments as needed by nested components. Instead of using
`state`, you use `this.props.relay.setVariables`. You can use
`$variableName` in your fragment to interpolate these variable
settings. As you change variables in the component, the query should
be re-run on the server.

Relay has a `Relay.Route` class where you specify a top-level GraphQL
query. It has nothing to do with URL routing. It just is the top-level
relay container that actually does the querying. Well, sort of...

The `graphql-relay-js` library has a `mutationWithClientMutationId`
method. Here you specify how to perform the mutation
(`mutateAndGetPayload`), followed by what data to return
(`outputFields`). That's the server side, I think.

On the client side, you run `Relay.Store.update(...)`, passing a
`Relay.Mutation` subclass. Here you specify the mutation through the
`getMutation` method (this is just the mutation name), the arguments
via `getVariables` (returns a JS object which is the args).

It looks like Relay is going to maintain a cache of all requested
objects. It stores each object one time with a unique ID. It is doing
normalization here.

Relay keeps track of what components reference what records, and will
re-render these when the underlying data changes.

Mutations contain information about what the mutation may
change. Therefore Relay can know what information it should refetch.

`Relay.Renderer` is a component that will perform a query, and when
the data is received, will render the contents. Relay will look in the
cache first, and then think about what additional information is
needed from the server.

The `Relay.Route` class describes how to actually do the query. Then
we put it and the container displaying that data in a
`Relay.RootContainer`. It is here that the root container can decide
whether to force a refresh from the server.

TODO:

* https://facebook.github.io/relay/docs/guides-mutations.html#content
* https://facebook.github.io/relay/docs/guides-network-layer.html#content

Okay, I got tired and stopped reading for a while. But the idea is
that Relay is going to manage your store for you.

* Relay allows you to declare data dependencies for your UI.
* It will translate these to GraphQL and query the server.
* Basically, Relay is moving you toward more declarative programming,
  and less imperative.
