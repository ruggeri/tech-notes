# SOAP (Simple Object Access Protocol)

- For client-server RPC. Kinda meaningless name...
- Developed around 1998-2000.
- XML based.
- Standardized wire format, request/response body, errors. Headers.
- WSDL: Web Services Description Language. Basic idea is that this XML
  document describes the RPC interface: mainly the operations and the
  request/response formats.
- Tools existed to do codegen from a WSDL file to scaffold
  serialization/deserialization code.
- For the client, you might be able to codegen a class where you could
  simply call a method as if it were local.
- Of course, client and server could be in totally different languages.
  Which was good.
- Over time, a larger set of specifications collectively called `WS-*`
  were developed. But this started to get very complicated. So people
  started to turn on it.

# JSON-RPC

- Idea was just to be a lot simpler than SOAP.
  - Kind of went back to XML-RPC, a simpler precursor idea to SOAP (but
    still based on XML).
- Became popular mid-2000s.
- Payloads might be:

```json
{"jsonrpc":"2.0","method":"sum","params":[3,5],"id":1}
{"jsonrpc":"2.0","result":8,"id":1}
```

- Very simple.
- Identifiers allow client to send multiple requests in parallel.
- JSON-RPC doesn't specify interface or schema.
- Server could just install a method handler for a given name. But then
  it needs to validate params itself.
- Worth noting: JSON-RPC can be easily performed by JavaScript code.

# REST (Representational State Transfer)

- Introduced by Roy Fielding in 2000.
- Kind of an alternative to general RPC idea.
- Centered around "resources", which have a URI (uniform resource
  identifier). You have resources which are collections `/customers` and
  resources which are individuals `/customers/123`.
- It maps HTTP methods to REST verbs with a standard meaning.
  - GET retrieves, POST creates, PUT/PATCH update, DELETE destroys.
- I don't want to talk about HATEOAS, which is part of Fielding's idea
  but not typical of REST implementations.
- REST typically relies on HTTP for transport and on HTTP status codes,
  content-negotiation, caching, authentication...
- REST works well if you have a lot of resource-like functionality.
- While REST has conventions about the shape of messages and what they
  are meant to represent, the types and the API interface is not
  specified in the same way that SOAP does.
- Complex, more purely RPC-like operations may map poorly onto REST, at
  which point REST isn't buying you anything. How resource-like is your
  API?
- Format of messages not specified. But often JSON. XML can be used. And
  you can also use `application/x-www-form-urlencoded` which was sort of
  HTTP's traditional encoding for forms.

# gRPC

- Google announces/releases 2015/2016.
- Evolved from internal "Stubby".
- Uses protocol buffers as both wire format and interface definition
  language.
  - Protobufs are binarized and can be more space efficient.
- HTTP/2 is the transport method.
  - Allows multiplexing of concurrent requests.
- Codegen for client and server.
- Standardizes "deadlines", cancellation, and error statuses.
- Has multiple RPC kinds:
  - Unary: one request, one response.
  - Server-streaming: returns a stream of responses to a request.
  - Client-streaming: client streams requests, receives a response.
    - Example is uploading a series of chunks of data, getting a final
      response.
  - Bidirectional-streaming: a stream of chat messages and chat
    responses.
- Web browsers don't expose HTTP/2 through APIs. So there is "gRPC-Web"
  which is a limited version that can run in the browser.
- gRPC is in many ways a modernized version of SOAP.
  - A lot of its features (deadlines, cancellation, standardized errors,
    streaming modes) could be built into any RPC framework, but gRPC
    adds convenience by just doing it.
  - SOAP did have a lot of adjacent standards in its family that add
    functionality that gRPC might provide. But gRPC maybe is more
    opinionated, and bundles more built-in functionality closer to the
    core standard.

# GraphQL

- Released by Facebook approximately 2015.
- Idea is for querying nested/interconnected data.
  - Different from general RPC which is kind of agnostic about whether
    there are underlying objects with interrelationships.
  - But doesn't really leverage HTTP's verbs and URIs like REST does.
- Queries look like:

```graphql
query {
  user(id: "123") {
    id
    name
    friends {
      id
      name
    }
  }
}
```

- Responses look like:

```
{
  "data": {
    "user": {
      "id": "123",
      "name": "Alice",
      "friends": [
        {"id": "456", "name": "Bob"},
        {"id": "789", "name": "Carol"}
      ]
    }
  }
}
```

- Typical transport is HTTP and JSON, but not tied to that.
  - Clients often build queries through string interpolation. Kind of
    like raw SQL construction without an ORM. They then just pass the
    query to browser `fetch` method.
  - Client normally deserializes objects as POJOs, not to a particular
    "class" on which you can define your own methods.
- I think typically there is *one endpoint* (`/graphql`), but client
  query specifies fields and relations to fetch.
  - Client can drop fields it doesn't need. But it can include relations
    that it will need, avoiding subsequent queries.
- A schema defines the types and the queries available:

```graphql
type User {
  id: ID!
  name: String!
  friends: [User!]!
}

type Query {
  user(id: ID!): User
  users(ids: [ID!]!): [User!]!
}
```

- Schema is used by server to validate requests. Client normally trusts
  server responses without validation.
- GraphQL doesn't specify how responses are built. That is done by a
  "resolver function". For instance:

```javascript
const resolvers = {
  Query: {
    user: (_, {id}, context) =>
      context.db.getUser(id)
  },

  User: {
    friends: (user, _, context) =>
      context.db.getFriends(user.id)
  }
};
```

- A GraphQL implementation will normally have a function `createSchema({
  typeDefs, resolvers })`. Then you will mount this schema at an
  endpoint like `/graphQL`. The library will create an appropriate
  Express handler (for instance) that validates the query and starts
  running the resolvers to produce the response.
- In this example, a query could look like `User(id: 123) { name,
  friends { id, name } }`. The GraphQL server side library uses schema
  to check request. It knows that `Query::user` returns a `User`, so it
  knows that the `friends` field in the request will need to be resolved
  by `User::friends`.
- Resolvers are typically (always?) async.
- GraphQL is designed to work nicely with DataLoader, which is a way in
  which N+1 queries are avoided. For instance:

```graphql
query {
  users(ids: ["1", "2", "3"]) {
    name
    friends {
      name
    }
  }
}
```

- This can avoid an N+1 query on `friends` relation if we use the
  DataLoader library (also from Facebook).
  - Basically, instead of firing multiple independent async requests to
    `db.getFriends(user.id)`, `friendsLoader.load(user.id)` will build a
    buffer of user ids, all of whom to fetch friends for.
  - We basically just switch the resolver logic to use the loader, which
    itself will wrap `db.getFriends(user_ids)`.
  - GraphQL will `Promise.all` await the promises returned by
    `friendsLoader`. It never waits on individual promises while they
    are constructed.
  - The result is that `friendsLoader` has "time" to build a buffer of
    `user.id` during GraphQL query processing, which allows the request
    for all the friends to be batched.
- Basically, you have the benefit of ActiveRecord `includes`. But the
  GraphQL resolver tree is maybe simpler, and clients can easily select
  whether they want to fetch `friends` without the server endpoint
  having to write code to decide whether to `includes` or not.
- GraphQL exposes `Query` and `Mutation` top level types. And even
  subscriptions which are intended to be used with WebSockets for the
  server to publish a series of messages.
- How do clients generate queries?
  - They can write the query as a template fragment and interpolate.
    That's kind of the SQL-like approach. The client calls `fetch`
    themselves and gets back a POJO. That's basically no framework on
    client.
  - There are some compiler/codegen tools that will turn GraphQL
    interface description into functions, and can even annotate those
    with types.
  - Other libraries like Relay allow React components to declare the
    data they require. They do this using "fragments" which I won't talk
    about here.
- What I would say is that GraphQL really is more like SQL than an ORM.
  - Client builds the query mostly through string interpolation.
  - Results come back as plain old POJOs. You can't generate new
    queries/follow relations by calling methods on the results.
- GraphQL doesn't play well with HTTP caching.
- Also, GraphQL can allow client to make arbitrarily complex queries.
- Basically, don't expect to be able to build arbitrary queries on the
  client. The typical approach is a static GraphQL query, with variable
  interpolation.
- Apollo is another library that will do the interpolation and fetching
  for you. But it will also normalize a cache (so queries for same
  resource use same object: one POJO backing two different requests for
  a friend). And Apollo manages reactive updates to server side push.

# TODO

- HTTP/JSON + OpenAPI: what is that??
- Browser-Based HTTP alternatives
  - WebSockets
  - WebRTC (older) and WebTransport (newer)
- Authorization
  - OAuth
  - JWT (JSON Web Token)
