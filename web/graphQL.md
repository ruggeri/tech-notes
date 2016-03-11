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

* Relay allows you to declare data dependencies for your UI.
* It will translate these to GraphQL and query the server.
* Basically, Relay is moving you toward more declarative programming,
  and less imperative.
