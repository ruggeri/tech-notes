* https://www.playframework.com/documentation/2.5.x/ScalaHome
* https://github.com/playframework/play-websocket-scala#master
* https://www.lightbend.com/activator/template/reactive-maps
* https://www.lightbend.com/activator/template/play-slick
* http://semisafe.com/coding/2015/06/12/play_basics_database_access.html
* https://www.theguardian.com/info/developer-blog/2015/dec/30/how-to-add-a-form-to-a-play-application

## Notes

* Use `activator new` to maybe start a project seed.
* But then I just use `sbt` inside for now.
    * Seems to be a bug where you have to `sbt run` in the directory
      before Intellij can compile.
    * Not sure if that fixes it forever.
    * I think maybe you have to add the target/.../twirl stuff to the
      sources.
* Easy to import into Intellij.
* Hopefully with `sbt run` we can get auto-compilation, and see
  compile errors in the browser.
* `conf/routes` holds mapping of URLs to controller actions.
* `app/controllers` holds controller logic.
* It renders views from `app/views`. Templates will naturally get
  compiled to Scala class.
* There's a test framework, but I'll ignore that for now.
* `play.api.libs.json.Json.format` will convert from class to JSON.
* The request will be passed into the controller action implicitly.
* `play.api.data.Form` will define a form object. You define a mapping
  of parameters names to types.
* BTW, router exposes methods to generate urls.
* Inside Scala template you use `@xyz` to interpolate Scala code.
* Weird, it looks like you run evolutions (slick database migration)
  through command line?
* Clearly has an asset pipeline. Compiles CoffeScript to JS in the
  demo.

## Docs Notes

* Controllers return actions. `Ok`, `BadRequest`, and `Redirect` are
  most common.
* Routes can capture parts of the path or the query string. These can
  be passed into the controller action generator.
    * Route helpers: if your route handler is
      `controllers.Application.hello(name)`, then you'll use
      `routes.Application.hello(name)` to get the route back.
    * Kinda weird.
* With responses, you can use `#as(HTML)` or whatnot to change the
  content-type. You can use `#withHeaders` and give it a map of header
  values. You can use `#withCookies` to set cookies. You can likewise
  use `#withSession`. This is signed with a secret key. You can add
  data simply by using `Ok("Hello
  World").withSession(request.session + ("saidHello" -> "yes"))`.
* Likewise you can use `#flashing` to set a flash, and `#flash` to
  retrieve it.
* You can typically write things like `request.body.asJson`.
    * You can also mandidate that a request contain a body of a
      certain type. You do this by `Action(parse.json) { ... }`. That
      will require a JSON content-type.
* `ActionTransformer` does a transform from a request to a subclass of
  `WrappedRequest`. This is like a middleware which adds functionality.
    * `ActionFilter` can filter requests and return its own response
      (typically an error). (This has one type parameter)
    * `ActionRefiner` I guess does both!
    * `andThen` is used to chain these.
* `render` method takes a partial function, where `Accepts.Html() =>
  ...` only runs if the requested type matches.
* Asynchronous framework.
    * When an action returns a future, we need to say `Action.async`.
    * Can easily add timeouts and use `Future.firstCompletedOf`. Then
      you use a case statement to determine which occurred.
* Twirl is the templating language
    * `@(x: Int, y: Float)` at top is arguments.
    * You also say `@name` to interpolate
    * You may have to `@(a more complex statement)`
    * `@for (x <- xs) {` works normally.
    * Escaping is done for you in Twirl.
    * Layouts are not especially privileged. They're just views with
      blocks.
* JSON: `json.obj` and `json.arr` are very helpful for generating
  json.
    * To do automatic conversion, you an implicit object that
      implements `Writes[T]`.
    * You can also implement `Reads[T]`. You might
      `jsonObj.validate[T]`, which will gives you the converted value
      or the error.
    * Apparently there is a way to autogenerate for case classes:
      `Json.reads[MyClass]`. This is apparently a macro?
* For forms, you want
  `Form(mapping(...)(MyClass.apply)(MyClass.unapply))`.
    * This will translate from a form fields to an object and vice
      versa.
    * In the mapping, you specify the types. You can also specify all
      kinds of validations.
    * You bind by `form.bindFromRequest.get`. This reads in the passed
      value.
    * You can test operate differently based on errors by using
      `#fold`, specifying failure and success callbacks.
    * You should use the `@helper.form(action = ...) {` to start a
      form.
    * Inside, use `@help.inputText(userForm("name"))`.
    * The idea here is that the helper will also put the value in
      correctly.
    * You can always use `#fill(object)` to pre-fill a form with data.
    * Nesting is possible, as are repeated values.
* Sounds like a method for CSRF protection is provided, but you need
  to opt in.
