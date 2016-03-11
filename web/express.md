* npm-exec trick (prefix `node_modules/.bin` to PATH).
    * forever and nodemon for reloading.
* Express, EJS
    * Use EJS for views (in `views` directory).
        * Express looks for a layout file.
        * TODO: not sure how to do partials. Or helpers.
    * Middleware:
        * `body-parser`. `method-override` for non-POST requests.
        * `csurf`.
            * TODO: set this up!
        * `cookie-session`. Can sign cookies.
            * TODO: how to set keys properly?
* Sequelize:
    * Use an `index.js` in `./models`. Exports all models.
    * Each model file exports a function taking `(sequelize,
      DataTypes, models)`.
        * The last argument helps for non-association code that wants
          to reference other model types.
    * You typically have an `associate` class method to hook up
      associations after all models defined.
        * TODO: I need more practice
    * In other code you load the `./models` `index.js` file. To get
      all the models.
    * `sequelize-cli` does migrations okay!
    * Use a `config/config.json` to setup DB. I guess `sequelize-cli`
      expects this name? Postgres gave me no trouble.
    * Validations (including async!) seem fine.
        * I believe this is required for uniqueness validations.
* Routes:
    * Put them in the `routes` directory. Like controllers.
* Auth
    * Can have virtual attributes in Sequelize. Can put a validation
      on `password` attribute to verify length.
    * Can add a `beforeValidate` hook to use bcrypt to salt/hash the
      password.
    * Can add a `findByCredentials` class method, and a
      `verifyPassword` instance method.
    * I wrote a middleware to find and get the user.
* Async style:
    * I try to return promises as much as possible.
    * I use `Promise.coroutine` from Bluebird a lot. This lets me
      write more synchronous-looking code.
    * I'm not 100% there in terms of not having to think about
      async...
    * I wrote a little helper to convert a coroutine to a route
      handler. Used this a lot (especially in routes).
* Forms:
    * The old-style way. Write the HTML form by hand.
    * TODO: an easier way to display validation errors?
    * TODO: Flash?
    * TODO: mass-assignment problems?
* TODO:
    * Asset pipeline? Webpack.
    * Caching?
    * Emails
    * How to use websockets too.
    * How to share route handlers for JSON/HTML.
