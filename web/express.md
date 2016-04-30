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

## More

* `express my-repo-name --ejs`
* `npm install --save sequelize sequelize-cli sqlite3`
* `npm-exec sequelize init`

```
{
  "development": {
    "dialect": "sqlite",
    "storage": "./db/development.db"
  }
}
```

* Leave structure as default. Add `db/.gitkeep` though.
* `npm-exec sequelize migration:create`

```
module.exports = {
  up: function (queryInterface, Sequelize) {
    return queryInterface.createTable(
      'users',
      {
        id: {
          type: Sequelize.INTEGER,
          primaryKey: true,
          autoIncrement: true
        },
        createdAt: {
          type: Sequelize.DATE
        },
        updatedAt: {
          type: Sequelize.DATE
        },
        username: {
          type: Sequelize.STRING,
          allowNull: false
        },
      }
    );
  },

  down: function (queryInterface, Sequelize) {
    return queryInterface.dropTable('users');
  }
};
```

```
modules.export = (sequelize, DataTypes) => {
  var User = sequelize.define("User", {
    username: DataTypes.STRING
  });

  return User;
};
```

* `npm-exec sequelize db:migrate`
* I'm using Node 6, btw.
    * A problem with sqlite3. May need:
    * https://www.bountysource.com/issues/33361413-deprecation-warnings-when-used-with-node-v6-0-0
* `npm install --save forever nodemon`
    * `npm-exec forever ./node_modules/nodemon/bin/nodemon.js ./bin/www`
* Use `app.use(bodyParser.urlencoded({ extended: true }));`
    * That lets you parse nested input names.
* Use `router.param('id', (req, res, next, id) => { ... })`
    * This is like a before filter, but you do your matching here.
    * Also params can be found in `req.params.id`.
* Validations:

```
// Model
module.exports = (sequelize, DataTypes) => {
  var User = sequelize.define("User", {
    username: {
      type: DataTypes.STRING,
      validate: {
        notEmpty: {
          msg: "Username must not be empty"
        }
      }
    }
  });

  return User;
};

// router
    .catch(err => {
      res.render('users/new', { errors: err.errors });

// View
  <% errors.forEach(err => { %>
  <%= err.message %>
  <% }); %>
```

* Uniqueness:

```
  function isUnique (value, next) {
    User.findOne({ where: { username: value } })
      .then(user => {
        if (!user) {
          return next();
        }

        next("Username must be unique");
      }).catch(next);
  }

  let User = sequelize.define("User", {
    username: {
      type: DataTypes.STRING,
      validate: {
        notEmpty: {
          msg: "Username must not be empty",
        },

        isUnique: isUnique
      }
    }
  });
```

* Make sure to return promises from Sequelize, and to
  chain. DB modifications are async.

```
{
  "development": {
    "host": "localhost",
    "port": 5432,
    "database": "my_facebook_dev",
    "dialect": "postgres"
  }
}
```

* `npm install --save bcrypt`

```
  function setHashHook (user, options, next) {
    let password = user.getDataValue('password');

    if (!password) {
      return next();
    }

    let saltRounds = 10;
    bcrypt.hash(password, saltRounds, (err, hash) => {
      if (err) {
        return next(err);
      }

      user.setDataValue('passwordDigest', hash);
      return next();
    });
  }

    passwordDigest: {
      type: DataTypes.STRING,

      defaultValue: '',
      validate: {
        notEmpty: {
          msg: "Password must not be empty"
        },
      },
    },

    password: {
      type: DataTypes.VIRTUAL,

      validate: {
        isLongEnough: (value) => {
          if (value.length > 7) {
            return;
          } else {
            throw "Password must be at least 8 characters long"
          }
        }
      }
    },

  User.beforeValidate(setHashHook);
```

* Fetching users:

```
    classMethods: {
      getUserByCredentials(username, password) {
        return User.findOne({ where: { username: username } })
          .then(user => {
            if (!user) {
              return null;
            } else {
              return user.isCorrectPassword(password);
            }
          });
      }
    },

    instanceMethods: {
      isCorrectPassword(password) {
        return new Promise((resolve, reject) => {
          bcrypt.compare(
            password,
            this.passwordDigest,
            (err, result) => {
              if (err) {
                reject(err);
              } else if (result === false) {
                resolve(null);
              } else {
                resolve(this);
              }
            }
          );
        });
      }
    }
```

* Storing session:

```
npm install --save cookie-session

+app.set('cookieSecret', process.env.COOKIE_SECRET || 'DEV_SECRET');
 app.use(cookieParser());
+app.use(cookieSession({
+  keys: [app.get('cookieSecret')]
+}));

@@ -12,8 +12,23 @@ router.post('/', (req, res, next) => {
     req.body.user.username,
     req.body.user.password
   ).then(user => {
+    req.session.userId = user.get('id');
+    res.redirect('/users');
     res.json(user);
   }).catch(next);

+router.currentUserFilter = (req, res, next) => {
+  if (!req.session.userId) {
+    res.locals.currentUser = null;
+    return next();
+  }
+
+  models.User.findById(req.session.userId)
+    .then(user => {
+      res.locals.currentUser = user;
+      return next();
+    }).catch(next);
+};
```
