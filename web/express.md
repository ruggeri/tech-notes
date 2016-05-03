**Initial Setup:**

* `alias npm-exec='PATH=$(npm bin):$PATH'`
* `express my-repo-name --ejs`

**Server Setup:**

* `npm install --save forever nodemon`

```
// package.json
{
  "scripts": {
    "start": "forever -c nodemon ./bin/www"
  },
}
```

**Sequelize Setup:**

* `npm install --save sequelize`
* `npm install --save-dev sequelize-cli`
* `npm-exec sequelize init`

```
// config/config.json
{
  "development": {
    "host": "localhost",
    "port": 5432,
    "database": "my_facebook_dev",
    "dialect": "postgres"
  }
}
```

* `npm-exec sequelize migration:create`

```
// In the migration file. Notice that I am careful
// to return the result of queryInterface#createTable,
// which is a promise.
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

* `npm-exec sequelize db:migrate`

```
// models/user.js
modules.export = (sequelize, DataTypes) => {
  var User = sequelize.define('User', {
    username: DataTypes.STRING
  }, {
    tableName: 'users'
  });

  return User;
};
```

**Model Validations:**

```
// models/user.js
// A simple validation.
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
```

```
// A more complex, asynchronous validation.
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

**Displaying Validation Errors:**

```
// router
    .catch(err => {
      if (!(err instanceof sequelize.ValidationError)) {
        return next(err);
      } else {
        res.render('users/new', { errors: err.errors });
      }
    });

// View
  <% errors.forEach(err => { %>
    <%= err.message %>
  <% }); %>
```

**Hashing Passwords:**

* `npm install --save bcrypt`
* Add a `passwordDigest` column.

```
// Validate a password digest is present:
    passwordDigest: {
      type: DataTypes.STRING,

      defaultValue: '',
      validate: {
        notEmpty: {
          msg: "Password must not be empty"
        },
      },
    },

// Virtual; attribute for verification of a password.
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

// Add a hook to hash the password. Notice the hook is async.
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

  User.beforeValidate(setHashHook);
```

**Fetch Users by Credentials:**

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

**Logging in a User:**

* Use `app.use(bodyParser.urlencoded({ extended: true }));`
    * That lets you parse nested input names like `user[username]` and
      `user[password]`.
* `npm install --save cookie-session`

```
// app.js
+app.set('cookieSecret', process.env.COOKIE_SECRET || 'DEV_SECRET');
 app.use(cookieParser());
+app.use(cookieSession({
+  keys: [app.get('cookieSecret')]
+}));
```

```
// Changes to session router to log someone in.
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

**Use Babel to Enable Async/Await:**

* `npm install --save-dev babel-core`
* `npm install --save-dev babel-cli`
* `npm install --save-dev babel-plugin-transform-async-to-generator`

```
// .babelrc
{ "presets": "es2015",
  "plugins": ["transform-async-to-generator"] }
```

* `package.json`: `"start": "forever -c nodemon bin/www --exec babel-node"`

```
// lib/wrap.js
export default function (promiseFn) {
  return (...args) => {
    let next = args[2];
    promiseFn(...args).catch(next);
  };
}
```

* `npm install es6-promisify`
    * This helps you convert async functions requiring callbacks to
      async functions returning promises.
* With `async`/`await` and `es6-promisify`, you can eliminate all
  `then` chaining and callbacks. Great simplification!
    * Sequelize is actually really promise friendly!
* TODO: can't use `babel-node` in production...

**Structuring For Clientside Code:**

* Make a `server/` directory and move most stuff into it:
    * `app.js bin config lib migrations models public routes views`
* Make a `client/` directory with `images`, `javascript/`,
  `stylesheets/`,.
* In `client/javascript`, further make `src/` and `vendor/` folders.
* `npm install --save-dev bower`.

```
// .bowerrc
{
  "directory": "client/javascript/vendor"
}
```

* `echo client/javascript/vendor >> .gitignore`
* Can now `bower install jquery` and the like.
* Move existing `./server/public/*` to `./client/`.

**Webpack**

* `npm install --save-dev less`
    * We'll use less for CSS language.
* `npm install --save-dev webpack babel-loader css-loader less-loader style-loader`

```
// webpack.config.js
// I'm going to name my JS files es6. This helps webpack avoid running
// vendor files through babel.
// I'm going to output to 'server/public/dist'

module.exports = {
  entry: ['./client/javascript/src/app.es6'],
  output: {
    path: 'server/public/dist',
    filename: '[name].bundle.js'
  },

  module: {
    loaders: [
      { test: /\.es6$/,
        loader: 'babel',
        query: {
          presets: ['es2015'],
        }
      },

      { test: /\.less$/,
        loader: 'style!css!less' }
    ],
  },
};
```

* `echo client/javascript/vendor >> .gitignore`
* Add a `require('../../stylesheets/style.less');` in your es6 file to
  pull in styles.
    * TODO: we need to figure out later how to avoid inlining all the
      styles.
    * TODO: Will need to figure out how to fingerprint.
* Will need to include `<script src='/dist/main.bundle.js'></script>`.

* Run: `./node_modules/.bin/webpack --watch`

## TODO:

* Next steps:
    * Start building the application!
    * Associations!
    * Frontend!
* Very Important:
    * Deployment; turn off babel-node.
* Medium Important:
    * Fingerprinting and long-lived assets.
        * I think `asset-rack` might be part of the solution?
    * `method-override` for non-GET, non-POST requests.
        * If requests made by `jQuery.ajax`, I think it can issue the
          proper requests.
* Not Important:
    * Layouts? Partials? Helpers?
        * Prolly less necessary given Angular/React.
    * Mass assignment
    * `csurf` middleware?
    * Flash
    * Emails
