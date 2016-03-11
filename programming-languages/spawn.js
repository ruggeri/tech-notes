// Version 1: with callbacks!

// The downside to this version is you either introduce a pyramid of
// doom, or a function definition boundary at each async call. Either
// way is silly; normally the "next thing to be done" simply follows
// on the next line. There's no sense in making extra functions all
// the time...

function pretendWorkCallback (value, callback) {
  console.log("Begin work!");

  setTimeout(function () {
    callback(value + 1);
  }, 500);
};

function runVersionOne () {
  pretendWorkCallback(0, function (result) {
    pretendWorkCallback(result, function (result) {
      pretendWorkCallback(result, function (result) {
        console.log(result);
      });
    });
  });
}

// runVersionOne();

// Version 2: with promises!

// With promises we avoid the pyramid of doom, though we still need a
// new `then` block at each async function. I don't know if thik this
// is an advantage over the named functions solution, though maybe
// it's more compressed and flows a bit better?
//
// Promises ease some rough edges of callbacks; in particular,
// mistakes about error handling. Some new problems can creep up (for
// instance, no one watches for errors in a chain, so a thrown error
// is lost). Also try/catch handling can still be confusing to get
// right...
//
// Even with promises, async is in our face. We need to know for every
// function whether it returns a promise. A fn that does one bit of
// async work needs to return a promise even if 99% of the work is
// sync.
//
// It feels dirty that some functions return values, while others
// return wrapped values. And it all depends on whether they did some
// async work, which as the caller I don't care about. As a user of an
// async function, I'm being asked to care about something I don't
// actually care about.
//
// But that is the fundamental "problem" with Node. If you think it's
// a problem (hint: I do).

// Half-assed promise/deferred hybrid.
function Promise () {
  this.thens = [];
};

Promise.prototype.then = function (callback) {
  var newPromise = new Promise();
  this.thens.push(function (value) {
    newPromise.resolve(callback(value));
  });

  return newPromise;
};

Promise.prototype.resolve = function (value) {
  if (typeof(value) === "object" && value.constructor == Promise) {
    // if resolving wiht a Promise, wait for that promise before
    // actually resolving...
    value.then(this.resolve.bind(this));
  } else {
    this.thens.forEach(function (callback) {
      callback(value);
    });
  }
};

// promisified version of pretendWork
function pretendWork (value) {
  console.log("Begin work!");

  // Half-assed promise here...
  var deferred = new Promise();
  setTimeout(function () {
    deferred.resolve(value + 1);
  }, 500);

  // Normally would return deferred.promise, except this is the
  // half-assed version of Promise.
  return deferred;
}

function runVersionTwo() {
  pretendWork(0)
    .then(function (value) {
      return pretendWork(value);
    }).then(function (value) {
      return pretendWork(value);
    }).then(function (value) {
      console.log(value);
    });
}

// runVersionTwo();

// Version 3: with generators + spawn.

// Using generators, we can write straight-line code that looks
// synchronous. Whenever we make an async function call, we yield a
// promise to spawn, which will wait for the result. Spawn can then
// restart the client code where it left off when the promise is
// resolved.
//
// Effectively, JavaScript will turn the generator code into callback
// style code similar to Version 1. But the generator takes care of
// this and it isn't in our face.
//
// We don't really need "promises" per se for this; we just need a way
// to for spawn to learn the result when it is finally computed. If we
// wanted to do this in node without promises we could try:
//
//     yield myAsyncFunction.bind(ctx, arg1, arg2);
//
// If spawn assumes that the last arg to myAsyncFunction is a standard
// Node callback of the type `function (err, value)`, it can just call
// the yielded function with a callback that resumes the generator.
//
// Of course, promises are a convenient way to handle the errors and
// whatnot, but my point is that they're actually not essential for
// this generator pattern.
//
// This pattern is probably my favorite yet, but it's getting pretty
// sophisticated. And you still have a bifurcation of async and sync
// functions. One style of function you call normally, and the other
// syncronously. And any async work in a function makes it async.
//
// But at least you aren't constantly defining and chaining up a
// series of continuation functions. This is much more similar to
// regular, async code.
//
// Congratulations: we have re-invented C#'s await keyword.
//
// Because spawn returns a promise, you don't need everyone to agree
// to use it. So long as a user of your spawn-style code knows how to
// deal with the promise object you return, they don't need to know
// you used a cool ES6 Harmony feature...

function* versionThreeGenerator () {
  var value;

  value = yield pretendWork(0);
  console.log("finished first");
  value = yield pretendWork(value);
  console.log("finished second");
  value = yield pretendWork(value);
  console.log("finished third");

  console.log(value);

  return value;
}

// I try hard to return a promise from spawn so that I can do async
// work in async work...
function spawn (iter, value) {
  var deferred = new Promise();

  function work (value) {
    var nxt = iter.next(value);

    if (nxt.done) {
      deferred.resolve(nxt.value);
    } else {
      nxt.value.then(function (value) {
        work(value);
      });
    }
  }

  work(value);

  return deferred;
}

// Observe the majesty of composed generators!
function* versionThreeSuperGenerator () {
  var value = 0;

  value += yield spawn(versionThreeGenerator());
  value += yield spawn(versionThreeGenerator());

  return value;
}

function runVersionThree () {
  spawn(versionThreeSuperGenerator())
    .then(function (value) {
      console.log(value);
    });
}

runVersionThree();

// Version 4: An Alternative Approach With Go
//
// Look: JS doesn't have threads which means every function is one of
// two types: yields back to the event loop, or doesn't. This is
// annoying, because it makes async everyone's business all the
// time.
//
// Compare this to Go. You write blocking code, then can trivially
// make it run in the background by using a gothread. Instead of using
// "yield" or await to pause a thread, you read a channel. This does
// require the creation of an explicit channel. A lot of code would
// prolly read like (in fake Go):
//
// def concurrentFunc ()
//   c = new Channel();
//   go(function () { write(c, slowSyncWork()) });
//   go(function () { write(c, slowSyncWork()) });
//   v1 = read(c);
//   v2 = read(c);
// end
//
// In Go, you only need to use channels *at those points where you
// want to run concurrent work*, not pervasively. For instance, if you
// have a waterfall of blocking code, this is stupid:
//
// def sillyFunc ()
//   c = new Channel();
//   go(function () { write(c, slowSyncWork(0)) });
//   v1 = read(c);
//   go(function () { write(c, slowSyncWork(v1)) });
//   v2 = read(c);
//   return v2;
// end
//
// The use of gothreads here is silly; sillyFunc isn't any faster. A
// better version is:
//
// def wiseFunc ()
//   return slowSyncWork(slowSyncWork(0));
// end
//
// This is great, because we get to *opt in* to channels/gothreads
// only at those places where we benefit from concurrency. Everywhere
// else code is written in a totally normal way; we can assume that
// every function is blocking. There is no necessity to talk about
// "async" functions; it's at our option if we *want* to call a
// function asyncronously.
//
// Go can do this because it has a scheduler built into the runtime,
// which is the key difference. I really think await is the inferior
// solution.
