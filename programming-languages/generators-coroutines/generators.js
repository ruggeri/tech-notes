function genToFunc (f) {
  var fIter;
  return (function (arg) {
    if (!fIter) {
      fIter = f(arg);
      return fIter.next().value;
    } else {
      return fIter.next(arg).value;
    }
  });
}

var f = genToFunc(function* (arg) {
  while (true) {
    console.log("f: " + arg)
    arg = yield [g, arg + 1];
  }
});

var g = genToFunc(function* (arg) {
  while (true) {
    console.log("g: " + arg)
    arg = yield [h, arg + 1];
  }
});

var h = genToFunc(function*  (arg) {
  while (true) {
    console.log("h: " + arg)
    arg = yield [f, arg + 1];
  }
});

function trampoline () {
  var nextFun = f;
  var nextVal = 1;
  // keeps passing control amongs the three
  setInterval(function () {
    var value = nextFun(nextVal);
    nextFun = value[0];
    nextVal = value[1];
  }, 500);
}

// Should print in sequence.
trampoline();
