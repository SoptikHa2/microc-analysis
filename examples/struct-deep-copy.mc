main() {
  var x, y;
  x = { foo: 1 };
  x.foo = 2;
  y = x;
  x.foo = 3;
  return y.foo;
}

