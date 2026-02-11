foo(a) {
    print a.foo;
    print a.bar;
    print a.baz;
    return a.foo + a.baz;
}

main() {
  var a, x;
  a = {foo: 1, bar: 2, baz: 3};
  print a.foo;
  print a.bar;
  print a.baz;
  a.baz = 42;
  x = foo(a);
  return x;
}
