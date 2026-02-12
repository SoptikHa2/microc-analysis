foo() {
  print 1;
  return foo;
}

main() {
  var f, pf, xf;
  f = foo;
  xf = foo();
  xf = f();
  pf = &f;
  xf = (*pf)();
  xf = xf();
  return (**(alloc pf))();
}

