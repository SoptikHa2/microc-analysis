foo(a) {
  print a[0];
  print a[1];
  print a[2];
  return a[0] + a[2];
}

main() {
  var a, x;
  a = [1,2,3];
  print a[0];
  print a[1];
  print a[2];
  a[2]= 42;
  x = foo(a);
  return x;
}
