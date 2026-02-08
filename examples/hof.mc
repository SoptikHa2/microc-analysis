inc(a) {
  return a + 1;
}

map(f) {
  return f(42);
}

main() {
  var a;
  a = map;
  return a(inc);
}
