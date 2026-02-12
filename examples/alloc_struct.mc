ctor() {
  return alloc { foo: 1, bar: 2 };
}

main() {
  var a, b;
  a = ctor();
  // Construct something huge to mess up stack if it was allocated there
  b = [4,3,34,2,21,312,312,31,321,312,312,3,1223,12,31,23,123,1,23,13,21,312,3,123,123,1,231];
  return (*a).foo;
}
