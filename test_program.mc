f() {
  var x, y, z;
  x = input;
  y = alloc x;
  *y = x;
  z = *y;
  return z;
}
