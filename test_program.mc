f() {
  var x,y,z;
  x = input;
  while (x > 1) {
    y = x/2;
    if (y>3) x = x-y;
    z = x-4;
    if (z>0) x = x/2;
    z = z-1;
  }
  output x;
  return 1;
}

ite(n) {
  var f;
  f = 1;
  while (n > 0) {
    f = f * n;
    n = n - 1;
  }
  return f;
}