f(n) {
  var r;

  if (n == 0) {
    r = 1;
  } else {
    r = n * f(n - 1);
  }
  return r;
}

main() {
  return f(5);
}
