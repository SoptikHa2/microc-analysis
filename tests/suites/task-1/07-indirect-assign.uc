 main() {
   var x, y;
   x = 1;
   y = &x;
   *y = 42;
   return x;
 }
