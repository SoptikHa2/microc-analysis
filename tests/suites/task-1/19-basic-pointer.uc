 main() {
   var x, y;
   x = 43;
   y = &x;
   *y = *y - 1;
   return x;
 }
