foo() {
   var x, y, z;
   x = input;
   y = alloc x;
   *y = x;
   z = *y;
   y = 1;
   return x;
 }
