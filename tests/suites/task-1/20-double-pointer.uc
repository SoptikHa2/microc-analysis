 main() {
   var x, y, z;
   x = 43;
   y = &x;
   z = &y;
   **z = *y - 1;
   return x;
 }
