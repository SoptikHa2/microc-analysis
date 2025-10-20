 sum(n) {
   var x;
   x = 0;
   while (n > 0) {
     x = x + 1;
     n = n - 1;
   }

   return x;
 }

 main() {
   return sum(50000);
 }
