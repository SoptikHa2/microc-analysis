 f() {
   var n,a,b;
   b = null;
   n = 42;

   while (n>0) {
     n = n-1;
     a = alloc {h: n, t: b};
     b = a;
   }

   return 0;
 }
