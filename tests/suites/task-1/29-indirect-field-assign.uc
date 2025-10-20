 main() {
   var r, s, t;
   r = { x: 1 };
   s = &r;
   t = &s;
   (**t).x = 42;
   return r.x;
 }
