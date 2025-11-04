 main() {
   var r,s;
   r = { a: null, b: null };
   s = { c: 1 };
   r.a = &s;
   r.b = &s;
   s.c = 2;
   return (*(r.a)).c + (*(r.b)).c;
 }
