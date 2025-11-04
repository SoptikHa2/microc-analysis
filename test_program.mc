 main() {
   var r,s;
   r = { a: null, b: null };
   s = { c: 1 };
   r.a = &s;
   r.c = &s;
   //s.c = 2;
   //return (*(r.a)).c + (*(r.b)).c;
   return 0;
 }
