main() {
   var a,b,c,d;
   a = { n:1 };
   b = { m: &a };
   c = b;
   d = a;
   d.n = 2;
   (*b.m).n = 3;
   (*c.m).n = 4;
   output a.n;
   output (*b.m).n;
   output (*c.m).n;
   output d.n;
   return 0;
 }
