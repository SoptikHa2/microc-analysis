 inc(s) {
   (*s).c = (*s).c + 1;
   return *s;
 }

 new() {
   return { c: 0, inc: inc };
 }

 main() {
   var c, p;
   c = new();
   p = &c;
   return ((((c.inc)(p)).inc)(p)).c;
 }
