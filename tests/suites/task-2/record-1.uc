 f() {
   var x, y;
   x = { a: 1, b: 2 };
   y = { a: &x, c: f };
   return (*y.a).a;
 }
