main() {
   var r, s;
   r = { a: 1 };
   s = { a: &r, b: 2 };
   (*(s.a)).a = 40 + s.b;
   return r.a;
}
