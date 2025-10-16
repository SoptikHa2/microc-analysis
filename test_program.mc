main() {
    var rec, ptr;

    rec = { foo: 3, bar: main };
    ptr = &(rec.bar);
    *ptr = 5;
    output rec.bar;
    return 0;
}
