foo(x) {
    x[0] = 4;
    return x;
}

bar() {
    var a;
    a = foo([2]);
    return a[0];
}

main() {
    var a;
    a = bar();
    return a;
}