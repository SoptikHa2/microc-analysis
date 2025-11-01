f() {
    var x, y;
    x = 0;
    y = &x;
    return x;
}

g() {
    var y, x;
    y = 0;
    x = &y;
    return y;
}