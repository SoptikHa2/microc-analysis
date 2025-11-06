id(x) {
    return x;
}

main() {
    var x, y, z;
    x = 1;
    y = id(x);
    z = id(&y);
    return 0;
}