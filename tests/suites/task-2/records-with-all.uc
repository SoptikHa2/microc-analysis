main() {
    var a,b,c,d;

    a = {x: 1};
    b = {x: &a};
    c = {x: main};
    d = {x: 1, y: &b};

    return 0;
}