main() {
    var n, k;

    k = {a: 1};
    n = alloc {a: &k, b: 2};
    output (*(*n).a).b; // error: missing field

    return 0;
}