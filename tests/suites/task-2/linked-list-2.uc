list_append(list, x) {
    var node;
    node = alloc {v: x, prev: list, next: null};
    if (list == null) {
    } else {
        (*list).next = node;
    }

    return node;
}