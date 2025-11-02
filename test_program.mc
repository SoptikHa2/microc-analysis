list_append(list, x) {
  var node;
  node = alloc {v: x, prev: list, next: null};
  (*node).next = node;

  return node;
}
