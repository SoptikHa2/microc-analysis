list_append(list, x) {
  var node;
  node = alloc {v: x, prev: list, next: null};
  if (list == null) {
  } else {
      (*list).next = node;
  }

  return node;
}

main(n) {
  var list;
  list = null;
  list = list_append(list, 1);
  return 0;
}