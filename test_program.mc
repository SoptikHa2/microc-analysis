rectype() {
  var node;
  node = alloc {next: null};
  (*node).next = node;
  return node;
}
