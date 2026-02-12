main() {
  var head, ni;
  head = { value: 42, next: null };
  ni = { value: 84, next: null };
  head.next = &ni;
  return (*head.next).value;
}
