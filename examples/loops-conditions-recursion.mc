sudé(i) {
  pro výsledek;

  když (i == 0) {
    výsledek = 1;
  }
  jinak když (i == 1) {
    výsledek = 0;
  }
  jinak když (i >= 2) {
    výsledek = !(sudé(i-1));
  }

  vrať výsledek;
}

main() {
  pro i, max, suma;
  max = 1000;

  i = 0;
  suma = 0;
  dokavaď (i < max) {
    pokud (sudé(i)) {
      suma = suma + i;
    }
    i = i + 1;
  }

  vrať suma;
}
