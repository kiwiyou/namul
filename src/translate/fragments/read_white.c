void read_white() {
    int c = -1;
    while ((c = getchar()) != EOF)
        if (c > ' ') {
            ungetc(c, stdin);
            break;
        }
    if (feof(stdin)) halt();
}
