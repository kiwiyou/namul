int64_t read_int() {
    read_white();
    int c = getchar();
    if (c == EOF) halt();
    int sign = c == '-';
    if (!sign) ungetc(c, stdin);
    uint64_t v = 0;
    while ('0' <= (c = getchar()) && c <= '9')
        v = v * 10 + (c - '0');
    ungetc(c, stdin);
    return sign ? -v : v;
}
