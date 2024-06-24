char *read_str(char *s, char *e) {
    read_white();
    while (s != e) {
        *s = getchar();
        if (*s <= ' ') {
            ungetc(*s, stdin);
            return s;
        }
        ++s;
    }
    return s;
}
