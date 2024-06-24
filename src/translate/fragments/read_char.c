char read_char() {
    int c = getchar();
    return c == EOF ? halt() : c;
}
