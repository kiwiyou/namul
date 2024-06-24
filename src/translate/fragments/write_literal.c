void write_literal(const char *literal, int len) {
    fwrite(literal, 1, len, stdout);
}
