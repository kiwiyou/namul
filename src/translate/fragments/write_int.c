void write_int(int64_t v) {
    char buf[32];
    int off = 32;
    int sign = v < 0;
    uint64_t work = v;
    if (sign) work = -work;
    do {
        buf[--off] = work % 10 + '0';
        work /= 10;
    } while (work);
    if (sign) buf[--off] = '-';
    fwrite(buf + off, 1, 32 - off, stdout);
}
