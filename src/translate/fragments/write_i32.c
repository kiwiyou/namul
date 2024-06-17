void write_i32(int32_t v) {
    char buf[16];
    int off = 16;
    int sign = v < 0;
    uint32_t work = v;
    if (sign) work = -work;
    do {
        buf[--off] = work % 10 + '0';
        work /= 10;
    } while (work);
    if (sign) buf[--off] = '-';
    if (writer->back + 16 - off > WRITER_BUF) flush(writer);
    for (int i = off; i < 16; ++i) writer->buf[writer->back + i - off] = buf[i];
    writer->back += 16 - off;
}
