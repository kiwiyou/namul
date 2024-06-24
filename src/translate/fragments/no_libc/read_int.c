int64_t read_int() {
    read_white();
    if (reader->off == reader->end) refill();
    int sign = reader->buf[reader->off] == '-';
    reader->off += sign;
    uint64_t v = 0;
    for(;;) {
        for (int i = reader->off; i < reader->end; ++i)
            if (reader->buf[i] < '0' || reader->buf[i] > '9') {
                reader->off = i;
                return sign ? -v : v;
            } else v = reader->buf[i] - '0' + v * 10;
        refill();
    }
}
