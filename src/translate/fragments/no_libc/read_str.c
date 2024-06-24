char *read_str(char *s, char *e) {
    read_white();
    if (s == e) return s;
    for(;;) {
        for (int i = reader->off; i < reader->end; ++i)
            if (reader->buf[i] <= ' ') {
                reader->off = i;
                return s;
            } else {
                *s++ = reader->buf[i];
                if (s == e) {
                    reader->off = i + 1;
                    return s;
                }
            }
        refill();
    }
}
