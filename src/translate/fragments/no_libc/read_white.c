void read_white() {
    for(;;) {
        for (int i = reader->off; i < reader->end; ++i)
            if (reader->buf[i] > ' ') {
                reader->off = i;
                return;
            }
        refill();
    }
}
