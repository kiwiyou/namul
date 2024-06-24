void write_literal(const char *literal, int len) {
    int written = 0;
    while (written < len) {
        if (writer->back == WRITER_BUF) flush();
        int remain = len - written;
        int shot = WRITER_BUF - writer->back;
        int chunk = remain < shot ? remain : shot;
        for (int i = 0; i < chunk; ++i) writer->buf[writer->back + i] = literal[written + i];
        writer->back += chunk;
        written += chunk;
    }
}
