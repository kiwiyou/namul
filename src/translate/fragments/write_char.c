void write_char(char c) {
    if (writer->back == WRITER_BUF) flush();
    writer->buf[writer->back++] = c;
}
