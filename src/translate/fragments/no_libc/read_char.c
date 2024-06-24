char read_char() {
    if (reader->off >= reader->end) refill();
    return reader->buf[reader->off++];
}
