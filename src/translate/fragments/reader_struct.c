#define READER_BUF 65536
typedef struct {
    char buf[READER_BUF];
    int off, end;
} Reader;
Reader *reader;
void refill() {
    reader->off = 0;
    reader->end = read(0, reader->buf, READER_BUF);
    if (reader->end == 0) halt();
}
