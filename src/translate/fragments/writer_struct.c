#define WRITER_BUF 32768
typedef struct {
    char buf[WRITER_BUF];
    int back;
} Writer;
Writer *writer;
void flush() {
    write(1, writer->buf, writer->back);
    writer->back = 0;
}
