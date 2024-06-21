#define FIX __attribute__((__optimize__("-fno-tree-loop-distribute-patterns")))
void *FIX memset(void *dst, int c, size_t len) {
    char *d = (char *)dst;
    for (int i = 0; i < len; ++i) d[i] = c;
    return dst;
}
void *FIX memcpy(void *dst, const void *src, size_t len) {
    char *d = (char *)dst, *s = (char *)src;
    for (int i = 0; i < len; ++i) d[i] = s[i];
    return dst;
}
