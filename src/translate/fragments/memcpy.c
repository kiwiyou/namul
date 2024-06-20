void memcpy_(char *dst, char *src, int len) {
    for (int i = 0; i < len; ++i) src[i] = dst[i];
}
