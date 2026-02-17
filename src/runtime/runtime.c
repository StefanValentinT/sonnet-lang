#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char* data;
    long len;
} Slice;

Slice show_i32(int x) {
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%d", x);
    char* heap = malloc(len);
    memcpy(heap, buf, len);
    return (Slice){ heap, len };
}

Slice show_i64(long x) {
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%ld", x);
    char* heap = malloc(len);
    memcpy(heap, buf, len);
    return (Slice){ heap, len };
}

Slice show_f64(double x) {
    char buf[64];
    int len = snprintf(buf, sizeof(buf), "%.6f", x);
    char* heap = malloc(len);
    memcpy(heap, buf, len);
    return (Slice){ heap, len };
}

Slice show_char(int c) {
    char* heap = malloc(1);
    heap[0] = (char)c;
    return (Slice){ heap, 1 };
}

Slice show_unit() {
    char* heap = malloc(2);
    heap[0] = '(';
    heap[1] = ')';
    return (Slice){ heap, 2 };
}

void* internal_alloc(size_t size) {
    return malloc(size);
}
