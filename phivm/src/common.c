#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "common.h"

int phi_log(const char * format, ...) {
    va_list va;
    va_start(va, format);
    return vprintf(format, va);
}

int phi_error(const char * format, ...) {
    va_list va;
    va_start(va, format);
    return vfprintf(stderr, format, va);
}

void phi_exit(int no) {
    exit(no);
}
