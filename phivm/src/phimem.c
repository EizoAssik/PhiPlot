#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "phimem.h"

#define LOCAL_SIZE 4096 

f64 * new_local(f64 * last) {
    f64 * new = (f64*) calloc(LOCAL_SIZE, sizeof(f64));
    if(last)
        memcpy(new, last, LOCAL_SIZE*sizeof(64));
    return new;
}

void free_local(f64 * frame) {
    free(frame);
}

