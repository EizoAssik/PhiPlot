#include <stdlib.h>
#include "common.h"
#include "phistack.h"

f64 *DS;

void __attribute__((constructor)) init_stack() {
    DS = (f64*) calloc(sizeof(f64), DS_SIZE);
}

inline f64  popv() { return DS[DTOP--]; }
inline void pushv(f64 v) { DS[++DTOP] = v; }
