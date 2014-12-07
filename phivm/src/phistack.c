#include "common.h"
#include "phistack.h"

f64  DS [DS_SIZE];

f64  popv() { return DS[DTOP--]; }
void pushv(f64 v) { DS[++DTOP] = v; }
