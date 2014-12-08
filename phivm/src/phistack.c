#include "common.h"
#include "phistack.h"

f64  DS [DS_SIZE];

inline f64  popv() { return DS[DTOP--]; }
inline void pushv(f64 v) { DS[++DTOP] = v; }
