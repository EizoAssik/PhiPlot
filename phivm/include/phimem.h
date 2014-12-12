#include "common.h"

#if (!defined(PHIVM_MEM_H))
#define PHIVM_MEM_H

f64 * new_local(f64 * last);
void free_local(f64 * frame);

#endif 
