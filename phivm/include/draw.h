#include "common.h"

#if (!defined(PHIVM_DRAW_H))
#define PHIVM_DRAW_H

extern f64 sin_rot, cos_rot, scale_x, scale_y, org_x, org_y;
extern ui64 DTOP;

void draw();
void dump_bmp();

#endif
