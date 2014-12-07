#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "draw.h"
#include "phistack.h"
#include "bmpio.h"

// static byte * pixels = (byte*) calloc(800, 800);
static byte pixels[800*800];

f64 sin_rot, cos_rot=1, scale_x=1, scale_y=1, org_x, org_y;
ui64 color;

void draw() {
    f64 x, y, raw_x;
    ui64 xi, yi;
    y = popv();
    x = popv();
#if (defined(PHIVM_DEBUG_DRAW))
    phi_log("(%lf, %lf)\n", x, y);
#endif
    x *= scale_x;
    y *= scale_y;
    raw_x = x;
    x  = cos_rot * x + y * sin_rot;
    y  = cos_rot * y - raw_x * sin_rot;
    x += org_x;
    y += org_y;
    xi = ((ui64) (x+400)) % 800;
    yi = ((ui64) (y+400)) % 800;
    pixels[800*yi + xi] = 0xFF;
}

void dump_bmp() {
    FILE * target = fopen("./t.bmp", "wb");
    dump(pixels, target);
    fclose(target);
}
