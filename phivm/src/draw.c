#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "draw.h"
#include "phistack.h"
#include "bmpio.h"

// static byte * pixels = (byte*) calloc(800, 800);
static byte pixels[800*800];

void draw() {
    f64 x, y;
    ui64 xi, yi;
    y = popv();
    x = popv();
#if (defined(PHIVM_DEBUG_DRAW))
    phi_log("(%lf, %lf)\n", x, y);
#endif
    xi = ((ui64) (x+400)) % 800;
    yi = ((ui64) (y+400)) % 800;
    pixels[800*yi + xi] = 0xFF;
}

void dump_bmp() {
    FILE * target = fopen("./t.bmp", "wb");
    dump(pixels, target);
    fclose(target);
}
