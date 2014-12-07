#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "draw.h"
#include "phistack.h"
#include "bmpio.h"

#if (!defined(IMAGE_SIZE))
#define IMAGE_SIZE 800
#endif

static byte * pixels = NULL;


f64 sin_rot, cos_rot=1, scale_x=1, scale_y=1, org_x, org_y;
byte color_r=0xFF, color_g=0xFF, color_b=0xFF;

void __attribute__((constructor)) init_pixels() {
    pixels = (byte*) calloc(IMAGE_SIZE, IMAGE_SIZE * 3);
}

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
    ui64 addr = xi + IMAGE_SIZE * yi;
    addr *= 3;
    pixels[addr]     = color_b;
    pixels[addr + 1] = color_g;
    pixels[addr + 2] = color_r;
}

void dump_bmp() {
    FILE * target = fopen("./t.bmp", "wb");
    dump(pixels, target);
    fclose(target);
}
