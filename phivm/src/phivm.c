#include <math.h>
#include "common.h"
#include "phivm.h"
#include "phistack.h"
#include "draw.h"

#define RS_SIZE  4096
#define PM_SIZE  4096
#define MEM_SIZE 4096

typedef void (*fnptr)();

static ui64 RS [RS_SIZE];
static ui64 PM [PM_SIZE];
static f64  MEM[MEM_SIZE];

ui64 DTOP = 0;
static ui64 RTOP = 0;
static ui64 PC   = 0;

void nop()  { return; }
void pop()  { DTOP--; }
void dup()  { DS[DTOP+1] = DS[DTOP]; DTOP++; }
void neg()  { DS[DTOP] = - DS[DTOP]; }
void push() { DS[++DTOP] = ((double*)PM)[PC++]; }
void and()  { DS[DTOP-1] *= DS[DTOP]; DTOP--; }
void or()   { DS[DTOP-1] += DS[DTOP]; DTOP--; }
void not()  { DS[DTOP] = (DS[DTOP]==0); }
void jmp()  { PC = PM[PC]; }
void jp()   { if(DS[DTOP]>0)  { PC = PM[PC]; } else { PC++; } pop(); }
void jz()   { if(DS[DTOP]==0) { PC = PM[PC]; } else { PC++; } pop(); }

void ret()   { PC = RS[RTOP--]; }
void call()  { RS[++RTOP] = PC + 1; PC = PM[PC]; }
void store() {
    f64 value = popv();
    ui64 addr = (ui64) popv();
    MEM[addr] = value;
}

void load() {
    ui64 addr = (ui64) popv();
    pushv(MEM[addr]);
}

static void set_rot() {
    f64 rot = popv();
    cos_rot = cos(rot);
    sin_rot = sin(rot);
}

static void set_org() {
    org_y = popv();
    org_x = popv();
}

static void set_scale() {
    scale_y = popv();
    scale_x = popv();
}

static void set_color() {
    color_b = (byte) popv();
    color_g = (byte) popv();
    color_r = (byte) popv();
}

static f64 swp;
void swap() { swp = DS[DTOP]; DS[DTOP] = DS[DTOP-1]; DS[DTOP-1] = swp; }

REGBINOP(add, +)
REGBINOP(sub, -)
REGBINOP(mul, *)
REGBINOP(div, /)
REGBINOP(eq, ==)
REGBINOP(lt, <)
REGBINOP(gt, >)

REGFUNC(phi_ln,  log)
REGFUNC(phi_sin, sin)
REGFUNC(phi_cos, cos)
REGFUNC(phi_tan, tan)
REGFUNC(phi_exp, exp)

void halt() {
#if (defined(PHIVM_DEBUG_CPU))
    phi_log("Halt.\n");
#endif
    dump_bmp();
    phi_exit(0);
}

#if (defined(PHIVM_DEBUG_CPU))
void debug() {
    ui64 i = 1;
    while(i<=DTOP) {
        phi_log("[%ld] %lf\n", i, DS[i]);
        i += 1;
    }
}
#endif

static fnptr OPCODE[] = {
    nop,  pop,  push, swap,
    dup,  add,  sub,  mul,
    div,  call, ret,  neg,
    and,  or,   jmp,  jp,
    jz,   lt,   eq,   gt,
    not,  draw, halt, 
    store, load,
    set_rot, set_scale, set_org, set_color, 
    phi_sin, phi_cos, phi_tan, phi_ln, phi_exp
#if (defined(PHIVM_DEBUG_CPU))
    debug,
#endif
};

void mainloop() {
    ui64  ins = 0;
    for(;;) {
        ins = PM[PC++];
        if (ins >= sizeof(OPCODE)/sizeof(void*)) {
            phi_error("BAD OPCODE: 0x%lX\n", ins);
            phi_exit(-1);
        } else {
            OPCODE[ins](); 
        }
    }
}

int main(int argc, const char *argv[]) {
    if (argc != 2) {
        phi_error("No hex given.\n");
        phi_exit(0);
    }
    FILE * hex = fopen(argv[1], "rb");
    if (!hex) {
        phi_error("Cannot open hex: %s\n", argv[1]);
        phi_exit(-1);
    }
    ui64 fs = filesize(hex);
    fread((void*) PM, 1, fs, hex);
    fclose(hex);
    mainloop();
    return 0;
}
