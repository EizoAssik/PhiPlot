#include <stdio.h>
#include "common.h"

#define DS_SIZE 4096
#define RS_SIZE 4096
#define PM_SIZE 4096

typedef void (*fnptr)();

static f64  DS[DS_SIZE];
static ui64 RS[RS_SIZE];
static ui64 PM[PM_SIZE];

static ui64 DTOP = 0;
static ui64 RTOP = 0;
static ui64 PC   = 0;

f64  popv() { return DS[DTOP--]; }
void pushv(f64 v) { DS[++DTOP] = v; }

void nop()  { return; }
void pop()  { DTOP--; }
void dup()  { DS[DTOP+1] = DS[DTOP]; DTOP++; }
void neg()  { DS[DTOP] = - DS[DTOP]; }
void push() { DS[++DTOP] = ((double*)PM)[PC++]; }
void and()  { DS[DTOP-1] *= DS[DTOP]; DTOP--; }
void or()   { DS[DTOP-1] += DS[DTOP]; DTOP--; }
void not()  { DS[DTOP] = (DS[DTOP]==0); }
void jmp()  { PC = PM[PC+1]; }
void jp()   { if(DS[DTOP]>0)  PC = PM[PC+1]; }
void jz()   { if(DS[DTOP]==0) PC = PM[PC+1]; }

void ret()  { PC = RS[RTOP--]; }
void call() { RS[++RTOP] = PC + 1; PC = PM[PC]; }

static f64 swp;
void swap() { swp = DS[DTOP]; DS[DTOP] = DS[DTOP-1]; DS[DTOP-1] = swp; }

REGBINOP(add, +)
REGBINOP(sub, -)
REGBINOP(mul, *)
REGBINOP(div, /)
REGBINOP(eq, ==)
REGBINOP(lt, <)
REGBINOP(gt, >)

void draw() {
    f64 x, y;
    x = popv();
    y = popv();
    phi_log("(%lf, %lf)\n", x, y);
}

void halt() {
    phi_log("Halt.\n");
    phi_exit(0);
}

void debug() {
    ui64 i = 1;
    while(i<=DTOP)
        phi_log("[%ld] %lf\n", i, DS[i++]);
}

static fnptr OPCODE[] = {
    nop,  pop,  push, swap,
    dup,  add,  sub,  mul,
    div,  call, ret,  neg,
    and,  or,   jmp,  jp,
    jz,   lt,   eq,   gt,
    not,  draw, halt, debug
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
