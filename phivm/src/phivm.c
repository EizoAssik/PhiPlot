#include "common.h"

#define DS_SIZE 4096
#define RS_SIZE 4096
#define PM_SIZE 4096

typedef void (*fnptr)();

static f64  DS[DS_SIZE];
static ui64 RS[RS_SIZE];
static ui64 PM[PM_SIZE] = {0x16, 0x00, 0x15};

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
void jp()   { if(DS[DTOP]>0)  PC = PM[PC+1]; }
void jz()   { if(DS[DTOP]==0) PC = PM[PC+1]; }

void ret()   { PC = RS[RTOP--]; }
void call()  { RS[++RTOP] = PC; }

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
    phi_log("(%ld, %ld)\n", x, y);
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
    and,  or,   jp,   jz,
    lt,   eq,   gt,   not,
    draw, halt, debug
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

int main() {
    mainloop();
    return 0;
}
