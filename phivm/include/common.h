#if (!defined(PHIVM_COMMON_H))
#define PHIVM_COMMON_H

typedef unsigned long int ui64;
typedef          double   f64;

ui64 filesize(FILE * file);
int phi_log(const char * format, ...);
int phi_error(const char * format, ...);
void phi_exit(int no);

#define REGBINOP(name, op) \
    void name () { \
        f64 r = popv(); \
        f64 l = popv(); \
        pushv(l op r); \
    }
    
#endif
