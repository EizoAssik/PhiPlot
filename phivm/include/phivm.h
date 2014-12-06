#if (!defined(PHIVM_PHIVM_H))
#define PHIVM_PHIVM_H

#define REGBINOP(name, op) \
    void name () { \
        f64 r = popv(); \
        f64 l = popv(); \
        pushv(l op r); \
    }

#define REGBINFUNC(name, func) \
    void name () { \
        f64 r = popv(); \
        f64 l = popv(); \
        pushv( func ( l, r) ); \
    }

#define REGFUNC(name, func) \
    void name () { \
        f64 l = popv(); \
        pushv( func ( l ) ); \
    }
    
#endif
