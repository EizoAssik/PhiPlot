#if (!defined(PHIVM_PHIVM_H))
#define PHIVM_PHIVM_H

#define REGBINOP(name, op) \
    void name () { \
        DTOP -= 1; \
        DS[DTOP] = DS[DTOP] op DS[DTOP+1]; \
    }

#define REGBINFUNC(name, func) \
    void name () { \
        DTOP -= 1; \
        DS[DTOP] = func ( DS[DTOP], DS[DTOP+1] ); \
    }

#define REGFUNC(name, func) \
    void name () { \
        DS[DTOP] = func (DS[DTOP]); \
    }
    
#endif
