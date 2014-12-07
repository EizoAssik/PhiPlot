#include <stdio.h>

#if (!defined(PHIVM_COMMON_H))
#define PHIVM_COMMON_H

typedef unsigned long int ui64;
typedef unsigned      int ui32;
typedef unsigned    short ui16;
typedef unsigned     char byte;
typedef          double   f64;

ui64 filesize(FILE * file);
int phi_log(const char * format, ...);
int phi_error(const char * format, ...);
void phi_exit(int no);
    
#endif
