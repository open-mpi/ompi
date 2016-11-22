#include "ompi_config.h"
#include <assert.h>

BEGIN_C_DECLS

void PRINT_INT(int* i);

void align_c(char *a, char *w, char *x, char *y, char *z)
{
    unsigned long aw, ax, ay, az;
    int diff, diff4;

    aw = (unsigned long) w;
    ax = (unsigned long) x;
    ay = (unsigned long) y;
    az = (unsigned long) z;

    if (! ((aw%16)||(ax%16)||(ay%16)||(az%16))) {
        diff4 = 16;
    }
    else if (! ((aw%12)||(ax%12)||(ay%12)||(az%12))) {
        diff4 = 12;
    }
    else if (! ((aw%8)||(ax%8)||(ay%8)||(az%8))) {
        diff4 = 8;
    }
    else if (! ((aw%4)||(ax%4)||(ay%4)||(az%4))) {
        diff4 = 4;
    }
    else if (! ((aw%2)||(ax%2)||(ay%2)||(az%2))) {
        diff4 = 2;
    }
    else {
        diff4 = 1;
    }
    diff = a - w;
    diff = (diff >= 0) ? diff : -diff;
    *a = diff;
    assert(diff4 == diff);
}


void align_c_(char *a, char *w, char *x, char *y, char *z)
{
  align_c(a, w, x, y, z);
}


void align_c__(char *a, char *w, char *x, char *y, char *z)
{
  align_c(a, w, x, y, z);
}


void ALIGN_C(char *a, char *w, char *x, char *y, char *z)
{
  align_c(a, w, x, y, z);
}


void ALIGN_C_(char *a, char *w, char *x, char *y, char *z)
{
  align_c(a, w, x, y, z);
}


void ALIGN_C__(char *a, char *w, char *x, char *y, char *z)
{
  align_c(a, w, x, y, z);
}

END_C_DECLS

