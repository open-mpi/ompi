#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

void align_c(char *w, char *x, char *y, char *z)
{
    unsigned long aw, ax, ay, az;
    FILE *f=stdout;
    if (!f) exit(1);
    aw = (unsigned long) w;
    ax = (unsigned long) x;
    ay = (unsigned long) y;
    az = (unsigned long) z;
    if (! ((aw%16)||(ax%16)||(ay%16)||(az%16))) {
        fprintf(f, "  %d\t", 16);
    }
    else if (! ((aw%12)||(ax%12)||(ay%12)||(az%12))) {
        fprintf(f, "  %d\t", 12);
    }
    else if (! ((aw%8)||(ax%8)||(ay%8)||(az%8))) {
        fprintf(f, "  %d\t", 8);
    }
    else if (! ((aw%4)||(ax%4)||(ay%4)||(az%4))) {
        fprintf(f, "  %d\t", 4);
    }
    else if (! ((aw%2)||(ax%2)||(ay%2)||(az%2))) {
        fprintf(f, "  %d\t", 2);
    }
    else {
        fprintf(f, "  %d\t", 1);
    }
    fflush(f);
}

#ifdef __cplusplus
}
#endif
