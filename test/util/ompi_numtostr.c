#include "ompi_config.h"
#include "util/numtostr.h"

#include <string.h>
#include <stdio.h>

int
main(int argc, char *argv[])
{
    char * tst;
    char * expected;
    
    tst = ltostr(10);
    expected = malloc(sizeof(long) * 8);
    snprintf(expected, sizeof(long) * 8, "%d", 10);
    if (strcmp(tst, expected) != 0) {
        exit(1);
    }
    
    free(tst);
    free(expected);
    
    tst = dtostr(5.32);
    expected = malloc(sizeof(long) * 8);
    snprintf(expected, sizeof(long) * 8, "%f", 5.32);
    if (strcmp(tst, expected) != 0) {
        exit(1);
    }
        
    return 0;
}