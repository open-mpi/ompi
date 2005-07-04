#include "ompi_config.h"
#include "opal/util/numtostr.h"

#include <string.h>
#include <stdio.h>

#include "support.h"

int
main(int argc, char *argv[])
{
    char * tst;
    char * expected;

    test_init("ompi_numtostr_t");
    
    tst = opal_ltostr(10);
    expected = malloc(sizeof(long) * 8);
    snprintf(expected, sizeof(long) * 8, "%d", 10);
    if (strcmp(tst, expected) != 0) {
      test_failure("opal_ltostr test failed");
    }
    else {
      test_success();
    }

    free(tst);
    free(expected);
    
    tst = opal_dtostr(5.32);
    expected = malloc(sizeof(long) * 8);
    snprintf(expected, sizeof(long) * 8, "%f", 5.32);
    if (strcmp(tst, expected) != 0) {
      test_failure("opal_dtostr test failed");
    }
    else {
      test_success();
    }

    test_finalize();
        
    return 0;
}
