/*
 * $HEADER$
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/param.h>

#include "lam_config.h"
#include "util/sys_info.h"

static bool test1(void);   /* verify it returns info */
static bool test2(void);   /* test second time through */


int main(int argc, char* argv[])
{
    bool test1f, test2f;

    test1f = test2f = false;

    /* All done */

    if (test1()) {
        printf("test1 passed\n");
        test1f = true;
    }

    if (test2()) {
        printf("test2 passed\n");
        test2f = true;
    }

    if (test1f && test2f) {
        printf("test succeeded\n");
        return 0;
    }

    printf("test failed\n");
    return -1;
}


static bool test1(void)
{
    /* Test trivial functionality. Program should return with init=true and all ptrs non-NULL */

    ompi_sys_info();
    if (ompi_system_info.init == false)
        return(false);

    if (ompi_system_info.sysname == NULL ||
        ompi_system_info.nodename == NULL ||
        ompi_system_info.release == NULL ||
        ompi_system_info.version == NULL ||
        ompi_system_info.machine == NULL ||
        ompi_system_info.path_sep == NULL)
        return(false);

    return true;
}


static bool test2(void)
{
/* test it a second time. system should return without crashing and with init=true */
    ompi_sys_info();
    return(ompi_system_info.init);
}
