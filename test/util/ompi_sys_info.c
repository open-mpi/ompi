/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

int main(int argc, char **argv)
{
    printf("Test disabled - Does not compile.\n");
    return 77;
}

#if 0

#include "util/sys_info.h"
#include "support.h"

static bool test1(void);   /* verify it returns info */
static bool test2(void);   /* test second time through */


int main(int argc, char* argv[])
{

    test_init("ompi_sys_info_t");

    if (test1()) {
        test_success();
    }
    else {
      test_failure("ompi_sys_info_t test1 failed");
    }

    if (test2()) {
        test_success();
    }
    else {
      test_failure("ompi_sys_info_t test2 failed");
    }

    test_finalize();
    return 0;
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
#endif
