/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/include/orte_constants.h"


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "support.h"
#include "orte/runtime/runtime.h"

#define NUM_ITERS 3

FILE *test_out;

int main (int argc, char* argv[])
{
    int rc, i;

    test_init("orte_start_shut");
    test_out = stderr;
    
    fprintf(test_out, "initialize the system\n");
    if (ORTE_SUCCESS != (rc = orte_init(true))) {
        fprintf(test_out, "couldn't complete init - error code %d\n", rc);
        exit(1);
    }

    fprintf(test_out, "shut system down\n");
    if (ORTE_SUCCESS != (rc = orte_finalize())) {
        fprintf(test_out, "couldn't complete finalize - error code %d\n", rc);
        exit(1);
    }

    fprintf(test_out, "orte_init_finalize: successful\n");
    
    rc = test_finalize();

    return rc;
}
