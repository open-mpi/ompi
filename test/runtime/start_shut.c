/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "orte/orte_constants.h"


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
    
    for (i=0; i < NUM_ITERS; i++) {
        if (ORTE_SUCCESS != (rc = orte_finalize())) {
            fprintf(test_out, "iter %d: couldn't complete orte system finalize - error %d\n", i, rc);
            exit(1);
        }
        fprintf(test_out, "\tfinalize successful\n");
        if (ORTE_SUCCESS != (rc = orte_init(true))) {
            fprintf(test_out, "iter %d: couldn't complete orte system init - error code %d\n", i, rc);
            exit(1);
        }
    }
    
    fprintf(test_out, "shut system down\n");
    if (ORTE_SUCCESS != (rc = orte_finalize())) {
        fprintf(test_out, "couldn't complete finalize - error code %d\n", rc);
        exit(1);
    }

    fprintf(test_out, "orte_start_shut: successful\n");
    
    rc = test_finalize();
    fclose(test_out);
    return rc;
}
