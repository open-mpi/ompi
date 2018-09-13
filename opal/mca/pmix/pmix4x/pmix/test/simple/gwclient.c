/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <src/include/pmix_config.h>
#include <pmix.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "src/class/pmix_object.h"
#include "src/util/output.h"
#include "src/util/printf.h"

static volatile bool completed = false;
static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    uint64_t seckey[2];
    pmix_proc_t proc;
    pmix_info_t *info;
    size_t n, ninfo;

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    }
    pmix_output(0, "GWClient ns %s rank %d: Running", myproc.nspace, myproc.rank);

    /* look for network data */
    memset(&proc, 0, sizeof(pmix_proc_t));
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, "my.net.key", NULL, 0, &val)) ||
        PMIX_DATA_ARRAY != val->type ||
        NULL == val->data.darray ||
        NULL == val->data.darray->array) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get my.net.key failed: %s",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        exit(rc);
    }
    /* the returned value has all the network allocation info in a
     * pmix_data_array_t */
    info = (pmix_info_t*)val->data.darray->array;
    ninfo = val->data.darray->size;
    pmix_output(0, "Client ns %s rank %d: got network assignment:",
                myproc.nspace, myproc.rank);
    for (n=0; n < ninfo; n++) {
        if (PMIX_STRING == info[n].value.type) {
            pmix_output(0, "\tKey: %s Value: %s",
                        info[n].key, info[n].value.data.string);
        } else if (PMIX_BYTE_OBJECT == info[n].value.type) {
            memcpy(seckey, info[n].value.data.bo.bytes, info[n].value.data.bo.size);
            pmix_output(0, "\tKey: %s sec key: %ld.%ld",
                        info[n].key, (long int)seckey[0], (long int)seckey[1]);
        }
    }
    PMIX_VALUE_RELEASE(val);

    /* finalize us */
    pmix_output(0, "Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc));
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(rc);
}
