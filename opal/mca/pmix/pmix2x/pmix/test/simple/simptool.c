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
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <src/include/pmix_config.h>
#include <pmix_tool.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "src/class/pmix_object.h"
#include "src/buffer_ops/types.h"
#include "src/util/output.h"
#include "src/util/printf.h"

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_proc_t myproc;
    pmix_info_t *info;
    size_t ninfo;

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, NULL, 0))) {
        fprintf(stderr, "PMIx_tool_init failed: %d\n", rc);
        exit(rc);
    }
    pmix_output(0, "Tool ns %s rank %d: Running", myproc.nspace, myproc.rank);

    /* query something */
    ninfo = 2;
    PMIX_INFO_CREATE(info, ninfo);
    (void)strncpy(info[0].key, "foobar", PMIX_MAX_KEYLEN);
    (void)strncpy(info[1].key, "spastic", PMIX_MAX_KEYLEN);
    if (PMIX_SUCCESS != (rc = PMIx_Query_info(info, ninfo))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Query_info failed: %d", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    if (0 != strncmp(info[0].key, "foobar", PMIX_MAX_KEYLEN)) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Query_info key[0] wrong: %s vs foobar",
                    myproc.nspace, myproc.rank, info[0].key);
    }
    if (0 != strncmp(info[1].key, "spastic", PMIX_MAX_KEYLEN)) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Query_info key[0] wrong: %s vs spastic",
                    myproc.nspace, myproc.rank, info[1].key);
    }
    if (PMIX_STRING != info[0].value.type) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Query_info key[0] wrong type: %d vs %d",
                    myproc.nspace, myproc.rank, info[0].value.type, PMIX_STRING);
    }
    if (PMIX_STRING != info[1].value.type) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Query_info key[1] wrong type: %d vs %d",
                    myproc.nspace, myproc.rank, info[1].value.type, PMIX_STRING);
    }
    if (0 != strcmp(info[0].value.data.string, "0")) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Query_info key[0] wrong value: %s vs 0",
                    myproc.nspace, myproc.rank, info[1].value.data.string);
    }
    if (0 != strcmp(info[1].value.data.string, "1")) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Query_info key[1] wrong value: %s vs 1",
                    myproc.nspace, myproc.rank, info[1].value.data.string);
    }
    PMIX_INFO_FREE(info, ninfo);

 done:
    /* finalize us */
    pmix_output(0, "Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(rc);
}
