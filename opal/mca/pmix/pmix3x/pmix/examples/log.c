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
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>

#include <pmix.h>
#include "examples.h"

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_info_t *info, *directives;
    bool flag;
    pmix_proc_t proc;
    bool syslog=false, global=false;

    /* check for CLI directives */
    if (1 < argc) {
        if (0 == strcmp(argv[argc-1], "--syslog")) {
            syslog = true;
        } else if (0 == strcmp(argv[argc-1], "--global-syslog")) {
            global = true;
        }
    }
    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    /* have rank 0 do the logs - doesn't really matter who does it */
    if (0 == myproc.rank) {
        /* always output a log message to stderr */
        PMIX_INFO_CREATE(info, 1);
        PMIX_INFO_LOAD(&info[0], PMIX_LOG_STDERR, "stderr log message\n", PMIX_STRING);
        PMIX_INFO_CREATE(directives, 1);
        PMIX_INFO_LOAD(&directives[0], PMIX_LOG_GENERATE_TIMESTAMP, NULL, PMIX_BOOL);
        rc = PMIx_Log(info, 1, directives, 1);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Log stderr failed: %s\n",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
            goto fence;
        }
        /* if requested, output one to syslog */
        if (syslog) {
            fprintf(stderr, "LOG TO LOCAL SYSLOG\n");
            PMIX_INFO_CREATE(info, 1);
            PMIX_INFO_LOAD(&info[0], PMIX_LOG_LOCAL_SYSLOG, "SYSLOG message\n", PMIX_STRING);
            rc = PMIx_Log(info, 1, NULL, 0);
            if (PMIX_SUCCESS != rc) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Log syslog failed: %s\n",
                        myproc.nspace, myproc.rank, PMIx_Error_string(rc));
                goto fence;
            }
        }
        if (global) {
            fprintf(stderr, "LOG TO GLOBAL SYSLOG\n");
            PMIX_INFO_CREATE(info, 1);
            PMIX_INFO_LOAD(&info[0], PMIX_LOG_GLOBAL_SYSLOG, "GLOBAL SYSLOG message\n", PMIX_STRING);
            rc = PMIx_Log(info, 1, NULL, 0);
            if (PMIX_SUCCESS != rc) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Log GLOBAL syslog failed: %s\n",
                        myproc.nspace, myproc.rank, PMIx_Error_string(rc));
                goto fence;
            }
        }
    }

  fence:
    fprintf(stderr, "%s:%d Calling Fence\n", myproc.nspace, myproc.rank);
    /* call fence to synchronize with our peers - no need to
     * collect any info as we didn't "put" anything */
    PMIX_INFO_CREATE(info, 1);
    flag = false;
    PMIX_INFO_LOAD(info, PMIX_COLLECT_DATA, &flag, PMIX_BOOL);
    PMIX_PROC_LOAD(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, info, 1))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    PMIX_INFO_FREE(info, 1);


  done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
