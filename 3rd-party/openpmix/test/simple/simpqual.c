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
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "src/include/pmix_config.h"
#include "include/pmix.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "src/class/pmix_object.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"

#define MAXCNT 1

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc;
    uint32_t nprocs;
    pmix_info_t *iptr;
    size_t sz;
    pmix_data_array_t darray;
    PMIX_HIDE_UNUSED_PARAMS(argc, argv);

    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        pmix_output(0, "%s: PMIx_Init failed: %s",
                    PMIX_NAME_PRINT(&myproc),
                    PMIx_Error_string(rc));
        exit(rc);
    }
    pmix_output(0, "%s: Running on node %s",
                PMIX_NAME_PRINT(&myproc),
                pmix_globals.hostname);

    /* get number of procs */
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        pmix_output(0, "%s: PMIx_Get job size failed: %s",
                    PMIX_NAME_PRINT(&myproc), PMIx_Error_string(rc));
        exit(rc);
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "%s job size %d", PMIX_NAME_PRINT(&myproc), nprocs);

    /* put a qualified value */
    PMIX_DATA_ARRAY_CONSTRUCT(&darray, 2, PMIX_INFO);
    iptr = (pmix_info_t*)darray.array;
    /* load the primary key-value */
    sz = myproc.rank * 100;
    PMIX_INFO_LOAD(&iptr[0], PMIX_GROUP_LOCAL_CID, &sz, PMIX_SIZE);
    /* provide a qualifier */
    sz = 123456;
    PMIX_INFO_LOAD(&iptr[1], "OURQUALIFIER", &sz, PMIX_SIZE);
    PMIX_INFO_SET_QUALIFIER(&iptr[1]);  // mark it as a qualifier
    /* load the value */
    PMIX_VALUE_LOAD(&value, &darray, PMIX_DATA_ARRAY);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    rc = PMIx_Put(PMIX_GLOBAL, PMIX_QUALIFIED_VALUE, &value);
    if (PMIX_SUCCESS != rc) {
        pmix_output(0, "%s failed to put qualified value: %s",
                    PMIX_NAME_PRINT(&myproc),
                    PMIx_Error_string(rc));
        exit(rc);
    }

    /* put another qualified value of the same key */
    PMIX_DATA_ARRAY_CONSTRUCT(&darray, 2, PMIX_INFO);
    iptr = (pmix_info_t*)darray.array;
    /* load the primary key-value */
    sz = 1 + myproc.rank * 100;
    PMIX_INFO_LOAD(&iptr[0], PMIX_GROUP_LOCAL_CID, &sz, PMIX_SIZE);
    /* provide a qualifier */
    sz = 891011;
    PMIX_INFO_LOAD(&iptr[1], "OURQUALIFIER", &sz, PMIX_SIZE);
    PMIX_INFO_SET_QUALIFIER(&iptr[1]);  // mark it as a qualifier
    /* load the value */
    PMIX_VALUE_LOAD(&value, &darray, PMIX_DATA_ARRAY);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    rc = PMIx_Put(PMIX_GLOBAL, PMIX_QUALIFIED_VALUE, &value);
    if (PMIX_SUCCESS != rc) {
        pmix_output(0, "%s failed to put another qualified value: %s",
                    PMIX_NAME_PRINT(&myproc),
                    PMIx_Error_string(rc));
        exit(rc);
    }
    /* put an unqualified value of the same key */
    sz = 2 + (myproc.rank * 100);
    PMIX_VALUE_LOAD(&value, &sz, PMIX_SIZE);
    rc = PMIx_Put(PMIX_GLOBAL, PMIX_GROUP_LOCAL_CID, &value);
    if (PMIX_SUCCESS != rc) {
        pmix_output(0, "%s failed to put unqualified value: %s",
                    PMIX_NAME_PRINT(&myproc),
                    PMIx_Error_string(rc));
        exit(rc);
    }

    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        pmix_output(0, "%s: PMIx_Commit failed: %s",
                    PMIX_NAME_PRINT(&myproc),
                    PMIx_Error_string(rc));

        exit(rc);
    }

    /* call fence to ensure the data is received */
    PMIX_PROC_CONSTRUCT(&proc);
    pmix_strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        pmix_output(0, "%s: PMIx_Fence failed: %s",
                    PMIX_NAME_PRINT(&myproc),
                    PMIx_Error_string(rc));
        exit(rc);
    }

    /* first my own value without qualifiers */
    pmix_output(0, "%s Looking for my own value w/o quals",
                PMIX_NAME_PRINT(&myproc));
    rc = PMIx_Get(&myproc, PMIX_GROUP_LOCAL_CID, NULL, 0, &val);
    if (PMIX_SUCCESS != rc) {
        pmix_output(0, "%s UNABLE TO GET MY OWN VALUE W/O QUALS: %s",
                    PMIX_NAME_PRINT(&myproc), PMIx_Error_string(rc));
    } else if (val->data.size == ((100*myproc.rank)+2)) {
        pmix_output(0, "%s Returned my own correct value w/o quals: %s",
                    PMIX_NAME_PRINT(&myproc), PMIx_Value_string(val));
    } else {
        pmix_output(0, "%s Returned my own incorrect value w/o quals: %s",
                    PMIX_NAME_PRINT(&myproc), PMIx_Value_string(val));
        exit(-1);
    }

    /* now with the correct qualifier */
    /* get the qualified value for a peer */
    if (1 < nprocs) {
        proc.rank = myproc.rank + 1;
        if (proc.rank == nprocs) {
            proc.rank = 0;
        }
        /* first without qualifiers */
        pmix_output(0, "%s Looking for value w/o quals",
                    PMIX_NAME_PRINT(&myproc));
        rc = PMIx_Get(&proc, PMIX_GROUP_LOCAL_CID, NULL, 0, &val);
        if (PMIX_SUCCESS != rc) {
            pmix_output(0, "%s UNABLE TO GET VALUE W/O QUALS: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Error_string(rc));
        } else if (val->data.size == ((100*proc.rank)+2)) {
            pmix_output(0, "%s Returned correct value w/o quals: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Value_string(val));
        } else {
            pmix_output(0, "%s Returned incorrect value w/o quals: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Value_string(val));
            exit(-1);
        }

        /* now with the correct qualifier */
        pmix_output(0, "%s Looking for first qualified value with correct qual",
                    PMIX_NAME_PRINT(&myproc));
        PMIX_INFO_CREATE(iptr, 1);
        sz = 123456;
        PMIX_INFO_LOAD(&iptr[0], "OURQUALIFIER", &sz, PMIX_SIZE);
        PMIX_INFO_SET_QUALIFIER(&iptr[0]);  // mark it as a qualifier
        rc = PMIx_Get(&proc, PMIX_GROUP_LOCAL_CID, iptr, 1, &val);
        if (PMIX_SUCCESS != rc) {
            pmix_output(0, "%s UNABLE TO GET QUALIFIED VALUE WITH CORRECT QUAL: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Error_string(rc));
        } else if (val->data.size == (100*proc.rank)) {
            pmix_output(0, "%s GOT CORRECT QUALIFIED VALUE WITH CORRECT QUAL: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Value_string(val));
        } else {
            pmix_output(0, "%s GOT INCORRECT QUALIFIED VALUE WITH CORRECT QUAL: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Value_string(val));
            exit(-1);
        }
        PMIX_INFO_FREE(iptr, 1);

        /* now second value with the correct qualifier */
        pmix_output(0, "%s Looking for second qualified value with correct qual",
                    PMIX_NAME_PRINT(&myproc));
        PMIX_INFO_CREATE(iptr, 1);
        sz = 891011;
        PMIX_INFO_LOAD(&iptr[0], "OURQUALIFIER", &sz, PMIX_SIZE);
        PMIX_INFO_SET_QUALIFIER(&iptr[0]);  // mark it as a qualifier
        rc = PMIx_Get(&proc, PMIX_GROUP_LOCAL_CID, iptr, 1, &val);
        if (PMIX_SUCCESS != rc) {
            pmix_output(0, "%s UNABLE TO GET SECOND QUALIFIED VALUE WITH CORRECT QUAL: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Error_string(rc));
        } else if (val->data.size == (100*proc.rank)+1) {
            pmix_output(0, "%s GOT CORRECT SECOND QUALIFIED VALUE WITH CORRECT QUAL: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Value_string(val));
        } else {
            pmix_output(0, "%s GOT INCORRECT SECOND QUALIFIED VALUE WITH CORRECT QUAL: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Value_string(val));
            exit(-1);
        }
        PMIX_INFO_FREE(iptr, 1);

        /* and now with an incorrect qualifier - should not return success */
        pmix_output(0, "%s Looking for qualified value with incorrect qual",
                    PMIX_NAME_PRINT(&myproc));
        PMIX_INFO_CREATE(iptr, 1);
        sz = 0;
        PMIX_INFO_LOAD(&iptr[0], "OURQUALIFIER", &sz, PMIX_SIZE);
        PMIX_INFO_SET_QUALIFIER(&iptr[0]);  // mark it as a qualifier
        rc = PMIx_Get(&proc, PMIX_GROUP_LOCAL_CID, iptr, 1, &val);
        if (PMIX_SUCCESS == rc) {
            pmix_output(0, "%s GOT QUALIFIED VALUE WITH INCORRECT QUAL: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Value_string(val));
        } else if (PMIX_ERR_NOT_FOUND == rc) {
            pmix_output(0, "%s DID NOT GET QUALIFIED VALUE WITH INCORRECT QUAL: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Error_string(rc));
        } else {
            pmix_output(0, "%s GOT INCORRECT STATUS FOR QUALIFIED VALUE WITH INCORRECT QUAL: %s",
                        PMIX_NAME_PRINT(&myproc), PMIx_Error_string(rc));
            exit(-1);
        }
        PMIX_INFO_FREE(iptr, 1);
    }

    /* finalize us */
    pmix_output(0, "%s: Finalizing", PMIX_NAME_PRINT(&myproc));
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "%s:PMIx_Finalize failed: %s\n",
                PMIX_NAME_PRINT(&myproc), PMIx_Error_string(rc));
    } else {
        fprintf(stderr, "%s:PMIx_Finalize successfully completed\n",
                PMIX_NAME_PRINT(&myproc));
    }
    fflush(stderr);
    return (rc);
}
