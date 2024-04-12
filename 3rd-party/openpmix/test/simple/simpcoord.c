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
 * Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
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

static pmix_proc_t myproc;

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp;
    pmix_proc_t proc;
    uint32_t nprocs, n;
    size_t ninfo, m;
    pmix_coord_t *coords;
    char *hostname;
    pmix_byte_object_t *bptr;
    PMIX_HIDE_UNUSED_PARAMS(argc, argv);

    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Init failed: %s", myproc.nspace, myproc.rank,
                    PMIx_Error_string(rc));
        exit(rc);
    }
    pmix_output(0, "Client ns %s rank %d: Running", myproc.nspace, myproc.rank);

    /* test something */
    pmix_strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get failed: %s", myproc.nspace, myproc.rank,
                    PMIx_Error_string(rc));
        exit(rc);
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "Client %s:%d job size %d", myproc.nspace, myproc.rank, nprocs);

    /* get our assumed hostname */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_HOSTNAME, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get hostname failed: %s", myproc.nspace,
                    myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    hostname = strdup(val->data.string);
    PMIX_VALUE_RELEASE(val);
    pmix_output(0, "Client %s:%d hostname %s", myproc.nspace, myproc.rank, hostname);

    /* get our assigned fabric endpts */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_FABRIC_ENDPT, NULL, 0, &val))) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get fabric endpt failed: %s", myproc.nspace,
                    myproc.rank, PMIx_Error_string(rc));
        goto nextstep;
    }
    pmix_output(0, "Client %s:%d was assigned %lu endpts", myproc.nspace, myproc.rank,
                (unsigned long) val->data.darray->size);

    bptr = (pmix_byte_object_t *) val->data.darray->array;
    ninfo = val->data.darray->size;
    /* print them out for diagnostics - someday we can figure
     * out an automated way of testing the answer. We know that
     * the simptest pnet component returns strings */
    {
        char **foo = NULL;
        for (n = 0; n < ninfo; n++) {
            PMIx_Argv_append_nosize(&foo, bptr[0].bytes);
        }
        tmp = PMIx_Argv_join(foo, ',');
        PMIx_Argv_free(foo);
        pmix_output(0, "Rank %u[%s]: ASSIGNED ENDPTS: %s", myproc.rank, hostname, tmp);
        free(tmp);
    }

nextstep:
    /* get our assigned fabric coordinates */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_FABRIC_COORDINATES, NULL, 0, &val))
        || NULL == val) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get fabric coordinate failed: %s", myproc.nspace,
                    myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    if (PMIX_DATA_ARRAY == val->type) {
        coords = (pmix_coord_t *) val->data.darray->array;
        ninfo = val->data.darray->size;
        /* print them out for diagnostics - someday we can figure
         * out an automated way of testing the answer */
        for (m = 0; m < ninfo; m++) {
            char **foo = NULL;
            char *view;
            for (n = 0; n < coords[m].dims; n++) {
                if (0 > asprintf(&tmp, "%d", coords[m].coord[n])) {
                    errno = ENOMEM;
                    abort();
                }
                PMIx_Argv_append_nosize(&foo, tmp);
                free(tmp);
            }
            tmp = PMIx_Argv_join(foo, ',');
            PMIx_Argv_free(foo);
            if (PMIX_COORD_LOGICAL_VIEW == coords[m].view) {
                view = "LOGICAL";
            } else {
                view = "PHYSICAL";
            }
            pmix_output(0, "Rank %u[%s]: COORD[%d] VIEW %s: %s", myproc.rank, hostname, (int) m,
                        view, tmp);
            free(tmp);
        }
    } else if (PMIX_COORD == val->type) {
        char **foo = NULL;
        char *view;
        for (n = 0; n < val->data.coord->dims; n++) {
            if (0 > asprintf(&tmp, "%d", val->data.coord->coord[n])) {
                errno = ENOMEM;
                abort();
            }
            PMIx_Argv_append_nosize(&foo, tmp);
            free(tmp);
        }
        tmp = PMIx_Argv_join(foo, ',');
        PMIx_Argv_free(foo);
        if (PMIX_COORD_LOGICAL_VIEW == val->data.coord->view) {
            view = "LOGICAL";
        } else {
            view = "PHYSICAL";
        }
        pmix_output(0, "Rank %u[%s]: COORD VIEW %s DIMS %lu: %s", myproc.rank, hostname, view,
                    val->data.coord->dims, tmp);
        free(tmp);
    } else {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get fabric coordinate returned wrong type: %s",
                    myproc.nspace, myproc.rank, PMIx_Data_type_string(val->type));
    }

done:
    /* finalize us */
    pmix_output(0, "Client ns %s rank %d: Finalizing", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n",
                myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return (rc);
}
