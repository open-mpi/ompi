/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "opal/class/opal_list.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/argv.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/info/info.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Unpublish_name = PMPI_Unpublish_name
#endif
#define MPI_Unpublish_name PMPI_Unpublish_name
#endif

static const char FUNC_NAME[] = "MPI_Unpublish_name";


int MPI_Unpublish_name(const char *service_name, MPI_Info info,
                       const char *port_name)
{
    int rc;
    char range[OPAL_MAX_INFO_VAL];
    int flag=0;
    opal_list_t pinfo;
    opal_value_t *rng;
    char **keys = NULL;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( NULL == port_name ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
        if ( NULL == service_name ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    OBJ_CONSTRUCT(&pinfo, opal_list_t);

    /* OMPI supports info keys to pass the range to
     * be searched for the given key */
    if (MPI_INFO_NULL != info) {
        ompi_info_get (info, "range", sizeof(range) - 1, range, &flag);
        if (flag) {
            if (0 == strcmp(range, "nspace")) {
                rng = OBJ_NEW(opal_value_t);
                rng->key = strdup(OPAL_PMIX_RANGE);
                rng->type = OPAL_INT;
                rng->data.integer = OPAL_PMIX_NAMESPACE;  // share only with procs in same nspace
                opal_list_append(&pinfo, &rng->super);
            } else if (0 == strcmp(range, "session")) {
                rng = OBJ_NEW(opal_value_t);
                rng->key = strdup(OPAL_PMIX_RANGE);
                rng->type = OPAL_INT;
                rng->data.integer = OPAL_PMIX_SESSION; // share only with procs in same session
                opal_list_append(&pinfo, &rng->super);
            } else {
                /* unrecognized scope */
                OPAL_LIST_DESTRUCT(&pinfo);
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                            FUNC_NAME);
            }
        }
    }

    /* unpublish the service_name */
    opal_argv_append_nosize(&keys, service_name);

    rc = opal_pmix.unpublish(keys, &pinfo);
    opal_argv_free(keys);
    OPAL_LIST_DESTRUCT(&pinfo);

    if ( OPAL_SUCCESS != rc ) {
        if (OPAL_ERR_NOT_FOUND == rc) {
            /* service couldn't be found */
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_SERVICE,
                                          FUNC_NAME);
        }
        if (OPAL_ERR_PERM == rc) {
            /* this process didn't own the specified service */
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ACCESS,
                                          FUNC_NAME);
        }

        /* none of the MPI-specific errors occurred - must be some
         * kind of internal error
         */
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN,
                                      FUNC_NAME);
    }

    return MPI_SUCCESS;
}
