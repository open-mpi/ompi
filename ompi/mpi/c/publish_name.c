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
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 *
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

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Publish_name = PMPI_Publish_name
#endif
#define MPI_Publish_name PMPI_Publish_name
#endif

static const char FUNC_NAME[] = "MPI_Publish_name";


int MPI_Publish_name(const char *service_name, MPI_Info info,
                     const char *port_name)
{
    int rc;
    char range[OPAL_MAX_INFO_VAL];
    int flag=0;
    opal_value_t *rng;
    opal_list_t values;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( NULL == port_name || 0 == strlen(port_name) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
        if ( NULL == service_name || 0 == strlen(service_name) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    OBJ_CONSTRUCT(&values, opal_list_t);

    /* OMPI supports info keys to pass the range and persistence to
     * be used for the given key */
    if (MPI_INFO_NULL != info) {
        ompi_info_get (info, "range", sizeof(range) - 1, range, &flag);
        if (flag) {
            if (0 == strcmp(range, "nspace")) {
                rng = OBJ_NEW(opal_value_t);
                rng->key = strdup(OPAL_PMIX_RANGE);
                rng->type = OPAL_INT;
                rng->data.integer = OPAL_PMIX_NAMESPACE;  // share only with procs in same nspace
                opal_list_append(&values, &rng->super);
            } else if (0 == strcmp(range, "session")) {
                rng = OBJ_NEW(opal_value_t);
                rng->key = strdup(OPAL_PMIX_RANGE);
                rng->type = OPAL_INT;
                rng->data.integer = OPAL_PMIX_SESSION; // share only with procs in same session
                opal_list_append(&values, &rng->super);
            } else {
                /* unrecognized scope */
                OPAL_LIST_DESTRUCT(&values);
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                            FUNC_NAME);
            }
        }
        ompi_info_get (info, "persistence", sizeof(range) - 1, range, &flag);
        if (flag) {
            if (0 == strcmp(range, "indef")) {
                rng = OBJ_NEW(opal_value_t);
                rng->key = strdup(OPAL_PMIX_PERSISTENCE);
                rng->type = OPAL_INT;
                rng->data.integer = OPAL_PMIX_PERSIST_INDEF;   // retain until specifically deleted
                opal_list_append(&values, &rng->super);
            } else if (0 == strcmp(range, "proc")) {
                rng = OBJ_NEW(opal_value_t);
                rng->key = strdup(OPAL_PMIX_PERSISTENCE);
                rng->type = OPAL_INT;
                rng->data.integer = OPAL_PMIX_PERSIST_PROC;    // retain until publishing process terminates
                opal_list_append(&values, &rng->super);
            } else if (0 == strcmp(range, "app")) {
                rng = OBJ_NEW(opal_value_t);
                rng->key = strdup(OPAL_PMIX_PERSISTENCE);
                rng->type = OPAL_INT;
                rng->data.integer = OPAL_PMIX_PERSIST_APP;     // retain until application terminates
                opal_list_append(&values, &rng->super);
            } else if (0 == strcmp(range, "session")) {
                rng = OBJ_NEW(opal_value_t);
                rng->key = strdup(OPAL_PMIX_PERSISTENCE);
                rng->type = OPAL_INT;
                rng->data.integer = OPAL_PMIX_PERSIST_SESSION; // retain until session/allocation terminates
                opal_list_append(&values, &rng->super);
            } else {
                /* unrecognized persistence */
                OPAL_LIST_DESTRUCT(&values);
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                            FUNC_NAME);
            }
        }
    }

    /* publish the service name */
    rng = OBJ_NEW(opal_value_t);
    rng->key = strdup(service_name);
    rng->type = OPAL_STRING;
    rng->data.string = strdup(port_name);
    opal_list_append(&values, &rng->super);

    rc = opal_pmix.publish(&values);
    OPAL_LIST_DESTRUCT(&values);

    if ( OPAL_SUCCESS != rc ) {
        if (OPAL_EXISTS == rc) {
            /* already exists - can't publish it */
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_FILE_EXISTS,
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
