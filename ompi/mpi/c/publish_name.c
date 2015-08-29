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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Publish_name = PMPI_Publish_name
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Publish_name";


int MPI_Publish_name(const char *service_name, MPI_Info info,
                     const char *port_name)
{
    int rc;
    char range[OPAL_MAX_INFO_VAL];
    int flag=0;
    opal_pmix_data_range_t rng;
    bool range_given = false;
    opal_pmix_persistence_t persist;
    bool persistence_given = false;
    opal_list_t values;
    opal_pmix_info_t *pinfo;

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

    OPAL_CR_ENTER_LIBRARY();

    /* OMPI supports info keys to pass the range and persistence to
     * be used for the given key */
    if (MPI_INFO_NULL != info) {
        ompi_info_get (info, "range", sizeof(range) - 1, range, &flag);
        if (flag) {
            range_given = true;
            if (0 == strcmp(range, "nspace")) {
                rng = OPAL_PMIX_NAMESPACE;  // share only with procs in same nspace
            } else if (0 == strcmp(range, "session")) {
                rng = OPAL_PMIX_SESSION; // share only with procs in same session
            } else {
                /* unrecognized range */
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                            FUNC_NAME);
            }
        }
        ompi_info_get (info, "persistence", sizeof(range) - 1, range, &flag);
        if (flag) {
            persistence_given = true;
            if (0 == strcmp(range, "indef")) {
                persist = OPAL_PMIX_PERSIST_INDEF;   // retain until specifically deleted
            } else if (0 == strcmp(range, "proc")) {
                persist = OPAL_PMIX_PERSIST_PROC;    // retain until publishing process terminates
            } else if (0 == strcmp(range, "app")) {
                persist = OPAL_PMIX_PERSIST_APP;     // retain until application terminates
            } else if (0 == strcmp(range, "session")) {
                persist = OPAL_PMIX_PERSIST_SESSION; // retain until session/allocation terminates
            } else {
                /* unrecognized persistence */
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                            FUNC_NAME);
            }
        }
    }
    if (!range_given) {
        /* default to nspace */
        rng = OPAL_PMIX_NAMESPACE;
    }
    if (!persistence_given) {
        persist = OPAL_PMIX_PERSIST_APP;
    }

    /* publish the values */
    OBJ_CONSTRUCT(&values, opal_list_t);
    pinfo = OBJ_NEW(opal_pmix_info_t);
    pinfo->key = strdup(service_name);
    pinfo->value.type = OPAL_STRING;
    pinfo->value.data.string = strdup(port_name);
    opal_list_append(&values, &pinfo->super);

    rc = opal_pmix.publish(rng, persist, &values);
    OPAL_LIST_DESTRUCT(&values);

    OPAL_CR_EXIT_LIBRARY();
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
