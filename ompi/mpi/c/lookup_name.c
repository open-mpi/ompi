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

#include "opal/class/opal_list.h"
#include "opal/mca/pmix/pmix.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Lookup_name = PMPI_Lookup_name
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Lookup_name";


int MPI_Lookup_name(const char *service_name, MPI_Info info, char *port_name)
{
    char range[OPAL_MAX_INFO_VAL];
    int flag=0, ret;
    opal_pmix_data_range_t rng;
    bool range_given = false;
    opal_list_t results;
    opal_pmix_pdata_t *pdat;

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

    OPAL_CR_ENTER_LIBRARY();

    /* OMPI supports info keys to pass the range to
     * be searched for the given key */
    if (MPI_INFO_NULL != info) {
        ompi_info_get (info, "range", sizeof(range) - 1, range, &flag);
        if (flag) {
            range_given = true;
            if (0 == strcmp(range, "nspace")) {
                rng = OPAL_PMIX_NAMESPACE;  // share only with procs in same nspace
            } else if (0 == strcmp(range, "session")) {
                rng = OPAL_PMIX_SESSION; // share only with procs in same session
            } else {
                /* unrecognized scope */
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                            FUNC_NAME);
            }
        }
    }
    if (!range_given) {
        /* default to nspace */
        rng = OPAL_PMIX_NAMESPACE;
    }

    /* collect the findings */
    OBJ_CONSTRUCT(&results, opal_list_t);
    pdat = OBJ_NEW(opal_pmix_pdata_t);
    pdat->key = strdup(service_name);
    opal_list_append(&results, &pdat->super);

    ret = opal_pmix.lookup(rng, &results);
    if (OPAL_SUCCESS != ret ||
        OPAL_STRING != pdat->value.type ||
        NULL == pdat->value.data.string) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NAME,
                                      FUNC_NAME);
    }

    strncpy ( port_name, pdat->value.data.string, MPI_MAX_PORT_NAME );
    OPAL_LIST_DESTRUCT(&results);

    OPAL_CR_EXIT_LIBRARY();
    return MPI_SUCCESS;
}
