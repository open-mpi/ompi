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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Lookup_name = PMPI_Lookup_name
#endif
#define MPI_Lookup_name PMPI_Lookup_name
#endif

static const char FUNC_NAME[] = "MPI_Lookup_name";


int MPI_Lookup_name(const char *service_name, MPI_Info info, char *port_name)
{
    char range[OPAL_MAX_INFO_VAL];
    int flag=0, ret;
    opal_value_t *rng;
    opal_list_t results, pinfo;
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

    /* collect the findings */
    OBJ_CONSTRUCT(&results, opal_list_t);
    pdat = OBJ_NEW(opal_pmix_pdata_t);
    pdat->value.key = strdup(service_name);
    opal_list_append(&results, &pdat->super);

    ret = opal_pmix.lookup(&results, &pinfo);
    OPAL_LIST_DESTRUCT(&pinfo);
    if (OPAL_SUCCESS != ret ||
        OPAL_STRING != pdat->value.type ||
        NULL == pdat->value.data.string) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NAME,
                                      FUNC_NAME);
    }

    strncpy ( port_name, pdat->value.data.string, MPI_MAX_PORT_NAME );
    OPAL_LIST_DESTRUCT(&results);

    return MPI_SUCCESS;
}
