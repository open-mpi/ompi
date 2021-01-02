/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2018 Cisco Systems, Inc.  All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/util/show_help.h"
#include "opal/util/string_copy.h"

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
    int flag=0, ret;
    pmix_status_t rc;
    pmix_pdata_t pdat;
    pmix_info_t pinfo;
    pmix_data_range_t rng;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( NULL == port_name ) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                          FUNC_NAME);
        }
        if ( NULL == service_name ) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                          FUNC_NAME);
        }
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    /* OMPI supports info keys to pass the range to
     * be searched for the given key */
    if (MPI_INFO_NULL != info) {
        opal_cstring_t *info_str;
        ompi_info_get (info, "range", &info_str, &flag);
        if (flag) {
            if (0 == strcmp(info_str->string, "nspace")) {
                rng = PMIX_RANGE_NAMESPACE;  // share only with procs in same nspace
            } else if (0 == strcmp(info_str->string, "session")) {
                rng = PMIX_RANGE_SESSION; // share only with procs in same session
            } else {
                /* unrecognized scope */
                return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                            FUNC_NAME);
            }
            OBJ_RELEASE(info_str);
        }
    }
    PMIX_INFO_LOAD(&pinfo, PMIX_RANGE, &rng, PMIX_DATA_RANGE);

    /* collect the findings */
    PMIX_PDATA_CONSTRUCT(&pdat);
    PMIX_LOAD_KEY(pdat.key, service_name);

    rc = PMIx_Lookup(&pdat, 1, &pinfo, 1);
    PMIX_INFO_DESTRUCT(&pinfo);
    if (PMIX_SUCCESS != rc ||
        PMIX_STRING != pdat.value.type ||
        NULL == pdat.value.data.string) {
        if (PMIX_ERR_NOT_SUPPORTED == rc) {
            ret = OMPI_ERR_NOT_SUPPORTED;
            opal_show_help("help-mpi-api.txt",
                           "MPI function not supported",
                           true,
                           FUNC_NAME,
                           "Underlying runtime environment does not support name lookup functionality");
        } else {
            ret = MPI_ERR_NAME;
        }

        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(ret, FUNC_NAME);
    }

    opal_string_copy( port_name, pdat.value.data.string,
                      MPI_MAX_PORT_NAME );
    PMIX_PDATA_DESTRUCT(&pdat);

    return MPI_SUCCESS;
}
