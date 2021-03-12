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
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "opal/class/opal_list.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"

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
    int ret;
    opal_cstring_t *info_str;
    int flag=0;
    pmix_status_t rc;
    pmix_info_t pinfo;
    pmix_data_range_t rng = PMIX_RANGE_SESSION;
    char **keys = NULL;

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

    /* unpublish the service_name */
    opal_argv_append_nosize(&keys, service_name);
    PMIX_INFO_LOAD(&pinfo, PMIX_RANGE, &rng, PMIX_DATA_RANGE);

    rc = PMIx_Unpublish(keys, &pinfo, 1);
    opal_argv_free(keys);
    PMIX_INFO_DESTRUCT(&pinfo);

    if ( PMIX_SUCCESS != rc ) {
        if (PMIX_ERR_NOT_FOUND == rc) {
            /* service couldn't be found */
            ret = MPI_ERR_SERVICE;
        } else if (PMIX_ERR_NO_PERMISSIONS == rc) {
            /* this process didn't own the specified service */
            ret = MPI_ERR_ACCESS;
        } else if (PMIX_ERR_NOT_SUPPORTED == rc) {
            /* this PMIX environment doesn't support publishing */
            ret = OMPI_ERR_NOT_SUPPORTED;
            opal_show_help("help-mpi-api.txt",
                           "MPI function not supported",
                           true,
                           FUNC_NAME,
                           "Underlying runtime environment does not support name publishing functionality");
        } else {
            ret = MPI_ERR_INTERN;
        }

        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(ret, FUNC_NAME);
    }

    return MPI_SUCCESS;
}
