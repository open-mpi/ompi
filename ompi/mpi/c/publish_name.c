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
#include "opal/util/show_help.h"

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
    int ret;
    opal_cstring_t *info_str;
    int flag=0;
    pmix_status_t rc;
    pmix_info_t pinfo[3];
    pmix_data_range_t rng = PMIX_RANGE_SESSION;
    pmix_persistence_t pers = PMIX_PERSIST_SESSION;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( NULL == port_name || 0 == strlen(port_name) ) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                          FUNC_NAME);
        }
        if ( NULL == service_name || 0 == strlen(service_name) ) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                          FUNC_NAME);
        }
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    /* OMPI supports info keys to pass the range and persistence to
     * be used for the given key */
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
        ompi_info_get (info, "persistence", &info_str, &flag);
        if (flag) {
            if (0 == strcmp(info_str->string, "indef")) {
                pers = PMIX_PERSIST_INDEF;   // retain until specifically deleted
            } else if (0 == strcmp(info_str->string, "proc")) {
                pers = PMIX_PERSIST_PROC;    // retain until publishing process terminates
            } else if (0 == strcmp(info_str->string, "app")) {
                pers = PMIX_PERSIST_APP;     // retain until application terminates
            } else if (0 == strcmp(info_str->string, "session")) {
                pers = PMIX_PERSIST_SESSION; // retain until session/allocation terminates
            } else {
                /* unrecognized persistence */
                return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                            FUNC_NAME);
            }
            OBJ_RELEASE(info_str);
        }
    }

    /* publish the service name */
    PMIX_INFO_LOAD(&pinfo[0], service_name, port_name, PMIX_STRING);
    PMIX_INFO_LOAD(&pinfo[1], PMIX_RANGE, &rng, PMIX_DATA_RANGE);
    PMIX_INFO_LOAD(&pinfo[2], PMIX_PERSISTENCE, &pers, PMIX_PERSIST);

    rc = PMIx_Publish(pinfo, 3);
    PMIX_INFO_DESTRUCT(&pinfo[0]);
    PMIX_INFO_DESTRUCT(&pinfo[1]);
    PMIX_INFO_DESTRUCT(&pinfo[2]);

    if ( PMIX_SUCCESS != rc ) {
        if (PMIX_EXISTS == rc) {
            /* already exists - can't publish it */
            ret = MPI_ERR_FILE_EXISTS;
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
