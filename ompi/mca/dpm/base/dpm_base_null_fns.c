/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 University of Houston. All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2007      Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>
#include <time.h>
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/dpm/base/base.h"


int ompi_dpm_base_null_connect_accept (ompi_communicator_t *comm, int root,
                                       const char *port_string, bool send_first,
                                       ompi_communicator_t **newcomm)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_disconnect(ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}

int ompi_dpm_base_null_spawn(int count, const char *array_of_commands[],
                             char **array_of_argv[],
                             const int array_of_maxprocs[],
                             const MPI_Info array_of_info[],
                             const char *port_name)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_dyn_init(void)
{
    return OMPI_SUCCESS;
}

int ompi_dpm_base_null_dyn_finalize (void)
{
    return OMPI_SUCCESS;
}

void ompi_dpm_base_null_mark_dyncomm (ompi_communicator_t *comm)
{
    return;
}

int ompi_dpm_base_null_open_port(char *port_name, ompi_rml_tag_t given_tag)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_parse_port(const char *port_name,
                                  char **hnp_uri, char **rml_uri, ompi_rml_tag_t *tag)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_route_to_port(char *rml_uri, ompi_process_name_t *rproc)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_close_port(const char *port_name)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_pconnect(char *port,
                                struct timeval *timeout,
                                ompi_dpm_base_paccept_connect_callback_fn_t cbfunc,
                                void *cbdata)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_paccept(char *port,
                               ompi_dpm_base_paccept_connect_callback_fn_t cbfunc,
                               void *cbdata)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

void ompi_dpm_base_null_pclose(char *port)
{
    return;
}
