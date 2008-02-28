/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/info/info.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/memchecker.h"

#include "orte/util/name_fns.h"
#include "opal/dss/dss.h"
#include "orte/runtime/orte_globals.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_connect = PMPI_Comm_connect
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_connect";


int MPI_Comm_connect(char *port_name, MPI_Info info, int root,
                     MPI_Comm comm, MPI_Comm *newcomm) 
{
    int rank, rc;
    int send_first=1;   /* yes, we are the active part in this game */
    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    orte_process_name_t port_proc_name;
    char *tmp_port=NULL;
    orte_rml_tag_t tag;

    MEMCHECKER(
        memchecker_comm(comm);
    );

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (ompi_comm_invalid (comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        }
        if ( OMPI_COMM_IS_INTER(comm)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COMM,
                                          FUNC_NAME);
        }
        if ( (0 > root) || (ompi_comm_size(comm) <= root) ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if ( NULL == newcomm ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if (NULL == info || ompi_info_is_freed(info)) {
          return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                        FUNC_NAME);
        }
    }
    
    rank = ompi_comm_rank ( comm );
    if ( MPI_PARAM_CHECK ) {
        if ( rank == root ) {
            if ( NULL == port_name ) 
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                              FUNC_NAME);
        }
    }

    /* parse info object. No prefedined values for this function in MPI-2,
     * so lets ignore it for the moment.
     *
     * if ( rank == root && MPI_INFO_NULL != info ) {
     * }
     */

    OPAL_CR_ENTER_LIBRARY();

    /* 
     * translate the port_name string into the according process_name_t 
     * structure. 
     */ 
    if ( rank == root ) { 
        tmp_port = ompi_dpm.parse_port (port_name, &tag);
        if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_process_name(&port_proc_name, tmp_port))) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_PORT, FUNC_NAME);
        }
        if ( OPAL_EQUAL == opal_dss.compare(&port_proc_name, ORTE_NAME_INVALID, ORTE_NAME) ) {
            *newcomm = MPI_COMM_NULL;
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_PORT, FUNC_NAME);
        }
        free (tmp_port);
    }

    rc = ompi_dpm.connect_accept (comm, root, &port_proc_name, send_first,
                                   &newcomp, tag);
    
    *newcomm = newcomp;
    OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
}
