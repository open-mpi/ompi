/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
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
#include <string.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Intercomm_merge = PMPI_Intercomm_merge
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

#define INTERCOMM_MERGE_TAG 1010

static const char FUNC_NAME[] = "MPI_Intercomm_merge";


int MPI_Intercomm_merge(MPI_Comm intercomm, int high,
                        MPI_Comm *newcomm) 
{
    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_proc_t **procs=NULL;
    int local_size, remote_size;
    int local_rank;
    int first;
    int total_size;
    int rc=MPI_SUCCESS;
    int thigh = high;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME); 

        if (ompi_comm_invalid ( intercomm ) ||
             !( intercomm->c_flags & OMPI_COMM_INTER ) ) 
            return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                            FUNC_NAME);

        if ( NULL == newcomm )
            return OMPI_ERRHANDLER_INVOKE ( intercomm, MPI_ERR_ARG, 
                                            FUNC_NAME);
    }

    local_size  = ompi_comm_size ( intercomm );
    local_rank  = ompi_comm_rank ( intercomm );
    remote_size = ompi_comm_remote_size ( intercomm );
    total_size  = local_size + remote_size;
    procs = (ompi_proc_t **) malloc ( total_size * sizeof(ompi_proc_t *));
    if ( NULL == procs ) {
        rc = MPI_ERR_INTERN;
        goto exit;
    }
    
    first = ompi_comm_determine_first ( intercomm, thigh );
    if ( MPI_UNDEFINED == first ) {
        rc = MPI_ERR_INTERN;
        goto exit;
    }

    if ( first ) {
        memcpy ( procs, intercomm->c_local_group->grp_proc_pointers, 
                 local_size * sizeof(ompi_proc_t *));
        memcpy ( &procs[local_size], intercomm->c_remote_group->grp_proc_pointers, 
                 remote_size * sizeof(ompi_proc_t *));
    }
    else {
        memcpy ( procs, intercomm->c_remote_group->grp_proc_pointers, 
                 remote_size * sizeof(ompi_proc_t *));
        memcpy ( &procs[remote_size], intercomm->c_local_group->grp_proc_pointers, 
                 local_size * sizeof(ompi_proc_t *));
    }
    
    newcomp = ompi_comm_allocate ( total_size, 0 );
    if ( NULL == newcomp ) {
        rc = MPI_ERR_INTERN;
        goto exit;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,              /* new comm */ 
                             intercomm,            /* old comm */
                             NULL,                 /* bridge comm */
                             NULL,                 /* local leader */
                             NULL,                 /* remote_leader */
                             OMPI_COMM_CID_INTER,  /* mode */
                             -1 );                 /* send_first */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    rc = ompi_comm_set ( newcomp,                  /* new comm */
                         intercomm,                /* old comm */
                         total_size,               /* local_size */
                         procs,                    /* local_procs*/
                         0,                        /* remote_size */
                         NULL,                     /* remote_procs */
                         NULL,                     /* attrs */
                         intercomm->error_handler, /* error handler*/
                         NULL                      /* topo mpodule */
                         );
    if ( MPI_SUCCESS != rc ) {
        goto exit;
    }

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate ( newcomp,              /* new comm */ 
                              intercomm,            /* old comm */
                              NULL,                 /* bridge comm */
                              NULL,                 /* local leader */
                              NULL,                 /* remote_leader */
                              OMPI_COMM_CID_INTER,  /* mode */
                              -1,                   /* send_first */
                              NULL );               /* coll module */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }
    

 exit:
    if ( NULL != procs ) {
        free ( procs );
    }
    if ( MPI_SUCCESS != rc ) {
        if ( MPI_COMM_NULL != newcomp ) {
            OBJ_RELEASE(newcomp);
        }
        *newcomm = MPI_COMM_NULL;
        return OMPI_ERRHANDLER_INVOKE(intercomm, rc,  FUNC_NAME);
    }

    *newcomm = newcomp;
    return MPI_SUCCESS;
}
