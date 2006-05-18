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

#include "ompi/mpi/c/bindings.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Intercomm_create = PMPI_Intercomm_create
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Intercomm_create";


int MPI_Intercomm_create(MPI_Comm local_comm, int local_leader,
                         MPI_Comm bridge_comm, int remote_leader,
                         int tag, MPI_Comm *newintercomm) 
{
    int local_size=0, local_rank=0;
    int lleader=0, rleader=0;
    ompi_communicator_t *newcomp=NULL;
    struct ompi_proc_t **rprocs=NULL;
    int rc=0, rsize=0;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME); 

        if ( ompi_comm_invalid ( local_comm ) ||
             ( local_comm->c_flags & OMPI_COMM_INTER ) ) 
            return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                            FUNC_NAME);

        if ( NULL == newintercomm )
            return OMPI_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG, 
                                            FUNC_NAME);
        
        /* if ( tag < 0 || tag > MPI_TAG_UB )
             return OMPI_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG, 
                                             FUNC_NAME);
        */
    }
    
    local_size = ompi_comm_size ( local_comm );
    local_rank = ompi_comm_rank ( local_comm );
    lleader = local_leader;
    rleader = remote_leader;

    if ( MPI_PARAM_CHECK ) {
        if ( (0 > local_leader) || (local_leader >= local_size) ) 
            return OMPI_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG, 
                                            FUNC_NAME);

        /* remember that the remote_leader and bridge_comm arguments
           just have to be valid at the local_leader */
        if ( local_rank == local_leader ) {
            if ( ompi_comm_invalid ( bridge_comm ) ||
                 (bridge_comm->c_flags & OMPI_COMM_INTER) ) {
                return OMPI_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_COMM, 
                                                FUNC_NAME);
            }            
            if ( (remote_leader < 0) || (remote_leader >= ompi_comm_size(bridge_comm))) {
                return OMPI_ERRHANDLER_INVOKE ( local_comm, MPI_ERR_ARG,
                                                FUNC_NAME);
            }
        } /* if ( local_rank == local_leader ) */
    }

    if ( local_rank == local_leader ) {
        MPI_Request req;
        
        /* local leader exchange group sizes lists */
        rc = MCA_PML_CALL(irecv(&rsize, 1, MPI_INT, rleader, tag, bridge_comm,
                                &req));
        if ( rc != MPI_SUCCESS ) {
            goto err_exit;
        }
        rc = MCA_PML_CALL(send (&local_size, 1, MPI_INT, rleader, tag,
                                MCA_PML_BASE_SEND_STANDARD, bridge_comm));
        if ( rc != MPI_SUCCESS ) {
            goto err_exit;
        }
        rc = ompi_request_wait_all ( 1, &req, MPI_STATUS_IGNORE);
        if ( rc != MPI_SUCCESS ) {
            goto err_exit;
        }
    }
    
    /* bcast size and list of remote processes to all processes in local_comm */
    rc = local_comm->c_coll.coll_bcast ( &rsize, 1, MPI_INT, lleader, 
                                         local_comm );
    if ( rc != MPI_SUCCESS ) {
        goto err_exit;
    }

    rprocs = ompi_comm_get_rprocs ( local_comm, bridge_comm, lleader,
                                   remote_leader, tag, rsize );
    if ( NULL == rprocs ) {
        goto err_exit;
    }

    if ( MPI_PARAM_CHECK ) {
	rc = ompi_comm_overlapping_groups(local_comm->c_local_group->grp_proc_count,
					  local_comm->c_local_group->grp_proc_pointers,
					  rsize,
					  rprocs);
	if ( OMPI_SUCCESS != rc ) {
	    goto err_exit;
	}
    }

    newcomp = ompi_comm_allocate ( local_comm->c_local_group->grp_proc_count, rsize);
    if ( NULL == newcomp ) {
        rc = MPI_ERR_INTERN;
        goto err_exit;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,                     /* new comm */ 
                             local_comm,                  /* old comm */
                             bridge_comm,                 /* bridge comm */
                             &lleader,                    /* local leader */
                             &rleader,                    /* remote_leader */
                             OMPI_COMM_CID_INTRA_BRIDGE,  /* mode */
                             -1 );                        /* send_first */

    if ( MPI_SUCCESS != rc ) {
        goto err_exit;
    }

    rc = ompi_comm_set ( newcomp,                                      /* new comm */
                         local_comm,                                   /* old comm */
                         local_comm->c_local_group->grp_proc_count,    /* local_size */
                         local_comm->c_local_group->grp_proc_pointers, /* local_procs*/
                         rsize,                                        /* remote_size */
                         rprocs,                                       /* remote_procs */
                         NULL,                                         /* attrs */
                         local_comm->error_handler,                    /* error handler*/
                         NULL                                          /* topo mpodule */
                         );
    if ( MPI_SUCCESS != rc ) {
        goto err_exit;
    }

    /* activate comm and init coll-module */
    rc = ompi_comm_activate ( newcomp,                     /* new comm */ 
                              local_comm,                  /* old comm */
                              bridge_comm,                 /* bridge comm */
                              &lleader,                    /* local leader */
                              &rleader,                    /* remote_leader */
                              OMPI_COMM_CID_INTRA_BRIDGE,  /* mode */
                              -1,                          /* send_first */
                              NULL );                      /* coll component */

    if ( MPI_SUCCESS != rc ) {
        goto err_exit;
    }
    
 err_exit:
    if ( NULL != rprocs ) {
        free ( rprocs );
    }
    if ( OMPI_SUCCESS != rc ) {
        *newintercomm = MPI_COMM_NULL;
        return OMPI_ERRHANDLER_INVOKE(local_comm, MPI_ERR_INTERN,
                                      FUNC_NAME);
    }

    *newintercomm = newcomp;
    return MPI_SUCCESS;
}
