/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "info/info.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_accept = PMPI_Comm_accept
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_accept(char *port_name, MPI_Info info, int root,
                    MPI_Comm comm, MPI_Comm *newcomm) 
{
    int rank, i, rc;
    int maxprocs;
    uint32_t *rprocs=NULL;
    uint32_t lleader=0, rleader=0; /* OOB contact information of our and other root */
    ompi_communicator_t *comp, *newcomp;

    comp = (ompi_communicator_t *) comm;

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                          "MPI_Comm_accept");
        if ( MPI_COMM_NULL == comm || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_accept");
        if ( OMPI_COMM_IS_INTER(comm))
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COMM,
                                          "MPI_Comm_accept");
        if ( 0 > root || ompi_comm_size(comm) < root ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          "MPI_Comm_accept");
        if ( NULL == newcomm )
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          "MPI_Comm_accept");
    }
    
    rank = ompi_comm_rank ( comm );
    if ( MPI_PARAM_CHECK ) {
        if ( rank == root ) {
            if ( NULL == port_name ) 
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                              "MPI_Comm_accept");
        }
    }


    if ( rank == root && MPI_INFO_NULL != info ) {
        /* parse info object. no prefedined values for this function in MPI-2 */
        
        /* accept connection  from other app */
        
        /* recv number of procs (maxprocs) of other app */
        rprocs = (uint32_t *)malloc (maxprocs * sizeof(uint32_t));
        if ( NULL == rprocs ) {
            rc = MPI_ERR_INTERN;
            goto exit;
        }
        
        /* recv list of procs of other app */
       /* send number of procs to other app */
       /* send list of process to other app */
    }
    
    /* bcast maxprocs to all processes in comm and allocate the rprocs array*/
    rc = comp->c_coll.coll_bcast_intra ( &maxprocs, 1, MPI_INT, root, comm);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }
    
    if ( rank != root ) {
        rprocs = (uint32_t *)malloc (maxprocs * sizeof(uint32_t));
        if ( NULL == rprocs ) {
            rc = MPI_ERR_INTERN;
            goto exit;
        }
    }
    
    /* bcast list of remote procs to all processes in comm */
    rc = comp->c_coll.coll_bcast_intra ( &rprocs, maxprocs, MPI_UNSIGNED, root, comm);
    if ( OMPI_SUCCESS != rc ) {
       goto exit;
    }
    
    /* setup the proc-structures for the new processes, which are not yet known */
    for ( i=0; i<maxprocs; i++ ) {
        /* if process rprocs[i] not yet in our list, add it. */
    }
    
    /* setup the intercomm-structure using ompi_comm_set (); */
    newcomp = ompi_comm_set ( comp,                                   /* old comm */
                              comp->c_local_group->grp_proc_count,    /* local_size */
                              comp->c_local_group->grp_proc_pointers, /* local_procs*/
                              maxprocs,                               /* remote_size */
                              rprocs,                                 /* remote_procs */
                              NULL,                                   /* attrs */
                              comp->error_handler,                    /* error handler */
                              NULL,                                   /* coll module */
                              NULL                                    /* topo module */
                              );
    if ( MPI_COMM_NULL == newcomp ) { 
        goto exit;
    }
    
    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,                   /* new comm */ 
                             comp,                      /* old comm */
                             NULL,                      /* bridge comm */
                             &lleader,                  /* local leader */
                             &rleader,                  /* remote_leader */
                             OMPI_COMM_CID_INTRA_OOB ); /* mode */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }
    
    /* PROBLEM: do we have to re-start some low level stuff
       to enable the usage of fast communication devices
       between the two worlds ? */
    
 exit:
    if ( NULL != rprocs ) {
        free ( rprocs );
    }
    if ( MPI_SUCCESS != rc ) {
        *newcomm = MPI_COMM_NULL;
        return OMPI_ERRHANDLER_INVOKE(comm, rc, "MPI_Comm_accept");
    }
    
    *newcomm = newcomp;
    return MPI_SUCCESS;
}
