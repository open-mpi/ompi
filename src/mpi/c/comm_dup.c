/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_dup = PMPI_Comm_dup
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm) {
    
    /* local variables */
    ompi_communicator_t *comp, *newcomp;
    int rsize, mode;
    ompi_proc_t **rprocs;
    
    /* argument checking */
    if ( MPI_PARAM_CHECK ) {
        if (ompi_mpi_finalized)  
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                         "MPI_Comm_dup");
        
        if (MPI_COMM_NULL == comm || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_dup");
        
        if ( NULL == newcomm )
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                         "MPI_Comm_dup");
    }

    comp = (ompi_communicator_t *) comm;
    if ( OMPI_COMM_IS_INTER ( comp ) ){
        rsize  = comp->c_remote_group->grp_proc_count;
        rprocs = comp->c_remote_group->grp_proc_pointers;
        mode   = OMPI_COMM_INTER_INTER;
    }
    else {
        rsize  = 0;
        rprocs = NULL;
        mode   = OMPI_COMM_INTRA_INTRA;
    }

    newcomp = ompi_comm_set ( mode,                                   /* mode */
                             comp,                                   /* old comm */
                             NULL,                                   /* bridge comm */
                             comp->c_local_group->grp_proc_count,    /* local_size */
                             comp->c_local_group->grp_proc_pointers, /* local_procs*/
                             rsize,                                  /* remote_size */
                             rprocs,                                 /* remote_procs */
                             comp->c_keyhash,                        /* attrs */
                             comp->error_handler,                    /* error handler */
                             NULL,                      /* coll module, to be modified */
                             NULL,                      /* topo module, to be modified */
                             MPI_UNDEFINED,                          /* local leader */
                             MPI_UNDEFINED                           /* remote leader */
                             );

    if ( newcomp == MPI_COMM_NULL ) 
        OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_INTERN, "MPI_Comm_dup");

    *newcomm = newcomp;
    return ( MPI_SUCCESS );
}
