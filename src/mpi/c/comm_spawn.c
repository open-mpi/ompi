/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/oob/oob.h"
#include "runtime/runtime.h"
#include "info/info.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_spawn = PMPI_Comm_spawn
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static char FUNC_NAME[] = "MPI_Comm_spawn";


int MPI_Comm_spawn(char *command, char **argv, int maxprocs, MPI_Info info,
                    int root, MPI_Comm comm, MPI_Comm *intercomm,
                    int *array_of_errcodes) 
{
    int rank, rc, i;
    ompi_communicator_t *comp, *newcomp;
    uint32_t *rprocs=NULL;
    uint32_t lleader=0, rleader=0; /* OOB contact information of me and the other root */
 
    comp = (ompi_communicator_t *) comm;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( MPI_COMM_NULL == comm || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        if ( OMPI_COMM_IS_INTER(comm))
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COMM,
                                          FUNC_NAME);
        if ( 0 > root || ompi_comm_size(comm) < root ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          FUNC_NAME);
        if ( NULL == intercomm ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          FUNC_NAME);
        if ( NULL == array_of_errcodes ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          FUNC_NAME);
    }
   
   rank = ompi_comm_rank ( comm );
   if ( MPI_PARAM_CHECK ) {
       if ( rank == root ) {
           if ( NULL == command ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
           if ( NULL == argv ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
           if ( 0 > maxprocs ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
       }
   }


   /* bcast maxprocs to all processes in comm and allocate the rprocs array*/
   rc = comp->c_coll.coll_bcast ( &maxprocs, 1, MPI_INT, root, comm);
   if ( OMPI_SUCCESS != rc ) {
       goto exit;
   }

   rprocs = (uint32_t *)malloc (maxprocs * sizeof(uint32_t));
   if ( NULL == rprocs ) {
       rc = MPI_ERR_INTERN;
       goto exit;
   }

   if ( rank == root && MPI_INFO_NULL != info ) {
       /* parse the info object */
       
       /* check potentially for: 
          - "host": desired host where to spawn the processes
          - "arch": desired architecture
          - "wdir": directory, where executable can be found
          - "path": list of directories where to look for the executable
          - "file": filename, where additional information is provided.
          - "soft": see page 92 of MPI-2.
       */

       /* map potentially MPI_ARGV_NULL to the value required by the start-cmd */
       /* start processes. if number of processes started != maxprocs 
          return MPI_ERR_SPAWN.*/

       /* publish your name */
       /* accept connection from other group.
          Root in the new application is rank 0 in their COMM_WORLD ? */
       /* unpublish name */

       /* send list of procs to other app */
       /* receive list of procs from other app */
   }

   /* bcast list of remote procs to all processes in comm */
   rc = comp->c_coll.coll_bcast ( &rprocs, maxprocs, MPI_UNSIGNED, root, comm);
   if ( OMPI_SUCCESS != rc ) {
       goto exit;
   }

   /* setup the proc-structures for the new processes */
   for ( i=0; i<maxprocs; i++ ) {
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

   /* set error codes */
    if (MPI_ERRCODES_IGNORE != array_of_errcodes) {
        for ( i=0; i < maxprocs; i++ ) {
            array_of_errcodes[i]=MPI_SUCCESS;
        }
    }

 exit:
    if ( NULL != rprocs) {
        free ( rprocs );
    }
    if ( MPI_SUCCESS != rc ) {
        *intercomm = MPI_COMM_NULL;
        return OMPI_ERRHANDLER_INVOKE(comm, rc, FUNC_NAME);
    }
    
    *intercomm = newcomp;
    return MPI_SUCCESS;
}
