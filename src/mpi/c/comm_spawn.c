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
#pragma weak MPI_Comm_spawn = PMPI_Comm_spawn
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_spawn(char *command, char **argv, int maxprocs, MPI_Info info,
                    int root, MPI_Comm comm, MPI_Comm *intercomm,
                    int *array_of_errcodes) 
{
    int rank;
    
    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                          "MPI_Comm_spawn");

        if ( MPI_COMM_NULL == comm || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_spawn");

        if ( 0 > root || ompi_comm_size(comm) < root ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          "MPI_Comm_spawn");

        if ( NULL == intercomm ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          "MPI_Comm_spawn");

        if ( NULL == array_of_errcodes ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          "MPI_Comm_spawn");
    }
   
   rank = ompi_comm_rank ( comm );
   if ( MPI_PARAM_CHECK ) {
       if ( rank == root ) {
           if ( NULL == command ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             "MPI_Comm_spawn");
           
           if ( NULL == argv ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                         "MPI_Comm_spawn");
           
           if ( 0 > maxprocs ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             "MPI_Comm_spawn");
       }
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
       /* start processes */
       /* publish your name */
       /* accept connection from other group.
          Root in the new application is rank 0 in their COMM_WORLD ? */
       /* unpublish name */
       /* send list of procs to other app */
       /* receive list of procs from other app */
   }

   /* bcast maxprocs to all processes in comm */
   /* bcast list of remote procs to all processes in comm */
   /* setup the proc-structures for the new processes */
   /* setup the intercomm-structure using ompi_comm_set (); */
   /* PROBLEM: How to determine the new comm-cid ? */
   /* PROBLEM: do we have to re-start some low level stuff
      to enable the usage of fast communication devices
      between the two worlds ? */

   /* set array of errorcodes */

    return MPI_SUCCESS;
}
