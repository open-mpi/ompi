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
#pragma weak MPI_Comm_spawn_multiple = PMPI_Comm_spawn_multiple
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Comm_spawn_multiple(int count, char **array_of_commands, char ***array_of_argv,
                            int *array_of_maxprocs, MPI_Info *array_of_info,
                            int root, MPI_Comm comm, MPI_Comm *intercomm,
                            int *array_of_errcodes) 
{
    int i;
    int rank;
    int totalnumprocs=0;
    
    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                          "MPI_Comm_spawn_multiple");
        if ( MPI_COMM_NULL == comm || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                         "MPI_Comm_spawn_multiple");
        if ( 0 > root || ompi_comm_size(comm) < root ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          "MPI_Comm_spawn_multiple");
        if ( NULL == intercomm ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          "MPI_Comm_spawn_multiple");
        if ( NULL == array_of_errcodes ) 
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          "MPI_Comm_spawn_multiple");
    }
   
   rank = ompi_comm_rank ( comm );
   if ( MPI_PARAM_CHECK ) {
       if ( rank == root ) {
           if ( 0 > count ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                             "MPI_Comm_spawn_multiple");
           if ( NULL == array_of_commands ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             "MPI_Comm_spawn_multiple");
           if ( NULL == array_of_argv ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                         "MPI_Comm_spawn_multiple");
           if ( NULL ==  array_of_maxprocs ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             "MPI_Comm_spawn_multiple");
           if ( NULL == array_of_info ) 
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             "MPI_Comm_spawn_multiple");
           for ( i=0; i<count; i++ ) {
               if ( NULL == array_of_commands[i] ) 
                   return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                                 "MPI_Comm_spawn_multiple");
               if ( NULL == array_of_argv[i] ) 
                   return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                                 "MPI_Comm_spawn_multiple");
               if ( 0 > array_of_maxprocs[i] ) 
                   return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                                 "MPI_Comm_spawn_multiple");
           }
       }
   }

   if ( rank == root ) {
       for ( i=0; i < count; i++ ) {
           totalnumprocs += array_of_maxprocs[i];

           /* parse the info[i] */

           /* check potentially for: 
              - "host": desired host where to spawn the processes
              - "arch": desired architecture
              - "wdir": directory, where executable can be found
              - "path": list of directories where to look for the executable
              - "file": filename, where additional information is provided.
              - "soft": see page 92 of MPI-2.
           */
       }

       /* map potentially array_of_argvs == MPI_ARGVS_NULL to a correct value */
       /* map potentially array_of_argvs[i] == MPI_ARGV_NULL to a correct value */
       /* start processes */
       /* publish name */
       /* accept connection from other group.
          Root in the new application is rank 0 in their COMM_WORLD ? */
       /* unpublish name */
       /* send list of procs from other app */
       /* receive list of procs from other app */
   }
   
   /* bcast totalnumprocs to all processes in comm */
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
