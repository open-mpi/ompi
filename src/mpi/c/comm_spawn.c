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

static const char FUNC_NAME[] = "MPI_Comm_spawn";


int MPI_Comm_spawn(char *command, char **argv, int maxprocs, MPI_Info info,
                    int root, MPI_Comm comm, MPI_Comm *intercomm,
                    int *array_of_errcodes) 
{
    int rank, rc, i;
    int send_first=0; /* we wait to be contacted */
    ompi_communicator_t *newcomp;
 
    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( MPI_COMM_NULL == comm || ompi_comm_invalid (comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        }
        if ( OMPI_COMM_IS_INTER(comm)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COMM,
                                          FUNC_NAME);
        }
        if ( 0 > root || ompi_comm_size(comm) < root ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if ( NULL == intercomm ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
        if ( NULL == array_of_errcodes ) {
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
         if ( NULL == command ) {
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
         }
         if ( NULL == argv ) {
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
         }
         if ( 0 > maxprocs ) {
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
         }
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
       /* start processes. if number of processes started != maxprocs 
          return MPI_ERR_SPAWN.*/

       /* publish your name. this should be based on the jobid of the 
          children, to support the scenario of having several
          spawns of non-interleaving communicators working */
       
       /* rc = ompi_comm_namepublish (service_name, port_name ); */
   }

   rc = ompi_comm_connect_accept (comm, root, NULL, send_first, &newcomp);

   if ( rank == root ) {
       /* unpublish name */
   }

   /* set error codes */
    if (MPI_ERRCODES_IGNORE != array_of_errcodes) {
        for ( i=0; i < maxprocs; i++ ) {
            array_of_errcodes[i]=rc;
        }
    }

    *intercomm = newcomp;
    OMPI_ERRHANDLER_RETURN (rc, comm, rc, FUNC_NAME);
}
