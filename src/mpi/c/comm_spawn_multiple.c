/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "info/info.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_spawn_multiple = PMPI_Comm_spawn_multiple
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_spawn_multiple";


int MPI_Comm_spawn_multiple(int count, char **array_of_commands, char ***array_of_argv,
                            int *array_of_maxprocs, MPI_Info *array_of_info,
                            int root, MPI_Comm comm, MPI_Comm *intercomm,
                            int *array_of_errcodes) 
{
    int i, rc, rank, tag;
    int totalnumprocs=0;
    ompi_communicator_t *newcomp;
    int send_first=0; /* they are contacting us first */

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
        if (NULL == array_of_info) {
          return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                        FUNC_NAME);
        }
        for (i = 0; i < count; ++i) {
          if (NULL == array_of_info[i] || 
              ompi_info_is_freed(array_of_info[i])) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
          }
        }
    }
   
   rank = ompi_comm_rank ( comm );
   if ( MPI_PARAM_CHECK ) {
       if ( rank == root ) {
         if ( 0 > count ) {
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                             FUNC_NAME);
         }
         if ( NULL == array_of_commands ) {
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
         }
         if ( NULL == array_of_argv ) {
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
         }
         if ( NULL ==  array_of_maxprocs ) {
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
         }
         if ( NULL == array_of_info ) {
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
         }
         for ( i=0; i<count; i++ ) {
           if ( NULL == array_of_commands[i] ) {
                   return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                                 FUNC_NAME);
           }
           if ( NULL == array_of_argv[i] ) {
                   return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                                 FUNC_NAME);
           }
           if ( 0 > array_of_maxprocs[i] ) {
                   return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                                 FUNC_NAME);
           }
         }
       }
   }

   if ( rank == root ) {
       for ( i=0; i < count; i++ ) {
           totalnumprocs += array_of_maxprocs[i];
       }
       /* parse the info[i] */

       /* check potentially for: 
          - "host": desired host where to spawn the processes
          - "arch": desired architecture
          - "wdir": directory, where executable can be found
          - "path": list of directories where to look for the executable
          - "file": filename, where additional information is provided.
          - "soft": see page 92 of MPI-2.
       */
       
       /* map potentially array_of_argvs == MPI_ARGVS_NULL to a correct value */
       /* map potentially array_of_argvs[i] == MPI_ARGV_NULL to a correct value.
          not required by the standard. */
       /* start processes */
       
       /* publish name, which should be based on the jobid of the children */
       
       /* rc = ompi_comm_namepublish (service_name, port_name ); */
   }

   rc = ompi_comm_connect_accept (comm, root, NULL, send_first, &newcomp, tag);

   if ( rank == root ) {
       /* unpublish name */
   }

   /* set array of errorcodes */
   if (MPI_ERRCODES_IGNORE != array_of_errcodes) {
       for ( i=0; i < totalnumprocs; i++ ) {
           array_of_errcodes[i]=rc;
       }
   }

   *intercomm = newcomp;
   OMPI_ERRHANDLER_RETURN (rc, comm, rc, FUNC_NAME);
}
