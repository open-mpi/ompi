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

static const char FUNC_NAME[] = "MPI_Comm_spawn_multiple";


int MPI_Comm_spawn_multiple(int count, char **array_of_commands, char ***array_of_argv,
                            int *array_of_maxprocs, MPI_Info *array_of_info,
                            int root, MPI_Comm comm, MPI_Comm *intercomm,
                            int *array_of_errcodes) 
{
    int i, rc, rank;
    int totalnumprocs=0;
    uint32_t *rprocs=NULL;
    ompi_communicator_t *comp, *newcomp;
    uint32_t lleader=0, rleader=0; /* OOB contact information of root and the other root */

    comp = (ompi_communicator_t *) comm;

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
   }

   /* bcast totalnumprocs to all processes in comm and allocate the rprocs array*/
   rc = comp->c_coll.coll_bcast ( &totalnumprocs, 1, MPI_INT, root, comm);
   if ( OMPI_SUCCESS != rc ) {
       goto exit;
   }

   rprocs = (uint32_t *)malloc (totalnumprocs * sizeof(uint32_t));
   if ( NULL == rprocs ) {
       rc = MPI_ERR_INTERN;
       goto exit;
   }

   if ( rank == root ) {
       /* map potentially array_of_argvs == MPI_ARGVS_NULL to a correct value */
       /* map potentially array_of_argvs[i] == MPI_ARGV_NULL to a correct value.
          not required by the standard. */
       /* start processes */

       /* publish name */
       /* accept connection from other group.
          Root in the new application is rank 0 in their COMM_WORLD ? */
       /* unpublish name */

       /* send list of procs from other app */
       /* receive list of procs from other app */
   }
   
   /* bcast list of remote procs to all processes in comm */
   rc = comp->c_coll.coll_bcast ( &rprocs, totalnumprocs, MPI_UNSIGNED, root, comm);
   if ( OMPI_SUCCESS != rc ) {
       goto exit;
   }

   /* setup the proc-structures for the new processes */
   for ( i=0; i < totalnumprocs; i++ ) {
   }

   /* setup the intercomm-structure using ompi_comm_set (); */
    newcomp = ompi_comm_set ( comp,                                   /* old comm */
                              comp->c_local_group->grp_proc_count,    /* local_size */
                              comp->c_local_group->grp_proc_pointers, /* local_procs*/
                              totalnumprocs,                          /* remote_size */
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

   /* set array of errorcodes */
    if (MPI_ERRCODES_IGNORE != array_of_errcodes) {
        for ( i=0; i < totalnumprocs; i++ ) {
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


    return MPI_SUCCESS;
}
