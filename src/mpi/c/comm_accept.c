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
    int rank;

    if ( MPI_PARAM_CHECK ) {
        if ( ompi_mpi_finalized )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, 
                                          "MPI_Comm_accept");
        if ( MPI_COMM_NULL == comm || ompi_comm_invalid (comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
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
       /* parse the info object */
       /* no prefedined values for this function in MPI-2*/
       
       /* accept connection  from other app */
       /* recv number of procs of other app */
       /* recv list of procs of other app */
       /* send number of procs to other app */
       /* send list of process to other app */
   }

   /* bcast number of procs of other app to comm */
   /* bcast list of procs of other app to comm */
   /* setup the proc-structures for the new processes, which are not yet known */
   /* setup the intercomm-structure using ompi_comm_set (); */
   /* PROBLEM: How to determine the new comm-cid ? */
   /* PROBLEM: do we have to re-start some low level stuff
      to enable the usage of fast communication devices
      between the two worlds ? */

    return MPI_SUCCESS;
}
