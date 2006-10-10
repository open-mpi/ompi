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
#include <stdio.h>

#include "opal/util/show_help.h"
#include "ompi/info/info.h"
#include "ompi/mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_spawn = PMPI_Comm_spawn
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_spawn";


int MPI_Comm_spawn(char *command, char **argv, int maxprocs, MPI_Info info,
                    int root, MPI_Comm comm, MPI_Comm *intercomm,
                    int *array_of_errcodes) 
{
    int rank, rc, i;
    int send_first=0; /* we wait to be contacted */
    ompi_communicator_t *newcomp;
    char port_name[MPI_MAX_PORT_NAME];
    char *tmp_port;
    orte_rml_tag_t tag;
 
    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( ompi_comm_invalid (comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        }
        if ( OMPI_COMM_IS_INTER(comm)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COMM,
                                          FUNC_NAME);
        }
        if ( (0 > root) || (ompi_comm_size(comm) <= root) ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if ( NULL == intercomm ) {
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
         if ( 0 > maxprocs ) {
               return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                             FUNC_NAME);
         }
       }
   }


   if ( rank == root ) {
       /* Open a port. The port_name is passed as an environment variable
	  to the children. */
       ompi_open_port (port_name);
       ompi_comm_start_processes (1, &command, &argv, &maxprocs, 
                                  &info, port_name);
       tmp_port = ompi_parse_port (port_name, &tag);
       free(tmp_port);
   }
   
   
   rc = ompi_comm_connect_accept (comm, root, NULL, send_first, &newcomp, tag);

   /* close the port again. Nothing has to be done for that at the moment.*/

   /* set error codes */
    if (MPI_ERRCODES_IGNORE != array_of_errcodes) {
        for ( i=0; i < maxprocs; i++ ) {
            array_of_errcodes[i]=rc;
        }
    }

    *intercomm = newcomp;
    OMPI_ERRHANDLER_RETURN (rc, comm, rc, FUNC_NAME);
}
