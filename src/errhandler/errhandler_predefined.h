/*
 * $HEADER$
 */

#ifndef LAM_ERRHANDLER_PREDEFINED_H
#define LAM_ERRHANDLER_PREDEFINED_H

#include "errhandler/errhandler.h"
#include "communicator/communicator.h"


/**
 * Handler function for MPI_ERRORS_ARE_FATAL
 */
void lam_mpi_errors_are_fatal_handler(lam_communicator_t **comm,
                                      int *error_code, ...);

/**
 * Handler function for MPI_ERRORS_RETURN
 */
void lam_mpi_errors_return_handler(lam_communicator_t **comm,
                                   int *error_code, ...);



#endif /* LAM_ERRHANDLER_PREDEFINED_H */
