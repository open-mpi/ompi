/*
 * $HEADER$
 */

#ifndef OMPI_ERRHANDLER_PREDEFINED_H
#define OMPI_ERRHANDLER_PREDEFINED_H


/**
 * Handler function for MPI_ERRORS_ARE_FATAL
 */
void ompi_mpi_errors_are_fatal_handler(struct ompi_communicator_t **comm,
                                      int *error_code, ...);

/**
 * Handler function for MPI_ERRORS_RETURN
 */
void ompi_mpi_errors_return_handler(struct ompi_communicator_t **comm,
                                   int *error_code, ...);

#endif /* OMPI_ERRHANDLER_PREDEFINED_H */
