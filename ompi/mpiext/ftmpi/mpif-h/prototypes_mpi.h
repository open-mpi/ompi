/*
 * Copyright (c) 2019-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * This file prototypes all MPI fortran functions in all four fortran
 * symbol conventions as well as all the internal real OMPI wrapper
 * functions (different from any of the four fortran symbol
 * conventions for clarity, at the cost of more typing for me...).
 * This file is included in the top-level build ONLY. The prototyping
 * is done ONLY for MPI_* bindings
 *
 * Zeroth, the OMPI wrapper functions, with a ompi_ prefix and _f
 * suffix.
 *
 * This is needed ONLY if the lower-level prototypes_pmpi.h has not
 * already been included.
 *
 * Note about function pointers: all function pointers are prototyped
 * here as (void*) rather than including the .h file that defines the
 * proper type (e.g., "op/op.h" defines ompi_op_fortran_handler_fn_t,
 * which is the function pointer type for fortran op callback
 * functions).  This is because there is no type checking coming in
 * from fortran, so why bother?  Also, including "op/op.h" (and
 * friends) makes the all the f77 bindings files dependant on these
 * files -- any change to any one of them will cause the recompilation
 * of the entire set of f77 bindings (ugh!).
 */

#ifndef OMPI_F77_PROTOTYPES_MPIEXT_FTMPI_H
#define OMPI_F77_PROTOTYPES_MPIEXT_FTMPI_H

#include "ompi/mpi/fortran/mpif-h/prototypes_mpi.h"

BEGIN_C_DECLS

PN2(void, MPIX_Comm_agree, mpix_comm_agree, MPIX_COMM_AGREE, (MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *ierr));
PN2(void, MPIX_Comm_failure_ack, mpix_comm_failure_ack, MPIX_COMM_FAILURE_ACK, (MPI_Fint *comm, MPI_Fint *ierr));
PN2(void, MPIX_Comm_failure_get_acked, mpix_comm_failure_get_acked, MPIX_COMM_FAILURE_GET_ACKED, (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr));
PN2(void, MPIX_Comm_iagree, mpix_comm_iagree, MPIX_COMM_IAGREE, (MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Comm_is_revoked, mpix_comm_is_revoked, MPIX_COMM_IS_REVOKED, (MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *ierr));
PN2(void, MPIX_Comm_revoke, mpix_comm_revoke, MPIX_COMM_REVOKE, (MPI_Fint *comm, MPI_Fint *ierr));
PN2(void, MPIX_Comm_shrink, mpix_comm_shrink, MPIX_COMM_SHRINK, (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr));
PN2(void, MPIX_Comm_get_failed, mpix_comm_get_failed, MPIX_COMM_GET_FAILED, (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr));
PN2(void, MPIX_Comm_ack_failed, mpix_comm_ack_failed, MPI_COMM_ACK_FAILED, (MPI_Fint *comm, MPI_Fint *num_to_ack, MPI_Fint *num_acked, MPI_Fint *ierr));


END_C_DECLS

#endif  /* OMPI_F77_PROTOTYPES_MPIEXT_FTMPI_H */
