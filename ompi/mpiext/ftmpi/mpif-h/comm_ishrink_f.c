/*
 * Copyright (c) 2010-2022 The University of Tennessee and the University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */
#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#include "ompi/mpiext/ftmpi/c/mpiext_ftmpi_c.h"
#include "ompi/mpiext/ftmpi/mpif-h/prototypes_mpi.h"

#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"
#include "ompi/communicator/comm_request.h"

#if OPAL_HAVE_WEAK_ALIASES
#pragma weak PMPIX_COMM_ISHRINK = ompix_comm_ishrink_f
#pragma weak pmpix_comm_ishrink = ompix_comm_ishrink_f
#pragma weak pmpix_comm_ishrink_ = ompix_comm_ishrink_f
#pragma weak pmpix_comm_ishrink__ = ompix_comm_ishrink_f
#pragma weak PMPIX_Comm_ishrink_f = ompix_comm_ishrink_f
#pragma weak PMPIX_Comm_ishrink_f08 = ompix_comm_ishrink_f

#pragma weak MPIX_COMM_ISHRINK = ompix_comm_ishrink_f
#pragma weak mpix_comm_ishrink = ompix_comm_ishrink_f
#pragma weak mpix_comm_ishrink_ = ompix_comm_ishrink_f
#pragma weak mpix_comm_ishrink__ = ompix_comm_ishrink_f
#pragma weak MPIX_Comm_ishrink_f = ompix_comm_ishrink_f
#pragma weak MPIX_Comm_ishrink_f08 = ompix_comm_ishrink_f

#else /* No weak aliases */
OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_ISHRINK,
                        pmpix_comm_ishrink,
                        pmpix_comm_ishrink_,
                        pmpix_comm_ishrink__,
                        ompix_comm_ishrink_f,
                        (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr),
                        (comm, newcomm, request, ierr))

OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_ISHRINK,
                        mpix_comm_ishrink,
                        mpix_comm_ishrink_,
                        mpix_comm_ishrink__,
                        ompix_comm_ishrink_f,
                        (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr),
                        (comm, newcomm, request, ierr))
#endif

/* The communicator the shrink produces, and where its Fortran handle has
 * to end up.  newcomm is ASYNCHRONOUS: the shrink cannot build the
 * communicator until the agreement on the failed group has completed, so
 * it writes c_newcomm long after this binding has returned.  Neither the
 * communicator nor its conversion can therefore live on our stack. */
struct ishrink_newcomm_t {
    MPI_Comm c_newcomm;
    MPI_Fint *f_newcomm;
};

static void ishrink_convert_newcomm(ompi_api_request_t *req)
{
    ompi_comm_request_t *request = (ompi_comm_request_t *) req;
    struct ishrink_newcomm_t *shrunk
        = (struct ishrink_newcomm_t *) request->lang_data;

    *shrunk->f_newcomm = PMPI_Comm_c2f(shrunk->c_newcomm);
}

static void ishrink_free_newcomm(ompi_api_request_t *req)
{
    free(((ompi_comm_request_t *) req)->lang_data);
}

void ompix_comm_ishrink_f(MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Request c_req;
    struct ishrink_newcomm_t *shrunk = malloc(sizeof(*shrunk));
    int c_ierr;

    if (NULL == shrunk) {
        *ierr = OMPI_INT_2_FINT(MPI_ERR_NO_MEM);
        *newcomm = PMPI_Comm_c2f(&ompi_mpi_comm_null.comm);
        return;
    }
    shrunk->f_newcomm = newcomm;

    c_ierr = PMPIX_Comm_ishrink(c_comm, &shrunk->c_newcomm, &c_req);
    *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS != c_ierr) {
        free(shrunk);
        *newcomm = PMPI_Comm_c2f(&ompi_mpi_comm_null.comm);
        return;
    }

    *request = PMPI_Request_c2f(c_req);

    /* Convert where the user observes completion, so *newcomm becomes
     * visible to the Fortran program at the same moment MPI_Wait/MPI_Test
     * reports the shrink done, and in the user's own thread.  The cleanup
     * releases shrunk when the request is disposed of, which covers a user
     * who frees the request without ever waiting on it. */
    ((ompi_comm_request_t *) c_req)->lang_data = shrunk;
    (void) ompi_api_request_set_lang_hooks(c_req, ishrink_convert_newcomm,
                                           ishrink_free_newcomm);
}
