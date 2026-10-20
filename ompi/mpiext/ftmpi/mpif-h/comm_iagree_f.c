/*
 * Copyright (c) 2010-2019 The University of Tennessee and the University
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

#include <stdlib.h>

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/mca/coll/base/coll_base_util.h"
#include "ompi/mpiext/ftmpi/c/mpiext_ftmpi_c.h"
#include "ompi/mpiext/ftmpi/mpif-h/prototypes_mpi.h"

#if OPAL_HAVE_WEAK_ALIASES
#pragma weak PMPIX_COMM_IAGREE = ompix_comm_iagree_f
#pragma weak pmpix_comm_iagree = ompix_comm_iagree_f
#pragma weak pmpix_comm_iagree_ = ompix_comm_iagree_f
#pragma weak pmpix_comm_iagree__ = ompix_comm_iagree_f
#pragma weak PMPIX_Comm_iagree_f = ompix_comm_iagree_f
#pragma weak PMPIX_Comm_iagree_f08 = ompix_comm_iagree_f

#pragma weak MPIX_COMM_IAGREE = ompix_comm_iagree_f
#pragma weak mpix_comm_iagree = ompix_comm_iagree_f
#pragma weak mpix_comm_iagree_ = ompix_comm_iagree_f
#pragma weak mpix_comm_iagree__ = ompix_comm_iagree_f
#pragma weak MPIX_Comm_iagree_f = ompix_comm_iagree_f
#pragma weak MPIX_Comm_iagree_f08 = ompix_comm_iagree_f

#else /* No weak aliases */
OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_IAGREE,
                        mpix_comm_iagree,
                        mpix_comm_iagree_,
                        mpix_comm_iagree__,
                        ompix_comm_iagree_f,
                        (MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *request, MPI_Fint *ierr),
                        (comm, flag, request, ierr))

OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_IAGREE,
                        pmpix_comm_iagree,
                        pmpix_comm_iagree_,
                        pmpix_comm_iagree__,
                        ompix_comm_iagree_f,
                        (MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *request, MPI_Fint *ierr),
                        (comm, flag, request, ierr))
#endif

/* flag is INOUT: the integer the caller contributes to the bitwise AND, and
 * on completion the reduced result.  The agreement writes that result when
 * the request completes, long after this binding has returned, so the int it
 * reduces cannot live on our stack and the conversion back to the caller's
 * INTEGER cannot happen here. */
static void iagree_convert_flag(ompi_api_request_t *req)
{
    ompi_coll_base_nbc_request_t *request = (ompi_coll_base_nbc_request_t *) req;

    *request->data.agree.flag = OMPI_INT_2_FINT(*request->data.agree.c_flag);
}

static void iagree_free_flag(ompi_api_request_t *req)
{
    free(((ompi_coll_base_nbc_request_t *) req)->data.agree.c_flag);
}

void ompix_comm_iagree_f(MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Request c_req;
    int *c_flag = malloc(sizeof(*c_flag));
    int c_ierr;

    if (NULL == c_flag) {
        *ierr = OMPI_INT_2_FINT(MPI_ERR_NO_MEM);
        return;
    }
    *c_flag = OMPI_FINT_2_INT(*flag);

    c_ierr = PMPIX_Comm_iagree(c_comm, c_flag, &c_req);
    *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS != c_ierr) {
        free(c_flag);
        return;
    }

    *request = PMPI_Request_c2f(c_req);

    if (!ompi_api_request_is(c_req)) {
        /* Nothing was scheduled -- a single-process communicator, or a
         * build without fault tolerance where this degrades to an
         * iallreduce that had nothing to do.  The result is already in
         * place and there is no collective request to defer to. */
        *flag = OMPI_INT_2_FINT(*c_flag);
        free(c_flag);
        return;
    }

    /* Convert the result where the user observes completion, and release
     * the C-side contribution when the request is disposed of.  An
     * agreement that reports MPI_ERR_PROC_FAILED still produces a valid
     * value, which is why the conversion runs ahead of the error check. */
    ((ompi_coll_base_nbc_request_t *) c_req)->data.agree.c_flag = c_flag;
    ((ompi_coll_base_nbc_request_t *) c_req)->data.agree.flag = flag;
    (void) ompi_coll_base_nbc_set_lang_hooks(c_req, iagree_convert_flag,
                                             iagree_free_flag);
}
