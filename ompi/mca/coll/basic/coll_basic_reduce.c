/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"


/*
 *	reduce_log_intra
 *
 *	Function:	- reduction using O(log N) algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 *
 *	These are the unsegmented tree reductions of the base component:
 *	binomial for commutative operations, in-order binary (the binomial
 *	tree does not preserve the order of the operands) otherwise.
 */
int
mca_coll_basic_reduce_log_intra(const void *sbuf, void *rbuf, size_t count,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                int root, struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    if (ompi_op_is_commute(op)) {
        return ompi_coll_base_reduce_intra_binomial(sbuf, rbuf, count, dtype, op, root,
                                                    comm, module, 0, 0);
    }
    return ompi_coll_base_reduce_intra_in_order_binary(sbuf, rbuf, count, dtype, op, root,
                                                       comm, module, 0, 0);
}


/*
 *	reduce_lin_inter
 *
 *	Function:	- reduction using O(N) algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_reduce_lin_inter(const void *sbuf, void *rbuf, size_t count,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                int root, struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    int i, err, size;
    ptrdiff_t dsize, gap;
    char *free_buffer = NULL;
    char *pml_buffer = NULL;

    /* Initialize */
    size = ompi_comm_remote_size(comm);

    if (MPI_PROC_NULL == root) {
        /* do nothing */
        err = OMPI_SUCCESS;
    } else if (MPI_ROOT != root) {
        /* If not root, send data to the root. */
        err = MCA_PML_CALL(send(sbuf, count, dtype, root,
                                MCA_COLL_BASE_TAG_REDUCE,
                                MCA_PML_BASE_SEND_STANDARD, comm));
    } else {
        /* Root receives and reduces messages  */
        dsize = opal_datatype_span(&dtype->super, count, &gap);

        free_buffer = (char*)malloc(dsize);
        if (NULL == free_buffer) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        pml_buffer = free_buffer - gap;


        /* Initialize the receive buffer. */
        err = MCA_PML_CALL(recv(rbuf, count, dtype, 0,
                                MCA_COLL_BASE_TAG_REDUCE, comm,
                                MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            if (NULL != free_buffer) {
                free(free_buffer);
            }
            return err;
        }

        /* Loop receiving and calling reduction function (C or Fortran). */
        for (i = 1; i < size; i++) {
            err = MCA_PML_CALL(recv(pml_buffer, count, dtype, i,
                                    MCA_COLL_BASE_TAG_REDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) {
                if (NULL != free_buffer) {
                    free(free_buffer);
                }
                return err;
            }

            /* Perform the reduction */
            ompi_op_reduce(op, pml_buffer, rbuf, count, dtype);
        }

        if (NULL != free_buffer) {
            free(free_buffer);
        }
    }

    /* All done */
    return err;
}
