/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Siberian State University of Telecommunications
 *                         and Information Science. All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_util.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"

/*
 * ompi_coll_base_scan_intra_linear
 *
 * Function:  Linear algorithm for inclusive scan.
 * Accepts:   Same as MPI_Scan
 * Returns:   MPI_SUCCESS or error code
 */
int
ompi_coll_base_scan_intra_linear(const void *sbuf, void *rbuf, size_t count,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    int size, rank, err;
    ptrdiff_t dsize, gap;
    char *free_buffer = NULL;
    char *pml_buffer = NULL;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If I'm rank 0, just copy into the receive buffer */

    if (0 == rank) {
        if (MPI_IN_PLACE != sbuf) {
            err = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    }

    /* Otherwise receive previous buffer and reduce. */

    else {
        /* Out of place with a commutative operation the prior answer is
         * received straight into the receive buffer and our data is combined
         * into it: no copy of the send buffer, no temporary buffer. */

        if (MPI_IN_PLACE != sbuf && ompi_op_is_commute(op)) {
            err = MCA_PML_CALL(recv(rbuf, count, dtype,
                                    rank - 1, MCA_COLL_BASE_TAG_SCAN, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) {
                return err;
            }

            /* rbuf = sbuf <op> rbuf */
            ompi_op_reduce(op, (void *) sbuf, rbuf, count, dtype);
            goto send_result;
        }

        /* Allocate a temporary buffer.  Rationale for this size is
         * listed in coll_basic_reduce.c.  Use this temporary buffer to
         * receive into, later. */

        dsize = opal_datatype_span(&dtype->super, count, &gap);
        free_buffer = malloc(dsize);
        if (NULL == free_buffer) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        pml_buffer = free_buffer - gap;

        /* Copy the send buffer into the receive buffer. */

        if (MPI_IN_PLACE != sbuf) {
            err = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
            if (MPI_SUCCESS != err) {
                if (NULL != free_buffer) {
                    free(free_buffer);
                }
                return err;
            }
        }

        /* Receive the prior answer */

        err = MCA_PML_CALL(recv(pml_buffer, count, dtype,
                                rank - 1, MCA_COLL_BASE_TAG_SCAN, comm,
                                MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            if (NULL != free_buffer) {
                free(free_buffer);
            }
            return err;
        }

        /* Perform the operation */

        ompi_op_reduce(op, pml_buffer, rbuf, count, dtype);

        /* All done */

        if (NULL != free_buffer) {
            free(free_buffer);
        }
    }

    /* Send result to next process. */

send_result:
    if (rank < (size - 1)) {
        return MCA_PML_CALL(send(rbuf, count, dtype, rank + 1,
                                 MCA_COLL_BASE_TAG_SCAN,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
    }

    /* All done */

    return MPI_SUCCESS;
}


/*
 * What the reduction after an exchange of the commutative recursive doubling
 * scan does, which depends on the rank of the peer and on how far the
 * partial result (psend) and the prefix result (recvbuf) have progressed.
 * All of them are element-wise, so they can be applied segment by segment.
 */
typedef enum {
    SCAN_RECD_FIRST_LOWER,   /* peer below, psend is still the input: the first prefix */
    SCAN_RECD_LOWER_NOPREFIX,/* peer below, psend is reduced, no prefix yet */
    SCAN_RECD_LOWER,         /* peer below, prefix exists */
    SCAN_RECD_FIRST_UPPER,   /* peer above, psend is still the input */
    SCAN_RECD_UPPER          /* peer above, psend is reduced */
} scan_recd_kind_t;

typedef struct {
    struct ompi_op_t *op;
    struct ompi_datatype_t *datatype;
    scan_recd_kind_t kind;
    char *own, *psend, *pspare, *precv, *recvbuf;
    int err;
} scan_recd_ctx_t;

static void scan_recd_apply(void *vctx, ptrdiff_t offset, size_t count)
{
    scan_recd_ctx_t *ctx = (scan_recd_ctx_t *) vctx;
    struct ompi_op_t *op = ctx->op;
    struct ompi_datatype_t *dt = ctx->datatype;
    char *own = ctx->own + offset, *psend = ctx->psend + offset, *pspare = ctx->pspare + offset;
    char *precv = ctx->precv + offset, *recvbuf = ctx->recvbuf + offset;
    int err = MPI_SUCCESS;

    switch (ctx->kind) {
    case SCAN_RECD_FIRST_LOWER:
        /* recvbuf = own <op> received, and the partial result is the same value */
        ompi_op_reduce(op, own, recvbuf, count, dt);
        err = ompi_datatype_copy_content_same_ddt(dt, count, pspare, recvbuf);
        break;
    case SCAN_RECD_LOWER_NOPREFIX:
        /* psend = received <op> psend; recvbuf = own <op> received */
        ompi_op_reduce(op, precv, psend, count, dt);
        err = ompi_datatype_copy_content_same_ddt(dt, count, recvbuf, precv);
        ompi_op_reduce(op, own, recvbuf, count, dt);
        break;
    case SCAN_RECD_LOWER:
        /* Accumulate prefix reduction: recvbuf = precv <op> recvbuf */
        ompi_op_reduce(op, precv, recvbuf, count, dt);
        /* Partial result: psend = precv <op> psend */
        ompi_op_reduce(op, precv, psend, count, dt);
        break;
    case SCAN_RECD_FIRST_UPPER:
        /* pspare = own <op> received */
        ompi_op_reduce(op, own, pspare, count, dt);
        break;
    case SCAN_RECD_UPPER:
        /* psend = precv <op> psend */
        ompi_op_reduce(op, precv, psend, count, dt);
        break;
    }
    if (MPI_SUCCESS != err) {
        ctx->err = err;
    }
}

/*
 * Recursive doubling scan for a commutative operation and an input that is
 * not MPI_IN_PLACE. The input vector is never copied up front: the partial
 * result (psend) is the user send buffer until the first reduction that
 * involves it, which is written to a spare buffer, and the prefix result is
 * built in recvbuf from the first block received from a lower rank. The
 * input is copied into recvbuf only on rank 0, which never receives from a
 * lower rank. Large messages are exchanged in segments, so that the
 * reduction of a segment overlaps the transfer of the following ones.
 */
static int ompi_coll_base_scan_recd_commute(
    const void *sendbuf, void *recvbuf, size_t count, struct ompi_datatype_t *datatype,
    struct ompi_op_t *op, struct ompi_communicator_t *comm)
{
    int err = MPI_SUCCESS;
    char *tmpspare_raw = NULL, *tmprecv_raw = NULL;
    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    int nsegs = ompi_coll_base_recdoubling_nsegs(count, datatype);
    ptrdiff_t dsize, gap;

    dsize = opal_datatype_span(&datatype->super, count, &gap);
    /* A single allocation holds both temporaries: it is cheaper than two
     * and avoids the allocator trimming and re-faulting them on every call */
    ptrdiff_t dsize_padded = (dsize + 63) & ~(ptrdiff_t) 63;
    tmpspare_raw = malloc(2 * dsize_padded);
    tmprecv_raw = (NULL == tmpspare_raw) ? NULL : tmpspare_raw + dsize_padded;
    if (NULL == tmpspare_raw) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup_and_return;
    }
    char *pspare = tmpspare_raw - gap;
    char *precv = tmprecv_raw - gap;
    char *own = (char *) sendbuf;
    char *psend = own;
    bool recvbuf_valid = false;

    for (int mask = 1; mask < comm_size; mask <<= 1) {
        int remote = rank ^ mask;
        if (remote >= comm_size) {
            continue;
        }
        bool from_lower = (rank > remote);
        scan_recd_ctx_t ctx = {op, datatype, SCAN_RECD_UPPER, own, psend, pspare, precv,
                               (char *) recvbuf, MPI_SUCCESS};

        /* Receive where the reduction will be written, when possible */
        char *target = precv;
        if (psend == own) {
            target = from_lower ? (char *) recvbuf : pspare;
        }
        if (from_lower) {
            ctx.kind = (psend == own) ? SCAN_RECD_FIRST_LOWER
                     : (!recvbuf_valid ? SCAN_RECD_LOWER_NOPREFIX : SCAN_RECD_LOWER);
        } else if (psend == own) {
            ctx.kind = SCAN_RECD_FIRST_UPPER;
        }

        if (nsegs > 1) {
            err = ompi_coll_base_segmented_sendrecv(psend, target, count, datatype, remote,
                                                    MCA_COLL_BASE_TAG_SCAN, comm, nsegs,
                                                    scan_recd_apply, &ctx);
        } else {
            err = ompi_coll_base_sendrecv(psend, count, datatype, remote,
                                          MCA_COLL_BASE_TAG_SCAN,
                                          target, count, datatype, remote,
                                          MCA_COLL_BASE_TAG_SCAN, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS == err) {
                scan_recd_apply(&ctx, 0, count);
            }
        }
        if (MPI_SUCCESS == err) {
            err = ctx.err;
        }
        if (MPI_SUCCESS != err) { goto cleanup_and_return; }

        if (SCAN_RECD_FIRST_LOWER == ctx.kind || SCAN_RECD_FIRST_UPPER == ctx.kind) {
            psend = pspare;
        }
        if (from_lower) {
            recvbuf_valid = true;
        }
    }

    if (!recvbuf_valid) {
        /* Rank 0: the result is its own input */
        err = ompi_datatype_copy_content_same_ddt(datatype, count, recvbuf, own);
    }

cleanup_and_return:
    free(tmpspare_raw);
    return err;
}

/*
 * ompi_coll_base_scan_intra_recursivedoubling
 *
 * Function:  Recursive doubling algorithm for inclusive scan.
 * Accepts:   Same as MPI_Scan
 * Returns:   MPI_SUCCESS or error code
 *
 * Description:  Implements recursive doubling algorithm for MPI_Scan.
 *               The algorithm preserves order of operations so it can
 *               be used both by commutative and non-commutative operations.
 *
 * Example for 5 processes and commutative operation MPI_SUM:
 * Process:   0                 1              2              3              4
 * recvbuf:  [0]               [1]            [2]            [3]            [4]
 *   psend:  [0]               [1]            [2]            [3]            [4]
 *
 *  Step 1:
 * recvbuf:  [0]               [0+1]          [2]            [2+3]          [4]
 *   psend:  [1+0]             [0+1]          [3+2]          [2+3]          [4]
 *
 *  Step 2:
 * recvbuf:  [0]               [0+1]          [(1+0)+2]      [(1+0)+(2+3)]  [4]
 *  psend:   [(3+2)+(1+0)]     [(2+3)+(0+1)]  [(1+0)+(3+2)]  [(1+0)+(2+3)]  [4]
 *
 *  Step 3:
 * recvbuf:  [0]               [0+1]           [(1+0)+2]     [(1+0)+(2+3)]  [((3+2)+(1+0))+4]
 *   psend:  [4+((3+2)+(1+0))]                                              [((3+2)+(1+0))+4]
 *
 * Time complexity (worst case): \ceil(\log_2(p))(2\alpha + 2m\beta + 2m\gamma)
 * Memory requirements (per process): 2 * count * typesize = O(count)
 * Limitations: intra-communicators only
 */
int ompi_coll_base_scan_intra_recursivedoubling(
    const void *sendbuf, void *recvbuf, size_t count, struct ompi_datatype_t *datatype,
    struct ompi_op_t *op, struct ompi_communicator_t *comm,
    mca_coll_base_module_t *module)
{
    int err = MPI_SUCCESS;
    char *tmpsend_raw = NULL, *tmprecv_raw = NULL;
    int comm_size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:base:scan_intra_recursivedoubling: rank %d/%d",
                 rank, comm_size));
    if (count == 0)
        return MPI_SUCCESS;

    if (comm_size >= 2 && sendbuf != MPI_IN_PLACE && ompi_op_is_commute(op)) {
        return ompi_coll_base_scan_recd_commute(sendbuf, recvbuf, count, datatype, op, comm);
    }

    if (sendbuf != MPI_IN_PLACE) {
        err = ompi_datatype_copy_content_same_ddt(datatype, count, recvbuf, (char *)sendbuf);
        if (MPI_SUCCESS != err) { goto cleanup_and_return; }
    }
    if (comm_size < 2)
        return MPI_SUCCESS;

    ptrdiff_t dsize, gap;
    dsize = opal_datatype_span(&datatype->super, count, &gap);
    /* A single allocation holds both temporaries: it is cheaper than two
     * and avoids the allocator trimming and re-faulting them on every call */
    ptrdiff_t dsize_padded = (dsize + 63) & ~(ptrdiff_t) 63;
    tmpsend_raw = malloc(2 * dsize_padded);
    tmprecv_raw = (NULL == tmpsend_raw) ? NULL : tmpsend_raw + dsize_padded;
    if (NULL == tmpsend_raw) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup_and_return;
    }
    char *psend = tmpsend_raw - gap;
    char *precv = tmprecv_raw - gap;
    err = ompi_datatype_copy_content_same_ddt(datatype, count, psend, recvbuf);
    if (MPI_SUCCESS != err) { goto cleanup_and_return; }
    int is_commute = ompi_op_is_commute(op);

    for (int mask = 1; mask < comm_size; mask <<= 1) {
        int remote = rank ^ mask;
        if (remote < comm_size) {
            err = ompi_coll_base_sendrecv(psend, count, datatype, remote,
                                          MCA_COLL_BASE_TAG_SCAN,
                                          precv, count, datatype, remote,
                                          MCA_COLL_BASE_TAG_SCAN, comm,
                                          MPI_STATUS_IGNORE, rank);
            if (MPI_SUCCESS != err) { goto cleanup_and_return; }

            if (rank > remote) {
                /* Accumulate prefix reduction: recvbuf = precv <op> recvbuf */
                ompi_op_reduce(op, precv, recvbuf, count, datatype);
                /* Partial result: psend = precv <op> psend */
                ompi_op_reduce(op, precv, psend, count, datatype);
            } else {
                if (is_commute) {
                    /* psend = precv <op> psend */
                    ompi_op_reduce(op, precv, psend, count, datatype);
                } else {
                    /* precv = psend <op> precv */
                    ompi_op_reduce(op, psend, precv, count, datatype);
                    char *tmp = psend;
                    psend = precv;
                    precv = tmp;
                }
            }
        }
    }

cleanup_and_return:
    free(tmpsend_raw);
    return err;
}
