/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#include "pnbc_internal.h"

/* linear iexscan
 * working principle:
 * 1. each node (but node 0) receives from left neigbor
 * 2. performs op
 * 3. all but rank p-1 do sends to it's right neigbor and exits
 *
 */
int ompi_coll_libpnbc_iexscan_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                             struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                             struct mca_coll_base_module_2_2_0_t *module) {
    int rank, p, res;
    ptrdiff_t gap, span;
    PNBC_Schedule *schedule;

    char inplace;
    PNBC_Handle *handle;
    ompi_coll_libpnbc_module_t *libpnbc_module = (ompi_coll_libpnbc_module_t*) module;

    PNBC_IN_PLACE(sendbuf, recvbuf, inplace);

    rank = ompi_comm_rank (comm);
    p = ompi_comm_size (comm);

    res = PNBC_Init_handle(comm, &handle, libpnbc_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
    }

  /*
   * FIXME - this is an initialisation function
   *         ** it must not do any real work **
   *         this should instead create a short
   *         schedule with just PNBC_Sched_copy
   *         Move this into algorithm selection
   */
    span = opal_datatype_span(&datatype->super, count, &gap);
    if (0 < rank) {
        handle->tmpbuf = malloc(span);
        if (handle->tmpbuf == NULL) {
            PNBC_Return_handle (handle);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        if (inplace) {
            res = PNBC_Copy(recvbuf, count, datatype, (char *)handle->tmpbuf-gap, count, datatype, comm);
        } else {
            res = PNBC_Copy(sendbuf, count, datatype, (char *)handle->tmpbuf-gap, count, datatype, comm);
        }
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            PNBC_Return_handle (handle);
            return res;
        }
    }

        schedule = OBJ_NEW(PNBC_Schedule);
        if (OPAL_UNLIKELY(NULL == schedule)) {
            PNBC_Return_handle (handle);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* make sure the schedule is released with the handle on error */
        handle->schedule = schedule;

        if (rank != 0) {
            res = PNBC_Sched_recv (recvbuf, false, count, datatype, rank-1, schedule, false);

            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                PNBC_Return_handle (handle);
                return res;
            }

            if (rank < p - 1) {
                /* we have to wait until we have the data */
                res = PNBC_Sched_barrier(schedule);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                    PNBC_Return_handle (handle);
                    return res;
                }

                res = PNBC_Sched_op (recvbuf, false, (void *)(-gap), true, count,
                                     datatype, op, schedule, true);

                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                    PNBC_Return_handle (handle);
                    return res;
                }

                /* send reduced data onward */
                res = PNBC_Sched_send ((void *)(-gap), true, count, datatype, rank + 1, schedule, false);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                    PNBC_Return_handle (handle);
                    return res;
                }
            }
        } else if (p > 1) {
            if (inplace) {
              res = PNBC_Sched_send (recvbuf, false, count, datatype, 1, schedule, false);
            } else {
              res = PNBC_Sched_send (sendbuf, false, count, datatype, 1, schedule, false);
            }
            if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
                PNBC_Return_handle (handle);
                return res;
            }
        }

        res = PNBC_Sched_commit(schedule);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            PNBC_Return_handle (handle);
            return res;
        }


    handle->schedule = schedule;

    *request = (ompi_request_t *) handle;

    return OMPI_SUCCESS;
}
