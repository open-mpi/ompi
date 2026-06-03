/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#include "ompi_config.h"

#include <string.h>

#include "opal/datatype/opal_convertor.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/coll/coll.h"
#include "opal/sys/atomic.h"
#include "opal/util/minmax.h"
#include "coll_smdirect.h"

#define IOVEC_MAX 128

static inline int ddt_copy(const void *from_addr, int from_count,
                           struct opal_datatype_t *from_dt,
                           bool is_from_contig, ptrdiff_t from_lb,
                           void * to_addr,
                           int to_count, struct opal_datatype_t *to_dt,
                           bool is_to_contig, ptrdiff_t to_lb) {
    int ret = OMPI_SUCCESS;
    size_t size; // temporary, ignored
    size_t copied = 0;
    size_t to_copy;

    /* from and to must have the same number of bytes so just use one */
    opal_datatype_type_size(to_dt, &to_copy);
    to_copy *= to_count;

    if (!is_from_contig && !is_to_contig) {

        size_t curr_len = 0;
        bool from_done = false, to_done = false;
        struct iovec from_iov[IOVEC_MAX];
        uint32_t from_iov_count = 0;
        uint32_t from_iov_idx = 0;
        opal_convertor_t from_convertor;

        struct iovec to_iov[IOVEC_MAX];
        uint32_t to_iov_count = 0;
        uint32_t to_iov_idx = 0;
        opal_convertor_t to_convertor;


        ret = opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                                                       from_dt, from_count,
                                                       from_addr, 0, &from_convertor);
        if (ret != OPAL_SUCCESS) {
            return ret;
        }

        ret = opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                                                       to_dt, to_count,
                                                       to_addr, 0, &to_convertor);
        if (ret != OPAL_SUCCESS) {
            return ret;
        }

        while (copied != to_copy) {
            /* get new iovecs */
            if (from_iov_idx == from_iov_count) {
                from_iov_count = IOVEC_MAX;
                assert(!from_done);
                from_done      = opal_convertor_raw(&from_convertor, from_iov, &from_iov_count, &size);
                from_iov_idx   = 0;
            }
            if (to_iov_idx == to_iov_count) {
                to_iov_count = IOVEC_MAX;
                assert(!to_done);
                to_done      = opal_convertor_raw(&to_convertor, to_iov, &to_iov_count, &size);
                to_iov_idx   = 0;
            }

            /* iterate until we've covered all elements */
            while (from_iov_idx < from_iov_count &&
                   to_iov_idx < to_iov_count) {
                curr_len = opal_min(from_iov[from_iov_idx].iov_len,
                                    to_iov[to_iov_idx].iov_len);
                memcpy(to_iov[to_iov_idx].iov_base, from_iov[from_iov_idx].iov_base, curr_len);
                copied += curr_len;

                from_iov[from_iov_idx].iov_len -= curr_len;
                if (from_iov[from_iov_idx].iov_len == 0) {
                    from_iov_idx++;
                }
                to_iov[to_iov_idx].iov_len -= curr_len;
                if (to_iov[to_iov_idx].iov_len == 0) {
                    to_iov_idx++;
                }
            }
        }

        opal_convertor_cleanup(&from_convertor);
        OBJ_DESTRUCT(&from_convertor);
        opal_convertor_cleanup(&to_convertor);
        OBJ_DESTRUCT(&to_convertor);

        assert(from_iov_idx == from_iov_count &&
               to_iov_idx == to_iov_count);

    } else if (!is_from_contig) {
        uintptr_t to_intptr = (uintptr_t)to_addr;
        to_intptr += to_lb;
        bool from_done = false;
        struct iovec from_iov[IOVEC_MAX];
        uint32_t from_iov_count = 0;
        uint32_t from_iov_idx = 0;
        opal_convertor_t from_convertor;

        ret = opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                                                       from_dt, from_count,
                                                       from_addr, 0, &from_convertor);
        if (ret != OPAL_SUCCESS) {
            return ret;
        }

        while (copied != to_copy) {
            /* get new iovecs */
            if (from_iov_idx == from_iov_count) {
                from_iov_count = IOVEC_MAX;
                assert(!from_done);
                from_done      = opal_convertor_raw(&from_convertor, from_iov, &from_iov_count, &size);
                from_iov_idx   = 0;
            }
            while (from_iov_idx < from_iov_count) {
                memcpy((void*)to_intptr+copied, from_iov[from_iov_idx].iov_base, from_iov[from_iov_idx].iov_len);
                copied += from_iov[from_iov_idx].iov_len;
                from_iov_idx++;
            }
        }

        opal_convertor_cleanup(&from_convertor);
        OBJ_DESTRUCT(&from_convertor);

    } else {
        uintptr_t from_intptr = (uintptr_t)from_addr;
        from_intptr += from_lb;
        bool to_done = false;
        struct iovec to_iov[IOVEC_MAX];
        uint32_t to_iov_count = 0;
        uint32_t to_iov_idx = 0;
        opal_convertor_t to_convertor;

        ret = opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                                                       to_dt, to_count,
                                                       to_addr, 0, &to_convertor);
        if (ret != OPAL_SUCCESS) {
            return ret;
        }

        while (copied != to_copy) {
            /* get new iovecs */
            if (to_iov_idx == to_iov_count) {
                to_iov_count = IOVEC_MAX;
                assert(!to_done);
                to_done      = opal_convertor_raw(&to_convertor, to_iov, &to_iov_count, &size);
                to_iov_idx   = 0;
            }
            while (to_iov_idx < to_iov_count) {
                memcpy(to_iov[to_iov_idx].iov_base, (void*)from_intptr+copied, to_iov[to_iov_idx].iov_len);
                copied += to_iov[to_iov_idx].iov_len;
                to_iov_idx++;
            }
        }

        opal_convertor_cleanup(&to_convertor);
        OBJ_DESTRUCT(&to_convertor);
    }

    return OMPI_SUCCESS;
}


int mca_coll_smdirect_bcast_intra(void *buff, int count,
                            struct ompi_datatype_t *datatype, int root,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    int ret = OMPI_SUCCESS;
    mca_coll_smdirect_module_t *sm_module = (mca_coll_smdirect_module_t*) module;

    /* Lazily enable the module the first time we invoke a collective
       on it */
    if (!sm_module->enabled) {
        if (OMPI_SUCCESS != (ret = ompi_coll_smdirect_lazy_enable(module, comm))) {
            return ret;
        }
    }

    mca_coll_smdirect_comm_t *data = sm_module->sm_comm_data;

    /* Setup some identities */

    const int rank = ompi_comm_rank(comm);
    const int size = ompi_comm_size(comm);

    /* extent is from lb to ub (e.g., MPI_SHORT_INT is 8) */
    const size_t control_size = mca_coll_smdirect_component.sm_control_size;

    /* get the current operation */
    int op_count = ++data->mcb_operation_count;

    /* wait for processes from the previous op to finish */
    FLAG_WAIT_FOR_IDLE(&data->procdata->mcsp_op_flag);

    opal_datatype_t *root_dtype = (opal_datatype_t*)(data->sm_bootstrap_meta->module_data_addr
                                                        + control_size * root
                                                        + sizeof(mca_coll_smdirect_procdata_t));
    int *root_count = (int*)((char*)root_dtype + mca_coll_smdirect_serialize_ddt_size(&datatype->super));
    /* set our input buffer information */
    int op_retain_cnt = 0;
    if (root == rank) {
        ptrdiff_t extent;
        ompi_datatype_type_extent(datatype, &extent);
        const size_t total_extent = count * extent;
        /* leafs provide the full input buffer */
        data->procdata->mcsp_indata = (void*)buff;
        data->procdata->mcsp_insize = total_extent;
        /* make our datatype available */
        mca_coll_smdirect_serialize_ddt(root_dtype, &datatype->super);
        *root_count = count;
        op_retain_cnt = size-1;
        /* make sure all writes are visible before we signal that our procdata for this op is ready */
        opal_atomic_wmb();
    } else {

        /* get the endpoint and map the memory region */

        mca_coll_smdirect_peerdata_t *peerdata = data->peerdata;
        mca_coll_smdirect_peerdata_t *peer = &peerdata[root];

        /* get the endpoint */
        if (NULL == (peer->endpoint = data->endpoints[root])) {
            peer->endpoint = MCA_SMSC_CALL(get_endpoint, (&ompi_comm_peer_lookup(comm, root)->super));
            data->endpoints[root] = peer->endpoint;
        }

        peer->procdata = (mca_coll_smdirect_procdata_t *)(data->sm_bootstrap_meta->module_data_addr
                                                          + control_size * root);

        /* make sure we're all on the same op */
        FLAG_WAIT_FOR_OP(&peer->procdata->mcsp_op_flag, op_count);

        opal_atomic_rmb();
        /* map the children's memory region */
        peer->mapping_ctx = MCA_SMSC_CALL(map_peer_region,
                                          peer->endpoint,
                                          0,
                                          peer->procdata->mcsp_indata,
                                          peer->procdata->mcsp_insize,
                                          &peer->mapping_ptr);
        assert(peer->mapping_ptr != NULL);
        assert(peer->mapping_ctx != NULL);

        bool is_from_contig = opal_datatype_is_contiguous_memory_layout(root_dtype, *root_count);
        bool is_to_contig = ompi_datatype_is_contiguous_memory_layout(datatype, count);

        ptrdiff_t from_lb, from_extent, to_lb, to_extent;
        opal_datatype_get_true_extent(root_dtype, &from_lb, &from_extent);
        ompi_datatype_get_true_extent(datatype, &to_lb, &to_extent);

        /* copy the data */
        if (is_from_contig && is_to_contig) {
            /* fast path: simple memcpy */
            memcpy(buff, peer->mapping_ptr, to_extent*count);
        } else {
            ret = ddt_copy(peer->mapping_ptr, *root_count, root_dtype, is_from_contig,
                           from_lb, buff, count, &datatype->super, is_to_contig, to_lb);
        }

        /* let the root know that we're done */
        opal_atomic_wmb();
        FLAG_RELEASE(&peer->procdata->mcsp_op_flag);

        MCA_SMSC_CALL(unmap_peer_region, peer->mapping_ctx);

    }
    /* only root waits for its children but we need to keep the op_count in sync */
    FLAG_RETAIN(&data->procdata->mcsp_op_flag, op_retain_cnt, op_count);

    FLAG_WAIT_FOR_IDLE(&data->procdata->mcsp_op_flag);

    return ret;
}
