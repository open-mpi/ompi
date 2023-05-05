/*
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2022 Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "mtl_ofi.h"

OMPI_DECLSPEC extern mca_mtl_ofi_component_t mca_mtl_ofi_component;

OBJ_CLASS_INSTANCE(mca_mtl_comm_t, opal_object_t, NULL, NULL);

mca_mtl_ofi_module_t ompi_mtl_ofi = {
    {
        (int)((1ULL << MTL_OFI_CID_BIT_COUNT_1) - 1), /* max cid */
        (int)((1ULL << (MTL_OFI_TAG_BIT_COUNT_1 - 1)) - 1) ,/* max tag value */
        0,           /* request reserve space */
        0,           /* flags */

        ompi_mtl_ofi_add_procs,
        ompi_mtl_ofi_del_procs,
        ompi_mtl_ofi_finalize,

         NULL,
         NULL,
         NULL,
         NULL,
         ompi_mtl_ofi_imrecv,
         NULL,

        ompi_mtl_ofi_cancel,
        ompi_mtl_ofi_add_comm,
        ompi_mtl_ofi_del_comm
    },
    0,
    0,
    NULL,
    NULL
};


static int ompi_mtl_ofi_init_contexts(struct mca_mtl_base_module_t *mtl,
                                      struct ompi_communicator_t *comm,
                                      mca_mtl_ofi_ep_type ep_type)
{
    int ret;
    int ctxt_id = ompi_mtl_ofi.total_ctxts_used;
    struct fi_cq_attr cq_attr = {0};
    cq_attr.format = FI_CQ_FORMAT_TAGGED;
    cq_attr.size = ompi_mtl_ofi.ofi_progress_event_count;

    if (OFI_REGULAR_EP == ep_type) {
        /*
         * For regular endpoints, just create the Lock object and register
         * progress function.
         */
        goto init_regular_ep;
    }

    /*
     * We only create upto Max number of contexts asked for by the user.
     * If user enables thread grouping feature and creates more number of
     * communicators than available contexts, then we set the threshold
     * context_id so that new communicators created beyond the threshold
     * will be assigned to contexts in a round-robin fashion.
     */
    if (ompi_mtl_ofi.num_ofi_contexts <= ompi_mtl_ofi.total_ctxts_used) {
        ompi_mtl_ofi.comm_to_context[comm->c_index] = comm->c_index %
                                                          ompi_mtl_ofi.total_ctxts_used;
        if (!ompi_mtl_ofi.threshold_comm_context_id) {
            ompi_mtl_ofi.threshold_comm_context_id = comm->c_index;

            opal_show_help("help-mtl-ofi.txt", "SEP thread grouping ctxt limit", true, ctxt_id,
                           ompi_process_info.nodename, __FILE__, __LINE__);
        }

        return OMPI_SUCCESS;
    }

    /* Init context info for Scalable EPs */
    ret = fi_tx_context(ompi_mtl_ofi.sep, ctxt_id, NULL, &ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep, NULL);
    if (ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_tx_context failed");
        goto init_error;
    }

    ret = fi_rx_context(ompi_mtl_ofi.sep, ctxt_id, NULL, &ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep, NULL);
    if (ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_rx_context failed");
        goto init_error;
    }

    ret = fi_cq_open(ompi_mtl_ofi.domain, &cq_attr, &ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq, NULL);
    if (ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_cq_open failed");
        goto init_error;
    }

    /* Bind CQ to TX/RX context object */
    ret = fi_ep_bind(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep, (fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq,
                     FI_TRANSMIT | FI_SELECTIVE_COMPLETION);
    if (0 != ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_bind CQ-EP (FI_TRANSMIT) failed");
        goto init_error;
    }

    ret = fi_ep_bind(ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep, (fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq,
                     FI_RECV | FI_SELECTIVE_COMPLETION);
    if (0 != ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_bind CQ-EP (FI_RECV) failed");
        goto init_error;
    }

    /* Enable Endpoint for communication. This commits the bind operations */
    ret = fi_enable(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep);
    if (0 != ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_enable (send context) failed");
        goto init_error;
    }

    ret = fi_enable(ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep);
    if (0 != ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_enable (recv context) failed");
        goto init_error;
    }

init_regular_ep:
    /* Initialize per-context lock */
    OBJ_CONSTRUCT(&ompi_mtl_ofi.ofi_ctxt[ctxt_id].context_lock, opal_mutex_t);

    if (!ompi_mtl_ofi.is_initialized) {
        ret = opal_progress_register(ompi_mtl_ofi_progress_no_inline);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(1, opal_common_ofi.output,
                                "%s:%d: opal_progress_register failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto init_error;
        }
    }

    ompi_mtl_ofi.comm_to_context[comm->c_index] = ompi_mtl_ofi.total_ctxts_used;
    ompi_mtl_ofi.total_ctxts_used++;

    return OMPI_SUCCESS;

init_error:
    if (ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep) {
        (void) fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep);
    }

    if (ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep) {
        (void) fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep);
    }

    if (ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq) {
        (void) fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq);
    }

    return ret;
}

static int ompi_mtl_ofi_finalize_contexts(struct mca_mtl_base_module_t *mtl,
                                          struct ompi_communicator_t *comm,
                                          mca_mtl_ofi_ep_type ep_type)
{
    int ret = OMPI_SUCCESS, ctxt_id = 0;

    if (OFI_REGULAR_EP == ep_type) {
        /* For regular EPs, simply destruct Lock object and exit */
        goto finalize_regular_ep;
    }

    if (ompi_mtl_ofi.thread_grouping &&
        ompi_mtl_ofi.threshold_comm_context_id &&
        ((uint32_t) ompi_mtl_ofi.threshold_comm_context_id <= comm->c_index)) {
        return OMPI_SUCCESS;
    }

    ctxt_id = ompi_mtl_ofi.thread_grouping ?
           ompi_mtl_ofi.comm_to_context[comm->c_index] : 0;

    /*
     * For regular EPs, TX/RX contexts are aliased to SEP object which is
     * closed in ompi_mtl_ofi_finalize(). So, skip handling those here.
     */
    if ((ret = fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep))) {
        goto finalize_err;
    }

    if ((ret = fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep))) {
        goto finalize_err;
    }

    if ((ret = fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq))) {
        goto finalize_err;
    }

finalize_regular_ep:
    /* Destroy context lock */
    OBJ_DESTRUCT(&ompi_mtl_ofi.ofi_ctxt[ctxt_id].context_lock);

    return OMPI_SUCCESS;

finalize_err:
    opal_show_help("help-mtl-ofi.txt", "OFI call fail", true,
                   "fi_close",
                   ompi_process_info.nodename, __FILE__, __LINE__,
                   fi_strerror(-ret), ret);

    return OMPI_ERROR;
}

int
ompi_mtl_ofi_add_procs(struct mca_mtl_base_module_t *mtl,
                       size_t nprocs,
                       struct ompi_proc_t** procs)
{
    int ret = OMPI_SUCCESS;
    size_t i;
    size_t size;
    size_t namelen;
    int count = 0;
    char *ep_name = NULL;
    fi_addr_t *fi_addrs = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    int num_peers_limit = (1 << ompi_mtl_ofi.num_bits_source_rank) - 1;

    namelen = ompi_mtl_ofi.epnamelen;

    /* We cannot add more ranks than available tag bits */
    if ((false == ompi_mtl_ofi.fi_cq_data) &&
        OPAL_UNLIKELY(((int) (nprocs + ompi_mtl_ofi.num_peers) > num_peers_limit))) {
        opal_output(0, "%s:%d: OFI provider: %s does not have enough bits for source rank in its tag.\n"
                       "Adding more ranks will result in undefined behaviour. Please enable\n"
                       "FI_REMOTE_CQ_DATA feature in the provider. For more info refer fi_cq(3).\n",
                       __FILE__, __LINE__, ompi_mtl_ofi.provider_name);
        fflush(stderr);
        ret = OMPI_ERROR;
        goto bail;
    }

    /**
     * Create array of fi_addrs.
     */
    fi_addrs = malloc(nprocs * sizeof(fi_addr_t));
    if (NULL == fi_addrs) {
        ret = OMPI_ERROR;
        goto bail;
    }

    for (i = 0; i < nprocs; ++i) {
        /**
         * Retrieve the processes' EP name from modex.
         */
        OFI_COMPAT_MODEX_RECV(ret,
                              &mca_mtl_ofi_component.super.mtl_version,
                              procs[i],
                              (void**)&ep_name,
                              &size);
        if (OMPI_SUCCESS != ret) {
            char *errhost = opal_get_proc_hostname(&procs[i]->super);
            opal_show_help("help-mtl-ofi.txt", "modex failed",
                           true, ompi_process_info.nodename,
			                     errhost, opal_strerror(ret), ret);
            free(errhost);
            goto bail;
        }

        /**
         * Map the EP name to fi_addr.
         */
        count = fi_av_insert(ompi_mtl_ofi.av, ep_name, 1, &fi_addrs[i], 0, NULL);
        if ((count < 0) || (1 != (size_t)count)) {
            opal_output_verbose(1, opal_common_ofi.output,
                                "%s:%d: fi_av_insert failed for address %s: %d\n",
                                __FILE__, __LINE__, ep_name, count);
            ret = OMPI_ERROR;
            goto bail;
        }
    }

    /**
     * Store the fi_addrs within the endpoint objects.
     */
    for (i = 0; i < nprocs; ++i) {
        endpoint = OBJ_NEW(mca_mtl_ofi_endpoint_t);
        if (NULL == endpoint) {
            opal_output_verbose(1, opal_common_ofi.output,
                                "%s:%d: mtl/ofi: could not allocate endpoint"
                                " structure\n",
                                __FILE__, __LINE__);
            ret = OMPI_ERROR;
            goto bail;
        }

        endpoint->mtl_ofi_module = &ompi_mtl_ofi;
        endpoint->peer_fiaddr = fi_addrs[i];

        /* FIXME: What happens if this endpoint already exists? */
        procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL] = endpoint;
    }

    /* Update global counter of number of procs added to this rank */
    ompi_mtl_ofi.num_peers += nprocs;

    ret = OMPI_SUCCESS;

bail:
    if (fi_addrs)
        free(fi_addrs);

    return ret;
}

int
ompi_mtl_ofi_del_procs(struct mca_mtl_base_module_t *mtl,
                       size_t nprocs,
                       struct ompi_proc_t** procs)
{
    int ret;
    size_t i;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;

    for (i = 0 ; i < nprocs ; ++i) {
        if (NULL != procs[i] &&
            NULL != procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL]) {
            endpoint = procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
            ret = fi_av_remove(ompi_mtl_ofi.av, &endpoint->peer_fiaddr, 1, 0);
            if (ret) {
                opal_output_verbose(1, opal_common_ofi.output,
                        "%s:%d: fi_av_remove failed: %s\n", __FILE__, __LINE__, fi_strerror(errno));
                return ret;
            }
            procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL] = NULL;
            OBJ_RELEASE(endpoint);
        }
    }

    return OMPI_SUCCESS;
}

int ompi_mtl_ofi_add_comm(struct mca_mtl_base_module_t *mtl,
                      struct ompi_communicator_t *comm)
{
    int ret = OMPI_SUCCESS;
    uint32_t comm_size;
    mca_mtl_comm_t* mtl_comm;

    mca_mtl_ofi_ep_type ep_type = (0 == ompi_mtl_ofi.enable_sep) ?
                                  OFI_REGULAR_EP : OFI_SCALABLE_EP;

    if (!OMPI_COMM_IS_GLOBAL_INDEX(comm)) {
        mtl_comm = OBJ_NEW(mca_mtl_comm_t);

        if (OMPI_COMM_IS_INTER(comm)) {
            comm_size = ompi_comm_remote_size(comm);
        } else {
            comm_size = ompi_comm_size(comm);
        }
        mtl_comm->c_index_vec = (c_index_vec_t *)malloc(sizeof(c_index_vec_t) * comm_size);
        if (NULL == mtl_comm->c_index_vec) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            OBJ_RELEASE(mtl_comm);
            goto error;
        } else {
            for (uint32_t i=0; i < comm_size; i++) {
                mtl_comm->c_index_vec[i].c_index_state = MCA_MTL_OFI_CID_NOT_EXCHANGED;
            }
        }
        if (OMPI_COMM_IS_INTRA(comm)) {
            mtl_comm->c_index_vec[comm->c_my_rank].c_index = comm->c_index;
            mtl_comm->c_index_vec[comm->c_my_rank].c_index_state = MCA_MTL_OFI_CID_EXCHANGED;
        }

        comm->c_mtl_comm = mtl_comm;

    } else  {

        comm->c_mtl_comm = NULL;

    }

    /*
     * If thread grouping enabled, add new OFI context for each communicator
     * other than MPI_COMM_SELF.
     */
    if ((ompi_mtl_ofi.thread_grouping && (MPI_COMM_SELF != comm)) ||
        /* If no thread grouping, add new OFI context only
         * for MPI_COMM_WORLD.
         */
        (!ompi_mtl_ofi.thread_grouping && (!ompi_mtl_ofi.is_initialized))) {

        ret = ompi_mtl_ofi_init_contexts(mtl, comm, ep_type);
        ompi_mtl_ofi.is_initialized = true;

        if (OMPI_SUCCESS != ret) {
            goto error;
        }
    }

error:
    return ret;
}

int ompi_mtl_ofi_del_comm(struct mca_mtl_base_module_t *mtl,
                          struct ompi_communicator_t *comm)
{
    int ret = OMPI_SUCCESS;
    mca_mtl_ofi_ep_type ep_type = (0 == ompi_mtl_ofi.enable_sep) ?
                                  OFI_REGULAR_EP : OFI_SCALABLE_EP;

    if(NULL != comm->c_mtl_comm) {
        free(comm->c_mtl_comm->c_index_vec);
        OBJ_RELEASE(comm->c_mtl_comm);
        comm->c_mtl_comm = NULL;
    }

    /*
     * Clean up OFI contexts information.
     */
    if ((ompi_mtl_ofi.thread_grouping && (MPI_COMM_SELF != comm)) ||
        (!ompi_mtl_ofi.thread_grouping && (MPI_COMM_WORLD == comm))) {

        ret = ompi_mtl_ofi_finalize_contexts(mtl, comm, ep_type);
    }

    return ret;
}

