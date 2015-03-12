/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (C) 2001-2011 Mellanox Technologies Ltd. ALL RIGHTS RESERVED.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"
#include "ompi/proc/proc.h"
#include "ompi/communicator/communicator.h"
#include "opal/memoryhooks/memory.h"
#include "opal/util/show_help.h"
#include "opal/mca/pmix/pmix.h"

#include "mtl_mxm.h"
#include "mtl_mxm_types.h"
#include "mtl_mxm_endpoint.h"
#include "mtl_mxm_request.h"

mca_mtl_mxm_module_t ompi_mtl_mxm = {
    {
       0, /* max context id */
       0, /* max tag value */
       0, /* request reserve space */
       0, /* flags */
       ompi_mtl_mxm_add_procs,
       ompi_mtl_mxm_del_procs,
       ompi_mtl_mxm_finalize,
       ompi_mtl_mxm_send,
       ompi_mtl_mxm_isend,
       ompi_mtl_mxm_irecv,
       ompi_mtl_mxm_iprobe,
       ompi_mtl_mxm_imrecv,
       ompi_mtl_mxm_improbe,
       ompi_mtl_mxm_cancel,
       ompi_mtl_mxm_add_comm,
       ompi_mtl_mxm_del_comm
    },
    0,
    0,
    NULL,
    NULL
};

#if MXM_API < MXM_VERSION(2,0)
static uint32_t ompi_mtl_mxm_get_job_id(void)
{
    uint8_t  unique_job_key[16];
    uint32_t job_key;
    unsigned long long *uu;
    char *generated_key;

    uu = (unsigned long long *) unique_job_key;

    generated_key = getenv("OMPI_MCA_orte_precondition_transports");
    memset(uu, 0, sizeof(unique_job_key));

    if (!generated_key || (strlen(generated_key) != 33) || sscanf(generated_key, "%016llx-%016llx", &uu[0], &uu[1]) != 2) {
        opal_show_help("help-mtl-mxm.txt", "no uuid present", true,
                       generated_key ? "could not be parsed from" :
                                       "not present in", ompi_process_info.nodename);
        return 0;
    }

    /*
     * decode OMPI_MCA_orte_precondition_transports that looks as
     * 000003ca00000000-0000000100000000
     * jobfam-stepid
     * to get jobid coded with ORTE_CONSTRUCT_LOCAL_JOBID()
     */
    #define GET_LOCAL_JOBID(local, job) \
        ( ((local) & 0xffff0000) | ((job) & 0x0000ffff) )
    job_key = GET_LOCAL_JOBID((uu[0]>>(8 * sizeof(int))) << 16, uu[1]>>(8 * sizeof(int)));

    return job_key;
}
#endif

int ompi_mtl_mxm_progress(void);
#if MXM_API >= MXM_VERSION(2,0)
static void ompi_mtl_mxm_mem_release_cb(void *buf, size_t length,
                                        void *cbdata, bool from_alloc);
#endif

#if MXM_API < MXM_VERSION(2,0)
static int ompi_mtl_mxm_get_ep_address(ompi_mtl_mxm_ep_conn_info_t *ep_info, mxm_ptl_id_t ptlid)
{
    size_t addrlen;
    mxm_error_t err;

    addrlen = sizeof(ep_info->ptl_addr[ptlid]);
    err = mxm_ep_address(ompi_mtl_mxm.ep, ptlid,
                         (struct sockaddr *) &ep_info->ptl_addr[ptlid], &addrlen);
    if (MXM_OK != err) {
        opal_show_help("help-mtl-mxm.txt", "unable to extract endpoint ptl address",
                       true, (int)ptlid, mxm_error_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
#else
static int ompi_mtl_mxm_get_ep_address(void **address_p, size_t *address_len_p)
{
    mxm_error_t err;

    *address_len_p = 0;
    err = mxm_ep_get_address(ompi_mtl_mxm.ep, NULL, address_len_p);
    if (err != MXM_ERR_BUFFER_TOO_SMALL) {
        MXM_ERROR("Failed to get ep address length");
        return OMPI_ERROR;
    }

    *address_p = malloc(*address_len_p);
    if (*address_p == NULL) {
        MXM_ERROR("Failed to allocate ep address buffer");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    err = mxm_ep_get_address(ompi_mtl_mxm.ep, *address_p, address_len_p);
    if (MXM_OK != err) {
        opal_show_help("help-mtl-mxm.txt", "unable to extract endpoint address",
                       true, mxm_error_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
#endif

#define max(a,b) ((a)>(b)?(a):(b))

static mxm_error_t
ompi_mtl_mxm_create_ep(mxm_h ctx, mxm_ep_h *ep, unsigned ptl_bitmap, int lr,
                         uint32_t jobid, uint64_t mxlr, int nlps)
{
    mxm_error_t err;

#if MXM_API < MXM_VERSION(2,0)
    ompi_mtl_mxm.mxm_ep_opts->job_id          = jobid;
    ompi_mtl_mxm.mxm_ep_opts->local_rank      = lr;
    ompi_mtl_mxm.mxm_ep_opts->num_local_procs = nlps;
    err = mxm_ep_create(ctx, ompi_mtl_mxm.mxm_ep_opts, ep);
#else
    err = mxm_ep_create(ctx, ompi_mtl_mxm.mxm_ep_opts, ep);
#endif
    return err;
}

/*
 * send information using modex (in some case there is limitation on data size for example ess/pmi)
 * set size of data sent for once
 *
 */
static int ompi_mtl_mxm_send_ep_address(void *address, size_t address_len)
{
    char *modex_component_name = mca_base_component_to_string(&mca_mtl_mxm_component.super.mtl_version);
    char *modex_name = malloc(strlen(modex_component_name) + 5);
    const size_t modex_max_size = 0x60;
    unsigned char *modex_buf_ptr;
    size_t modex_buf_size;
    size_t modex_cur_size;
    int modex_name_id = 0;
    int rc;

    /* Send address length */
    sprintf(modex_name, "%s-len", modex_component_name);
    OPAL_MODEX_SEND_STRING(rc, PMIX_SYNC_REQD, PMIX_GLOBAL,
                           modex_name, &address_len, sizeof(address_len));
    if (OMPI_SUCCESS != rc) {
        MXM_ERROR("failed to send address length");
        goto bail;
    }

    /* Send address, in parts.
     * modex name looks as mtl.mxm.1.5-18 where mtl.mxm.1.5 is the component and 18 is part index.
     */
    modex_buf_size = address_len;
    modex_buf_ptr = address;
    while (modex_buf_size) {
        sprintf(modex_name, "%s-%d", modex_component_name, modex_name_id);
        modex_cur_size = (modex_buf_size < modex_max_size) ? modex_buf_size : modex_max_size;
        OPAL_MODEX_SEND_STRING(rc, PMIX_SYNC_REQD, PMIX_GLOBAL,
                               modex_name, modex_buf_ptr, modex_cur_size);
        if (OMPI_SUCCESS != rc) {
            MXM_ERROR("Open MPI couldn't distribute EP connection details");
            goto bail;
        }

        modex_name_id++;
        modex_buf_ptr += modex_cur_size;
        modex_buf_size -= modex_cur_size;
    }

    rc = OMPI_SUCCESS;

bail:
    free(modex_component_name);
    free(modex_name);
    return rc;
}

/*
 * recieve information using modex
 */
static int ompi_mtl_mxm_recv_ep_address(ompi_proc_t *source_proc, void **address_p,
                                        size_t *address_len_p)
{
    char *modex_component_name = mca_base_component_to_string(&mca_mtl_mxm_component.super.mtl_version);
    char *modex_name = malloc(strlen(modex_component_name) + 5);
    unsigned char *modex_buf_ptr;
    size_t modex_cur_size;
    size_t modex_buf_size;
    size_t *address_len_buf_ptr;
    int modex_name_id = 0;
    int rc;

    *address_p = NULL;
    *address_len_p = 0;

    /* Receive address length */
    sprintf(modex_name, "%s-len", modex_component_name);
    OPAL_MODEX_RECV_STRING(rc, modex_name, &source_proc->super,
                           (char**)&address_len_buf_ptr,
                           &modex_cur_size);
    if (OMPI_SUCCESS != rc) {
        MXM_ERROR("Failed to receive ep address length");
        goto bail;
    }

    /* Allocate buffer to hold the address */
    *address_len_p = *address_len_buf_ptr;
    *address_p = malloc(*address_len_p);
    if (*address_p == NULL) {
        MXM_ERROR("Failed to allocate modex receive buffer");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto bail;
    }

    /* Receive the data, in parts */
    modex_buf_size = 0;
    while (modex_buf_size < *address_len_p) {
        sprintf(modex_name, "%s-%d", modex_component_name, modex_name_id);
        OPAL_MODEX_RECV_STRING(rc, modex_name, &source_proc->super,
                               (char**)&modex_buf_ptr,
                               &modex_cur_size);
        if (OMPI_SUCCESS != rc) {
            MXM_ERROR("Open MPI couldn't distribute EP connection details");
            free(*address_p);
            *address_p = NULL;
            *address_len_p = 0;
            goto bail;
        }

        memcpy((char*)(*address_p) + modex_buf_size, modex_buf_ptr, modex_cur_size);
        modex_buf_size += modex_cur_size;
        modex_name_id++;
    }

    rc = OMPI_SUCCESS;
bail:
    free(modex_component_name);
    free(modex_name);
    return rc;
}

int ompi_mtl_mxm_module_init(void)
{
#if MXM_API < MXM_VERSION(2,0)
    ompi_mtl_mxm_ep_conn_info_t ep_info;
#endif
    void *ep_address;
    size_t ep_address_len;
    mxm_error_t err;
    uint32_t jobid;
    uint64_t mxlr;
    ompi_proc_t **procs;
    unsigned ptl_bitmap;
    size_t totps, proc;
    int lr, nlps;
    int rc;

    mxlr = 0;
    lr = -1;
    jobid = 0;

#if MXM_API < MXM_VERSION(2,0)
    jobid = ompi_mtl_mxm_get_job_id();
    if (0 == jobid) {
    	MXM_ERROR("Failed to generate jobid");
    	return OMPI_ERROR;
    }
#endif

    if (NULL == (procs = ompi_proc_world(&totps))) {
        MXM_ERROR("Unable to obtain process list");
        return OMPI_ERROR;
    }

    if (totps < (size_t)ompi_mtl_mxm.mxm_np) {
        MXM_VERBOSE(1, "MXM support will be disabled because of total number "
                    "of processes (%lu) is less than the minimum set by the "
                    "mtl_mxm_np MCA parameter (%u)", totps, ompi_mtl_mxm.mxm_np);
        free(procs);
        return OMPI_ERR_NOT_SUPPORTED;
    }
    MXM_VERBOSE(1, "MXM support enabled");

    if (ORTE_NODE_RANK_INVALID == (lr = ompi_process_info.my_node_rank)) {
        MXM_ERROR("Unable to obtain local node rank");
        free(procs);
        return OMPI_ERROR;
    }
    nlps = ompi_process_info.num_local_peers + 1;

    for (proc = 0; proc < totps; proc++) {
        if (OPAL_PROC_ON_LOCAL_NODE(procs[proc]->super.proc_flags)) {
            mxlr = max(mxlr, procs[proc]->super.proc_name.vpid);
        }
    }
    free(procs);

    /* Setup the endpoint options and local addresses to bind to. */
#if MXM_API < MXM_VERSION(2,0)
    ptl_bitmap = ompi_mtl_mxm.mxm_ctx_opts->ptl_bitmap;
#else
    ptl_bitmap = 0;
#endif

    /* Open MXM endpoint */
    err = ompi_mtl_mxm_create_ep(ompi_mtl_mxm.mxm_context, &ompi_mtl_mxm.ep,
                                 ptl_bitmap, lr, jobid, mxlr, nlps);
    if (MXM_OK != err) {
        opal_show_help("help-mtl-mxm.txt", "unable to create endpoint", true,
        		mxm_error_string(err));
        return OMPI_ERROR;
    }

    /*
     * Get address for each PTL on this endpoint, and share it with other ranks.
     */
#if MXM_API < MXM_VERSION(2,0)
    if ((ptl_bitmap & MXM_BIT(MXM_PTL_SELF)) &&
            OMPI_SUCCESS != ompi_mtl_mxm_get_ep_address(&ep_info, MXM_PTL_SELF)) {
    	return OMPI_ERROR;
    }
    if ((ptl_bitmap & MXM_BIT(MXM_PTL_RDMA)) &&
            OMPI_SUCCESS != ompi_mtl_mxm_get_ep_address(&ep_info, MXM_PTL_RDMA)) {
    	return OMPI_ERROR;
    }
    if ((ptl_bitmap & MXM_BIT(MXM_PTL_SHM)) &&
            OMPI_SUCCESS != ompi_mtl_mxm_get_ep_address(&ep_info, MXM_PTL_SHM)) {
            return OMPI_ERROR;
    }

    ep_address = &ep_info;
    ep_address_len = sizeof(ep_info);
#else
    rc = ompi_mtl_mxm_get_ep_address(&ep_address, &ep_address_len);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }
#endif

    rc = ompi_mtl_mxm_send_ep_address(ep_address, ep_address_len);
    if (OMPI_SUCCESS != rc) {
        MXM_ERROR("Modex session failed.");
        return rc;
    }

#if MXM_API >= MXM_VERSION(2,0)
    free(ep_address);
#endif

    /* Register the MXM progress function */
    opal_progress_register(ompi_mtl_mxm_progress);

#if MXM_API >= MXM_VERSION(2,0)
    if (ompi_mtl_mxm.using_mem_hooks) {
        opal_mem_hooks_register_release(ompi_mtl_mxm_mem_release_cb, NULL);
    }
#endif
    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_finalize(struct mca_mtl_base_module_t* mtl)
{
#if MXM_API >= MXM_VERSION(2,0)
    if (ompi_mtl_mxm.using_mem_hooks) {
        opal_mem_hooks_unregister_release(ompi_mtl_mxm_mem_release_cb);
    }
#endif
    opal_progress_unregister(ompi_mtl_mxm_progress);
    mxm_ep_destroy(ompi_mtl_mxm.ep);
    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_add_procs(struct mca_mtl_base_module_t *mtl, size_t nprocs,
                           struct ompi_proc_t** procs)
{
#if MXM_API < MXM_VERSION(2,0)
    ompi_mtl_mxm_ep_conn_info_t *ep_info;
    mxm_conn_req_t *conn_reqs;
    size_t ep_index = 0;
#endif
    void *ep_address = NULL;
    size_t ep_address_len;
    mxm_error_t err;
    size_t i;
    int rc;
    mca_mtl_mxm_endpoint_t *endpoint;

    assert(mtl == &ompi_mtl_mxm.super);

#if MXM_API < MXM_VERSION(2,0)
    /* Allocate connection requests */
    conn_reqs = calloc(nprocs, sizeof(mxm_conn_req_t));
    ep_info   = calloc(nprocs, sizeof(ompi_mtl_mxm_ep_conn_info_t));
    if (NULL == conn_reqs || NULL == ep_info) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto bail;
    }
#endif

    /* Get the EP connection requests for all the processes from modex */
    for (i = 0; i < nprocs; ++i) {
        if (NULL != procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL]) {
           continue; /* already connected to this endpoint */
        }
        rc = ompi_mtl_mxm_recv_ep_address(procs[i], &ep_address, &ep_address_len);
        if (rc != OMPI_SUCCESS) {
            goto bail;
        }

#if MXM_API < MXM_VERSION(2,0)
        if (ep_address_len != sizeof(ep_info[i])) {
            MXM_ERROR("Invalid endpoint address length");
            free(ep_address);
            rc = OMPI_ERROR;
            goto bail;
        }

        memcpy(&ep_info[i], ep_address, ep_address_len);
        free(ep_address);
        conn_reqs[ep_index].ptl_addr[MXM_PTL_SELF] = (struct sockaddr *)&(ep_info[i].ptl_addr[MXM_PTL_SELF]);
        conn_reqs[ep_index].ptl_addr[MXM_PTL_SHM]  = (struct sockaddr *)&(ep_info[i].ptl_addr[MXM_PTL_SHM]);
        conn_reqs[ep_index].ptl_addr[MXM_PTL_RDMA] = (struct sockaddr *)&(ep_info[i].ptl_addr[MXM_PTL_RDMA]);
        ep_index++;

#else
        endpoint = OBJ_NEW(mca_mtl_mxm_endpoint_t);
        endpoint->mtl_mxm_module = &ompi_mtl_mxm;
        err = mxm_ep_connect(ompi_mtl_mxm.ep, ep_address, &endpoint->mxm_conn);
        free(ep_address);
        if (err != MXM_OK) {
            MXM_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
            rc = OMPI_ERROR;
            goto bail;
        }
        procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL] = endpoint;
#endif
    }

#if MXM_API < MXM_VERSION(2,0)
    /* Connect to remote peers */
    err = mxm_ep_connect(ompi_mtl_mxm.ep, conn_reqs, ep_index, -1);
    if (MXM_OK != err) {
        MXM_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
        for (i = 0; i < ep_index; ++i) {
            if (MXM_OK != conn_reqs[i].error) {
                MXM_ERROR("MXM EP connect to %s error: %s\n",
                          (NULL == procs[i]->super.proc_hostname) ?
                           "unknown" : procs[i]->proc_hostname,
                          mxm_error_string(conn_reqs[i].error));
            }
        }
        rc = OMPI_ERROR;
        goto bail;
    }

    /* Save returned connections */
    for (i = 0; i < ep_index; ++i) {
        endpoint = OBJ_NEW(mca_mtl_mxm_endpoint_t);
        endpoint->mtl_mxm_module = &ompi_mtl_mxm;
        endpoint->mxm_conn = conn_reqs[i].conn;
        procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL] = endpoint;
    }

#endif

#if MXM_API >= MXM_VERSION(3,1)
    if (ompi_mtl_mxm.bulk_connect) {
        mxm_ep_wireup(ompi_mtl_mxm.ep);
    }
#endif

    rc = OMPI_SUCCESS;

bail:
#if MXM_API < MXM_VERSION(2,0)
    free(conn_reqs);
    free(ep_info);
#endif
    return rc;
}

int ompi_mtl_add_single_proc(struct mca_mtl_base_module_t *mtl,
                             struct ompi_proc_t* procs)
{
    void *ep_address = NULL;
    size_t ep_address_len;
    mxm_error_t err;
    int rc;
    mca_mtl_mxm_endpoint_t *endpoint;

    assert(mtl == &ompi_mtl_mxm.super);

    if (NULL != procs->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL]) {
       return OMPI_SUCCESS;
    }
    rc = ompi_mtl_mxm_recv_ep_address(procs, &ep_address, &ep_address_len);
    if (rc != OMPI_SUCCESS) {
        return rc;
    }

#if MXM_API < MXM_VERSION(2,0)
    ompi_mtl_mxm_ep_conn_info_t ep_info;
    mxm_conn_req_t conn_req;

    if (ep_address_len != sizeof(ep_info)) {
        MXM_ERROR("Invalid endpoint address length");
        free(ep_address);
        return OMPI_ERROR;
    }

    memcpy(&ep_info, ep_address, ep_address_len);
    free(ep_address);
    conn_req.ptl_addr[MXM_PTL_SELF] = (struct sockaddr *)&(ep_info.ptl_addr[MXM_PTL_SELF]);
    conn_req.ptl_addr[MXM_PTL_SHM]  = (struct sockaddr *)&(ep_info.ptl_addr[MXM_PTL_SHM]);
    conn_req.ptl_addr[MXM_PTL_RDMA] = (struct sockaddr *)&(ep_info.ptl_addr[MXM_PTL_RDMA]);

    /* Connect to remote peers */
    err = mxm_ep_connect(ompi_mtl_mxm.ep, conn_req, 1, -1);
    if (MXM_OK != err) {
        MXM_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
        if (MXM_OK != conn_req.error) {
            MXM_ERROR("MXM EP connect to %s error: %s\n",
                      (NULL == procs->super.proc_hostname) ?
                       "unknown" : procs->proc_hostname,
                      mxm_error_string(conn_reqs.error));
        }
        return OMPI_ERROR;
    }

    /* Save returned connections */
    endpoint = OBJ_NEW(mca_mtl_mxm_endpoint_t);
    endpoint->mtl_mxm_module = &ompi_mtl_mxm;
    endpoint->mxm_conn = conn_reqs.conn;
    procs->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL] = endpoint;
#else
    endpoint = OBJ_NEW(mca_mtl_mxm_endpoint_t);
    endpoint->mtl_mxm_module = &ompi_mtl_mxm;
    err = mxm_ep_connect(ompi_mtl_mxm.ep, ep_address, &endpoint->mxm_conn);
    free(ep_address);
    if (err != MXM_OK) {
        MXM_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
        return OMPI_ERROR;
    }
    procs->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL] = endpoint;
#endif

#if MXM_API >= MXM_VERSION(3,1)
    if (ompi_mtl_mxm.bulk_connect) {
        mxm_ep_wireup(ompi_mtl_mxm.ep);
    }
#endif
    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_del_procs(struct mca_mtl_base_module_t *mtl, size_t nprocs,
                           struct ompi_proc_t** procs)
{
    size_t i;

#if MXM_API >= MXM_VERSION(3,1)
    if (ompi_mtl_mxm.bulk_disconnect) {
        size_t nprocs_world;
        ompi_proc_t **procs;
        procs = ompi_proc_world(&nprocs_world);
        if (nprocs == nprocs_world) {
            mxm_ep_powerdown(ompi_mtl_mxm.ep);
        }
        free(procs);
    }
#endif

    /* XXX: Directly accessing the obj_reference_count is an abstraction
     * violation of the object system. We know this needs to be fixed, but
     * are deferring the fix to a later time as it involves a design issue
     * in the way we handle endpoints as objects
     */
    for (i = 0; i < nprocs; ++i) {
        mca_mtl_mxm_endpoint_t *endpoint = (mca_mtl_mxm_endpoint_t*)
            procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
        if (endpoint) {
            mxm_ep_disconnect(endpoint->mxm_conn);
            OBJ_RELEASE(endpoint);
        }
    }
    opal_pmix.fence(NULL, 0);
    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_add_comm(struct mca_mtl_base_module_t *mtl,
                          struct ompi_communicator_t *comm)
{
    mxm_error_t err;
    mxm_mq_h mq;

    assert(mtl == &ompi_mtl_mxm.super);
    assert(NULL != ompi_mtl_mxm.mxm_context);

    err = mxm_mq_create(ompi_mtl_mxm.mxm_context, comm->c_contextid, &mq);
    if (MXM_OK != err) {
        opal_show_help("help-mtl-mxm.txt", "mxm mq create", true, mxm_error_string(err));
        return OMPI_ERROR;
    }

    comm->c_pml_comm = (void*)mq;
    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_del_comm(struct mca_mtl_base_module_t *mtl,
                          struct ompi_communicator_t *comm)
{
    assert(mtl == &ompi_mtl_mxm.super);
    if (NULL != ompi_mtl_mxm.mxm_context) {
        mxm_mq_destroy((mxm_mq_h)comm->c_pml_comm);
    }
    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_progress(void)
{
    mxm_error_t err;

    err = mxm_progress(ompi_mtl_mxm.mxm_context);
    if ((MXM_OK != err) && (MXM_ERR_NO_PROGRESS != err) ) {
        opal_show_help("help-mtl-mxm.txt", "errors during mxm_progress", true, mxm_error_string(err));
    }
    return 1;
}

#if MXM_API >= MXM_VERSION(2,0)
static void ompi_mtl_mxm_mem_release_cb(void *buf, size_t length,
                                        void *cbdata, bool from_alloc)
{
    mxm_mem_unmap(ompi_mtl_mxm.mxm_context, buf, length,
                  from_alloc ? MXM_MEM_UNMAP_MARK_INVALID : 0);
}
#endif

OBJ_CLASS_INSTANCE(
        ompi_mtl_mxm_message_t,
        opal_free_list_item_t,
        NULL,
        NULL);
