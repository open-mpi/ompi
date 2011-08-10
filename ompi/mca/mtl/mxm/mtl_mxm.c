/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"
#include "ompi/proc/proc.h"
#include "ompi/communicator/communicator.h"

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
       ompi_mtl_mxm_cancel,
       ompi_mtl_mxm_add_comm,
       ompi_mtl_mxm_del_comm
    }
};


static uint32_t ompi_mtl_mxm_get_job_id(void)
{
    uint8_t  unique_job_key[16];
    uint32_t job_key;
    unsigned long long *uu;
    char *generated_key;
    uint16_t *jkp;

    jkp = (uint16_t *) unique_job_key;
    uu = (unsigned long long *) unique_job_key;

    generated_key = getenv("OMPI_MCA_orte_precondition_transports");
    memset(uu, 0, sizeof(unique_job_key));

    if (!generated_key || (strlen(generated_key) != 33) || sscanf(generated_key, "%016llx-%016llx", &uu[0], &uu[1]) != 2) {
        orte_show_help("help-mtl-mxm.txt", "no uuid present", true,
                       generated_key ? "could not be parsed from" :
                                       "not present in", orte_process_info.nodename);
        return 0;
    }

    job_key  = ((jkp[2] ^ jkp[3]) >> 8) | ((jkp[0] ^ jkp[1]) << 8);
    job_key ^= ((jkp[6] ^ jkp[7]) >> 8) | ((jkp[4] ^ jkp[5]) << 8);
    job_key &= ~0xff;
    return job_key;
}

int ompi_mtl_mxm_progress(void);

static int ompi_mtl_mxm_get_ep_address(ompi_mtl_mxm_ep_conn_info_t *ep_info, mxm_ptl_id_t ptlid)
{
    size_t addrlen;
    mxm_error_t err;

    addrlen = sizeof(ep_info->ptl_addr[ptlid]);
    err = mxm_ep_address(ompi_mtl_mxm.ep, ptlid,
    		(struct sockaddr *) &ep_info->ptl_addr[ptlid], &addrlen);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "unable to extract endpoint address",
        		true, mxm_error_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int ompi_mtl_mxm_module_init(void)
{
    struct sockaddr_mxm_local_proc sa_bind_self;
    struct sockaddr_mxm_ib_local sa_bind_rdma;
    mxm_ep_opts_t ep_opt;
    ompi_mtl_mxm_ep_conn_info_t ep_info;
    mxm_error_t err;
    uint32_t jobid;

    jobid = ompi_mtl_mxm_get_job_id();
    if (0 == jobid) {
    	MXM_ERROR("Failed to generate jobid");
    	return OMPI_ERROR;
    }

    /* Setup the endpoint options and local addresses to bind to. */
    mxm_fill_ep_opts(&ep_opt);

    sa_bind_self.sa_family = AF_MXM_LOCAL_PROC;
    sa_bind_self.context_id = jobid;
    sa_bind_self.process_id = getpid();

    sa_bind_rdma.sa_family = AF_MXM_IB_LOCAL;
    sa_bind_rdma.lid = 0;
    sa_bind_rdma.pkey = 0;
    sa_bind_rdma.qp_num = 0;
    sa_bind_rdma.sl = 0;

    ep_opt.ptl_bind_addr[MXM_PTL_SELF] = (struct sockaddr*)&sa_bind_self;
    ep_opt.ptl_bind_addr[MXM_PTL_RDMA] = (struct sockaddr*)&sa_bind_rdma;

    /* Open MXM endpoint */
    err = mxm_ep_create(ompi_mtl_mxm.mxm_context, &ep_opt, &ompi_mtl_mxm.ep);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "unable to create endpoint", true,
        		mxm_error_string(err));
        return OMPI_ERROR;
    }

    /*
     * Get address for each PTL on this endpoint, and share it with other ranks.
     */
    if (OMPI_SUCCESS != ompi_mtl_mxm_get_ep_address(&ep_info, MXM_PTL_SELF)) {
    	return OMPI_ERROR;
    }
    if (OMPI_SUCCESS != ompi_mtl_mxm_get_ep_address(&ep_info, MXM_PTL_RDMA)) {
    	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_modex_send(&mca_mtl_mxm_component.super.mtl_version,
                                        &ep_info, sizeof(ep_info))) {
        MXM_ERROR("Open MPI couldn't distribute EP connection details");
        return OMPI_ERROR;
    }

    /* Register the MXM progress function */
    opal_progress_register(ompi_mtl_mxm_progress);
    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_finalize(struct mca_mtl_base_module_t* mtl)
{
    opal_progress_unregister(ompi_mtl_mxm_progress);
    mxm_ep_destroy(ompi_mtl_mxm.ep);
    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_add_procs(struct mca_mtl_base_module_t *mtl, size_t nprocs,
                           struct ompi_proc_t** procs, /*const*/
                           struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    ompi_mtl_mxm_ep_conn_info_t **ep_info;
    mxm_conn_req_t *conn_reqs;
    mxm_error_t err;
    size_t size;
    size_t i;
    int rc;

    assert(mtl == &ompi_mtl_mxm.super);

    /* Allocate connection requests */
    conn_reqs = malloc(nprocs * sizeof *conn_reqs);
    ep_info = malloc(nprocs * sizeof *ep_info);
    if (NULL == conn_reqs || NULL == ep_info) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto bail;
    }

    /* Get the EP connection requests for all the processes from modex */
    for (i = 0; i < nprocs; ++i) {
        rc = ompi_modex_recv(&mca_mtl_mxm_component.super.mtl_version, procs[i],
                             (void**)&ep_info[i], &size);
        if (rc != OMPI_SUCCESS || size != sizeof(ompi_mtl_mxm_ep_conn_info_t)) {
            goto bail;
        }

        conn_reqs[i].ptl_addr[MXM_PTL_SELF] = (struct sockaddr *)&ep_info[i]->ptl_addr[MXM_PTL_SELF];
        conn_reqs[i].ptl_addr[MXM_PTL_SHM] = NULL;
        conn_reqs[i].ptl_addr[MXM_PTL_RDMA] = (struct sockaddr *)&ep_info[i]->ptl_addr[MXM_PTL_RDMA];
    }

    /* Connect to remote peers */
    err = mxm_ep_connect(ompi_mtl_mxm.ep, conn_reqs, nprocs, 1000);
    if (MXM_OK != err) {
        MXM_ERROR("MXM returned connect error: %s\n", mxm_error_string(err));
        for (i = 0; i < nprocs; ++i) {
            if (MXM_OK != conn_reqs[i].error) {
                MXM_ERROR("MXM EP connect to %s error: %s\n", procs[i]->proc_hostname,
                          mxm_error_string(conn_reqs[i].error));
            }
        }
        goto bail;
    }

    /* Save returned connections */
    for (i = 0; i < nprocs; ++i) {
        mtl_peer_data[i] = (mca_mtl_mxm_endpoint_t *) OBJ_NEW(mca_mtl_mxm_endpoint_t);
        mtl_peer_data[i]->mtl_mxm_module = &ompi_mtl_mxm;
        mtl_peer_data[i]->mxm_conn = conn_reqs[i].conn;
    }

bail:
    free(conn_reqs);
    free(ep_info);
    return rc;
}

int ompi_mtl_mxm_del_procs(struct mca_mtl_base_module_t *mtl, size_t nprocs,
                           struct ompi_proc_t** procs,
                           struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    size_t i;

    for (i = 0; i < nprocs; ++i) {
        mxm_ep_disconnect(mtl_peer_data[i]->mxm_conn);
        OBJ_RELEASE(mtl_peer_data[i]);
    }
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
        orte_show_help("help-mtl-mxm.txt", "mxm mq create", true, mxm_error_string(err));
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
        orte_show_help("help-mtl-mxm.txt", "errors during mxm_progress", true, mxm_error_string(err));
    }
    return 1;
}
