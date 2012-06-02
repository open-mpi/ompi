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
       ompi_mtl_mxm_imrecv,
       ompi_mtl_mxm_improbe,
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
        		true, (int)ptlid, mxm_error_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

#define max(a,b) ((a)>(b)?(a):(b))

int ompi_mtl_mxm_module_init(void)
{
    struct sockaddr_mxm_local_proc sa_bind_self;
    struct sockaddr_mxm_ib_local sa_bind_rdma;
    struct sockaddr_mxm_shm_proc sa_bind_shm;
    mxm_ep_opts_t ep_opt;
    ompi_mtl_mxm_ep_conn_info_t ep_info;
    mxm_error_t err;
    uint32_t jobid;
    uint64_t mxlr;
    ompi_proc_t *mp, **procs;
    unsigned    ptl_bitmap;
    size_t totps, proc;
    int lr, nlps;

    mxlr = 0;
    lr = -1;
    nlps = 0;

    mp = ompi_proc_local();
    jobid = ompi_mtl_mxm_get_job_id();
    if (0 == jobid) {
    	MXM_ERROR("Failed to generate jobid");
    	return OMPI_ERROR;
    }

#if 0 /* https://svn.open-mpi.org/trac/ompi/ticket/2971 */
    if ((rc = ompi_proc_refresh()) != OMPI_SUCCESS) {
        MXM_ERROR("Unable to refresh processes");
        return OMPI_ERROR;
    }
#endif

    if (NULL == (procs = ompi_proc_world(&totps))) {
        MXM_ERROR("Unable to obtain process list");
        return OMPI_ERROR;
    }

    if (totps < (size_t)ompi_mtl_mxm.mxm_np) {
        MXM_VERBOSE(1, "MXM support will be disabled because of total number of processes"
                "(%lu) is less then default (%u)",totps, ompi_mtl_mxm.mxm_np);
                return OMPI_ERR_NOT_SUPPORTED;
    }
    MXM_VERBOSE(1, "MXM support enabled");

    for (proc = 0; proc < totps; proc++) {
        if (mp == procs[proc]) {
            lr = nlps++;
            mxlr = max(mxlr, procs[proc]->proc_name.vpid);
            continue;
        }

        if (OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags)) {
            mxlr = max(mxlr, procs[proc]->proc_name.vpid);
            nlps++;
        }
    }

    /* Setup the endpoint options and local addresses to bind to. */
    mxm_fill_ep_opts(&ep_opt);

    sa_bind_self.sa_family = AF_MXM_LOCAL_PROC;
    sa_bind_self.context_id = lr;
    sa_bind_self.process_id = mp->proc_name.vpid;

    sa_bind_rdma.sa_family = AF_MXM_IB_LOCAL;
    sa_bind_rdma.lid = 0;
    sa_bind_rdma.pkey = 0;
    sa_bind_rdma.qp_num = 0;
    sa_bind_rdma.sl = 0;

    sa_bind_shm.sa_family = AF_MXM_SHM_PROC;
    sa_bind_shm.jobid = jobid;
    sa_bind_shm.process_id = lr;
    sa_bind_shm.context_id = mxlr;
    sa_bind_shm.num_procs = nlps;

    ptl_bitmap = ompi_mtl_mxm.mxm_opts.ptl_bitmap;

    ep_opt.ptl_bind_addr[MXM_PTL_SELF] = (ptl_bitmap & MXM_BIT(MXM_PTL_SELF))?
            (struct sockaddr*)&sa_bind_self:NULL;
    ep_opt.ptl_bind_addr[MXM_PTL_RDMA] =(ptl_bitmap & MXM_BIT(MXM_PTL_RDMA))?
            (struct sockaddr*)&sa_bind_rdma:NULL;
    ep_opt.ptl_bind_addr[MXM_PTL_SHM] =(ptl_bitmap & MXM_BIT(MXM_PTL_SHM))?
            (struct sockaddr*)&sa_bind_shm:NULL;

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
     
    /* 
     * send information using modex (in some case there is limitation on data size for example ess/pmi)
     * set size of data sent for once
     */
    {
        size_t modex_max_size = 0x60;
        unsigned char *modex_buf_ptr = (unsigned char *)&ep_info;
        size_t modex_buf_size = sizeof(ep_info);
        size_t modex_cur_size = (modex_buf_size < modex_max_size ? modex_buf_size : modex_max_size);
        char *modex_component_name = mca_base_component_to_string(&mca_mtl_mxm_component.super.mtl_version);
        char *modex_name = malloc(strlen(modex_component_name) + 5);
        int modex_name_id = 0;

        while (modex_buf_size) {
            /* modex name looks as mtl.mxm.1.5-18 where mtl.mxm.1.5 is the component and 18 is portion index */
            sprintf(modex_name, "%s-%d", modex_component_name, modex_name_id);     
      
            if (OMPI_SUCCESS != ompi_modex_send_string((const char *)modex_name, modex_buf_ptr, modex_cur_size)) {
        MXM_ERROR("Open MPI couldn't distribute EP connection details");
                free(modex_component_name);
                free(modex_name);
        return OMPI_ERROR;
            }
            modex_name_id++;
            modex_buf_ptr += modex_cur_size;
            modex_buf_size -= modex_cur_size;
            modex_cur_size = (modex_buf_size < modex_max_size ? modex_buf_size : modex_max_size);
        }
        free(modex_component_name);
        free(modex_name);
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
    ompi_mtl_mxm_ep_conn_info_t *ep_info;
    mxm_conn_req_t *conn_reqs;
    mxm_error_t err;
    size_t i;
    int rc;

    assert(mtl == &ompi_mtl_mxm.super);

    /* Allocate connection requests */
    conn_reqs = malloc(nprocs * sizeof(mxm_conn_req_t));
    ep_info = malloc(nprocs * sizeof(ompi_mtl_mxm_ep_conn_info_t));
    memset(conn_reqs, 0x0, sizeof(mxm_conn_req_t));
    memset(ep_info, 0x0, sizeof(ompi_mtl_mxm_ep_conn_info_t));
    if (NULL == conn_reqs || NULL == ep_info) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto bail;
    }

    /* Get the EP connection requests for all the processes from modex */
    for (i = 0; i < nprocs; ++i) {
        /* 
         * recieve information using modex
         */
        {
            unsigned char *ep_info_ptr = (unsigned char*)(ep_info + i);
            unsigned char *modex_buf_ptr = NULL;
            size_t modex_buf_size = sizeof(ompi_mtl_mxm_ep_conn_info_t);
            size_t modex_cur_size = 0;
            char *modex_component_name = mca_base_component_to_string(&mca_mtl_mxm_component.super.mtl_version);
            char *modex_name = malloc(strlen(modex_component_name) + 5);
            int modex_name_id = 0;

            while (modex_buf_size > 0) {
               /* modex name looks as mtl.mxm.1.5-18 where mtl.mxm.1.5 is the component and 18 is portion index */
               sprintf(modex_name, "%s-%d", modex_component_name, modex_name_id);           

                if (OMPI_SUCCESS != ompi_modex_recv_string((const char *)modex_name, procs[i], (void**)&modex_buf_ptr, &modex_cur_size)) {
                    MXM_ERROR("Open MPI couldn't distribute EP connection details");
                    free(modex_name);
            goto bail;
        }
                /* fill in ep_info[i] with recieved data */
                memcpy((void*)ep_info_ptr, modex_buf_ptr, modex_cur_size);

                if (modex_cur_size > modex_buf_size) {
                    MXM_ERROR("Open MPI couldn't distribute EP connection details: incorrect size of message");
                    free(modex_component_name);
                    free(modex_name);
                    goto bail;
                }
                modex_name_id++;
                ep_info_ptr += modex_cur_size;
                modex_buf_size -= modex_cur_size;
                modex_cur_size = 0;
            }
            free(modex_component_name);
            free(modex_name);
        }

        conn_reqs[i].ptl_addr[MXM_PTL_SELF] = (struct sockaddr *)&(ep_info[i].ptl_addr[MXM_PTL_SELF]);
        conn_reqs[i].ptl_addr[MXM_PTL_SHM] = (struct sockaddr *)&(ep_info[i].ptl_addr[MXM_PTL_SHM]);
        conn_reqs[i].ptl_addr[MXM_PTL_RDMA] = (struct sockaddr *)&(ep_info[i].ptl_addr[MXM_PTL_RDMA]);
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
        rc = OMPI_ERROR;
        goto bail;
    }

    /* Save returned connections */
    for (i = 0; i < nprocs; ++i) {
        mtl_peer_data[i] = (mca_mtl_mxm_endpoint_t *) OBJ_NEW(mca_mtl_mxm_endpoint_t);
        mtl_peer_data[i]->mtl_mxm_module = &ompi_mtl_mxm;
        mtl_peer_data[i]->mxm_conn = conn_reqs[i].conn;
    }
	rc = OMPI_SUCCESS;

bail:
    if (conn_reqs)
    	free(conn_reqs);
    if (ep_info)
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
