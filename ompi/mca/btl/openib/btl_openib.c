/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <inttypes.h>
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "opal/util/show_help.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_openib.h"
#include "btl_openib_frag.h" 
#include "btl_openib_proc.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_xrc.h"
#include "ompi/datatype/convertor.h" 
#include "ompi/datatype/datatype.h" 
#include "ompi/datatype/dt_arch.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/runtime/params.h"
#include "orte/util/sys_info.h"
#include <errno.h> 
#include <string.h> 
#include <math.h>
#include <inttypes.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif


mca_btl_openib_module_t mca_btl_openib_module = {
    {
        &mca_btl_openib_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* btl_rdma_pipeline_send_length */
        0, /* btl_rdma_pipeline_frag_size */
        0, /* btl_min_rdma_pipeline_size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0, /* TODO this should be PUT btl flags */
        mca_btl_openib_add_procs,
        mca_btl_openib_del_procs,
        mca_btl_openib_register, 
        mca_btl_openib_finalize,
        /* we need alloc free, pack */ 
        mca_btl_openib_alloc, 
        mca_btl_openib_free, 
        mca_btl_openib_prepare_src,
        mca_btl_openib_prepare_dst,
        mca_btl_openib_send,
        mca_btl_openib_put,
        mca_btl_openib_get,
        mca_btl_base_dump,
        NULL, /* mpool */
        mca_btl_openib_register_error_cb, /* error call back registration */
        mca_btl_openib_ft_event
    }
};

/*
 * Local functions
 */
static int mca_btl_openib_size_queues( struct mca_btl_openib_module_t* openib_btl, size_t nprocs);
static int mca_btl_finalize_hca(struct mca_btl_openib_hca_t *hca);


static void show_init_error(const char *file, int line, 
                            const char *func, const char *dev) 
{
    if (ENOMEM == errno) {
        int ret;
        struct rlimit limit;
        char *str_limit = NULL;

        ret = getrlimit(RLIMIT_MEMLOCK, &limit);
        if (0 != ret) {
            asprintf(&str_limit, "Unknown");
        } else if (limit.rlim_cur == RLIM_INFINITY) {
            asprintf(&str_limit, "unlimited");
        } else {
            asprintf(&str_limit, "%ld", (long)limit.rlim_cur);
        }

        opal_show_help("help-mpi-btl-openib.txt", "init-fail-no-mem",
                       true, orte_system_info.nodename, 
                       file, line, func, dev, str_limit);

        if (NULL != str_limit) free(str_limit);
    } else {
        opal_show_help("help-mpi-btl-openib.txt", "init-fail-create-q",
                       true, orte_system_info.nodename, 
                       file, line, func, strerror(errno), errno, dev);
    }
}


/* 
 *  add a proc to this btl module 
 *    creates an endpoint that is setup on the
 *    first send to the endpoint
 */ 
int mca_btl_openib_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*)btl;
    int i,j, rc;
    int rem_subnet_id_port_cnt;
    int lcl_subnet_id_port_cnt = 0;
    int btl_rank = 0;
    mca_btl_base_endpoint_t* endpoint;
    
    for(j=0; j < mca_btl_openib_component.ib_num_btls; j++){ 
        if(mca_btl_openib_component.openib_btls[j]->port_info.subnet_id
           == openib_btl->port_info.subnet_id) { 
            if(openib_btl == mca_btl_openib_component.openib_btls[j]) { 
                btl_rank = lcl_subnet_id_port_cnt;
            }
            lcl_subnet_id_port_cnt++;
        }
    }

#if HAVE_XRC
    if(MCA_BTL_XRC_ENABLED &&
            NULL == mca_btl_openib_component.ib_addr_table.ht_table) {
        if(OPAL_SUCCESS != opal_hash_table_init(
                    &mca_btl_openib_component.ib_addr_table, nprocs)) {
            BTL_ERROR(("XRC internal error. Failed to allocate ib_table\n"));
            return OMPI_ERROR;
        }
    }
#endif

    for(i = 0; i < (int) nprocs; i++) {
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_openib_proc_t* ib_proc;
        
        if(NULL == (ib_proc = mca_btl_openib_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        rem_subnet_id_port_cnt  = 0;
        /* check if the remote proc has a reachable subnet first */
        BTL_VERBOSE(("got %d port_infos \n", ib_proc->proc_port_count));
        for(j = 0; j < (int) ib_proc->proc_port_count; j++){
            BTL_VERBOSE(("got a subnet %016x\n",
                         ib_proc->proc_ports[j].subnet_id));
            if(ib_proc->proc_ports[j].subnet_id ==
               openib_btl->port_info.subnet_id) {
                BTL_VERBOSE(("Got a matching subnet!\n"));
                rem_subnet_id_port_cnt ++;
            }
        }
        
        if(!rem_subnet_id_port_cnt ) {
            /* no use trying to communicate with this endpointlater */
            BTL_VERBOSE(("No matching subnet id was found, moving on.. \n"));
            continue;
        }

#if 0
        num_endpoints = rem_subnet_id_port_cnt  / lcl_subnet_id_port_cnt + 
            (btl_rank < (rem_subnet_id_port_cnt  / lcl_subnet_id_port_cnt)) ? 1:0;
#endif 

        if(rem_subnet_id_port_cnt  < lcl_subnet_id_port_cnt && 
           btl_rank >= rem_subnet_id_port_cnt ) {
            BTL_VERBOSE(("Not enough remote ports on this subnet id, moving on.. \n"));
            continue;
            
        }
        OPAL_THREAD_LOCK(&ib_proc->proc_lock);

        /* The btl_proc datastructure is shared by all IB BTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        endpoint = OBJ_NEW(mca_btl_openib_endpoint_t);
        assert(((opal_object_t*)endpoint)->obj_reference_count == 1);
        if(NULL == endpoint) {
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
            
#if HAVE_XRC
        if (MCA_BTL_XRC_ENABLED) {
            int rem_port_cnt = 0;
            for(j = 0; j < (int) ib_proc->proc_port_count && rem_port_cnt < btl_rank; j++) {
                if(ib_proc->proc_ports[j].subnet_id ==
                        openib_btl->port_info.subnet_id) {
                    rem_port_cnt ++;
                }
            }
            assert(rem_port_cnt == btl_rank);
            /* Push the subnet and lid to in the component */
            rc = mca_btl_openib_ib_address_add_new(
                    ib_proc->proc_ports[j].subnet_id,
                    ib_proc->proc_ports[j].lid, endpoint);
            if (OMPI_SUCCESS != rc ) {
                OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
                return OMPI_ERROR;
            }
        }
#endif
        mca_btl_openib_endpoint_init(openib_btl, endpoint);
        rc = mca_btl_openib_proc_insert(ib_proc, endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(endpoint);
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }
        
        orte_pointer_array_add((orte_std_cntr_t*)&endpoint->index,
                               openib_btl->hca->endpoints, (void*)endpoint);
        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
        
        peers[i] = endpoint;
    }

    return mca_btl_openib_size_queues(openib_btl, nprocs);
}

static inline struct ibv_cq *ibv_create_cq_compat(struct ibv_context *context,
        int cqe, void *cq_context, struct ibv_comp_channel *channel,
        int comp_vector)
{
#if OMPI_IBV_CREATE_CQ_ARGS == 3
    return ibv_create_cq(context, cqe, channel);
#else
    return ibv_create_cq(context, cqe, cq_context, channel, comp_vector); 
#endif
}

/* 
 * create both the high and low priority completion queues 
 * and the shared receive queue (if requested)
 */ 
static int create_srq(mca_btl_openib_module_t *openib_btl)
{
    int qp;

    /* create the SRQ's */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
        struct ibv_srq_init_attr attr; 

        if(!BTL_OPENIB_QP_TYPE_PP(qp)) { 
            attr.attr.max_wr = mca_btl_openib_component.qp_infos[qp].rd_num + 
                mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
            attr.attr.max_sge = mca_btl_openib_component.ib_sg_list_size;
            openib_btl->qps[qp].u.srq_qp.rd_posted = 0;
#if HAVE_XRC
            if(BTL_OPENIB_QP_TYPE_XRC(qp)) {
                openib_btl->qps[qp].u.srq_qp.srq =
                    ibv_create_xrc_srq(openib_btl->hca->ib_pd,
                            openib_btl->hca->xrc_domain,
                            openib_btl->hca->ib_cq[qp_cq_prio(qp)], &attr);
            } else
#endif
            {

               openib_btl->qps[qp].u.srq_qp.srq =
                   ibv_create_srq(openib_btl->hca->ib_pd, &attr);
            }
            if (NULL == openib_btl->qps[qp].u.srq_qp.srq) { 
                show_init_error(__FILE__, __LINE__, "ibv_create_srq",
                                ibv_get_device_name(openib_btl->hca->ib_dev));
                return OMPI_ERROR; 
            }
        }
    }
       
    return OMPI_SUCCESS;
}

static int adjust_cq(mca_btl_openib_hca_t *hca, const int cq)
{
    uint32_t cq_size = hca->cq_size[cq];

    /* make sure we don't exceed the maximum CQ size and that we 
     * don't size the queue smaller than otherwise requested 
     */
     if(cq_size < mca_btl_openib_component.ib_cq_size[cq])
        cq_size = mca_btl_openib_component.ib_cq_size[cq];

    if(cq_size > (uint32_t)hca->ib_dev_attr.max_cq)
        cq_size = hca->ib_dev_attr.max_cq;

    if(NULL == hca->ib_cq[cq]) {
        hca->ib_cq[cq] = ibv_create_cq_compat(hca->ib_dev_context, cq_size,
#if OMPI_ENABLE_PROGRESS_THREADS == 1
                hca, hca->ib_channel,
#else
                NULL, NULL,
#endif
                0);
    
        if (NULL == hca->ib_cq[cq]) {
            show_init_error(__FILE__, __LINE__, "ibv_create_cq",
                        ibv_get_device_name(hca->ib_dev));
            return OMPI_ERROR;
        }

#if OMPI_ENABLE_PROGRESS_THREADS == 1
        if(ibv_req_notify_cq(hca->ib_cq[cq], 0)) {
            show_init_error(__FILE__, __LINE__, "ibv_req_notify_cq",
                            ibv_get_device_name(hca->ib_dev));
            return OMPI_ERROR;
        }

        OPAL_THREAD_LOCK(&hca->hca_lock);
        if (!hca->progress) {
            int rc;
            hca->progress = true;
            if(OPAL_SUCCESS != (rc = opal_thread_start(&hca->thread))) {
                BTL_ERROR(("Unable to create progress thread, retval=%d", rc));
                return rc;
            }
        }
        OPAL_THREAD_UNLOCK(&hca->hca_lock);
#endif
    }
#ifdef HAVE_IBV_RESIZE_CQ
    else if (cq_size > mca_btl_openib_component.ib_cq_size[cq]){
        int rc;
        rc = ibv_resize_cq(hca->ib_cq[cq], cq_size);
        /* For ConnectX the resize CQ is not implemented and verbs returns -ENOSYS 
         * but should return ENOSYS. So it is reason for abs */
        if(rc && ENOSYS != abs(rc)) {
            BTL_ERROR(("cannot resize completion queue, error: %d", rc));
            return OMPI_ERROR;
        }
    }
#endif

    return OMPI_SUCCESS;
}

static int mca_btl_openib_size_queues(struct mca_btl_openib_module_t* openib_btl, size_t nprocs) 
{
    uint32_t send_cqes, recv_cqes;
    int rc = OMPI_SUCCESS, qp;
    mca_btl_openib_hca_t *hca = openib_btl->hca;

    /* figure out reasonable sizes for completion queues */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
        if(BTL_OPENIB_QP_TYPE_SRQ(qp)) {
            send_cqes = mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
            recv_cqes = mca_btl_openib_component.qp_infos[qp].rd_num;
        } else {
            send_cqes = (mca_btl_openib_component.qp_infos[qp].rd_num +
                mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv) * nprocs;
            recv_cqes = send_cqes;
        }
        openib_btl->hca->cq_size[qp_cq_prio(qp)] += recv_cqes;
        openib_btl->hca->cq_size[BTL_OPENIB_LP_CQ] += send_cqes;
    }
   
    rc = adjust_cq(hca, BTL_OPENIB_HP_CQ);
    if(rc != OMPI_SUCCESS)
        goto out;

    rc = adjust_cq(hca, BTL_OPENIB_LP_CQ);
    if(rc != OMPI_SUCCESS)
        goto out;

    if(0 == openib_btl->num_peers)
       rc = create_srq(openib_btl); 

out:
    openib_btl->num_peers += nprocs;
    return rc;
}
/* 
 * delete the proc as reachable from this btl module 
 */
int mca_btl_openib_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** peers)
{
    int i,ep_index;
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl;
    mca_btl_openib_endpoint_t* endpoint;
        
    for (i=0 ; i < (int) nprocs ; i++) {
        mca_btl_base_endpoint_t* del_endpoint = peers[i];
        for(ep_index=0;
            ep_index < orte_pointer_array_get_size(openib_btl->hca->endpoints);
            ep_index++) {
            endpoint = 
                orte_pointer_array_get_item(openib_btl->hca->endpoints,
                        ep_index);
            if(!endpoint || endpoint->endpoint_btl != openib_btl) {
                continue;
            }
            if (endpoint == del_endpoint) {
                BTL_VERBOSE(("in del_procs %d, setting another endpoint to null\n", 
                             ep_index));
                orte_pointer_array_set_item(openib_btl->hca->endpoints,
                        ep_index, NULL);
                assert(((opal_object_t*)endpoint)->obj_reference_count == 1);
                OBJ_RELEASE(endpoint);
            }
        }
    }

    return OMPI_SUCCESS;
}

/* 
 *Register callback function to support send/recv semantics 
 */ 
int mca_btl_openib_register(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_tag_t tag, 
                        mca_btl_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl; 
    
    OPAL_THREAD_LOCK(&openib_btl->ib_lock); 
    openib_btl->ib_reg[tag].cbfunc = cbfunc; 
    openib_btl->ib_reg[tag].cbdata = cbdata; 
    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock); 
    return OMPI_SUCCESS;
}


/* 
 *Register callback function for error handling..
 */ 
int mca_btl_openib_register_error_cb(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_module_error_cb_fn_t cbfunc)
{
    
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl; 
    openib_btl->error_cb = cbfunc; /* stash for later */
    return OMPI_SUCCESS;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
  * @param size (IN) Size of segment to allocate    
 * 
 * When allocating a segment we pull a pre-alllocated segment 
 * from one of two free lists, an eager list and a max list
 */
mca_btl_base_descriptor_t* mca_btl_openib_alloc(
    struct mca_btl_base_module_t* btl,
    uint8_t order,
    size_t size)
{
    mca_btl_openib_com_frag_t* frag = NULL;
    mca_btl_openib_module_t* openib_btl; 
    int rc;
    openib_btl = (mca_btl_openib_module_t*) btl; 
    MCA_BTL_IB_FRAG_ALLOC_BY_SIZE(openib_btl, frag, size, rc);
    
    if(NULL == frag)
        return NULL;
    
    /* not all upper layer users set this */
    to_base_frag(frag)->segment.seg_len = size;
    to_base_frag(frag)->base.order = order;
   
    assert(to_send_frag(frag)->qp_idx <= order);
    return &to_base_frag(frag)->base;
}

/** 
 * Return a segment 
 * 
 * Return the segment to the appropriate 
 *  preallocated segment list 
 */ 
int mca_btl_openib_free(
                    struct mca_btl_base_module_t* btl, 
                    mca_btl_base_descriptor_t* des) 
{
    /* is this fragment pointing at user memory? */
    if(MCA_BTL_OPENIB_FRAG_SEND_USER == openib_frag_type(des) ||
            MCA_BTL_OPENIB_FRAG_RECV_USER == openib_frag_type(des)) {
        mca_btl_openib_com_frag_t* frag = to_com_frag(des);

        if(frag->registration != NULL) {
            btl->btl_mpool->mpool_deregister(btl->btl_mpool,
                    (mca_mpool_base_registration_t*)frag->registration);
            frag->registration = NULL;
        }
    }
  
    /* reset those field on free so we will not have to do it on alloc */
    to_base_frag(des)->base.des_flags = 0;
    if(MCA_BTL_OPENIB_FRAG_RECV == openib_frag_type(des) ||
            MCA_BTL_OPENIB_FRAG_RECV_USER == openib_frag_type(des)) {
        to_base_frag(des)->base.des_src = NULL;
        to_base_frag(des)->base.des_src_cnt = 0;
    } else if(MCA_BTL_OPENIB_FRAG_SEND == openib_frag_type(des) ||
            MCA_BTL_OPENIB_FRAG_SEND_USER == openib_frag_type(des)) {
        to_base_frag(des)->base.des_dst = NULL;
        to_base_frag(des)->base.des_dst_cnt = 0;
        if(MCA_BTL_OPENIB_FRAG_SEND == openib_frag_type(des))
            to_com_frag(des)->sg_entry.addr = (uint64_t)to_send_frag(des)->hdr;
    }
    MCA_BTL_IB_FRAG_RETURN(des);
        
    return OMPI_SUCCESS; 
}


/**
 * register user buffer or pack 
 * data into pre-registered buffer and return a 
 * descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 *  
 * prepare source's behavior depends on the following: 
 * Has a valid memory registration been passed to prepare_src? 
 *    if so we attempt to use the pre-registered user-buffer, if the memory registration 
 *    is too small (only a portion of the user buffer) then we must reregister the user buffer 
 * Has the user requested the memory to be left pinned? 
 *    if so we insert the memory registration into a memory tree for later lookup, we 
 *    may also remove a previous registration if a MRU (most recently used) list of 
 *    registrations is full, this prevents resources from being exhausted.
 * Is the requested size larger than the btl's max send size? 
 *    if so and we aren't asked to leave the registration pinned, then we register the memory if 
 *    the users buffer is contiguous 
 * Otherwise we choose from two free lists of pre-registered memory in which to pack the data into. 
 * 
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration, 
    struct ompi_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size
)
{
    mca_btl_openib_module_t *openib_btl;
    mca_btl_openib_reg_t *openib_reg;
    mca_btl_openib_com_frag_t *frag = NULL;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    openib_btl = (mca_btl_openib_module_t*)btl;

    if(ompi_convertor_need_buffers(convertor) == false && 0 == reserve) {
        /* GMS  bloody HACK! */
        if(registration != NULL || max_data > btl->btl_max_send_size) {
            MCA_BTL_IB_FRAG_ALLOC_SEND_USER(openib_btl, frag, rc);
            if(NULL == frag) {
                return NULL;
            }
            
            iov.iov_len = max_data;
            iov.iov_base = NULL;
            
            ompi_convertor_pack(convertor, &iov, &iov_count, &max_data);
            
            *size = max_data;

            if(NULL == registration) {
                rc = btl->btl_mpool->mpool_register(btl->btl_mpool,
                        iov.iov_base, max_data, 0, &registration);
                if(OMPI_SUCCESS != rc || NULL == registration) {
                    MCA_BTL_IB_FRAG_RETURN(frag);
                    return NULL;
                }
                /* keep track of the registration we did */
                to_com_frag(frag)->registration =
                    (mca_btl_openib_reg_t*)registration;
            }
            openib_reg = (mca_btl_openib_reg_t*)registration;


            frag->sg_entry.length = max_data;
            frag->sg_entry.lkey = openib_reg->mr->lkey;
            frag->sg_entry.addr = (uint64_t)iov.iov_base;

            to_base_frag(frag)->base.order = order;
            to_base_frag(frag)->segment.seg_len = max_data;
            to_base_frag(frag)->segment.seg_addr.pval = iov.iov_base;
            to_base_frag(frag)->segment.seg_key.key32[0] =
                (uint32_t)frag->sg_entry.lkey;
            
            assert(MCA_BTL_NO_ORDER == order);
            
            BTL_VERBOSE(("frag->sg_entry.lkey = %lu .addr = %llu "
                        "frag->segment.seg_key.key32[0] = %lu",
                        frag->sg_entry.lkey, frag->sg_entry.addr,
                        frag->sg_entry.lkey));


            return &to_base_frag(frag)->base;
        }
    }
    
    assert(MCA_BTL_NO_ORDER == order); 
    
    if(max_data + reserve > btl->btl_max_send_size) {
        max_data = btl->btl_max_send_size - reserve;
    }
    
    MCA_BTL_IB_FRAG_ALLOC_BY_SIZE(openib_btl, frag, max_data + reserve, rc);
   
    if(NULL == frag)
        return NULL;

    iov.iov_len = max_data;
    iov.iov_base = (unsigned char*)
        to_base_frag(frag)->segment.seg_addr.pval + reserve;
    rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data);
    
    *size = max_data;
    to_base_frag(frag)->segment.seg_len = max_data + reserve;
    to_base_frag(frag)->base.order = order;

    return &to_base_frag(frag)->base;
}

/**
 * Prepare the dst buffer
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 * prepare dest's behavior depends on the following: 
 * Has a valid memory registration been passed to prepare_src? 
 *    if so we attempt to use the pre-registered user-buffer, if the memory registration 
 *    is to small (only a portion of the user buffer) then we must reregister the user buffer 
 * Has the user requested the memory to be left pinned? 
 *    if so we insert the memory registration into a memory tree for later lookup, we 
 *    may also remove a previous registration if a MRU (most recently used) list of 
 *    registrations is full, this prevents resources from being exhausted.
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size)
{
    mca_btl_openib_module_t *openib_btl;
    mca_btl_openib_com_frag_t *frag;
    mca_btl_openib_reg_t *openib_reg;
    int rc;
    void *buffer;

    openib_btl = (mca_btl_openib_module_t*)btl;
        
    MCA_BTL_IB_FRAG_ALLOC_RECV_USER(openib_btl, frag, rc);
    if(NULL == frag) {
        return NULL;
    }
    
    ompi_convertor_get_current_pointer(convertor, &buffer);

    if(NULL == registration){
        /* we didn't get a memory registration passed in, so we have to
         * register the region ourselves
         */ 
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool, buffer, *size, 0,
                &registration);
        if(OMPI_SUCCESS != rc || NULL == registration) {
            MCA_BTL_IB_FRAG_RETURN(frag);
            return NULL;
        }
        /* keep track of the registration we did */
        frag->registration = (mca_btl_openib_reg_t*)registration;
    }
    openib_reg = (mca_btl_openib_reg_t*)registration;

    frag->sg_entry.length = *size;
    frag->sg_entry.lkey = openib_reg->mr->lkey;
    frag->sg_entry.addr = (uint64_t)buffer;

    to_base_frag(frag)->segment.seg_addr.pval = buffer;
    to_base_frag(frag)->segment.seg_len = *size;
    to_base_frag(frag)->segment.seg_key.key32[0] = openib_reg->mr->rkey;
    to_base_frag(frag)->base.order = order;

    BTL_VERBOSE(("frag->sg_entry.lkey = %lu .addr = %llu "
                "frag->segment.seg_key.key32[0] = %lu",
                frag->sg_entry.lkey, frag->sg_entry.addr,
                openib_reg->mr->rkey));

    return &to_base_frag(frag)->base;
}

static int mca_btl_finalize_hca(struct mca_btl_openib_hca_t *hca)
{
#if OMPI_HAVE_THREADS
    int hca_to_remove;
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    if(hca->progress) {
        hca->progress = false;
        if (pthread_cancel(hca->thread.t_handle)) {
            BTL_ERROR(("Failed to cancel OpenIB progress thread"));
        }
        opal_thread_join(&hca->thread, NULL);
    }
    if (ibv_destroy_comp_channel(hca->ib_channel)) {
        BTL_VERBOSE(("Failed to close comp_channel"));
        return OMPI_ERROR;
    }
#endif
    /* signaling to async_tread to stop poll for this hca */
    if(mca_btl_openib_component.use_async_event_thread) {
        hca_to_remove = -(hca->ib_dev_context->async_fd);
        if (write(mca_btl_openib_component.async_pipe[1], &hca_to_remove,
                    sizeof(int)) < 0){
            BTL_ERROR(("Failed to write to pipe"));
            return OMPI_ERROR;
        }
    }
#endif
    /* Release CQs */
    if(hca->ib_cq[BTL_OPENIB_HP_CQ] != NULL) {
        if (ibv_destroy_cq(hca->ib_cq[BTL_OPENIB_HP_CQ])) {
            BTL_VERBOSE(("Failed to close HP CQ"));
            return OMPI_ERROR;
        }
    }

    if(hca->ib_cq[BTL_OPENIB_LP_CQ] != NULL) {
        if (ibv_destroy_cq(hca->ib_cq[BTL_OPENIB_LP_CQ])) {
            BTL_VERBOSE(("Failed to close LP CQ"));
            return OMPI_ERROR;
        }
    }

    if (OMPI_SUCCESS != mca_mpool_base_module_destroy(hca->mpool)) {
        BTL_VERBOSE(("Failed to release mpool"));
        return OMPI_ERROR;
    }

#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED) {
        if (OMPI_SUCCESS != mca_btl_openib_close_xrc_domain(hca)) {
            BTL_ERROR(("XRC Internal error. Failed to close xrc domain"));
            return OMPI_ERROR;
        }
    }
#endif

    if (ibv_dealloc_pd(hca->ib_pd)) {
        BTL_VERBOSE(("Warning! Failed to release PD"));
        return OMPI_ERROR;
    }
    if (ibv_close_device(hca->ib_dev_context)) {
        if (ompi_mpi_leave_pinned || ompi_mpi_leave_pinned_pipeline) {
            BTL_VERBOSE(("Warning! Failed to close HCA"));
            return OMPI_SUCCESS;
        } else {
            BTL_ERROR(("Error! Failed to close HCA"));
            return OMPI_ERROR;
        }
    }
    OBJ_DESTRUCT(&hca->hca_lock); 
    free(hca);
    return OMPI_SUCCESS;
}

int mca_btl_openib_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_openib_module_t* openib_btl; 
    mca_btl_openib_endpoint_t* endpoint;
    int ep_index, rdma_index, i;
    int qp, rc = OMPI_SUCCESS;
    
    openib_btl = (mca_btl_openib_module_t*) btl; 

    /* Remove the btl from component list */
    if ( mca_btl_openib_component.ib_num_btls > 1 ) {
        for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++){
            if (mca_btl_openib_component.openib_btls[i] == openib_btl){
                mca_btl_openib_component.openib_btls[i] =
                    mca_btl_openib_component.openib_btls[mca_btl_openib_component.ib_num_btls-1];
                break;
            }
        }
    }

    mca_btl_openib_component.ib_num_btls--;

    /* Release eager RDMAs */
    for(rdma_index=0;
            rdma_index < orte_pointer_array_get_size(openib_btl->eager_rdma_buffers);
        rdma_index++) {
        endpoint=orte_pointer_array_get_item(openib_btl->eager_rdma_buffers,rdma_index);
        if(!endpoint) {
            continue;
        }
        OBJ_RELEASE(endpoint);
    }
    /* Release all QPs */
    for(ep_index=0;
            ep_index < orte_pointer_array_get_size(openib_btl->hca->endpoints);
            ep_index++) {
        endpoint=orte_pointer_array_get_item(openib_btl->hca->endpoints,
                ep_index);
        if(!endpoint) {
            BTL_VERBOSE(("In finalize, got another null endpoint\n"));
            continue;
        }
        if(endpoint->endpoint_btl != openib_btl)
            continue;
        OBJ_RELEASE(endpoint);
    }
    /* Release SRQ resources */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
        if(!BTL_OPENIB_QP_TYPE_PP(qp)) {
                MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(
                        &openib_btl->qps[qp].u.srq_qp.pending_frags[0]);
                MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(
                        &openib_btl->qps[qp].u.srq_qp.pending_frags[1]);
                if (ibv_destroy_srq(openib_btl->qps[qp].u.srq_qp.srq)){
                    BTL_VERBOSE(("Failed to close SRQ %d", qp));
                    return OMPI_ERROR;
                }
                OBJ_DESTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[0]);
                OBJ_DESTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[1]);
                break;
        }
        /* Destroy free lists */
        OBJ_DESTRUCT(&openib_btl->qps[qp].send_free);
        OBJ_DESTRUCT(&openib_btl->qps[qp].recv_free);
    }

    OBJ_DESTRUCT(&openib_btl->send_free_control);
    OBJ_DESTRUCT(&openib_btl->send_user_free);
    OBJ_DESTRUCT(&openib_btl->recv_user_free);

    /* Release pending lists */
    if (!(--openib_btl->hca->btls)) {
        /* All btls for the HCA were closed
         * Now we can close the HCA
         */     
        if (OMPI_SUCCESS != mca_btl_finalize_hca(openib_btl->hca)) {
            BTL_VERBOSE(("Failed to close HCA"));
            rc = OMPI_ERROR;
        }
    }

#if OMPI_HAVE_THREADS
    if (mca_btl_openib_component.use_async_event_thread &&
            0 == mca_btl_openib_component.ib_num_btls &&
            mca_btl_openib_component.async_thread != 0) {
        /* signaling to async_tread to stop */
        int async_command=0;
        if(write(mca_btl_openib_component.async_pipe[1], &async_command,
                    sizeof(int)) < 0) {
            BTL_ERROR(("Failed to communicate with async event thread"));
            rc = OMPI_ERROR;
        } else {
            if(pthread_join(mca_btl_openib_component.async_thread, NULL)) {
                BTL_ERROR(("Failed to stop OpenIB async event thread"));
                rc = OMPI_ERROR;
            }
        }
        close(mca_btl_openib_component.async_pipe[0]);
        close(mca_btl_openib_component.async_pipe[1]);
    }
#endif

    OBJ_DESTRUCT(&openib_btl->ib_lock); 
    free(openib_btl);

    BTL_VERBOSE(("Success in closing BTL resources"));

    return rc;
}

/*
 *  Initiate a send. 
 */

int mca_btl_openib_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor, 
    mca_btl_base_tag_t tag)
   
{
    mca_btl_openib_send_frag_t* frag = to_send_frag(descriptor); 

    assert(openib_frag_type(frag) == MCA_BTL_OPENIB_FRAG_SEND);
  
    to_com_frag(frag)->endpoint = endpoint; 
    frag->hdr->tag = tag;

    return mca_btl_openib_endpoint_send(endpoint, frag);
}

/*
 * RDMA WRITE local buffer to remote buffer address.
 */

int mca_btl_openib_put( mca_btl_base_module_t* btl,
                    mca_btl_base_endpoint_t* ep,
                    mca_btl_base_descriptor_t* descriptor)
{
    struct ibv_send_wr* bad_wr; 
    mca_btl_openib_out_frag_t* frag = to_out_frag(descriptor); 
    int qp = descriptor->order;
    uint64_t rem_addr = descriptor->des_dst->seg_addr.lval;
    uint32_t rkey = descriptor->des_dst->seg_key.key32[0];

    assert(openib_frag_type(frag) == MCA_BTL_OPENIB_FRAG_SEND_USER ||
            openib_frag_type(frag) == MCA_BTL_OPENIB_FRAG_SEND);

    if(ep->endpoint_state != MCA_BTL_IB_CONNECTED) {
        int rc;
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        rc = check_endpoint_state(ep, descriptor, &ep->pending_put_frags);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc)
            return OMPI_SUCCESS;
        if(OMPI_SUCCESS != rc)
            return rc;
    }

    if(MCA_BTL_NO_ORDER == qp)
        qp = mca_btl_openib_component.rdma_qp;

    /* check for a send wqe */
    if (qp_get_wqe(ep, qp) < 0) {
        qp_put_wqe(ep, qp);
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        opal_list_append(&ep->pending_put_frags, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        return OMPI_SUCCESS;
    }
    /* post descriptor */
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    if((ep->endpoint_proc->proc_ompi->proc_arch & OMPI_ARCH_ISBIGENDIAN)
            != (ompi_proc_local()->proc_arch & OMPI_ARCH_ISBIGENDIAN)) {
        rem_addr = opal_swap_bytes8(rem_addr);
        rkey = opal_swap_bytes4(rkey);
    }
#endif
    frag->sr_desc.wr.rdma.remote_addr = rem_addr;
    frag->sr_desc.wr.rdma.rkey = rkey;

    to_com_frag(frag)->sg_entry.addr =
        (uint64_t)descriptor->des_src->seg_addr.pval; 
    to_com_frag(frag)->sg_entry.length = descriptor->des_src->seg_len; 
    to_com_frag(frag)->endpoint = ep;
#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED && BTL_OPENIB_QP_TYPE_XRC(qp))
        frag->sr_desc.xrc_remote_srq_num=ep->rem_info.rem_srqs[qp].rem_srq_num;
#endif
   
    descriptor->order = qp;
    /* Setting opcode on a frag constructor isn't enough since prepare_src
     * may return send_frag instead of put_frag */ 
    frag->sr_desc.opcode = IBV_WR_RDMA_WRITE;
    if(ibv_post_send(ep->qps[qp].qp->lcl_qp, &frag->sr_desc, &bad_wr))
        return OMPI_ERROR;

    return OMPI_SUCCESS; 
}


/*
 * RDMA READ remote buffer to local buffer address.
 */

int mca_btl_openib_get(mca_btl_base_module_t* btl,
                    mca_btl_base_endpoint_t* ep,
                    mca_btl_base_descriptor_t* descriptor)
{
    struct ibv_send_wr* bad_wr; 
    mca_btl_openib_get_frag_t* frag = to_get_frag(descriptor); 
    int qp = descriptor->order;
    uint64_t rem_addr = descriptor->des_src->seg_addr.lval;
    uint32_t rkey = descriptor->des_src->seg_key.key32[0];

    assert(openib_frag_type(frag) == MCA_BTL_OPENIB_FRAG_RECV_USER);

    if(ep->endpoint_state != MCA_BTL_IB_CONNECTED) {
        int rc;
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        rc = check_endpoint_state(ep, descriptor, &ep->pending_get_frags);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc)
            return OMPI_SUCCESS;
        if(OMPI_SUCCESS != rc)
            return rc;
    }

    if(MCA_BTL_NO_ORDER == qp)
        qp = mca_btl_openib_component.rdma_qp;

    /* check for a send wqe */
    if (qp_get_wqe(ep, qp) < 0) {
        qp_put_wqe(ep, qp);
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        opal_list_append(&ep->pending_get_frags, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        return OMPI_SUCCESS;
    }

    /* check for a get token */
    if(OPAL_THREAD_ADD32(&ep->get_tokens,-1) < 0) {
        qp_put_wqe(ep, qp);
        OPAL_THREAD_ADD32(&ep->get_tokens,1);
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        opal_list_append(&ep->pending_get_frags, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        return OMPI_SUCCESS;
    }
    
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    if((ep->endpoint_proc->proc_ompi->proc_arch & OMPI_ARCH_ISBIGENDIAN)
            != (ompi_proc_local()->proc_arch & OMPI_ARCH_ISBIGENDIAN)) {
        rem_addr = opal_swap_bytes8(rem_addr); 
        rkey = opal_swap_bytes4(rkey); 
    }
#endif
    frag->sr_desc.wr.rdma.remote_addr = rem_addr; 
    frag->sr_desc.wr.rdma.rkey = rkey;

    to_com_frag(frag)->sg_entry.addr =
        (uint64_t)descriptor->des_dst->seg_addr.pval; 
    to_com_frag(frag)->sg_entry.length  = descriptor->des_dst->seg_len; 
    to_com_frag(frag)->endpoint = ep;
       
#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED && BTL_OPENIB_QP_TYPE_XRC(qp))
        frag->sr_desc.xrc_remote_srq_num=ep->rem_info.rem_srqs[qp].rem_srq_num;
#endif
    descriptor->order = qp;
    if(ibv_post_send(ep->qps[qp].qp->lcl_qp, &frag->sr_desc, &bad_wr))
        return OMPI_ERROR;

    return OMPI_SUCCESS; 
}

int mca_btl_openib_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
