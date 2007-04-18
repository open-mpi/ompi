/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
#include "ompi/datatype/convertor.h" 
#include "ompi/datatype/datatype.h" 
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
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
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0,  /* TODO this should be PUT btl flags */
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
        mca_btl_openib_register_error_cb /* error call back registration */
    }
};

int mca_btl_openib_size_queues( struct mca_btl_openib_module_t* openib_btl, size_t nprocs);


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
            asprintf(&str_limit, "%ld", (long) limit.rlim_cur);
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
    int btl_rank;

    for(j=0; j < mca_btl_openib_component.ib_num_btls; j++){ 
        if(mca_btl_openib_component.openib_btls[j].port_info.subnet_id
           == openib_btl->port_info.subnet_id) { 
            lcl_subnet_id_port_cnt++;
            }
        if(openib_btl == &(mca_btl_openib_component.openib_btls[j])) { 
            btl_rank = j;
        }
    }
    for(i = 0; i < (int) nprocs; i++) {
        
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_openib_proc_t* ib_proc;
        mca_btl_base_endpoint_t* endpoint;
        
        
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

        /* The btl_proc datastructure is shared by all IB PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        endpoint = OBJ_NEW(mca_btl_openib_endpoint_t);
        if(NULL == endpoint) {
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
            
        endpoint->endpoint_btl = openib_btl;
        endpoint->use_eager_rdma = openib_btl->hca->use_eager_rdma &
            mca_btl_openib_component.use_eager_rdma;
        endpoint->subnet_id = openib_btl->port_info.subnet_id; 
        rc = mca_btl_openib_proc_insert(ib_proc, endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(endpoint);
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }
        
        orte_pointer_array_add((orte_std_cntr_t*)&endpoint->index,
                               openib_btl->endpoints, (void*)endpoint);
        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
        
        peers[i] = endpoint;
    }
    
    return mca_btl_openib_size_queues(openib_btl, nprocs);

}

int mca_btl_openib_size_queues( struct mca_btl_openib_module_t* openib_btl, size_t nprocs) 
{
    int min_cq_size;
    int first_time = (0 == openib_btl->num_peers);
    int rc;
    openib_btl->num_peers += nprocs; 
    if(mca_btl_openib_component.use_srq) { 
        openib_btl->rd_num = mca_btl_openib_component.rd_num + log2(nprocs) * mca_btl_openib_component.srq_rd_per_peer; 
        if(openib_btl->rd_num > mca_btl_openib_component.srq_rd_max)
            openib_btl->rd_num = mca_btl_openib_component.srq_rd_max;
        openib_btl->rd_low = openib_btl->rd_num - 1;
        min_cq_size = openib_btl->rd_num * 2 * openib_btl->num_peers;
        if(!first_time) { 
            struct ibv_srq_attr srq_attr;
            srq_attr.max_wr = openib_btl->rd_num;
            rc = ibv_modify_srq(openib_btl->srq[BTL_OPENIB_HP_QP],
                    &srq_attr, IBV_SRQ_MAX_WR); 
            if(rc) { 
                BTL_ERROR(("cannot resize high priority shared receive queue, error: %d", rc));
                return OMPI_ERROR;
            }
            rc = ibv_modify_srq(openib_btl->srq[BTL_OPENIB_LP_QP],
                    &srq_attr, IBV_SRQ_MAX_WR); 
            if(rc) { 
                BTL_ERROR(("cannot resize low priority shared receive queue, error: %d", rc));
                return OMPI_ERROR;
            }
		
        }
	    
    } else 
	{
	    min_cq_size = ( mca_btl_openib_component.rd_num >  (int32_t) mca_btl_openib_component.eager_rdma_num  ? 
			    mca_btl_openib_component.rd_num : (int32_t) mca_btl_openib_component.eager_rdma_num ) * 
		2 * openib_btl->num_peers;
	    
	}


    if(min_cq_size > (int32_t) mca_btl_openib_component.ib_cq_size) { 
        mca_btl_openib_component.ib_cq_size = min_cq_size > openib_btl->hca->ib_dev_attr.max_cq ? 
            openib_btl->hca->ib_dev_attr.max_cq : min_cq_size;
#if OMPI_MCA_BTL_OPENIB_HAVE_RESIZE_CQ
        if(!first_time) { 
            rc = ibv_resize_cq(openib_btl->ib_cq[BTL_OPENIB_LP_QP], mca_btl_openib_component.ib_cq_size);
            if(rc) {
                BTL_ERROR(("cannot resize low priority completion queue, error: %d", rc));
                return OMPI_ERROR;
            }
            rc = ibv_resize_cq(openib_btl->ib_cq[BTL_OPENIB_HP_QP],
                    mca_btl_openib_component.ib_cq_size);
            if(rc) {
                BTL_ERROR(("cannot resize high priority completion queue, error: %d", rc));
                return OMPI_ERROR;
            }
        }
#endif
    }
    if(first_time) { 
        /* never been here before, setup cq and srq */
        mca_btl_openib_component.ib_cq_size = (int) mca_btl_openib_component.ib_cq_size > 
            openib_btl->hca->ib_dev_attr.max_cq ? 
            openib_btl->hca->ib_dev_attr.max_cq : 
            (int) mca_btl_openib_component.ib_cq_size;
        return mca_btl_openib_create_cq_srq(openib_btl); 
    }
    return OMPI_SUCCESS;
}
/* 
 * delete the proc as reachable from this btl module 
 */
int mca_btl_openib_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** peers)
{
    BTL_DEBUG(("TODO\n"));
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
 * 
 * When allocating a segment we pull a pre-alllocated segment 
 * from one of two free lists, an eager list and a max list
 */
mca_btl_base_descriptor_t* mca_btl_openib_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_openib_frag_t* frag = NULL;
    mca_btl_openib_module_t* openib_btl; 
    int rc;
    openib_btl = (mca_btl_openib_module_t*) btl; 
    
    if(size <= mca_btl_openib_component.eager_limit){ 
        MCA_BTL_IB_FRAG_ALLOC_EAGER(btl, frag, rc);
    } else if(size <= mca_btl_openib_component.max_send_size) { 
        MCA_BTL_IB_FRAG_ALLOC_MAX(btl, frag, rc); 
    }
    
    if(NULL == frag)
        return NULL;

    frag->segment.seg_len = size <= openib_btl->super.btl_eager_limit ? size : openib_btl->super.btl_eager_limit;  
    frag->base.des_flags = 0; 
    
    return (mca_btl_base_descriptor_t*)frag;
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
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*)des; 

    if(((MCA_BTL_OPENIB_SEND_FRAG_FRAG == frag->type) ||
            (MCA_BTL_OPENIB_RECV_FRAG_FRAG == frag->type)) 
            && frag->registration != NULL) {
        btl->btl_mpool->mpool_deregister(btl->btl_mpool,
                                      (mca_mpool_base_registration_t*)
                                      frag->registration);
        frag->registration = NULL;
    }
    MCA_BTL_IB_FRAG_RETURN(((mca_btl_openib_module_t*) btl), frag);
        
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
 *    if so we attempt to use the pre-registred user-buffer, if the memory registration 
 *    is to small (only a portion of the user buffer) then we must reregister the user buffer 
 * Has the user requested the memory to be left pinned? 
 *    if so we insert the memory registration into a memory tree for later lookup, we 
 *    may also remove a previous registration if a MRU (most recently used) list of 
 *    registions is full, this prevents resources from being exhausted.
 * Is the requested size larger than the btl's max send size? 
 *    if so and we aren't asked to leave the registration pinned than we register the memory if 
 *    the users buffer is contiguous 
 * Otherwise we choose from two free lists of pre-registered memory in which to pack the data into. 
 * 
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration, 
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    mca_btl_openib_module_t *openib_btl;
    mca_btl_openib_frag_t *frag = NULL;
    mca_btl_openib_reg_t *openib_reg;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    openib_btl = (mca_btl_openib_module_t*)btl;

    if(ompi_convertor_need_buffers(convertor) == false && 0 == reserve) {
        if(registration != NULL || max_data > btl->btl_max_send_size) {
            MCA_BTL_IB_FRAG_ALLOC_SEND_FRAG(btl, frag, rc);
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
                    MCA_BTL_IB_FRAG_RETURN(openib_btl, frag);
                    return NULL;
                }
                /* keep track of the registration we did */
                frag->registration = (mca_btl_openib_reg_t*)registration;
            }
            openib_reg = (mca_btl_openib_reg_t*)registration;

            frag->base.des_flags = 0;
            frag->base.des_src = &frag->segment;
            frag->base.des_src_cnt = 1;
            frag->base.des_dst = NULL;
            frag->base.des_dst_cnt = 0;
            frag->base.des_flags = 0;

            frag->sg_entry.length = max_data;
            frag->sg_entry.lkey = openib_reg->mr->lkey;
            frag->sg_entry.addr = (unsigned long)iov.iov_base;

            frag->segment.seg_len = max_data;
            frag->segment.seg_addr.pval = iov.iov_base;
            frag->segment.seg_key.key32[0] = (uint32_t)frag->sg_entry.lkey;

            BTL_VERBOSE(("frag->sg_entry.lkey = %lu .addr = %llu "
                        "frag->segment.seg_key.key32[0] = %lu",
                        frag->sg_entry.lkey, frag->sg_entry.addr,
                        frag->segment.seg_key.key32[0]));

            return &frag->base;
        }
    }

    if(max_data + reserve <= btl->btl_eager_limit) {
        /* the data is small enough to fit in the eager frag and
         * memory is not prepinned */
        MCA_BTL_IB_FRAG_ALLOC_EAGER(btl, frag, rc);
    }

    if(NULL == frag) {
        /* the data doesn't fit into eager frag or eger frag is
         * not available */
        MCA_BTL_IB_FRAG_ALLOC_MAX(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        if(max_data + reserve > btl->btl_max_send_size) {
            max_data = btl->btl_max_send_size - reserve;
        }
    }

    iov.iov_len = max_data;
    iov.iov_base = (unsigned char*)frag->segment.seg_addr.pval + reserve;
    rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data);
    if(rc < 0) {
        MCA_BTL_IB_FRAG_RETURN(openib_btl, frag);
        return NULL;
    }
    *size  = max_data;
    frag->segment.seg_len = max_data + reserve;
    frag->segment.seg_key.key32[0] = (uint32_t)frag->sg_entry.lkey;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;

    return &frag->base;
}

/**
 * Prepare the dst buffer
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 * prepare dest's behavior depends on the following: 
 * Has a valid memory registration been passed to prepare_src? 
 *    if so we attempt to use the pre-registred user-buffer, if the memory registration 
 *    is to small (only a portion of the user buffer) then we must reregister the user buffer 
 * Has the user requested the memory to be left pinned? 
 *    if so we insert the memory registration into a memory tree for later lookup, we 
 *    may also remove a previous registration if a MRU (most recently used) list of 
 *    registions is full, this prevents resources from being exhausted.
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_openib_module_t *openib_btl;
    mca_btl_openib_frag_t *frag;
    mca_btl_openib_reg_t *openib_reg;
    int rc;
    ptrdiff_t lb;

    openib_btl = (mca_btl_openib_module_t*)btl;
    
    MCA_BTL_IB_FRAG_ALLOC_RECV_FRAG(btl, frag, rc);
    if(NULL == frag) {
        return NULL;
    }
    
    ompi_ddt_type_lb(convertor->pDesc, &lb);
    frag->segment.seg_addr.pval = convertor->pBaseBuf + lb +
        convertor->bConverted;

    if(NULL == registration){
        /* we didn't get a memory registration passed in, so we have to
         * register the region ourselves
         */ 
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool,
                frag->segment.seg_addr.pval, *size, 0, &registration);
        if(OMPI_SUCCESS != rc || NULL == registration) {
            MCA_BTL_IB_FRAG_RETURN(openib_btl, frag);
            return NULL;
        }
        /* keep track of the registration we did */
        frag->registration = (mca_btl_openib_reg_t*)registration;
    }
    openib_reg = (mca_btl_openib_reg_t*)registration;

    frag->sg_entry.length = *size;
    frag->sg_entry.lkey = openib_reg->mr->lkey;
    frag->sg_entry.addr = (unsigned long) frag->segment.seg_addr.pval;

    frag->segment.seg_len = *size;
    frag->segment.seg_key.key32[0] = openib_reg->mr->rkey;

    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_flags = 0;

    BTL_VERBOSE(("frag->sg_entry.lkey = %lu .addr = %llu "
                "frag->segment.seg_key.key32[0] = %lu",
                frag->sg_entry.lkey, frag->sg_entry.addr,
                frag->segment.seg_key.key32[0]));

    return &frag->base;
}

int mca_btl_openib_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_openib_module_t* openib_btl; 
    openib_btl = (mca_btl_openib_module_t*) btl; 

#if 0 
    if(openib_btl->send_free_eager.fl_num_allocated != 
       openib_btl->send_free_eager.super.opal_list_length){ 
        opal_output(0, "btl ib send_free_eager frags: %d allocated %d returned \n", 
                    openib_btl->send_free_eager.fl_num_allocated, 
                    openib_btl->send_free_eager.super.opal_list_length); 
    }
    if(openib_btl->send_free_max.fl_num_allocated != 
      openib_btl->send_free_max.super.opal_list_length){ 
        opal_output(0, "btl ib send_free_max frags: %d allocated %d returned \n", 
                    openib_btl->send_free_max.fl_num_allocated, 
                    openib_btl->send_free_max.super.opal_list_length); 
    }
    if(openib_btl->send_free_frag.fl_num_allocated != 
       openib_btl->send_free_frag.super.opal_list_length){ 
        opal_output(0, "btl ib send_free_frag frags: %d allocated %d returned \n", 
                    openib_btl->send_free_frag.fl_num_allocated, 
                    openib_btl->send_free_frag.super.opal_list_length); 
    }
    
    if(openib_btl->recv_free_eager.fl_num_allocated != 
       openib_btl->recv_free_eager.super.opal_list_length){ 
        opal_output(0, "btl ib recv_free_eager frags: %d allocated %d returned \n", 
                    openib_btl->recv_free_eager.fl_num_allocated, 
                    openib_btl->recv_free_eager.super.opal_list_length); 
    }

    if(openib_btl->recv_free_max.fl_num_allocated != 
       openib_btl->recv_free_max.super.opal_list_length){ 
        opal_output(0, "btl ib recv_free_max frags: %d allocated %d returned \n", 
                    openib_btl->recv_free_max.fl_num_allocated, 
                    openib_btl->recv_free_max.super.opal_list_length); 
    }
#endif 

    return OMPI_SUCCESS;
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
    
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*)descriptor; 
    frag->endpoint = endpoint; 
    frag->hdr->tag = tag;
    frag->wr_desc.sr_desc.opcode = IBV_WR_SEND;
    return mca_btl_openib_endpoint_send(endpoint, frag);
}

/*
 * RDMA WRITE local buffer to remote buffer address.
 */

int mca_btl_openib_put( mca_btl_base_module_t* btl,
                    mca_btl_base_endpoint_t* endpoint,
                    mca_btl_base_descriptor_t* descriptor)
{
    int rc;
    struct ibv_send_wr* bad_wr; 
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*) descriptor; 
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl;

    /* setup for queued requests */
    frag->endpoint = endpoint;
    frag->wr_desc.sr_desc.opcode = IBV_WR_RDMA_WRITE; 

    /* check for a send wqe */
    if (OPAL_THREAD_ADD32(&endpoint->sd_wqe[BTL_OPENIB_LP_QP],-1) < 0) {

        OPAL_THREAD_ADD32(&endpoint->sd_wqe[BTL_OPENIB_LP_QP],1);
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        opal_list_append(&endpoint->pending_put_frags, (opal_list_item_t *)frag);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        return OMPI_SUCCESS;

    /* post descriptor */
    } else {
        
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED; 
        frag->wr_desc.sr_desc.wr.rdma.remote_addr = frag->base.des_dst->seg_addr.lval; 
        frag->wr_desc.sr_desc.wr.rdma.rkey = frag->base.des_dst->seg_key.key32[0]; 
        frag->sg_entry.addr = (unsigned long) frag->base.des_src->seg_addr.pval; 
        frag->sg_entry.length  = frag->base.des_src->seg_len; 
        
        if(ibv_post_send(endpoint->lcl_qp[BTL_OPENIB_LP_QP], 
                         &frag->wr_desc.sr_desc, 
                         &bad_wr)){ 
            rc = OMPI_ERROR;
        } else {
            rc = OMPI_SUCCESS;
        }
    
        if(mca_btl_openib_component.use_srq) { 
            mca_btl_openib_post_srr(openib_btl, 1, BTL_OPENIB_HP_QP);
            mca_btl_openib_post_srr(openib_btl, 1, BTL_OPENIB_LP_QP);
        } else { 
            btl_openib_endpoint_post_rr(endpoint, 1, BTL_OPENIB_HP_QP);
            btl_openib_endpoint_post_rr(endpoint, 1, BTL_OPENIB_LP_QP);
        }
    }
    return rc; 
}


/*
 * RDMA READ remote buffer to local buffer address.
 */

int mca_btl_openib_get( mca_btl_base_module_t* btl,
                    mca_btl_base_endpoint_t* endpoint,
                    mca_btl_base_descriptor_t* descriptor)
{
    int rc;
    struct ibv_send_wr* bad_wr; 
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*) descriptor; 
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl;
    frag->endpoint = endpoint;
    frag->wr_desc.sr_desc.opcode = IBV_WR_RDMA_READ; 

    /* check for a send wqe */
    if (OPAL_THREAD_ADD32(&endpoint->sd_wqe[BTL_OPENIB_LP_QP],-1) < 0) {

        OPAL_THREAD_ADD32(&endpoint->sd_wqe[BTL_OPENIB_LP_QP],1);
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        opal_list_append(&endpoint->pending_get_frags, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        return OMPI_SUCCESS;

    /* check for a get token */
    } else if(OPAL_THREAD_ADD32(&endpoint->get_tokens,-1) < 0) {

        OPAL_THREAD_ADD32(&endpoint->sd_wqe[BTL_OPENIB_LP_QP],1);
        OPAL_THREAD_ADD32(&endpoint->get_tokens,1);
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        opal_list_append(&endpoint->pending_get_frags, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        return OMPI_SUCCESS;

    } else { 
    
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED; 
        frag->wr_desc.sr_desc.wr.rdma.remote_addr = frag->base.des_src->seg_addr.lval; 
        frag->wr_desc.sr_desc.wr.rdma.rkey = frag->base.des_src->seg_key.key32[0]; 
        frag->sg_entry.addr = (unsigned long) frag->base.des_dst->seg_addr.pval; 
        frag->sg_entry.length  = frag->base.des_dst->seg_len; 
        
        if(ibv_post_send(endpoint->lcl_qp[BTL_OPENIB_LP_QP], 
                         &frag->wr_desc.sr_desc, 
                         &bad_wr)){ 
            BTL_ERROR(("error posting send request errno (%d) says %s", errno, strerror(errno))); 
            rc = ORTE_ERROR;
        }  else {
            rc = ORTE_SUCCESS;
        }
        
        if(mca_btl_openib_component.use_srq) { 
            mca_btl_openib_post_srr(openib_btl, 1, BTL_OPENIB_HP_QP);
            mca_btl_openib_post_srr(openib_btl, 1, BTL_OPENIB_LP_QP);
        } else { 
            btl_openib_endpoint_post_rr(endpoint, 1, BTL_OPENIB_HP_QP);
            btl_openib_endpoint_post_rr(endpoint, 1, BTL_OPENIB_LP_QP);
        }
    }
    return rc; 
}

/* 
 * create both the high and low priority completion queues 
 * and the shared receive queue (if requested)
 */ 
int mca_btl_openib_create_cq_srq(mca_btl_openib_module_t *openib_btl)
{
    /* Allocate Protection Domain */ 
    openib_btl->poll_cq = false; 
    
    if (mca_btl_openib_component.use_srq) { 
        
        struct ibv_srq_init_attr attr; 
        attr.attr.max_wr = mca_btl_openib_component.srq_rd_max;
        attr.attr.max_sge = mca_btl_openib_component.ib_sg_list_size;

        openib_btl->srd_posted[BTL_OPENIB_HP_QP] = 0; 
        openib_btl->srd_posted[BTL_OPENIB_LP_QP] = 0; 
        
        openib_btl->srq[BTL_OPENIB_HP_QP] =
            ibv_create_srq(openib_btl->hca->ib_pd, &attr); 
        if (NULL == openib_btl->srq[BTL_OPENIB_HP_QP]) { 
            show_init_error(__FILE__, __LINE__, "ibv_create_srq",
                            ibv_get_device_name(openib_btl->hca->ib_dev));
            return OMPI_ERROR; 
        }
        
        openib_btl->srq[BTL_OPENIB_LP_QP] =
            ibv_create_srq(openib_btl->hca->ib_pd, &attr); 
        if (NULL == openib_btl->srq[BTL_OPENIB_LP_QP]) { 
            show_init_error(__FILE__, __LINE__, "ibv_create_srq",
                            ibv_get_device_name(openib_btl->hca->ib_dev));
            return OMPI_ERROR; 
        }
        
        
    } else { 
        openib_btl->srq[BTL_OPENIB_HP_QP] = NULL; 
        openib_btl->srq[BTL_OPENIB_LP_QP] = NULL;
    } 
    
    /* Create the low and high priority queue pairs */ 
#if OMPI_MCA_BTL_OPENIB_IBV_CREATE_CQ_ARGS == 3
    openib_btl->ib_cq[BTL_OPENIB_LP_QP] =
        ibv_create_cq(openib_btl->hca->ib_dev_context,
                mca_btl_openib_component.ib_cq_size, NULL); 
#else
    openib_btl->ib_cq[BTL_OPENIB_LP_QP] =
        ibv_create_cq(openib_btl->hca->ib_dev_context,
                mca_btl_openib_component.ib_cq_size, NULL, NULL, 0); 
#endif
    
    if (NULL == openib_btl->ib_cq[BTL_OPENIB_LP_QP]) {
        show_init_error(__FILE__, __LINE__, "ibv_create_cq",
                        ibv_get_device_name(openib_btl->hca->ib_dev));
        return OMPI_ERROR;
    }

#if OMPI_MCA_BTL_OPENIB_IBV_CREATE_CQ_ARGS == 3
    openib_btl->ib_cq[BTL_OPENIB_HP_QP] =
        ibv_create_cq(openib_btl->hca->ib_dev_context,
                mca_btl_openib_component.ib_cq_size, NULL); 
#else
    openib_btl->ib_cq[BTL_OPENIB_HP_QP] =
        ibv_create_cq(openib_btl->hca->ib_dev_context,
                mca_btl_openib_component.ib_cq_size, NULL, NULL, 0); 
#endif    

    if(NULL == openib_btl->ib_cq[BTL_OPENIB_HP_QP]) {
        show_init_error(__FILE__, __LINE__, "ibv_create_cq",
                        ibv_get_device_name(openib_btl->hca->ib_dev));
        return OMPI_ERROR;
    }
    
    return OMPI_SUCCESS;
}
