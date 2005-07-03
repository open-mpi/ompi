/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "opal/threads/mutex.h"
#include "datatype/convertor.h"
#include "include/sys/atomic.h"
#include "util/output.h"
#include "util/if.h"
#include "util/proc_info.h"
#include "util/printf.h"
#include "util/sys_info.h"
#include "class/ompi_fifo.h"
#include "class/ompi_free_list.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "mca/mpool/base/base.h"
#include "btl_self.h"
#include "btl_self_frag.h"


mca_btl_base_module_t mca_btl_self = {
    &mca_btl_self_component.super,
    0, /* btl_eager_limit */
    0, /* btl_min_send_size */
    0, /* btl_max_send_size */
    0, /* btl_min_rdma_size */
    0, /* btl_max_rdma_size */
    0, /* btl_exclusivity */
    0, /* btl_latency */
    0, /* btl_bandwidth */
    0, /* btl flags */
    mca_btl_self_add_procs,
    mca_btl_self_del_procs,
    mca_btl_self_register,
    mca_btl_self_finalize,
    mca_btl_self_alloc,
    mca_btl_self_free,
    mca_btl_self_prepare_src,
    mca_btl_self_prepare_dst,
    mca_btl_self_send, 
    mca_btl_self_rdma,  /* put */
    mca_btl_self_rdma   /* get */
};


int mca_btl_self_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **procs, 
    struct mca_btl_base_endpoint_t **peers,
    ompi_bitmap_t* reachability)
{
    size_t i;
    for(i=0; i<nprocs; i++) {
        if(procs[i] == ompi_proc_local_proc) {
            ompi_bitmap_set_bit(reachability, i);
        }
    }
    return OMPI_SUCCESS;
}


int mca_btl_self_del_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs,
    struct ompi_proc_t **procs, 
    struct mca_btl_base_endpoint_t **peers)
{
    return OMPI_SUCCESS;
}


/**
 * MCA->BTL Clean up any resources held by BTL module
 * before the module is unloaded.
 *
 * @param btl (IN)   BTL module.
 *
 * Prior to unloading a BTL module, the MCA framework will call
 * the BTL finalize method of the module. Any resources held by
 * the BTL should be released and if required the memory corresponding
 * to the BTL module freed.
 *
 */

int mca_btl_self_finalize(struct mca_btl_base_module_t* btl)
{
    return OMPI_SUCCESS;
}


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BTL of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */

int mca_btl_self_register(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_tag_t tag,
    mca_btl_base_module_recv_cb_fn_t cbfunc,
    void* cbdata)
{
    mca_btl_self_component.self_reg[tag].cbfunc = cbfunc;
    mca_btl_self_component.self_reg[tag].cbdata = cbdata;
    return OMPI_SUCCESS;
}
                                                                                                                 

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
extern mca_btl_base_descriptor_t* mca_btl_self_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_self_frag_t* frag;
    int rc;
    if(size <= mca_btl_self.btl_eager_limit) {
        MCA_BTL_SELF_FRAG_ALLOC_EAGER(frag,rc);
    } else {
        MCA_BTL_SELF_FRAG_ALLOC_SEND(frag,rc);
    }
    frag->base.des_flags = 0;
    return (mca_btl_base_descriptor_t*)frag;
}
                                                                                                                   
/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
extern int mca_btl_self_free(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* des)
{
    mca_btl_self_frag_t* frag = (mca_btl_self_frag_t*)des;
    if(frag->size <= mca_btl_self.btl_eager_limit) {
        MCA_BTL_SELF_FRAG_RETURN_EAGER(frag);
    } else if (frag->size <= mca_btl_self.btl_max_send_size) {
        MCA_BTL_SELF_FRAG_RETURN_SEND(frag);
    } else {
        MCA_BTL_SELF_FRAG_RETURN_RDMA(frag);
    }
    return OMPI_SUCCESS;
}


/**
 * Prepare data for send/put
 *
 * @param btl (IN)      BTL module
 */
struct mca_btl_base_descriptor_t* mca_btl_self_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_self_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int32_t free_after;
    int rc;

    /* non-contigous data */
    if(ompi_convertor_need_buffers(convertor) || max_data < mca_btl_self.btl_max_send_size ) {
        MCA_BTL_SELF_FRAG_ALLOC_SEND(frag, rc);
        if(NULL == frag) {
            return NULL;
        }

        if(reserve + max_data > frag->size) {
            max_data = frag->size - reserve;
        } 
        iov.iov_len = max_data;
        iov.iov_base = (void*)((unsigned char*)(frag+1) + reserve);

        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
        if(rc < 0) {
            MCA_BTL_SELF_FRAG_RETURN_SEND(frag);
            return NULL;
        }
        frag->base.des_flags = 0;
        frag->segment.seg_addr.pval = frag+1;
        frag->segment.seg_len = reserve + max_data;
        *size = max_data;
    } else {
        MCA_BTL_SELF_FRAG_ALLOC_RDMA(frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;

        /* convertor should return offset into users buffer */
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
        if(rc < 0) {
            MCA_BTL_SELF_FRAG_RETURN_RDMA(frag);
            return NULL;
        }
        frag->segment.seg_addr.pval = iov.iov_base;
        frag->segment.seg_len = reserve + max_data;
        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->base.des_flags = 0;
        *size = max_data;
    }
    return &frag->base;
}

/**
 * Prepare data for receive.
 */
struct mca_btl_base_descriptor_t* mca_btl_self_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_self_frag_t* frag;
    size_t max_data = *size;
    int rc;

    MCA_BTL_SELF_FRAG_ALLOC_RDMA(frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    /* setup descriptor to point directly to user buffer */
    frag->segment.seg_addr.pval = (unsigned char*)convertor->pBaseBuf + convertor->bConverted;
    frag->segment.seg_len = reserve + max_data;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = 0;
    return &frag->base;
}

 
 
/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */

int mca_btl_self_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* des,
    mca_btl_base_tag_t tag)
{
    /* upcall */
    des->des_dst = des->des_src;
    des->des_dst_cnt = des->des_src_cnt;
    des->des_src = NULL;
    des->des_src_cnt = 0;
    mca_btl_self_component.self_reg[tag].cbfunc(btl,tag,des,OMPI_SUCCESS);
    des->des_src = des->des_dst;
    des->des_src_cnt = des->des_dst_cnt;
    des->des_dst = NULL;
    des->des_dst_cnt = 0;

    /* send completion */
    des->des_cbfunc(btl,endpoint,des,OMPI_SUCCESS);
    return OMPI_SUCCESS;
}

/**
 * Initiate a put to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
                                                                                                                           
extern int mca_btl_self_rdma(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* des)
{
    mca_btl_base_segment_t* src = des->des_src;
    mca_btl_base_segment_t* dst = des->des_dst;
    size_t src_cnt = des->des_src_cnt;
    size_t dst_cnt = des->des_dst_cnt;
    unsigned char* src_addr = dst->seg_addr.pval;
    size_t src_len = src->seg_len;
    unsigned char* dst_addr = dst->seg_addr.pval;
    size_t dst_len = dst->seg_len;

    while(src_len && dst_len) {

        if(src_len == dst_len) {
            memcpy(dst_addr, src_addr, src_len);

            /* advance src */
            if(--src_cnt != 0) {
                src++;
                src_addr = src->seg_addr.pval;
                src_len = src->seg_len;
            } else {
                src_len = 0;
            }

            /* advance dst */
            if(--dst_cnt != 0) {
                dst++;
                dst_addr = dst->seg_addr.pval;
                dst_len = dst->seg_len;
            } else {
                dst_len = 0;
            }
                
        } else {
            size_t bytes = src_len < dst_len ? src_len : dst_len;
            memcpy(dst_addr, src_addr, bytes);

            /* advance src */
            src_len -= bytes;
            if(src_len == 0) {
                if(--src_cnt != 0) {
                    src++;
                    src_addr = src->seg_addr.pval;
                    src_len = src->seg_len;
                }
            } else {
                src_addr += bytes;
            }

            /* advance dst */
            dst_len -= bytes;
            if(dst_len == 0) {
                if(--dst_cnt != 0) {
                    dst++;
                    dst_addr = src->seg_addr.pval;
                    dst_len = src->seg_len;
                }
            } else {
                dst_addr += bytes;
            }
        }
    }

    /* rdma completion */
    des->des_cbfunc(btl,endpoint,des,OMPI_SUCCESS);
    return OMPI_SUCCESS;
}


