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

#include "threads/mutex.h"
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
#include "mca/bmi/bmi.h"
#include "mca/mpool/base/base.h"
#include "bmi_self.h"
#include "bmi_self_frag.h"


mca_bmi_base_module_t mca_bmi_self = {
    &mca_bmi_self_component.super,
    0, /* bmi_eager_limit */
    0, /* bmi_min_send_size */
    0, /* bmi_max_send_size */
    0, /* bmi_min_rdma_size */
    0, /* bmi_max_rdma_size */
    0, /* bmi_exclusivity */
    0, /* bmi_latency */
    0, /* bmi_bandwidth */
    0, /* bmi flags */
    mca_bmi_self_add_procs,
    mca_bmi_self_del_procs,
    mca_bmi_self_register,
    mca_bmi_self_finalize,
    mca_bmi_self_alloc,
    mca_bmi_self_free,
    mca_bmi_self_prepare_src,
    NULL,
    mca_bmi_self_send, 
    NULL,  /* put */
    NULL   /* get */
};


int mca_bmi_self_add_procs(
    struct mca_bmi_base_module_t* bmi, 
    size_t nprocs, 
    struct ompi_proc_t **procs, 
    struct mca_bmi_base_endpoint_t **peers,
    ompi_bitmap_t* reachability)
{
    size_t i;
    for(i=0; i<nprocs; i++) {

    }
    return OMPI_SUCCESS;
}


int mca_bmi_self_del_procs(
    struct mca_bmi_base_module_t* bmi, 
    size_t nprocs,
    struct ompi_proc_t **procs, 
    struct mca_bmi_base_endpoint_t **peers)
{
    return OMPI_SUCCESS;
}


/**
 * MCA->BMI Clean up any resources held by BMI module
 * before the module is unloaded.
 *
 * @param bmi (IN)   BMI module.
 *
 * Prior to unloading a BMI module, the MCA framework will call
 * the BMI finalize method of the module. Any resources held by
 * the BMI should be released and if required the memory corresponding
 * to the BMI module freed.
 *
 */

int mca_bmi_self_finalize(struct mca_bmi_base_module_t* bmi)
{
    return OMPI_SUCCESS;
}


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param bmi (IN)     BMI module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BMI of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */

int mca_bmi_self_register(
    struct mca_bmi_base_module_t* bmi,
    mca_bmi_base_tag_t tag,
    mca_bmi_base_module_recv_cb_fn_t cbfunc,
    void* cbdata)
{
    mca_bmi_self_component.self_reg[tag].cbfunc = cbfunc;
    mca_bmi_self_component.self_reg[tag].cbdata = cbdata;
    return OMPI_SUCCESS;
}
                                                                                                                 

/**
 * Allocate a segment.
 *
 * @param bmi (IN)      BMI module
 * @param size (IN)     Request segment size.
 */
extern mca_bmi_base_descriptor_t* mca_bmi_self_alloc(
    struct mca_bmi_base_module_t* bmi,
    size_t size)
{
    mca_bmi_self_frag_t* frag;
    int rc;
    if(size <= mca_bmi_self.bmi_eager_limit) {
        MCA_BMI_SELF_FRAG_ALLOC_EAGER(frag,rc);
    } else {
        MCA_BMI_SELF_FRAG_ALLOC_SEND(frag,rc);
    }
    frag->base.des_flags = 0;
    return (mca_bmi_base_descriptor_t*)frag;
}
                                                                                                                   
/**
 * Return a segment allocated by this BMI.
 *
 * @param bmi (IN)      BMI module
 * @param segment (IN)  Allocated segment.
 */
extern int mca_bmi_self_free(
    struct mca_bmi_base_module_t* bmi,
    mca_bmi_base_descriptor_t* des)
{
    mca_bmi_self_frag_t* frag = (mca_bmi_self_frag_t*)des;
    if(frag->size <= mca_bmi_self.bmi_eager_limit) {
        MCA_BMI_SELF_FRAG_RETURN_EAGER(frag);
    } else if (frag->size <= mca_bmi_self.bmi_max_send_size) {
        MCA_BMI_SELF_FRAG_RETURN_SEND(frag);
    } else {
        MCA_BMI_SELF_FRAG_RETURN_RDMA(frag);
    }
    return OMPI_SUCCESS;
}


/**
 * Prepare data for send/put
 *
 * @param bmi (IN)      BMI module
 */
struct mca_bmi_base_descriptor_t* mca_bmi_self_prepare_src(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_bmi_self_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int32_t free_after;
    int rc;

    /* non-contigous data */
    if(ompi_convertor_need_buffers(convertor)) {
        MCA_BMI_SELF_FRAG_ALLOC_SEND(frag, rc);
        if(NULL == frag) {
            return NULL;
        }

        if(reserve + max_data > frag->size) {
            max_data = frag->size - reserve;
        } 
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*)(frag+1) + reserve;

        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
        if(rc < 0) {
            MCA_BMI_SELF_FRAG_RETURN_SEND(frag);
            return NULL;
        }
        frag->base.des_flags = 0;
        frag->segment.seg_addr.pval = frag+1;
        frag->segment.seg_len = reserve + max_data;
        *size = max_data;
    } else {
        MCA_BMI_SELF_FRAG_ALLOC_RDMA(frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;

        /* convertor should return offset into users buffer */
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
        if(rc < 0) {
            MCA_BMI_SELF_FRAG_RETURN_RDMA(frag);
            return NULL;
        }
        frag->base.des_flags = 0;
        frag->segment.seg_addr.pval = iov.iov_base;
        frag->segment.seg_len = reserve + max_data;
        *size = max_data;
    }
    return &frag->base;
}

/**
 * Prepare data for receive.
 */
struct mca_bmi_base_descriptor_t* mca_bmi_self_prepare_dst(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_bmi_self_frag_t* frag;
    size_t max_data = *size;
    int rc;

    MCA_BMI_SELF_FRAG_ALLOC_RDMA(frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    /* setup descriptor to point directly to user buffer */
    frag->base.des_flags = 0;
    frag->segment.seg_addr.pval = (unsigned char*)convertor->pBaseBuf + convertor->bConverted;
    frag->segment.seg_len = reserve + max_data;
    return &frag->base;
}

 
 
/**
 * Initiate a send to the peer.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */

int mca_bmi_self_send(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct mca_bmi_base_descriptor_t* des,
    mca_bmi_base_tag_t tag)
{
    des->des_dst = des->des_src;
    des->des_dst_cnt = des->des_src_cnt;
    des->des_src = NULL;
    des->des_src_cnt = 0;
    mca_bmi_self_component.self_reg[tag].cbfunc(bmi,tag,des,OMPI_SUCCESS);
    des->des_src = des->des_dst;
    des->des_src_cnt = des->des_dst_cnt;
    des->des_dst = NULL;
    des->des_dst_cnt = 0;
    return OMPI_SUCCESS;
}


