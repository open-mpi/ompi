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
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <inttypes.h>
#include "opal/prefetch.h"
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_ud.h"
#include "btl_ud_frag.h"
#include "btl_ud_proc.h"
#include "btl_ud_endpoint.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/openib/mpool_openib.h"
#include <errno.h>
#include <string.h>
#include <math.h>


mca_btl_ud_module_t mca_btl_ud_module = {
    {
        &mca_btl_ud_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        MCA_BTL_FLAGS_SEND,
        mca_btl_ud_add_procs,
        mca_btl_ud_del_procs,
        mca_btl_ud_register,
        mca_btl_ud_finalize,
        /* we need alloc free, pack */
        mca_btl_ud_alloc,
        mca_btl_ud_free,
        mca_btl_ud_prepare_src,
        NULL, /*mca_btl_ud_prepare_dst */
        mca_btl_ud_send,
        NULL, /*mca_btl_ud_put */
        NULL, /*mca_btl_ud_get */
        mca_btl_ud_dump,
        NULL, /* mpool */
        NULL /* register error */
        
    }
};



/*
 *  add a proc to this btl module
 *    creates an endpoint that is setup on the
 *    first send to the endpoint
 */
int mca_btl_ud_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **ompi_procs,
    struct mca_btl_base_endpoint_t** peers,
    ompi_bitmap_t* reachable)
{
    mca_btl_ud_module_t* ud_btl = (mca_btl_ud_module_t*)btl;
    struct ibv_ah_attr ah_attr;
    int i, rc;

    for(i = 0; i < (int) nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_ud_proc_t* ib_proc;
        mca_btl_base_endpoint_t* ib_peer;

        if(NULL == (ib_proc = mca_btl_ud_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface
         * addresses exported as we are trying to use. If not, then
         * don't bind this PTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&ib_proc->proc_lock);

        /* The btl_proc datastructure is shared by all IB PTL
         * instances that are trying to reach this destination.
         * Cache the peer instance on the btl_proc.
         */
        ib_peer = OBJ_NEW(mca_btl_ud_endpoint_t);
        if(NULL == ib_peer) {
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        rc = mca_btl_ud_proc_insert(ib_proc, ib_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(ib_peer);
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }

        BTL_VERBOSE(("modex_recv HP QP num %d, LP QP num %d, LID = %d",
          ib_peer->rem_addr.qp_num_hp,
          ib_peer->rem_addr.qp_num_lp,
          ib_peer->rem_addr.lid));

        /* Set up IB address handles for the endpoint */
        ah_attr.is_global = 0;
        ah_attr.dlid = ib_peer->rem_addr.lid;
        ah_attr.sl = mca_btl_ud_component.ib_service_level;
        ah_attr.src_path_bits = mca_btl_ud_component.ib_src_path_bits;
        ah_attr.port_num = ud_btl->port_num;

        ib_peer->rmt_ah_hp = ibv_create_ah(ud_btl->ib_pd, &ah_attr);
        if(NULL == ib_peer->rmt_ah_hp) {
            BTL_ERROR(("error creating address handle errno says %s\n",
                        strerror(errno)));
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }

        ib_peer->rmt_ah_lp = ibv_create_ah(ud_btl->ib_pd, &ah_attr);
        if(NULL == ib_peer) {
            BTL_ERROR(("error creating address handle errno says %s\n",
                        strerror(errno)));
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
        peers[i] = ib_peer;
    }

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    if(mca_btl_ud_component.use_srq) {
        ud_btl->rd_num = mca_btl_ud_component.rd_num +
            log2(nprocs) * mca_btl_ud_component.srq_rd_per_peer;
        if(ud_btl->rd_num > mca_btl_ud_component.srq_rd_max)
           ud_btl->rd_num = mca_btl_ud_component.srq_rd_max;
    }
#endif
    return OMPI_SUCCESS;
}


/*
 * delete the proc as reachable from this btl module
 */
int mca_btl_ud_del_procs(struct mca_btl_base_module_t* btl,
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
int mca_btl_ud_register(struct mca_btl_base_module_t* btl,
                        mca_btl_base_tag_t tag,
                        mca_btl_base_module_recv_cb_fn_t cbfunc,
                        void* cbdata)
{

    mca_btl_ud_module_t* ud_btl = (mca_btl_ud_module_t*) btl;

    OPAL_THREAD_LOCK(&ud_btl->ib_lock);
    ud_btl->ib_reg[tag].cbfunc = cbfunc;
    ud_btl->ib_reg[tag].cbdata = cbdata;
    OPAL_THREAD_UNLOCK(&ud_btl->ib_lock);
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
mca_btl_base_descriptor_t* mca_btl_ud_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_ud_frag_t* frag;
    int rc;

    if(size <= mca_btl_ud_component.eager_limit) {
        MCA_BTL_IB_FRAG_ALLOC_EAGER(btl, frag, rc);
        frag->segment.seg_len = size;
    } else if(size <= mca_btl_ud_component.max_send_size) {
        MCA_BTL_IB_FRAG_ALLOC_MAX(btl, frag, rc);
        frag->segment.seg_len = size;
    } else {
        return NULL;
    }

    return (mca_btl_base_descriptor_t*)frag;
}


/**
 * Return a segment
 *
 * Return the segment to the appropriate
 *  preallocated segment list
 */
int mca_btl_ud_free(struct mca_btl_base_module_t* btl,
                    mca_btl_base_descriptor_t* des)
{
    mca_btl_ud_frag_t* frag = (mca_btl_ud_frag_t*)des;

    if(frag->size == 0) {
        btl->btl_mpool->mpool_release(btl->btl_mpool,
                (mca_mpool_base_registration_t*)frag->ud_reg);
        MCA_BTL_IB_FRAG_RETURN_FRAG(btl, frag);
    }
    else if(frag->size == mca_btl_ud_component.max_send_size){
        MCA_BTL_IB_FRAG_RETURN_MAX(btl, frag);
    } else if(frag->size == mca_btl_ud_component.eager_limit){
        MCA_BTL_IB_FRAG_RETURN_EAGER(btl, frag);
    } else {
        BTL_ERROR(("invalid descriptor"));
    }

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
mca_btl_base_descriptor_t* mca_btl_ud_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    mca_btl_ud_module_t* ud_btl;
    mca_btl_ud_frag_t* frag;
    mca_mpool_openib_registration_t * ud_reg;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int32_t free_after;
    int rc;

    ud_btl = (mca_btl_ud_module_t*) btl;
    ud_reg = (mca_mpool_openib_registration_t*) registration;

    if(OPAL_UNLIKELY(NULL != ud_reg &&
            0 == ompi_convertor_need_buffers(convertor))) {
        /* the memory is already pinned and we have contiguous user data */

        MCA_BTL_IB_FRAG_ALLOC_FRAG(btl, frag, rc);
        if(NULL == frag){
            return NULL;
        }

        iov.iov_len = max_data;
        iov.iov_base = NULL;

        ompi_convertor_pack(convertor,
                &iov, &iov_count, &max_data, &free_after);

        frag->segment.seg_len = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;

        /*frag->sg_entry.length = max_data;*/
        frag->sg_entry.lkey = ud_reg->mr->lkey;
        frag->sg_entry.addr = (unsigned long)iov.iov_base;

        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->base.des_flags = 0;
        frag->ud_reg = ud_reg;
        btl->btl_mpool->mpool_retain(btl->btl_mpool, registration);
        return &frag->base;

    } else if(OPAL_UNLIKELY(max_data > btl->btl_max_send_size &&
            ompi_convertor_need_buffers(convertor) == 0 && reserve == 0)) {
        /* The user buffer is contigous and we are asked to send more than
           the max send size. */

        MCA_BTL_IB_FRAG_ALLOC_FRAG(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }

        iov.iov_len = max_data;
        iov.iov_base = NULL;

        ompi_convertor_pack(convertor,
                &iov, &iov_count, &max_data, &free_after);

        frag->segment.seg_len = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;
        frag->base.des_flags = 0;

        rc = btl->btl_mpool->mpool_register(btl->btl_mpool, iov.iov_base,
                max_data, 0, (mca_mpool_base_registration_t**) &ud_reg);
        if(OMPI_SUCCESS != rc || NULL == ud_reg) {
            BTL_ERROR(("mpool_register(%p,%lu) failed",
                    iov.iov_base, max_data));
            MCA_BTL_IB_FRAG_RETURN_FRAG(btl, frag);
            return NULL;
        }

        /*frag->sg_entry.length = max_data;*/
        frag->sg_entry.lkey = ud_reg->mr->lkey;
        frag->sg_entry.addr = (unsigned long) iov.iov_base;

        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->ud_reg = ud_reg;
        return &frag->base;

    } else
    if (max_data + reserve <= btl->btl_eager_limit) {
        /* the data is small enough to fit in the eager frag and
           either we received no prepinned memory or leave pinned is
           not set */
        MCA_BTL_IB_FRAG_ALLOC_EAGER(btl, frag, rc);
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*)frag->segment.seg_addr.pval + reserve;

        rc = ompi_convertor_pack(convertor,
                &iov, &iov_count, &max_data, &free_after);
        if(OPAL_UNLIKELY(rc < 0)) {
            MCA_BTL_IB_FRAG_RETURN_EAGER(btl, frag);
            return NULL;
        }

        frag->segment.seg_len = max_data + reserve;
        frag->sg_entry.length = max_data + reserve + sizeof(mca_btl_ud_header_t);

        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->base.des_flags = 0;
        *size  = max_data;

        return &frag->base;

    } else {
        MCA_BTL_IB_FRAG_ALLOC_MAX(btl, frag, rc);
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        if(OPAL_UNLIKELY(max_data + reserve > btl->btl_max_send_size)) {
            max_data = btl->btl_max_send_size - reserve;
        }

        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*)frag->segment.seg_addr.pval + reserve;

        rc = ompi_convertor_pack(convertor,
                &iov, &iov_count, &max_data, &free_after);
        if(OPAL_UNLIKELY(rc < 0)) {
            MCA_BTL_IB_FRAG_RETURN_MAX(btl, frag);
            return NULL;
        }

        frag->segment.seg_len = max_data + reserve;
        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->base.des_flags=0;
        *size  = max_data;

        return &frag->base;
    }
    return NULL;
}


int mca_btl_ud_finalize(struct mca_btl_base_module_t* btl)
{
    return OMPI_SUCCESS;
}

/*
 *  Initiate a send.
 */

int mca_btl_ud_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag)
{
    int rc;

    mca_btl_ud_frag_t* frag = (mca_btl_ud_frag_t*)descriptor;
    OPAL_PREFETCH(frag, 1, 1);
    MCA_BTL_UD_START_TIME(post_send);
    frag->endpoint = endpoint;
    frag->hdr->tag = tag;

    /*OPAL_THREAD_LOCK(&endpoint->endpoint_lock);*/
    rc = mca_btl_ud_endpoint_post_send(
            (mca_btl_ud_module_t*)btl, endpoint, frag);
    /*OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);*/
    
    MCA_BTL_UD_END_TIME(post_send);
    return rc;
}


/*
 * Initialize the btl module by allocating a protection domain
 *  and creating both the high and low priority completion queues
 */
int mca_btl_ud_module_init(mca_btl_ud_module_t *ud_btl)
{
    struct mca_mpool_base_resources_t mpool_resources;
    struct ibv_context *ctx = ud_btl->ib_dev_context;
    struct ibv_recv_wr* bad_wr;
    mca_btl_ud_frag_t* frag;
    ompi_free_list_item_t* item;
    uint32_t length;
    int32_t rc, i;

    ud_btl->ib_pd = ibv_alloc_pd(ctx);
    if(NULL == ud_btl->ib_pd) {
        BTL_ERROR(("error allocating pd for %s errno says %s\n",
                ibv_get_device_name(ud_btl->ib_dev), strerror(errno)));
        return OMPI_ERROR;
    }
        
    mpool_resources.ib_pd = ud_btl->ib_pd;
    ud_btl->super.btl_mpool =
            mca_mpool_base_module_create(mca_btl_ud_component.ib_mpool_name,
                    &ud_btl->super, &mpool_resources);

    if(NULL == ud_btl->super.btl_mpool) {
        BTL_ERROR(("error creating openib memory pool! aborting ud btl initialization"));
        return OMPI_ERROR;
    }


#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    if(mca_btl_ud_component.use_srq) {
        struct ibv_srq_init_attr attr;
        attr.attr.max_wr = mca_btl_ud_component.srq_rd_max;
        attr.attr.max_sge = mca_btl_ud_component.ib_sg_list_size;

        ud_btl->srd_posted_hp = 0;
        ud_btl->srd_posted_lp = 0;

        ud_btl->srq_hp = ibv_create_srq(ud_btl->ib_pd, &attr);
        if(NULL == ud_btl->srq_hp) {
            BTL_ERROR(("error in ibv_create_srq\n"));
            return OMPI_ERROR;
        }

        ud_btl->srq_lp = ibv_create_srq(ud_btl->ib_pd, &attr);
        if(NULL == ud_btl->srq_hp) {
            BTL_ERROR(("error in ibv_create_srq\n"));
            return OMPI_ERROR;
        }

    } else {
        ud_btl->srq_hp = NULL;
        ud_btl->srq_lp = NULL;
    }
#endif

    /* Create the low and high priority completion queues */
#if OMPI_MCA_BTL_OPENIB_IBV_CREATE_CQ_ARGS == 3
    ud_btl->ib_cq_lp =
        ibv_create_cq(ctx, mca_btl_ud_component.ib_cq_size, NULL);
    
    ud_btl->ib_cq_hp =
        ibv_create_cq(ctx, mca_btl_ud_component.ib_cq_size, NULL);
#else
    ud_btl->ib_cq_lp = ibv_create_cq(ctx,
            mca_btl_ud_component.ib_cq_size, NULL, NULL, 0);
    
    ud_btl->ib_cq_hp = ibv_create_cq(ctx,
            mca_btl_ud_component.ib_cq_size, NULL, NULL, 0);
#endif

    if(NULL == ud_btl->ib_cq_lp) {
        BTL_ERROR(("error creating low priority cq for %s errno says %s\n",
                ibv_get_device_name(ud_btl->ib_dev), strerror(errno)));
        return OMPI_ERROR;
    }

    if(NULL == ud_btl->ib_cq_hp) {
        BTL_ERROR(("error creating high priority cq for %s errno says %s\n",
                ibv_get_device_name(ud_btl->ib_dev), strerror(errno)));
        return OMPI_ERROR;
    }

    /* Set up our packet sequence numbers */
    ud_btl->addr.psn_hp = lrand48() & 0xffffff;
    ud_btl->addr.psn_lp = lrand48() & 0xffffff;

    /* Set up the QPs for this BTL */
    if(OMPI_SUCCESS != mca_btl_ud_endpoint_init_qp(&ud_btl->super,
            ud_btl->ib_cq_hp,
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
            ud_btl->srq_hp,
#endif
            &ud_btl->qp_hp,
            ud_btl->addr.psn_hp)) {
        return OMPI_ERROR;
    }

    if(OMPI_SUCCESS != mca_btl_ud_endpoint_init_qp(&ud_btl->super,
            ud_btl->ib_cq_lp,
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
            ud_btl->srq_lp,
#endif
            &ud_btl->qp_lp,
            ud_btl->addr.psn_lp)) {
        return OMPI_ERROR;
    }

    /* Place our QP numbers in our local address information */
    ud_btl->addr.qp_num_hp = ud_btl->qp_hp->qp_num;
    ud_btl->addr.qp_num_lp = ud_btl->qp_lp->qp_num;

    OBJ_CONSTRUCT(&ud_btl->ib_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ud_btl->send_free_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&ud_btl->send_free_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&ud_btl->send_free_frag, ompi_free_list_t);

    OBJ_CONSTRUCT(&ud_btl->recv_free_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&ud_btl->recv_free_max, ompi_free_list_t);

    OBJ_CONSTRUCT(&ud_btl->pending_frags_hp, opal_list_t);
    OBJ_CONSTRUCT(&ud_btl->pending_frags_lp, opal_list_t);

    /* Initialize pool of send fragments */
    length = sizeof(mca_btl_ud_frag_t) + sizeof(mca_btl_ud_header_t) +
        ud_btl->super.btl_eager_limit + 2*MCA_BTL_IB_FRAG_ALIGN;

    ompi_free_list_init(&ud_btl->send_free_eager,
                        length,
                        OBJ_CLASS(mca_btl_ud_send_frag_eager_t),
                        mca_btl_ud_component.ib_free_list_num,
                        mca_btl_ud_component.ib_free_list_max,
                        mca_btl_ud_component.ib_free_list_inc,
                        ud_btl->super.btl_mpool);

    ompi_free_list_init(&ud_btl->recv_free_eager,
                        length + sizeof(mca_btl_ud_ib_header_t),
                        OBJ_CLASS(mca_btl_ud_recv_frag_eager_t),
                        mca_btl_ud_component.ib_free_list_num,
                        mca_btl_ud_component.ib_free_list_max,
                        mca_btl_ud_component.ib_free_list_inc,
                        ud_btl->super.btl_mpool);

    length = sizeof(mca_btl_ud_frag_t) + sizeof(mca_btl_ud_header_t) +
        ud_btl->super.btl_max_send_size + 2*MCA_BTL_IB_FRAG_ALIGN;

    ompi_free_list_init(&ud_btl->send_free_max,
                        length,
                        OBJ_CLASS(mca_btl_ud_send_frag_max_t),
                        mca_btl_ud_component.ib_free_list_num,
                        mca_btl_ud_component.ib_free_list_max,
                        mca_btl_ud_component.ib_free_list_inc,
                        ud_btl->super.btl_mpool);

    /* Initialize pool of receive fragments */
    ompi_free_list_init (&ud_btl->recv_free_max,
                         length + sizeof(mca_btl_ud_ib_header_t),
                         OBJ_CLASS (mca_btl_ud_recv_frag_max_t),
                         mca_btl_ud_component.ib_free_list_num,
                         mca_btl_ud_component.ib_free_list_max,
                         mca_btl_ud_component.ib_free_list_inc,
                         ud_btl->super.btl_mpool);

    length = sizeof(mca_btl_ud_frag_t) +
        sizeof(mca_btl_ud_header_t) + 2*MCA_BTL_IB_FRAG_ALIGN;

    ompi_free_list_init(&ud_btl->send_free_frag,
                        length,
                        OBJ_CLASS(mca_btl_ud_send_frag_frag_t),
                        mca_btl_ud_component.ib_free_list_num,
                        mca_btl_ud_component.ib_free_list_max,
                        mca_btl_ud_component.ib_free_list_inc,
                        ud_btl->super.btl_mpool);

    /* Post receive descriptors */
    for(i = 0; i < ud_btl->rd_num; i++) {
        /* High Priority (eager) */
        OMPI_FREE_LIST_WAIT(&ud_btl->recv_free_eager, item, rc);
        frag = (mca_btl_ud_frag_t*)item;

        frag->sg_entry.length = frag->size +
                sizeof(mca_btl_ud_header_t) + sizeof(mca_btl_ud_ib_header_t);
        if(ibv_post_recv(ud_btl->qp_hp,
                    &frag->wr_desc.rd_desc, &bad_wr)) {
            BTL_ERROR(("error posting recv, errno %s\n", strerror(errno)));
            return OMPI_ERROR;
        }

        /* Low Priority (max) */
        OMPI_FREE_LIST_WAIT(&ud_btl->recv_free_max, item, rc);
        frag = (mca_btl_ud_frag_t*)item;

        frag->sg_entry.length = frag->size +
                sizeof(mca_btl_ud_header_t) + sizeof(mca_btl_ud_ib_header_t);
        if(ibv_post_recv(ud_btl->qp_lp,
                    &frag->wr_desc.rd_desc, &bad_wr)) {
            BTL_ERROR(("error posting recv, errno %s\n", strerror(errno)));
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}


/*
 * Dump profiling information
 */
void mca_btl_ud_dump(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    int verbose)
{
    mca_btl_base_dump(btl, endpoint, verbose);
}

