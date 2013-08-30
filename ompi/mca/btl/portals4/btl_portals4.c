/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <inttypes.h>

#include "ompi/runtime/ompi_module_exchange.h"

#include "opal/class/opal_bitmap.h"
#include "ompi/constants.h"
#include "ompi/mca/btl/btl.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/proc/proc.h"

#include "btl_portals4.h"
#include "btl_portals4_recv.h"

mca_btl_portals4_module_t mca_btl_portals4_module = {
    {
        &mca_btl_portals4_component.super,

        /* NOTE: All these default values are set in
           component_open() */

        0,   /* max size of first frag */
        0,   /* min send size */
        0,   /* max send size */
        0,   /* btl_rdma_pipeline_send_length */
        0,   /* btl_rdma_pipeline_frag_size */
        0,   /* btl_min_rdma_pipeline_size */
        0,   /* exclusivity - higher than sm, lower than self */
        0,   /* latency */
        0,   /* bandwidth */
        0,   /* btl flags */
        0,   /* btl segment size */

        mca_btl_portals4_add_procs,
        mca_btl_portals4_del_procs,
        NULL,  /* btl_register */
        mca_btl_portals4_finalize,
        mca_btl_portals4_alloc,
        mca_btl_portals4_free,
        mca_btl_portals4_prepare_src,
        mca_btl_portals4_prepare_dst,
        mca_btl_portals4_send,
        NULL, /* mca_btl_portals4_sendi, */
        NULL, /* mca_btl_portals4_put, */
        mca_btl_portals4_get,
        mca_btl_base_dump,
        NULL, /* mpool */
        NULL, /* register error */
        NULL
    },
};

int
mca_btl_portals4_add_procs(struct mca_btl_base_module_t* btl_base,
                          size_t nprocs,
                          struct ompi_proc_t **procs,
                          struct mca_btl_base_endpoint_t** btl_peer_data,
                          opal_bitmap_t* reachable)
{
    int ret;
    struct ompi_proc_t *curr_proc = NULL;
    ptl_process_t *id;
    size_t i, size;
    bool need_activate = false;

    opal_output_verbose(50, ompi_btl_base_framework.framework_output,
                        "mca_btl_portals4_add_procs: Adding %d procs (%d)", (int) nprocs,
                        (int) mca_btl_portals4_module.portals_num_procs);

    assert(&mca_btl_portals4_module == (mca_btl_portals4_module_t*) btl_base);
    if (0 == mca_btl_portals4_module.portals_num_procs) {
        need_activate = true;
    }

    for (i = 0 ; i < nprocs ; ++i) {
        curr_proc = procs[i];

        /* portals doesn't support heterogeneous yet... */
        if (ompi_proc_local()->proc_arch != curr_proc->proc_arch) {
            continue;
        }

        ret = ompi_modex_recv(&mca_btl_portals4_component.super.btl_version,
                              curr_proc, (void**) &id, &size);

        if (OMPI_SUCCESS != ret) {
            opal_output(0, "ompi_modex_recv failed: %d", ret);
            return ret;
        } else if (sizeof(ptl_process_t) != size) {
            opal_output(0, "ompi_modex_recv returned size %d, expected %d",
                        (int) size, (int) sizeof(ptl_process_t));
            return OMPI_ERROR;
        }

        if (NULL == procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]) {
            btl_peer_data[i] = malloc(sizeof(mca_btl_base_endpoint_t));
            if (NULL == btl_peer_data[i]) return OMPI_ERROR;
            btl_peer_data[i]->ptl_proc = *id;
            procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4] = btl_peer_data[i];            

            OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                                 "add_procs: nid=%x pid=%x\n", id->phys.nid, id->phys.pid));
        }  else {
            ptl_process_t *proc = (ptl_process_t*) procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4];
            if (proc->phys.nid != id->phys.nid ||
                proc->phys.pid != id->phys.pid) {
                opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                                    "%s:%d: existing peer and modex peer don't match\n",
                                    __FILE__, __LINE__);
                return OMPI_ERROR;
            }
            btl_peer_data[i] = (mca_btl_base_endpoint_t*) proc;
        }

        OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_num_procs, 1);
        /* and here we can reach */
        opal_bitmap_set_bit(reachable, i);
    }
    if (need_activate && mca_btl_portals4_module.portals_num_procs > 0) {
        ret = mca_btl_portals4_recv_enable(&mca_btl_portals4_module);
    }

    return OMPI_SUCCESS;
}


int
mca_btl_portals4_del_procs(struct mca_btl_base_module_t *btl,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_btl_base_endpoint_t **btl_peer_data)
{
    size_t i;

    assert(&mca_btl_portals4_module == (mca_btl_portals4_module_t*) btl);

    opal_output_verbose(50, ompi_btl_base_framework.framework_output,
                        "mca_btl_portals4_del_procs: Removing %d procs (%d)", (int) nprocs,
                        (int) mca_btl_portals4_module.portals_num_procs);

    /* See comment in btl_portals4_endpoint.h about why we look at the
       portals4 entry in proc_endpoints instead of the peer_data */
    for (i = 0 ; i < nprocs ; ++i) {
        if (NULL != procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]) {
            free(procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);
            procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4] = NULL;
        }
        OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_num_procs, -1);
    }

    return OMPI_SUCCESS;
}

mca_btl_base_descriptor_t*
mca_btl_portals4_alloc(struct mca_btl_base_module_t* btl_base,
                      struct mca_btl_base_endpoint_t* endpoint,
                      uint8_t order,
                      size_t size,
                      uint32_t flags)
{
    mca_btl_portals4_frag_t* frag;

    assert(&mca_btl_portals4_module == (mca_btl_portals4_module_t*) btl_base);

    if (size <= mca_btl_portals4_module.super.btl_eager_limit) {
        OMPI_BTL_PORTALS4_FRAG_ALLOC_EAGER(&mca_btl_portals4_module, frag);
        if (NULL == frag) return NULL;
        frag->segments[0].base.seg_len = size;
    } else {
        OMPI_BTL_PORTALS4_FRAG_ALLOC_MAX(&mca_btl_portals4_module, frag);
        if (NULL == frag) return NULL;
        frag->segments[0].base.seg_len =
            size <= mca_btl_portals4_module.super.btl_max_send_size ?
            size : mca_btl_portals4_module.super.btl_max_send_size ;
    }

    frag->md_h = PTL_INVALID_HANDLE;
    frag->base.des_src_cnt = 1;
    frag->base.des_flags = flags | MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    frag->base.order = MCA_BTL_NO_ORDER;

    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
	"mca_btl_portals4_alloc: %p\n", (void *) &frag->base));
    return &frag->base;
}

int
mca_btl_portals4_free(struct mca_btl_base_module_t* btl_base,
                      mca_btl_base_descriptor_t* des)
{
    mca_btl_portals4_frag_t* frag = (mca_btl_portals4_frag_t*) des;

    assert(&mca_btl_portals4_module == (mca_btl_portals4_module_t*) btl_base);

    if (BTL_PORTALS4_FRAG_TYPE_EAGER == frag->type) {
        /* don't ever unlink eager frags */
        OMPI_BTL_PORTALS4_FRAG_RETURN_EAGER(&mca_btl_portals4_module.super, frag);

    } else if (BTL_PORTALS4_FRAG_TYPE_MAX == frag->type) {
        if (frag->me_h != PTL_INVALID_HANDLE) {
            frag->me_h = PTL_INVALID_HANDLE;
        }
        OMPI_BTL_PORTALS4_FRAG_RETURN_MAX(&mca_btl_portals4_module.super, frag);

    } else if (BTL_PORTALS4_FRAG_TYPE_USER == frag->type) {
        if (frag->me_h != PTL_INVALID_HANDLE) {
            frag->me_h = PTL_INVALID_HANDLE;
        }
        OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, -1);
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
            "mca_btl_portals4_free: Decrementing portals_outstanding_ops=%d\n", mca_btl_portals4_module.portals_outstanding_ops));
        OMPI_BTL_PORTALS4_FRAG_RETURN_USER(&mca_btl_portals4_module.super, frag);
    } else {
        return OMPI_ERR_BAD_PARAM;
    }

    return OMPI_SUCCESS;
}

/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */

mca_btl_base_descriptor_t*
mca_btl_portals4_prepare_src(struct mca_btl_base_module_t* btl_base,
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration,
                            struct opal_convertor_t* convertor,
                            uint8_t order,
                            size_t reserve,
                            size_t* size,
                            uint32_t flags)
{
    mca_btl_portals4_frag_t* frag;
    size_t max_data = *size;
    struct iovec iov;
    uint32_t iov_count = 1;
    int ret;

    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
        "mca_btl_portals4_prepare_src reserve=%ld size=%ld max_data=%ld\n", reserve, *size, max_data));

    if (0 != reserve || 0 != opal_convertor_need_buffers(convertor)) {
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "mca_btl_portals4_prepare_src NEED BUFFERS or RESERVE\n"));
        frag = (mca_btl_portals4_frag_t*) mca_btl_portals4_alloc(btl_base, peer, MCA_BTL_NO_ORDER, max_data + reserve, flags);
        if (NULL == frag)  {
            return NULL;
        }

        if (max_data + reserve > frag->size) {
            max_data = frag->size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segments[0].base.seg_addr.pval + reserve;
        ret = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );

        *size  = max_data;
        if ( ret < 0 ) {
            return NULL;
        }

        frag->segments[0].base.seg_len = max_data + reserve;
        frag->base.des_src_cnt = 1;

    } else {
        /* no need to pack - rdma operation out of user's buffer */
        ptl_me_t me;

        /* reserve space in the event queue for rdma operations immediately */
        while (OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, 1) >
               mca_btl_portals4_module.portals_max_outstanding_ops) {
            OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, -1);
            OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "Call to mca_btl_portals4_component_progress (1)\n"));
            mca_btl_portals4_component_progress();
        }

        OMPI_BTL_PORTALS4_FRAG_ALLOC_USER(&mca_btl_portals4_module.super, frag);
        if (NULL == frag){
            OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, -1);
            return NULL;
        }
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
            "mca_btl_portals4_prepare_src: Incrementing portals_outstanding_ops=%d\n", mca_btl_portals4_module.portals_outstanding_ops));

        iov.iov_len = max_data;
        iov.iov_base = NULL;

        opal_convertor_pack(convertor, &iov, &iov_count, &max_data );

        frag->segments[0].base.seg_len = max_data;
        frag->segments[0].base.seg_addr.pval = iov.iov_base;
        frag->segments[0].key = OPAL_THREAD_ADD64(&(mca_btl_portals4_module.portals_rdma_key), 1);
        frag->base.des_src_cnt = 1;

        /* either a put or get.  figure out which later */
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                             "rdma src posted for frag 0x%lx, callback 0x%lx, bits %"PRIu64", flags say %d" ,
                             (unsigned long) frag,
                             (unsigned long) frag->base.des_cbfunc,
                             frag->segments[0].key, flags));

        /* create a match entry */
          me.start = frag->segments[0].base.seg_addr.pval;
          me.length = frag->segments[0].base.seg_len;
          me.ct_handle = PTL_CT_NONE;
          me.min_free = 0;
          me.uid = PTL_UID_ANY;
          me.options = PTL_ME_OP_GET | PTL_ME_USE_ONCE |
              PTL_ME_EVENT_LINK_DISABLE |
              PTL_ME_EVENT_COMM_DISABLE |
              PTL_ME_EVENT_UNLINK_DISABLE;
          me.match_id.phys.nid = peer->ptl_proc.phys.nid;
          me.match_id.phys.pid = peer->ptl_proc.phys.pid;
          me.match_bits = frag->segments[0].key;
          me.ignore_bits = BTL_PORTALS4_PROTOCOL_MASK |
              BTL_PORTALS4_CONTEXT_MASK |
              BTL_PORTALS4_SOURCE_MASK;
          me.ignore_bits = 0;

          ret = PtlMEAppend(mca_btl_portals4_module.portals_ni_h,
                            mca_btl_portals4_module.recv_idx,
                            &me,
                            PTL_PRIORITY_LIST,
                            frag,
                            &(frag->me_h));
          if (PTL_OK != ret) {
              opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                                  "%s:%d: PtlMEAppend failed: %d\n",
                                  __FILE__, __LINE__, ret);
              OMPI_BTL_PORTALS4_FRAG_RETURN_USER(&mca_btl_portals4_module.super, frag);
              OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, -1); 
              return NULL;
          }
          OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
              "PtlMEAppend (prepare_src) frag=%p, me_h=%d start=%p length=%ld nid=%x pid=%x match_bits=%lx\n",
              (void *)frag, frag->me_h, me.start, me.length,
              me.match_id.phys.nid, me.match_id.phys.pid, me.match_bits));
    }
    frag->base.des_src = &frag->segments[0].base;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = flags | MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    frag->base.order = MCA_BTL_NO_ORDER;
    return &frag->base;
}

mca_btl_base_descriptor_t*
mca_btl_portals4_prepare_dst(struct mca_btl_base_module_t* btl_base,
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration,
                            struct opal_convertor_t* convertor,
                            uint8_t order,
                            size_t reserve,
                            size_t* size,
                            uint32_t flags)
{
    mca_btl_portals4_frag_t* frag;

    /* reserve space in the event queue for rdma operations immediately */
    while (OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, 1) >
           mca_btl_portals4_module.portals_max_outstanding_ops) {
        OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, -1);
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "Call to mca_btl_portals4_component_progress (2)\n"));
        mca_btl_portals4_component_progress();
    }

    OMPI_BTL_PORTALS4_FRAG_ALLOC_USER(&mca_btl_portals4_module.super, frag);
    if (NULL == frag) {
        OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, -1);
        return NULL;
    }
    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
        "mca_btl_portals4_prepare_dst: Incrementing portals_outstanding_ops=%d\n", mca_btl_portals4_module.portals_outstanding_ops));

    frag->segments[0].base.seg_len = *size;
    opal_convertor_get_current_pointer( convertor, (void**)&(frag->segments[0].base.seg_addr.pval) );
    frag->segments[0].key = OPAL_THREAD_ADD64(&(mca_btl_portals4_module.portals_rdma_key), 1);
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segments[0].base;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = flags | MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    frag->base.order = MCA_BTL_NO_ORDER;
    frag->md_h = PTL_INVALID_HANDLE;

    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
        "mca_btl_portals4_prepare_dst &base=%p reserve=%ld size=%ld pid=%x key=%ld\n", 
        (void *)&frag->base, reserve, *size, peer->ptl_proc.phys.pid, frag->segments[0].key));
    return &frag->base;
}

int
mca_btl_portals4_finalize(struct mca_btl_base_module_t *btl)
{
    int ret;

    assert(&mca_btl_portals4_module == (mca_btl_portals4_module_t*) btl);

    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
         "mca_btl_portals4_finalize portals_outstanding_ops=%d\n", mca_btl_portals4_module.portals_outstanding_ops));

    /* sanity check */
    assert(mca_btl_portals4_module.portals_outstanding_ops  >= 0);

    /* finalize all communication */
    while (mca_btl_portals4_module.portals_outstanding_ops > 0) {
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                            "mca_btl_portals4_finalize portals_outstanding_ops: %d",
                            mca_btl_portals4_module.portals_outstanding_ops));

        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "Call to mca_btl_portals4_component_progress (3)\n"));
        mca_btl_portals4_component_progress();
    }

    PtlMEUnlink(mca_btl_portals4_module.long_overflow_me_h);
    PtlMDRelease(mca_btl_portals4_module.zero_md_h);

#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    if (NULL != mca_btl_portals4_module.send_md_hs) {
        int i;
        int num_mds = mca_btl_portals4_get_num_mds();

        for (i = 0 ; i < num_mds ; ++i) {
            if (!PtlHandleIsEqual(mca_btl_portals4_module.send_md_hs[i], PTL_INVALID_HANDLE)) {
                PtlMDRelease(mca_btl_portals4_module.send_md_hs[i]);
            }
        }

        free(mca_btl_portals4_module.send_md_hs);
    }
#else
    if (!PtlHandleIsEqual(mca_btl_portals4_module.send_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(mca_btl_portals4_module.send_md_h);
    }
#endif

    PtlPTFree(mca_btl_portals4_module.portals_ni_h, mca_btl_portals4_module.recv_idx);

    ret = mca_btl_portals4_recv_disable(&mca_btl_portals4_module);
    if (PTL_OK != ret) OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "Error freeing recv list: %d", ret));

    /* destroy eqs */
    ret = PtlEQFree(mca_btl_portals4_module.recv_eq_h);
    if (PTL_OK != ret) OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "Error freeing EQ recv: %d", ret));

    ret = PtlNIFini(mca_btl_portals4_module.portals_ni_h);
    if (PTL_OK != ret) OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "Error returned by PtlNIFini\n"));
    PtlFini();

    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "mca_btl_portals4_finalize OK\n"));

    /* Maybe other objects have to be freed */

    return OMPI_SUCCESS;
}
