/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include "opal_stdint.h"

#include "opal/class/opal_bitmap.h"
#include "opal/constants.h"
#include "opal/mca/btl/btl.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/util/proc.h"
#include "opal/mca/pmix/pmix.h"

#include "btl_portals4.h"
#include "btl_portals4_recv.h"


mca_btl_base_registration_handle_t *
mca_btl_portals4_register_mem(mca_btl_base_module_t *btl,
                              mca_btl_base_endpoint_t *endpoint,
                              void *base,
                              size_t size,
                              uint32_t flags);

int mca_btl_portals4_deregister_mem(mca_btl_base_module_t *btl,
                                    mca_btl_base_registration_handle_t *handle);

mca_btl_portals4_module_t mca_btl_portals4_module = {
    .super = {
        .btl_component = &mca_btl_portals4_component.super,

        /* NOTE: All the default values are set in
           component_open() */

        .btl_add_procs = mca_btl_portals4_add_procs,
        .btl_del_procs = mca_btl_portals4_del_procs,
        .btl_finalize = mca_btl_portals4_finalize,
        .btl_alloc = mca_btl_portals4_alloc,
        .btl_free = mca_btl_portals4_free,
        .btl_prepare_src = mca_btl_portals4_prepare_src,
        .btl_register_mem = mca_btl_portals4_register_mem,
        .btl_deregister_mem = mca_btl_portals4_deregister_mem,
        .btl_send = mca_btl_portals4_send,
        .btl_get = mca_btl_portals4_get,
        .btl_dump = mca_btl_base_dump,
    },
};

static int
btl_portals4_init_interface(void)
{
    mca_btl_portals4_module_t *portals4_btl;
    unsigned int ret, interface;
    ptl_md_t md;
    ptl_me_t me;

// The initialisation of EQ, PT and ME must be done after the SetMap !
    for (interface=0; interface<mca_btl_portals4_component.num_btls; interface++) {
        portals4_btl = mca_btl_portals4_component.btls[interface];

        /* create event queue */
        ret = PtlEQAlloc(portals4_btl->portals_ni_h,
                     mca_btl_portals4_component.recv_queue_size,
                     &portals4_btl->recv_eq_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlEQAlloc failed for NI %d: %d",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }
        mca_btl_portals4_component.eqs_h[interface] = portals4_btl->recv_eq_h;
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
            "PtlEQAlloc (recv_eq=%d) OK for NI %d\n", portals4_btl->recv_eq_h, interface));

        /* Create recv_idx portal table entry */
        ret = PtlPTAlloc(portals4_btl->portals_ni_h,
                     PTL_PT_ONLY_USE_ONCE |
                     PTL_PT_ONLY_TRUNCATE,
                     portals4_btl->recv_eq_h,
                     REQ_BTL_TABLE_ID,
                     &portals4_btl->recv_idx);
        if (PTL_OK != ret) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed for NI %d: %d",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
            "PtlPTAlloc (recv_idx) OK for NI %d recv_idx=%d", interface, portals4_btl->recv_idx));

        if (portals4_btl->recv_idx != REQ_BTL_TABLE_ID) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc did not allocate the requested PT: %d",
                           __FILE__, __LINE__, portals4_btl->recv_idx);
            goto error;
        }

        /* bind zero-length md for sending acks */
        md.start     = NULL;
        md.length    = 0;
        md.options   = 0;
        md.eq_handle = PTL_EQ_NONE;
        md.ct_handle = PTL_CT_NONE;

        ret = PtlMDBind(portals4_btl->portals_ni_h,
                    &md,
                    &portals4_btl->zero_md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed for NI %d: %d",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
            "PtlMDBind (zero-length md=%d) OK for NI %d", portals4_btl->zero_md_h, interface));

        /* Bind MD across all memory */
        md.start = 0;
        md.length = PTL_SIZE_MAX;
        md.options = 0;
        md.eq_handle = portals4_btl->recv_eq_h;
        md.ct_handle = PTL_CT_NONE;

        ret = PtlMDBind(portals4_btl->portals_ni_h,
                    &md,
                    &portals4_btl->send_md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed for NI %d: %d\n",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }

        /* Handle long overflows */
        me.start = NULL;
        me.length = 0;
        me.ct_handle = PTL_CT_NONE;
        me.min_free = 0;
        me.uid = PTL_UID_ANY;
        me.options = PTL_ME_OP_PUT |
            PTL_ME_EVENT_LINK_DISABLE |
            PTL_ME_EVENT_COMM_DISABLE |
            PTL_ME_EVENT_UNLINK_DISABLE;
        if (mca_btl_portals4_component.use_logical) {
            me.match_id.rank = PTL_RANK_ANY;
        } else {
            me.match_id.phys.nid = PTL_NID_ANY;
            me.match_id.phys.pid = PTL_PID_ANY;
        }
        me.match_bits = BTL_PORTALS4_LONG_MSG;
        me.ignore_bits = BTL_PORTALS4_CONTEXT_MASK |
            BTL_PORTALS4_SOURCE_MASK |
            BTL_PORTALS4_TAG_MASK;
        ret = PtlMEAppend(portals4_btl->portals_ni_h,
                      portals4_btl->recv_idx,
                      &me,
                      PTL_OVERFLOW_LIST,
                      NULL,
                      &portals4_btl->long_overflow_me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed for NI %d: %d",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "PtlMEAppend (overflow list) OK for NI %d", interface));
    }

    ret = mca_btl_portals4_recv_enable(portals4_btl);
    if (PTL_OK != ret) {
        opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: Initialization of recv buffer failed: %d",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    return OPAL_SUCCESS;

 error:
    opal_output_verbose(1, opal_btl_base_framework.framework_output, "Error in btl_portals4_init_interface");

    for (interface=0; interface<mca_btl_portals4_component.num_btls; interface++) {
        portals4_btl = mca_btl_portals4_component.btls[interface];
        if (NULL != portals4_btl) mca_btl_portals4_free_module(portals4_btl);
    }
    mca_btl_portals4_component.num_btls = 0;
    if (NULL != mca_btl_portals4_component.btls)  free(mca_btl_portals4_component.btls);
    if (NULL != mca_btl_portals4_component.eqs_h) free(mca_btl_portals4_component.eqs_h);
    mca_btl_portals4_component.btls = NULL;
    mca_btl_portals4_component.eqs_h = NULL;

    return OPAL_ERROR;
}

static int
create_endpoint(int                       interface,
                opal_proc_t              *proc,
                mca_btl_base_endpoint_t **endpoint)
{
    int ret;
    size_t size;
    ptl_process_t *id;

    OPAL_MODEX_RECV(ret, &mca_btl_portals4_component.super.btl_version,
                    &proc->proc_name, (void**) &id, &size);

    if (OPAL_ERR_NOT_FOUND == ret) {
        OPAL_OUTPUT_VERBOSE((30, opal_btl_base_framework.framework_output,
            "btl/portals4: Portals 4 BTL not available on peer: %s", opal_strerror(ret)));
        return ret;
    } else if (OPAL_SUCCESS != ret) {
        opal_output_verbose(0, opal_btl_base_framework.framework_output,
            "btl/portals4: opal_modex_recv failed: %s", opal_strerror(ret));
        return ret;
    }
    if (size < sizeof(ptl_process_t)) {  /* no available connection */
        return OPAL_ERROR;
    }
    if ((size % sizeof(ptl_process_t)) != 0) {
        opal_output_verbose(0, opal_btl_base_framework.framework_output,
            "btl/portals4: invalid format in modex");
        return OPAL_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
        "btl/portals4: %d NI(s) declared in the modex", (int) (size/sizeof(ptl_process_t))));

    *endpoint = malloc(sizeof(mca_btl_base_endpoint_t));
    if (NULL == *endpoint) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    (*endpoint)->ptl_proc = id[interface];

    return OPAL_SUCCESS;
}

static int
create_peer_and_endpoint(int                       interface,
                         opal_proc_t              *proc,
                         ptl_process_t            *phys_peer,
                         mca_btl_base_endpoint_t **endpoint)
{
    int ret;
    size_t size;
    ptl_process_t *id;

    OPAL_MODEX_RECV(ret, &mca_btl_portals4_component.super.btl_version,
                    &proc->proc_name, (void**) &id, &size);

    if (OPAL_ERR_NOT_FOUND == ret) {
        OPAL_OUTPUT_VERBOSE((30, opal_btl_base_framework.framework_output,
            "btl/portals4: Portals 4 BTL not available on peer: %s", opal_strerror(ret)));
        return ret;
    } else if (OPAL_SUCCESS != ret) {
        opal_output_verbose(0, opal_btl_base_framework.framework_output,
            "btl/portals4: opal_modex_recv failed: %s", opal_strerror(ret));
        return ret;
    }
    if (size < sizeof(ptl_process_t)) {  /* no available connection */
        return OPAL_ERROR;
    }
    if ((size % sizeof(ptl_process_t)) != 0) {
        opal_output_verbose(0, opal_btl_base_framework.framework_output,
            "btl/portals4: invalid format in modex");
        return OPAL_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
        "btl/portals4: %d NI(s) declared in the modex", (int) (size/sizeof(ptl_process_t))));

    /*
     * check if create_endpoint() already created the endpoint.
     * if not, create it here.
     */
    if (NULL == *endpoint) {
        *endpoint = malloc(sizeof(mca_btl_base_endpoint_t));
        if (NULL == *endpoint) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    /*
     * regardless of who created the endpoint, set the rank here
     * because we are using logical mapping.
     */
    (*endpoint)->ptl_proc.rank = proc->proc_name.vpid;

    phys_peer->phys.pid = id[interface].phys.pid;
    phys_peer->phys.nid = id[interface].phys.nid;
    opal_output_verbose(50, opal_btl_base_framework.framework_output,
        "logical: global rank=%d pid=%d nid=%d\n",
        proc->proc_name.vpid, phys_peer->phys.pid, phys_peer->phys.nid);

    return OPAL_SUCCESS;
}

static int
create_maptable(struct mca_btl_portals4_module_t *portals4_btl,
                size_t                            nprocs,
                opal_proc_t                     **procs,
                mca_btl_base_endpoint_t         **endpoint)
{
    int ret;
    ptl_process_t *maptable;

    maptable = malloc(sizeof(ptl_process_t) * nprocs);
    if (NULL == maptable) {
        opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: malloc failed\n",
                            __FILE__, __LINE__);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    for (uint32_t i = 0 ; i < nprocs ; i++) {
        struct opal_proc_t *curr_proc;

        curr_proc = procs[i];

        /* portals doesn't support heterogeneous yet... */
        if (opal_proc_local_get()->proc_arch != curr_proc->proc_arch) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                                "Portals 4 BTL does not support heterogeneous operations.");
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                "Proc %s architecture %x, mine %x.",
                OPAL_NAME_PRINT(curr_proc->proc_name),
                curr_proc->proc_arch, opal_proc_local_get()->proc_arch);
            return OPAL_ERR_NOT_SUPPORTED;
        }

        ret = create_peer_and_endpoint(portals4_btl->interface_num,
                                       curr_proc,
                                       &maptable[i],
                                       &endpoint[i]);
        if (OPAL_SUCCESS != ret) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                                "%s:%d: create_maptable::create_peer_and_endpoint failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        }
    }

    ret = PtlSetMap(portals4_btl->portals_ni_h,
                    nprocs,
                    maptable);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: logical mapping failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }
    opal_output_verbose(90, opal_btl_base_framework.framework_output,
        "logical mapping OK\n");
    free(maptable);

    return OPAL_SUCCESS;
}

#define NEED_ALL_PROCS (mca_btl_portals4_component.use_logical)

int
mca_btl_portals4_add_procs(struct mca_btl_base_module_t* btl_base,
                          size_t nprocs,
                          struct opal_proc_t **procs,
                          struct mca_btl_base_endpoint_t** btl_peer_data,
                          opal_bitmap_t* reachable)
{
    struct mca_btl_portals4_module_t* portals4_btl = (struct mca_btl_portals4_module_t*) btl_base;
    int ret;
    size_t i;
    bool need_activate = false;

    opal_output_verbose(50, opal_btl_base_framework.framework_output,
                        "mca_btl_portals4_add_procs: Adding %d procs (%d) for NI %d",
                        (int) nprocs,
                        (int) portals4_btl->portals_num_procs,
                        portals4_btl->interface_num);

    if (0 == portals4_btl->portals_num_procs) {
        need_activate = true;
    }

    /*
     * The PML handed us a list of procs that need Portals4
     * peer info.  Complete those procs here.
     */
    for (i = 0 ; i < nprocs ; ++i) {
        struct opal_proc_t *curr_proc = procs[i];

        /* portals doesn't support heterogeneous yet... */
        if (opal_proc_local_get()->proc_arch != curr_proc->proc_arch) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                                "Portals 4 BTL does not support heterogeneous operations.");
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                "Proc %s architecture %x, mine %x.",
                OPAL_NAME_PRINT(curr_proc->proc_name),
                curr_proc->proc_arch, opal_proc_local_get()->proc_arch);
            return OPAL_ERR_NOT_SUPPORTED;
        }

        ret = create_endpoint(portals4_btl->interface_num,
                              curr_proc,
                              &btl_peer_data[i]);

        OPAL_THREAD_ADD32(&portals4_btl->portals_num_procs, 1);
        /* and here we can reach */
        opal_bitmap_set_bit(reachable, i);

        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
            "add_procs: rank=%x nid=%x pid=%x for NI %d\n",
            i,
            btl_peer_data[i]->ptl_proc.phys.nid,
            btl_peer_data[i]->ptl_proc.phys.pid,
            portals4_btl->interface_num));
    }

    if (need_activate && portals4_btl->portals_num_procs > 0) {
        if (mca_btl_portals4_component.use_logical) {
            ret = create_maptable(portals4_btl, nprocs, procs, btl_peer_data);
            if (OPAL_SUCCESS != ret) {
                opal_output_verbose(1, opal_btl_base_framework.framework_output,
                                    "%s:%d: mca_btl_portals4_add_procs::create_maptable() failed: %d\n",
                                    __FILE__, __LINE__, ret);
                return ret;
            }
        }

        ret = btl_portals4_init_interface();
        if (OPAL_SUCCESS != ret) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                                "%s:%d: portals4 interface initialization failed: %d",
                                __FILE__, __LINE__, ret);
            return ret;
        }
    }

    return OPAL_SUCCESS;
}


int
mca_btl_portals4_del_procs(struct mca_btl_base_module_t *btl,
			  size_t nprocs,
			  struct opal_proc_t **procs,
			  struct mca_btl_base_endpoint_t **btl_peer_data)
{
    struct mca_btl_portals4_module_t* portals4_btl = (struct mca_btl_portals4_module_t*) btl;
    size_t i;

    opal_output_verbose(50, opal_btl_base_framework.framework_output,
                        "mca_btl_portals4_del_procs: Removing %d procs (%d)", (int) nprocs,
                        (int) portals4_btl->portals_num_procs);

    /* See comment in btl_portals4_endpoint.h about why we look at the
       portals4 entry in proc_endpoints instead of the peer_data */
    for (i = 0 ; i < nprocs ; ++i) {
        free(btl_peer_data[i]);
        OPAL_THREAD_ADD32(&portals4_btl->portals_num_procs, -1);
    }

    if (0 == portals4_btl->portals_num_procs)
        mca_btl_portals4_free_module(portals4_btl);

    return OPAL_SUCCESS;
}

mca_btl_base_descriptor_t*
mca_btl_portals4_alloc(struct mca_btl_base_module_t* btl_base,
                      struct mca_btl_base_endpoint_t* endpoint,
                      uint8_t order,
                      size_t size,
                      uint32_t flags)
{
    struct mca_btl_portals4_module_t* portals4_btl = (struct mca_btl_portals4_module_t*) btl_base;
    mca_btl_portals4_frag_t* frag;

    if (size <= portals4_btl->super.btl_eager_limit) {
        OPAL_BTL_PORTALS4_FRAG_ALLOC_EAGER(portals4_btl, frag);
        if (NULL == frag) return NULL;
        frag->segments[0].base.seg_len = size;
    } else {
        OPAL_BTL_PORTALS4_FRAG_ALLOC_MAX(portals4_btl, frag);
        if (NULL == frag) return NULL;
        frag->segments[0].base.seg_len =
            size <= portals4_btl->super.btl_max_send_size ?
            size : portals4_btl->super.btl_max_send_size ;
    }

    frag->base.des_segment_count = 1;
    frag->base.des_flags = flags | MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    frag->base.order = MCA_BTL_NO_ORDER;

    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
	"mca_btl_portals4_alloc: %p\n", (void *) &frag->base));
    return &frag->base;
}

int
mca_btl_portals4_free(struct mca_btl_base_module_t* btl_base,
                      mca_btl_base_descriptor_t* des)
{
    struct mca_btl_portals4_module_t* portals4_btl = (struct mca_btl_portals4_module_t*) btl_base;
    mca_btl_portals4_frag_t* frag = (mca_btl_portals4_frag_t*) des;

    if (BTL_PORTALS4_FRAG_TYPE_EAGER == frag->type) {
        /* don't ever unlink eager frags */
        OPAL_BTL_PORTALS4_FRAG_RETURN_EAGER(portals4_btl, frag);

    } else if (BTL_PORTALS4_FRAG_TYPE_MAX == frag->type) {
        if (frag->me_h != PTL_INVALID_HANDLE) {
            frag->me_h = PTL_INVALID_HANDLE;
        }
        OPAL_BTL_PORTALS4_FRAG_RETURN_MAX(portals4_btl, frag);

    } else if (BTL_PORTALS4_FRAG_TYPE_USER == frag->type) {
        if (frag->me_h != PTL_INVALID_HANDLE) {
            frag->me_h = PTL_INVALID_HANDLE;
        }
        OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
            "mca_btl_portals4_free: Decrementing portals_outstanding_ops=%d\n", portals4_btl->portals_outstanding_ops));
        OPAL_BTL_PORTALS4_FRAG_RETURN_USER(portals4_btl, frag);
    } else {
        return OPAL_ERR_BAD_PARAM;
    }

    return OPAL_SUCCESS;
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
                            struct opal_convertor_t* convertor,
                            uint8_t order,
                            size_t reserve,
                            size_t* size,
                            uint32_t flags)
{
    struct mca_btl_portals4_module_t* portals4_btl = (struct mca_btl_portals4_module_t*) btl_base;
    mca_btl_portals4_frag_t* frag;
    size_t max_data = *size;
    struct iovec iov;
    uint32_t iov_count = 1;
    int ret;

    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
        "mca_btl_portals4_prepare_src NI=%d reserve=%ld size=%ld max_data=%ld\n", portals4_btl->interface_num, reserve, *size, max_data));

    if (0 != reserve || 0 != opal_convertor_need_buffers(convertor)) {
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "mca_btl_portals4_prepare_src NEED BUFFERS or RESERVE\n"));
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
            mca_btl_portals4_free(btl_base, (mca_btl_base_descriptor_t *) frag);
            return NULL;
        }

        frag->segments[0].base.seg_len = max_data + reserve;
        frag->base.des_segment_count = 1;
    }

    frag->base.des_segments = &frag->segments[0].base;
    frag->base.des_flags = flags | MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    frag->base.order = MCA_BTL_NO_ORDER;
    return &frag->base;
}

mca_btl_base_registration_handle_t *
mca_btl_portals4_register_mem(mca_btl_base_module_t *btl_base,
                              mca_btl_base_endpoint_t *endpoint,
                              void *base,
                              size_t size,
                              uint32_t flags)
{
    struct mca_btl_portals4_module_t   *portals4_btl = (struct mca_btl_portals4_module_t*) btl_base;
    mca_btl_base_registration_handle_t *handle = NULL;
    ptl_me_t me;
    int ret;

    handle = (mca_btl_base_registration_handle_t *)malloc(sizeof(mca_btl_base_registration_handle_t));
    if (!handle) {
        return NULL;
    }

    handle->key = OPAL_THREAD_ADD64(&(portals4_btl->portals_rdma_key), 1);

    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
        "mca_btl_portals4_register_mem NI=%d base=%p size=%ld handle=%p key=%ld\n",
        portals4_btl->interface_num, base, size, (void *)handle, handle->key));

    if (MCA_BTL_FLAGS_PUT == flags) {
        /* create a match entry */
        me.start = base;
        me.length = size;
        me.ct_handle = PTL_CT_NONE;
        me.min_free = 0;
        me.uid = PTL_UID_ANY;
        me.options = PTL_ME_OP_GET | PTL_ME_USE_ONCE |
            PTL_ME_EVENT_LINK_DISABLE |
            PTL_ME_EVENT_COMM_DISABLE |
            PTL_ME_EVENT_UNLINK_DISABLE;

        if (mca_btl_portals4_component.use_logical) {
            me.match_id.rank = endpoint->ptl_proc.rank;
        } else {
            me.match_id.phys.nid = endpoint->ptl_proc.phys.nid;
            me.match_id.phys.pid = endpoint->ptl_proc.phys.pid;
        }
        me.match_bits = handle->key;
        me.ignore_bits = BTL_PORTALS4_PROTOCOL_MASK |
            BTL_PORTALS4_CONTEXT_MASK |
            BTL_PORTALS4_SOURCE_MASK;
        me.ignore_bits = 0;

        ret = PtlMEAppend(portals4_btl->portals_ni_h,
                          portals4_btl->recv_idx,
                          &me,
                          PTL_PRIORITY_LIST,
                          handle,
                          &(handle->me_h));
        if (PTL_OK != ret) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                                "%s:%d: PtlMEAppend failed: %d\n",
                                __FILE__, __LINE__, ret);
            OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
            return NULL;
        }
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
            "PtlMEAppend (mca_btl_portals4_register_mem) handle=%p, me_h=%d start=%p length=%ld rank=%x nid=%x pid=%x match_bits=%lx\n",
            (void *)handle, handle->me_h, me.start, me.length,
            me.match_id.rank, me.match_id.phys.nid, me.match_id.phys.pid, me.match_bits));
    }
    return handle;
}

int
mca_btl_portals4_deregister_mem(mca_btl_base_module_t *btl_base,
                                mca_btl_base_registration_handle_t *handle)
{
    struct mca_btl_portals4_module_t   *portals4_btl = (struct mca_btl_portals4_module_t*) btl_base;

    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
        "mca_btl_portals4_deregister_mem NI=%d handle=%p key=%ld\n",
        portals4_btl->interface_num, (void *)handle, handle->key));

    free(handle);

    return OPAL_SUCCESS;
}

int
mca_btl_portals4_finalize(struct mca_btl_base_module_t *btl)
{
    struct mca_btl_portals4_module_t* portals4_btl = (struct mca_btl_portals4_module_t*) btl;

    mca_btl_portals4_free_module(portals4_btl);

    OBJ_DESTRUCT(&portals4_btl->portals_frag_eager);
    OBJ_DESTRUCT(&portals4_btl->portals_frag_max);
    OBJ_DESTRUCT(&portals4_btl->portals_frag_user);
    OBJ_DESTRUCT(&portals4_btl->portals_recv_blocks);

    free(portals4_btl);

    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
        "mca_btl_portals4_finalize NI %d: OK\n", portals4_btl->interface_num));

    return OPAL_SUCCESS;
}

void mca_btl_portals4_free_module(mca_btl_portals4_module_t *portals4_btl)
{
    int ret;

    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
         "mca_btl_portals4_free_module portals_outstanding_ops=%d\n", portals4_btl->portals_outstanding_ops));

    /* sanity check */
    assert(portals4_btl->portals_outstanding_ops  >= 0);

    /* finalize all communication */
    while (portals4_btl->portals_outstanding_ops > 0) {
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                            "mca_btl_portals4_free_module portals_outstanding_ops: %d",
                            portals4_btl->portals_outstanding_ops));

        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "Call to mca_btl_portals4_component_progress (3)\n"));
        mca_btl_portals4_component_progress();
    }

    if (!PtlHandleIsEqual(portals4_btl->send_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(portals4_btl->send_md_h);
        portals4_btl->send_md_h = PTL_INVALID_HANDLE;
    }
    if (!PtlHandleIsEqual(portals4_btl->zero_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(portals4_btl->zero_md_h);
        portals4_btl->zero_md_h = PTL_INVALID_HANDLE;
    }

    if (!PtlHandleIsEqual(portals4_btl->long_overflow_me_h, PTL_INVALID_HANDLE)) {
        PtlMEUnlink(portals4_btl->long_overflow_me_h);
        portals4_btl->long_overflow_me_h = PTL_INVALID_HANDLE;
    }

    if ((ptl_pt_index_t) ~0UL != mca_btl_portals4_module.recv_idx) {
        PtlPTFree(portals4_btl->portals_ni_h, portals4_btl->recv_idx);
        portals4_btl->recv_idx= (ptl_pt_index_t) ~0UL;
    }

    if (PTL_EQ_NONE != portals4_btl->recv_eq_h) {
        ret = PtlEQFree(portals4_btl->recv_eq_h);
        if (PTL_OK != ret) OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "Error freeing EQ recv: %d", ret));
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "PtlEQFree: recv_eq_h=%d portals4_btl=%p",
            portals4_btl->recv_eq_h, (void*)portals4_btl));

        portals4_btl->recv_eq_h = PTL_EQ_NONE;
    }
    if (!PtlHandleIsEqual(portals4_btl->portals_ni_h, PTL_INVALID_HANDLE)) {
        ret = PtlNIFini(portals4_btl->portals_ni_h);
        if (PTL_OK != ret) OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "Error returned by PtlNIFini: %d\n", ret));
        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "PtlNIFini: portals_ni_h=%d portals4_btl=%p",
            portals4_btl->portals_ni_h, (void*)portals4_btl));

        portals4_btl->portals_ni_h = PTL_INVALID_HANDLE;
    }
    ret = mca_btl_portals4_recv_disable(portals4_btl);
    if (PTL_OK != ret) OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "Error freeing recv list: %d", ret));
}
