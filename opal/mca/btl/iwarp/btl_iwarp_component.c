/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2006-2015 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Laboratory.  All rights reserved
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <infiniband/verbs.h>
#include <errno.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stddef.h>

#include "opal/mca/memory/memory.h"
#include "opal/mca/event/event.h"
#include "opal/align.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/mca/timer/base/base.h"
#include "opal/sys/atomic.h"
#include "opal/util/sys_limits.h"
#include "opal/util/argv.h"
#include "opal/memoryhooks/memory.h"
/* Define this before including hwloc.h so that we also get the hwloc
   verbs helper header file, too.  We have to do this level of
   indirection because the hwloc subsystem is a component -- we don't
   know its exact path.  We have to rely on the framework header files
   to find the right hwloc verbs helper file for us. */
#define OPAL_HWLOC_WANT_VERBS_HELPER 1
#include "opal/mca/hwloc/hwloc-internal.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal_stdint.h"
#include "opal/util/show_help.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/common/verbs/common_verbs.h"
#include "opal/runtime/opal_params.h"
#include "opal/runtime/opal.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/proc.h"

#include "btl_iwarp.h"
#include "btl_iwarp_frag.h"
#include "btl_iwarp_endpoint.h"
#include "btl_iwarp_eager_rdma.h"
#include "btl_iwarp_proc.h"
#include "btl_iwarp_ini.h"
#include "btl_iwarp_mca.h"
#include "btl_iwarp_async.h"
#include "connect/base.h"
#include "btl_iwarp_ip.h"

#define EPS 1.e-6
/*
 * Local functions
 */
static int btl_iwarp_component_register(void);
static int btl_iwarp_component_open(void);
static int btl_iwarp_component_close(void);
static mca_btl_base_module_t **btl_iwarp_component_init(int*, bool, bool);
static int btl_iwarp_component_progress(void);
/*
 * Local variables
 */
static mca_btl_iwarp_device_t *receive_queues_device = NULL;
static int num_devices_intentionally_ignored = 0;

mca_btl_iwarp_component_t mca_btl_iwarp_component = {
    .super = {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        .btl_version = {
            MCA_BTL_DEFAULT_VERSION("iwarp"),
            .mca_open_component = btl_iwarp_component_open,
            .mca_close_component = btl_iwarp_component_close,
            .mca_register_component_params = btl_iwarp_component_register,
        },
        .btl_data = {
            /* The component is checkpoint ready */
            .param_field = MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .btl_init = btl_iwarp_component_init,
        .btl_progress = btl_iwarp_component_progress,
    }
};

static int btl_iwarp_component_register(void)
{
    int ret;

    /* register IB component parameters */
    if (OPAL_SUCCESS != (ret = btl_iwarp_register_mca_params())) {
        return ret;
    }

    mca_btl_iwarp_component.max_send_size =
        mca_btl_iwarp_module.super.btl_max_send_size;
    mca_btl_iwarp_component.eager_limit =
        mca_btl_iwarp_module.super.btl_eager_limit;

    /* if_include and if_exclude need to be mutually exclusive */
    if (OPAL_SUCCESS !=
        mca_base_var_check_exclusive("ompi",
        mca_btl_iwarp_component.super.btl_version.mca_type_name,
        mca_btl_iwarp_component.super.btl_version.mca_component_name,
        "if_include",
        mca_btl_iwarp_component.super.btl_version.mca_type_name,
        mca_btl_iwarp_component.super.btl_version.mca_component_name,
        "if_exclude")) {
        /* Return ERR_NOT_AVAILABLE so that a warning message about
           "open" failing is not printed */
        return OPAL_ERR_NOT_AVAILABLE;
    }

    return OPAL_SUCCESS;
}

/*
 *  Called by MCA framework to open the component
 */
static int btl_iwarp_component_open(void)
{
    opal_mutex_t *lock = &mca_btl_iwarp_component.srq_manager.lock;
    opal_hash_table_t *srq_addr_table = &mca_btl_iwarp_component.srq_manager.srq_addr_table;

    /* Construct hash table that stores pointers to SRQs */
    OBJ_CONSTRUCT(lock, opal_mutex_t);
    OBJ_CONSTRUCT(srq_addr_table, opal_hash_table_t);

    /* initialize state */
    mca_btl_iwarp_component.ib_num_btls = 0;
    mca_btl_iwarp_component.num_default_gid_btls = 0;
    mca_btl_iwarp_component.iwarp_btls = NULL;
    OBJ_CONSTRUCT(&mca_btl_iwarp_component.devices, opal_pointer_array_t);
    mca_btl_iwarp_component.devices_count = 0;
    mca_btl_iwarp_component.cpc_explicitly_defined = false;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_iwarp_component.ib_procs, opal_list_t);
    mca_btl_iwarp_component.memory_registration_verbose = -1;

    return OPAL_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

static int btl_iwarp_component_close(void)
{
    int rc = OPAL_SUCCESS;

    /* remove the async event from the event base */
    mca_btl_iwarp_async_fini ();

    OBJ_DESTRUCT(&mca_btl_iwarp_component.srq_manager.lock);
    OBJ_DESTRUCT(&mca_btl_iwarp_component.srq_manager.srq_addr_table);

    opal_btl_iwarp_connect_base_finalize();
    opal_btl_iwarp_ini_finalize();

    if (NULL != mca_btl_iwarp_component.default_recv_qps) {
        free(mca_btl_iwarp_component.default_recv_qps);
    }

    /* close memory registration debugging output */
    opal_output_close (mca_btl_iwarp_component.memory_registration_verbose);

    return rc;
}

static void inline pack8(char **dest, uint8_t value)
{
    /* Copy one character */
    **dest = (char) value;
    /* Most the dest ahead one */
    ++*dest;
}

/*
 *  Register local iwarp port information with the modex so that it
 *  can be shared with all other peers.
 */
static int btl_iwarp_modex_send(void)
{
    int rc, i, j;
    int modex_message_size;
    char *message, *offset;
    size_t size, msg_size;
    opal_btl_iwarp_connect_base_module_t *cpc;

    opal_output(-1, "Starting to modex send");
    if (0 == mca_btl_iwarp_component.ib_num_btls) {
        return 0;
    }
    modex_message_size = offsetof(mca_btl_iwarp_modex_message_t, end);

    /* The message is packed into multiple parts:
     * 1. a uint8_t indicating the number of modules (ports) in the message
     * 2. for each module:
     *    a. the common module data
     *    b. a uint8_t indicating how many CPCs follow
     *    c. for each CPC:
     *       a. a uint8_t indicating the index of the CPC in the all[]
     *          array in btl_iwarp_connect_base.c
     *       b. a uint8_t indicating the priority of this CPC
     *       c. a uint8_t indicating the length of the blob to follow
     *       d. a blob that is only meaningful to that CPC
     */
    msg_size =
        /* uint8_t for number of modules in the message */
        1 +
        /* For each module: */
        mca_btl_iwarp_component.ib_num_btls *
        (
         /* Common module data */
         modex_message_size +
         /* uint8_t for how many CPCs follow */
         1
         );
    /* For each module, add in the size of the per-CPC data */
    for (i = 0; i < mca_btl_iwarp_component.ib_num_btls; i++) {
        for (j = 0;
             j < mca_btl_iwarp_component.iwarp_btls[i]->num_cpcs;
             ++j) {
            msg_size +=
                /* uint8_t for the index of the CPC */
                1 +
                /* uint8_t for the CPC's priority */
                1 +
                /* uint8_t for the blob length */
                1 +
                /* blob length */
                mca_btl_iwarp_component.iwarp_btls[i]->cpcs[j]->data.cbm_modex_message_len;
        }
    }
    message = (char *) malloc(msg_size);
    if (NULL == message) {
        BTL_ERROR(("Failed malloc"));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Pack the number of modules */
    offset = message;
    pack8(&offset, mca_btl_iwarp_component.ib_num_btls);
    opal_output(-1, "modex sending %d btls (packed: %d, offset now at %d)", mca_btl_iwarp_component.ib_num_btls, *((uint8_t*) message), (int) (offset - message));

    /* Pack each of the modules */
    for (i = 0; i < mca_btl_iwarp_component.ib_num_btls; i++) {

        /* Pack the modex common message struct.  */
        size = modex_message_size;

        (mca_btl_iwarp_component.iwarp_btls[i]->port_info).vendor_id =
            (mca_btl_iwarp_component.iwarp_btls[i]->device->ib_dev_attr).vendor_id;

        (mca_btl_iwarp_component.iwarp_btls[i]->port_info).vendor_part_id =
            (mca_btl_iwarp_component.iwarp_btls[i]->device->ib_dev_attr).vendor_part_id;

        (mca_btl_iwarp_component.iwarp_btls[i]->port_info).transport_type =
            MCA_BTL_IWARP_TRANSPORT_IWARP;

        memcpy(offset,
               &(mca_btl_iwarp_component.iwarp_btls[i]->port_info),
               size);
        opal_output(-1, "modex packed btl port modex message: 0x%" PRIx64 ", %d, (size: %d)",
                    mca_btl_iwarp_component.iwarp_btls[i]->port_info.subnet_id,
                    mca_btl_iwarp_component.iwarp_btls[i]->port_info.mtu,
                    (int) size);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        MCA_BTL_IWARP_MODEX_MSG_HTON(*(mca_btl_iwarp_modex_message_t *)offset);
#endif
        offset += size;
        opal_output(-1, "modex packed btl %d: modex message, offset now %d",
                    i, (int) (offset -message));

        /* Pack the number of CPCs that follow */
        pack8(&offset,
              mca_btl_iwarp_component.iwarp_btls[i]->num_cpcs);
        opal_output(-1, "modex packed btl %d: to pack %d cpcs (packed: %d, offset now %d)",
                    i, mca_btl_iwarp_component.iwarp_btls[i]->num_cpcs,
                    *((uint8_t*) (offset - 1)), (int) (offset-message));

        /* Pack each CPC */
        for (j = 0;
             j < mca_btl_iwarp_component.iwarp_btls[i]->num_cpcs;
             ++j) {
            uint8_t u8;

            cpc = mca_btl_iwarp_component.iwarp_btls[i]->cpcs[j];
            opal_output(-1, "modex packed btl %d: packing cpc %s",
                        i, cpc->data.cbm_component->cbc_name);
            /* Pack the CPC index */
            u8 = opal_btl_iwarp_connect_base_get_cpc_index(cpc->data.cbm_component);
            pack8(&offset, u8);
            opal_output(-1, "packing btl %d: cpc %d: index %d (packed %d, offset now %d)",
                        i, j, u8, *((uint8_t*) (offset-1)), (int)(offset-message));
            /* Pack the CPC priority */
            pack8(&offset, cpc->data.cbm_priority);
            opal_output(-1, "packing btl %d: cpc %d: priority %d (packed %d, offset now %d)",
                        i, j, cpc->data.cbm_priority, *((uint8_t*) (offset-1)), (int)(offset-message));
            /* Pack the blob length */
            u8 = cpc->data.cbm_modex_message_len;
            pack8(&offset, u8);
            opal_output(-1, "packing btl %d: cpc %d: message len %d (packed %d, offset now %d)",
                        i, j, u8, *((uint8_t*) (offset-1)), (int)(offset-message));
            /* If the blob length is > 0, pack the blob */
            if (u8 > 0) {
                memcpy(offset, cpc->data.cbm_modex_message, u8);
                offset += u8;
                opal_output(-1, "packing btl %d: cpc %d: blob packed %d %x (offset now %d)",
                            i, j,
                            ((uint32_t*)cpc->data.cbm_modex_message)[0],
                            ((uint32_t*)cpc->data.cbm_modex_message)[1],
                            (int)(offset-message));
            }

            /* Sanity check */
            assert((size_t) (offset - message) <= msg_size);
        }
    }

    /* All done -- send it! */
    OPAL_MODEX_SEND(rc, OPAL_PMIX_GLOBAL,
                    &mca_btl_iwarp_component.super.btl_version,
                    message, msg_size);
    free(message);
    opal_output(-1, "Modex sent!  %d calculated, %d actual\n", (int) msg_size, (int) (offset - message));

    return rc;
}

/*
 * Active Message Callback function on control message.
 */

static void btl_iwarp_control(mca_btl_base_module_t* btl,
        mca_btl_base_tag_t tag, mca_btl_base_descriptor_t* des,
        void* cbdata)
{
    /* don't return credits used for control messages */
    mca_btl_iwarp_module_t *obtl = (mca_btl_iwarp_module_t*)btl;
    mca_btl_iwarp_endpoint_t* ep = to_com_frag(des)->endpoint;
    mca_btl_iwarp_control_header_t *ctl_hdr =
        (mca_btl_iwarp_control_header_t *) to_base_frag(des)->segment.seg_addr.pval;
    mca_btl_iwarp_eager_rdma_header_t *rdma_hdr;
    mca_btl_iwarp_header_coalesced_t *clsc_hdr =
        (mca_btl_iwarp_header_coalesced_t*)(ctl_hdr + 1);
    mca_btl_active_message_callback_t* reg;
    size_t len = des->des_segments->seg_len - sizeof(*ctl_hdr);

    switch (ctl_hdr->type) {
    case MCA_BTL_IWARP_CONTROL_CREDITS:
        assert(0); /* Credit message is handled elsewhere */
        break;
    case MCA_BTL_IWARP_CONTROL_RDMA:
       rdma_hdr = (mca_btl_iwarp_eager_rdma_header_t*)ctl_hdr;

       BTL_VERBOSE(("prior to NTOH received  rkey %" PRIu32
                    ", rdma_start.lval %" PRIx64 ", pval %p, ival %" PRIu32,
                    rdma_hdr->rkey,
                    rdma_hdr->rdma_start.lval,
                    rdma_hdr->rdma_start.pval,
                    rdma_hdr->rdma_start.ival
                  ));

       if(ep->nbo) {
           BTL_IWARP_EAGER_RDMA_CONTROL_HEADER_NTOH(*rdma_hdr);
       }

       BTL_VERBOSE(("received  rkey %" PRIu32
                    ", rdma_start.lval %" PRIx64 ", pval %p,"
                    " ival %" PRIu32, rdma_hdr->rkey,
                    rdma_hdr->rdma_start.lval,
                    rdma_hdr->rdma_start.pval, rdma_hdr->rdma_start.ival));

       if (ep->eager_rdma_remote.base.pval) {
           BTL_ERROR(("Got RDMA connect twice!"));
           return;
       }
       ep->eager_rdma_remote.rkey = rdma_hdr->rkey;
       ep->eager_rdma_remote.base.lval = rdma_hdr->rdma_start.lval;
       ep->eager_rdma_remote.tokens=mca_btl_iwarp_component.eager_rdma_num - 1;
       break;
    case MCA_BTL_IWARP_CONTROL_COALESCED:
        {
            size_t pad = 0;
            while(len > 0) {
                size_t skip;
                mca_btl_iwarp_header_coalesced_t* unalign_hdr = 0;
                mca_btl_base_descriptor_t tmp_des;
                mca_btl_base_segment_t tmp_seg;

                assert(len >= sizeof(*clsc_hdr));

                if(ep->nbo)
                    BTL_IWARP_HEADER_COALESCED_NTOH(*clsc_hdr);

                skip = (sizeof(*clsc_hdr) + clsc_hdr->alloc_size - pad);

                tmp_des.des_segments = &tmp_seg;
                tmp_des.des_segment_count = 1;
                tmp_seg.seg_addr.pval = clsc_hdr + 1;
                tmp_seg.seg_len = clsc_hdr->size;

                /* call registered callback */
                reg = mca_btl_base_active_message_trigger + clsc_hdr->tag;
                reg->cbfunc( &obtl->super, clsc_hdr->tag, &tmp_des, reg->cbdata );
                len -= (skip + pad);
                unalign_hdr = (mca_btl_iwarp_header_coalesced_t*)
                    ((unsigned char*)clsc_hdr + skip);
                pad = (size_t)BTL_IWARP_COALESCE_HDR_PADDING(unalign_hdr);
                clsc_hdr = (mca_btl_iwarp_header_coalesced_t*)((unsigned char*)unalign_hdr +
                                                                pad);
            }
        }
       break;
    case MCA_BTL_IWARP_CONTROL_CTS:
        OPAL_OUTPUT((-1, "received CTS from %s (buffer %p): posted recvs %d, sent cts %d",
                     opal_get_proc_hostname(ep->endpoint_proc->proc_opal),
                     (void*) ctl_hdr,
                     ep->endpoint_posted_recvs, ep->endpoint_cts_sent));
        ep->endpoint_cts_received = true;

        /* Only send the CTS back and mark connected if:
           - we have posted our receives (it's possible that we can
             get this CTS before this side's CPC has called
             cpc_complete())
           - we have not yet sent our CTS

           We don't even want to mark the endpoint connected() until
           we have posted our receives because otherwise we will
           trigger credit management (because the rd_credits will
           still be negative), and Bad Things will happen. */
        if (ep->endpoint_posted_recvs) {
            /* need to hold to lock for both send_cts and connected */
            OPAL_THREAD_LOCK(&ep->endpoint_lock);
            if (!ep->endpoint_cts_sent) {
                mca_btl_iwarp_endpoint_send_cts(ep);
            }
            mca_btl_iwarp_endpoint_connected(ep);
        }
        break;
    default:
        BTL_ERROR(("Unknown message type received by BTL"));
       break;
    }
}

static int iwarp_reg_mr (void *reg_data, void *base, size_t size,
                          mca_rcache_base_registration_t *reg)
{
    mca_btl_iwarp_device_t *device = (mca_btl_iwarp_device_t*)reg_data;
    mca_btl_iwarp_reg_t *iwarp_reg = (mca_btl_iwarp_reg_t*)reg;
    enum ibv_access_flags access_flag = 0;

    if (reg->access_flags & MCA_RCACHE_ACCESS_REMOTE_READ) {
        access_flag |= IBV_ACCESS_REMOTE_READ;
    }

    if (reg->access_flags & MCA_RCACHE_ACCESS_REMOTE_WRITE) {
        access_flag |= IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_LOCAL_WRITE;
    }

    if (reg->access_flags & MCA_RCACHE_ACCESS_LOCAL_WRITE) {
        access_flag |= IBV_ACCESS_LOCAL_WRITE;
    }

#if HAVE_DECL_IBV_ATOMIC_HCA
    if (reg->access_flags & MCA_RCACHE_ACCESS_REMOTE_ATOMIC) {
        access_flag |= IBV_ACCESS_REMOTE_ATOMIC | IBV_ACCESS_LOCAL_WRITE;
    }
#endif

    if (device->mem_reg_max &&
        device->mem_reg_max < (device->mem_reg_active + size)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    device->mem_reg_active += size;

#if HAVE_DECL_IBV_ACCESS_SO
    if (reg->flags & MCA_RCACHE_FLAGS_SO_MEM) {
        access_flag |= IBV_ACCESS_SO;
    }
#endif

    iwarp_reg->mr = ibv_reg_mr(device->ib_pd, base, size, access_flag);

    if (NULL == iwarp_reg->mr) {
        OPAL_OUTPUT_VERBOSE((5, mca_btl_iwarp_component.memory_registration_verbose,
                             "ibv_reg_mr() failed: base=%p, bound=%p, size=%d, flags=0x%x, errno=%d",
                              reg->base, reg->bound, (int) (reg->bound - reg->base + 1), reg->flags, errno));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    iwarp_reg->btl_handle.lkey = iwarp_reg->mr->lkey;
    iwarp_reg->btl_handle.rkey = iwarp_reg->mr->rkey;

    OPAL_OUTPUT_VERBOSE((30, mca_btl_iwarp_component.memory_registration_verbose,
                         "iwarp_reg_mr: base=%p, bound=%p, size=%d, flags=0x%x", reg->base, reg->bound,
                         (int) (reg->bound - reg->base + 1), reg->flags));

    return OPAL_SUCCESS;
}

static int iwarp_dereg_mr(void *reg_data, mca_rcache_base_registration_t *reg)
{
    mca_btl_iwarp_device_t *device = (mca_btl_iwarp_device_t*)reg_data;
    mca_btl_iwarp_reg_t *iwarp_reg = (mca_btl_iwarp_reg_t*)reg;

    OPAL_OUTPUT_VERBOSE((30, mca_btl_iwarp_component.memory_registration_verbose,
                         "iwarp_dereg_mr: base=%p, bound=%p, size=%d, flags=0x%x", reg->base, reg->bound,
                         (int) (reg->bound - reg->base + 1), reg->flags));

    if(iwarp_reg->mr != NULL) {
        if(ibv_dereg_mr(iwarp_reg->mr)) {
            BTL_ERROR(("%s: error unpinning iwarp memory errno says %s",
                       __func__, strerror(errno)));
            return OPAL_ERROR;
        }

    }

    device->mem_reg_active -= (uint64_t) (reg->bound - reg->base + 1);

    iwarp_reg->mr = NULL;
    return OPAL_SUCCESS;
}

static inline int param_register_uint(const char* param_name, unsigned int default_value, unsigned int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_btl_iwarp_component.super.btl_version,
                                           param_name, NULL, MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                           NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
    return *storage;
}

static int init_one_port(opal_list_t *btl_list, mca_btl_iwarp_device_t *device,
                         uint8_t port_num, struct ibv_port_attr *ib_port_attr)
{
    mca_btl_iwarp_module_t *iwarp_btl;
    mca_btl_base_selected_module_t *ib_selected;
    uint64_t subnet_id;
    char param[40];

    /* Ensure that the requested GID index (via the
       btl_iwarp_gid_index MCA param) is within the GID table
       size. */
    if (mca_btl_iwarp_component.gid_index >
        ib_port_attr->gid_tbl_len) {
        opal_show_help("help-mpi-btl-iwarp.txt", "gid index too large",
                       true, opal_process_info.nodename,
                       ibv_get_device_name(device->ib_dev), port_num,
                       mca_btl_iwarp_component.gid_index,
                       ib_port_attr->gid_tbl_len);
        return OPAL_ERR_NOT_FOUND;
    }
    BTL_VERBOSE(("looking for %s:%d GID index %d",
                 ibv_get_device_name(device->ib_dev), port_num,
                 mca_btl_iwarp_component.gid_index));

    subnet_id = mca_btl_iwarp_get_ip_subnet_id(device->ib_dev, port_num);

    if(mca_btl_iwarp_component.num_default_gid_btls > 0 &&
            IB_DEFAULT_GID_PREFIX == subnet_id &&
            mca_btl_iwarp_component.warn_default_gid_prefix) {
        opal_show_help("help-mpi-btl-iwarp.txt", "default subnet prefix",
                true, opal_process_info.nodename);
    }

    if (IB_DEFAULT_GID_PREFIX == subnet_id) {
        mca_btl_iwarp_component.num_default_gid_btls++;
    }

    iwarp_btl = (mca_btl_iwarp_module_t *) calloc(1, sizeof(mca_btl_iwarp_module_t));
    if(NULL == iwarp_btl) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    memcpy(iwarp_btl, &mca_btl_iwarp_module,
            sizeof(mca_btl_iwarp_module));
    memcpy(&iwarp_btl->ib_port_attr, ib_port_attr,
            sizeof(struct ibv_port_attr));
    ib_selected = OBJ_NEW(mca_btl_base_selected_module_t);
    ib_selected->btl_module = (mca_btl_base_module_t*) iwarp_btl;
    iwarp_btl->device = device;
    iwarp_btl->port_num = (uint8_t) port_num;

    iwarp_btl->port_info.subnet_id = subnet_id;
    iwarp_btl->port_info.mtu = device->mtu;

    iwarp_btl->cpcs = NULL;
    iwarp_btl->num_cpcs = 0;
    iwarp_btl->local_procs = 0;

    mca_btl_base_active_message_trigger[MCA_BTL_TAG_IB].cbfunc = btl_iwarp_control;
    mca_btl_base_active_message_trigger[MCA_BTL_TAG_IB].cbdata = NULL;

    if (iwarp_btl->super.btl_get_limit > iwarp_btl->ib_port_attr.max_msg_sz) {
        iwarp_btl->super.btl_get_limit = iwarp_btl->ib_port_attr.max_msg_sz;
    }

    iwarp_btl->super.btl_get_alignment = 0;

    if (iwarp_btl->super.btl_put_limit > iwarp_btl->ib_port_attr.max_msg_sz) {
        iwarp_btl->super.btl_put_limit = iwarp_btl->ib_port_attr.max_msg_sz;
    }

    iwarp_btl->super.btl_put_local_registration_threshold = iwarp_btl->device->max_inline_data;
    iwarp_btl->super.btl_get_local_registration_threshold = 0;

#if HAVE_DECL_IBV_ATOMIC_HCA
    iwarp_btl->atomic_ops_be = false;

#ifdef HAVE_STRUCT_IBV_EXP_DEVICE_ATTR_EXT_ATOM
    /* check that 8-byte atomics are supported */
    if (!(device->ib_exp_dev_attr.ext_atom.log_atomic_arg_sizes & (1<<3ull))) {
        iwarp_btl->super.btl_flags &= ~MCA_BTL_FLAGS_ATOMIC_FOPS;
        iwarp_btl->super.btl_atomic_flags = 0;
        iwarp_btl->super.btl_atomic_fop = NULL;
        iwarp_btl->super.btl_atomic_cswap = NULL;
    }
#endif

#ifdef HAVE_STRUCT_IBV_EXP_DEVICE_ATTR_EXP_ATOMIC_CAP
    switch (iwarp_btl->device->ib_exp_dev_attr.exp_atomic_cap)
#else
    switch (iwarp_btl->device->ib_dev_attr.atomic_cap)
#endif
    {
    case IBV_ATOMIC_GLOB:
        iwarp_btl->super.btl_flags |= MCA_BTL_ATOMIC_SUPPORTS_GLOB;
        break;
#if HAVE_DECL_IBV_EXP_ATOMIC_HCA_REPLY_BE
    case IBV_EXP_ATOMIC_HCA_REPLY_BE:
        iwarp_btl->atomic_ops_be = true;
        break;
#endif
    case IBV_ATOMIC_HCA:
        break;
    case IBV_ATOMIC_NONE:
        default:
        /* no atomics or an unsupported atomic type */
        iwarp_btl->super.btl_flags &= ~MCA_BTL_FLAGS_ATOMIC_FOPS;
        iwarp_btl->super.btl_atomic_flags = 0;
        iwarp_btl->super.btl_atomic_fop = NULL;
        iwarp_btl->super.btl_atomic_cswap = NULL;
    }
#endif

    iwarp_btl->super.btl_put_alignment = 0;

    iwarp_btl->super.btl_registration_handle_size = sizeof (mca_btl_base_registration_handle_t);

    /* Check bandwidth configured for this device */
    sprintf(param, "bandwidth_%s", ibv_get_device_name(device->ib_dev));
    param_register_uint(param, iwarp_btl->super.btl_bandwidth, &iwarp_btl->super.btl_bandwidth);

    /* Check bandwidth configured for this device/port */
    sprintf(param, "bandwidth_%s:%d", ibv_get_device_name(device->ib_dev),
            port_num);
    param_register_uint(param, iwarp_btl->super.btl_bandwidth, &iwarp_btl->super.btl_bandwidth);

    /* Check bandwidth configured for this device/port/LID */
    sprintf(param, "bandwidth_%s:%d:0",
            ibv_get_device_name(device->ib_dev), port_num);
    param_register_uint(param, iwarp_btl->super.btl_bandwidth, &iwarp_btl->super.btl_bandwidth);

    /* Check latency configured for this device */
    sprintf(param, "latency_%s", ibv_get_device_name(device->ib_dev));
    param_register_uint(param, iwarp_btl->super.btl_latency, &iwarp_btl->super.btl_latency);

    /* Check latency configured for this device/port */
    sprintf(param, "latency_%s:%d", ibv_get_device_name(device->ib_dev),
                port_num);
    param_register_uint(param, iwarp_btl->super.btl_latency, &iwarp_btl->super.btl_latency);

    /* Check latency configured for this device/port/LID */
    sprintf(param, "latency_%s:%d:0", ibv_get_device_name(device->ib_dev),
                port_num);
    param_register_uint(param, iwarp_btl->super.btl_latency, &iwarp_btl->super.btl_latency);

    /* Auto-detect the port bandwidth */
    if (0 == iwarp_btl->super.btl_bandwidth) {
        if (OPAL_SUCCESS !=
            opal_common_verbs_port_bw(ib_port_attr,
                                      &iwarp_btl->super.btl_bandwidth)) {
            /* If we can't figure out the bandwidth, declare
               this port unreachable (do not* return
               ERR_VALUE_OF_OUT_OF_BOUNDS; that is reserved
               for when we exceed the number of allowable
               BTLs). */
           return OPAL_ERR_UNREACH;
        }
    }

    opal_list_append(btl_list, (opal_list_item_t*) ib_selected);
    opal_pointer_array_add(device->device_btls, (void*) iwarp_btl);
    ++device->btls;
    ++mca_btl_iwarp_component.ib_num_btls;
    if (-1 != mca_btl_iwarp_component.ib_max_btls &&
        mca_btl_iwarp_component.ib_num_btls >=
        mca_btl_iwarp_component.ib_max_btls) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    return OPAL_SUCCESS;
}

static void device_construct(mca_btl_iwarp_device_t *device)
{
    device->ib_dev = NULL;
    device->ib_dev_context = NULL;
    device->ib_pd = NULL;
    device->mpool = NULL;
    device->rcache = NULL;
#if OPAL_ENABLE_PROGRESS_THREADS == 1
    device->ib_channel = NULL;
#endif
    device->btls = 0;
    device->endpoints = NULL;
    device->device_btls = NULL;
    device->ib_cq[BTL_IWARP_HP_CQ] = NULL;
    device->ib_cq[BTL_IWARP_LP_CQ] = NULL;
    device->cq_size[BTL_IWARP_HP_CQ] = 0;
    device->cq_size[BTL_IWARP_LP_CQ] = 0;
    device->non_eager_rdma_endpoints = 0;
    device->hp_cq_polls = mca_btl_iwarp_component.cq_poll_ratio;
    device->eager_rdma_polls = mca_btl_iwarp_component.eager_rdma_poll_ratio;
    device->pollme = true;
    device->eager_rdma_buffers_count = 0;
    device->eager_rdma_buffers = NULL;
    device->qps = NULL;
    OBJ_CONSTRUCT(&device->device_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&device->send_free_control, opal_free_list_t);
    device->max_inline_data = 0;
    device->ready_for_use = false;
}

static void device_destruct(mca_btl_iwarp_device_t *device)
{
    int i;

#if OPAL_ENABLE_PROGRESS_THREADS == 1
    if (device->progress) {
        device->progress = false;
        if (pthread_cancel(device->thread.t_handle)) {
            BTL_ERROR(("Failed to cancel OpenIB progress thread"));
            goto device_error;
        }
        opal_thread_join(&device->thread, NULL);
    }

    if (ibv_destroy_comp_channel(device->ib_channel)) {
        BTL_VERBOSE(("Failed to close comp_channel"));
        goto device_error;
    }
#endif

    /* signaling to async_tread to stop poll for this device */
    mca_btl_iwarp_async_rem_device (device);

    if(device->eager_rdma_buffers) {
        int i;
        for(i = 0; i < device->eager_rdma_buffers_count; i++)
            if(device->eager_rdma_buffers[i])
                OBJ_RELEASE(device->eager_rdma_buffers[i]);
        free(device->eager_rdma_buffers);
    }

    if (NULL != device->qps) {
        for (i = 0; i < mca_btl_iwarp_component.num_qps; i++) {
            OBJ_DESTRUCT(&device->qps[i].send_free);
            OBJ_DESTRUCT(&device->qps[i].recv_free);
        }
        free(device->qps);
    }

    OBJ_DESTRUCT(&device->send_free_control);

    /* Release CQs */
    if(device->ib_cq[BTL_IWARP_HP_CQ] != NULL) {
        if (ibv_destroy_cq(device->ib_cq[BTL_IWARP_HP_CQ])) {
            BTL_VERBOSE(("Failed to close HP CQ"));
            goto device_error;
        }
    }

    if(device->ib_cq[BTL_IWARP_LP_CQ] != NULL) {
        if (ibv_destroy_cq(device->ib_cq[BTL_IWARP_LP_CQ])) {
            BTL_VERBOSE(("Failed to close LP CQ"));
            goto device_error;
        }
    }

    if (OPAL_SUCCESS != mca_rcache_base_module_destroy (device->rcache)) {
        BTL_VERBOSE(("failed to release registration cache"));
        goto device_error;
    }

    if (ibv_dealloc_pd(device->ib_pd)) {
        BTL_VERBOSE(("Warning! Failed to release PD"));
        goto device_error;
    }

    OBJ_DESTRUCT(&device->device_lock);

    if (ibv_close_device(device->ib_dev_context)) {
        if (1 == opal_leave_pinned || opal_leave_pinned_pipeline) {
            BTL_VERBOSE(("Warning! Failed to close device"));
            goto device_error;
        } else {
            BTL_ERROR(("Error! Failed to close device"));
            goto device_error;
        }
    }
    BTL_VERBOSE(("device was successfully released"));
    return;
device_error:
    BTL_VERBOSE(("Failed to destroy device resources"));
}

OBJ_CLASS_INSTANCE(mca_btl_iwarp_device_t, opal_object_t, device_construct,
        device_destruct);

static int
get_port_list(mca_btl_iwarp_device_t *device, int *allowed_ports)
{
    int i, j, k, num_ports = 0;
    const char *dev_name;
    char *name;

    dev_name = ibv_get_device_name(device->ib_dev);
    name = (char*) malloc(strlen(dev_name) + 4);
    if (NULL == name) {
        return 0;
    }

    /* Assume that all ports are allowed.  num_ports will be adjusted
       below to reflect whether this is true or not. */
    for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
        allowed_ports[num_ports++] = i;
    }
    num_ports = 0;
    if (NULL != mca_btl_iwarp_component.if_include_list) {
        /* If only the device name is given (eg. mtdevice0,mtdevice1) use all
           ports */
        i = 0;
        while (mca_btl_iwarp_component.if_include_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_btl_iwarp_component.if_include_list[i])) {
                num_ports = device->ib_dev_attr.phys_port_cnt;
                goto done;
            }
            ++i;
        }
        /* Include only requested ports on the device */
        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name,"%s:%d",dev_name,i);
            for (j = 0;
                 NULL != mca_btl_iwarp_component.if_include_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_btl_iwarp_component.if_include_list[j])) {
                    allowed_ports[num_ports++] = i;
                    break;
                }
            }
        }
    } else if (NULL != mca_btl_iwarp_component.if_exclude_list) {
        /* If only the device name is given (eg. mtdevice0,mtdevice1) exclude
           all ports */
        i = 0;
        while (mca_btl_iwarp_component.if_exclude_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_btl_iwarp_component.if_exclude_list[i])) {
                num_ports = 0;
                goto done;
            }
            ++i;
        }
        /* Exclude the specified ports on this device */
        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name,"%s:%d",dev_name,i);
            for (j = 0;
                 NULL != mca_btl_iwarp_component.if_exclude_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_btl_iwarp_component.if_exclude_list[j])) {
                    /* If found, set a sentinel value */
                    j = -1;
                    break;
                }
            }
            /* If we didn't find it, it's ok to include in the list */
            if (-1 != j) {
                allowed_ports[num_ports++] = i;
            }
        }
    } else {
        num_ports = device->ib_dev_attr.phys_port_cnt;
    }

done:

    /* Remove the following from the error-checking if_list:
       - bare device name
       - device name suffixed with port number */
    if (NULL != mca_btl_iwarp_component.if_list) {
        for (i = 0; NULL != mca_btl_iwarp_component.if_list[i]; ++i) {

            /* Look for raw device name */
            if (0 == strcmp(mca_btl_iwarp_component.if_list[i], dev_name)) {
                j = opal_argv_count(mca_btl_iwarp_component.if_list);
                opal_argv_delete(&j, &(mca_btl_iwarp_component.if_list),
                                 i, 1);
                --i;
            }
        }
        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name, "%s:%d", dev_name, i);
            for (j = 0; NULL != mca_btl_iwarp_component.if_list[j]; ++j) {
                if (0 == strcmp(mca_btl_iwarp_component.if_list[j], name)) {
                    k = opal_argv_count(mca_btl_iwarp_component.if_list);
                    opal_argv_delete(&k, &(mca_btl_iwarp_component.if_list),
                                     j, 1);
                    --j;
                    break;
                }
            }
        }
    }

    free(name);

    return num_ports;
}

/*
 * Prefer values that are already in the target
 */
static void merge_values(opal_btl_iwarp_ini_values_t *target,
                         opal_btl_iwarp_ini_values_t *src)
{
    if (!target->mtu_set && src->mtu_set) {
        target->mtu = src->mtu;
        target->mtu_set = true;
    }

    if (!target->use_eager_rdma_set && src->use_eager_rdma_set) {
        target->use_eager_rdma = src->use_eager_rdma;
        target->use_eager_rdma_set = true;
    }

    if (NULL == target->receive_queues && NULL != src->receive_queues) {
        target->receive_queues = strdup(src->receive_queues);
    }

    if (!target->max_inline_data_set && src->max_inline_data_set) {
        target->max_inline_data = src->max_inline_data;
        target->max_inline_data_set = true;
    }
}

static bool inline is_credit_message(const mca_btl_iwarp_recv_frag_t *frag)
{
    mca_btl_iwarp_control_header_t* chdr =
        (mca_btl_iwarp_control_header_t *) to_base_frag(frag)->segment.seg_addr.pval;
    return (MCA_BTL_TAG_IB == frag->hdr->tag) &&
        (MCA_BTL_IWARP_CONTROL_CREDITS == chdr->type);
}

static bool inline is_cts_message(const mca_btl_iwarp_recv_frag_t *frag)
{
    mca_btl_iwarp_control_header_t* chdr =
        (mca_btl_iwarp_control_header_t *) to_base_frag(frag)->segment.seg_addr.pval;
    return (MCA_BTL_TAG_IB == frag->hdr->tag) &&
        (MCA_BTL_IWARP_CONTROL_CTS == chdr->type);
}

static int32_t atoi_param(char *param, int32_t dflt)
{
    if (NULL == param || '\0' == param[0]) {
        return dflt ? dflt : 1;
    }

    return atoi(param);
}

static int get_var_source (const char *var_name, mca_base_var_source_t *source)
{
    int vari = mca_base_var_find ("opal", "btl", "iwarp", var_name);
    if (0 > vari) {
        return vari;
    }

    return mca_base_var_get_value (vari, NULL, source, NULL);
}

static int setup_qps(void)
{
    char **queues, **params = NULL;
    int num_pp_qps = 0, num_srq_qps = 0, qp = 0;
    uint32_t max_qp_size, max_size_needed;
    int32_t min_freelist_size = 0;
    int smallest_pp_qp = INT_MAX, ret = OPAL_ERROR;

    queues = opal_argv_split(mca_btl_iwarp_component.receive_queues, ':');
    if (0 == opal_argv_count(queues)) {
        opal_show_help("help-mpi-btl-iwarp.txt",
                       "no qps in receive_queues", true,
                       opal_process_info.nodename,
                       mca_btl_iwarp_component.receive_queues);
        ret = OPAL_ERROR;
        goto error;
    }

    while (queues[qp] != NULL) {
        if (0 == strncmp("P,", queues[qp], 2)) {
            num_pp_qps++;
            if (smallest_pp_qp > qp) {
                smallest_pp_qp = qp;
            }
        } else if (0 == strncmp("S,", queues[qp], 2)) {
            num_srq_qps++;
        } else if (0 == strncmp("X,", queues[qp], 2)) {
            opal_show_help("help-mpi-btl-iwarp.txt", "No XRC support", true,
                           opal_process_info.nodename,
                           mca_btl_iwarp_component.receive_queues);
            ret = OPAL_ERR_NOT_AVAILABLE;
            goto error;
        } else {
            opal_show_help("help-mpi-btl-iwarp.txt",
                           "invalid qp type in receive_queues", true,
                           opal_process_info.nodename,
                           mca_btl_iwarp_component.receive_queues,
                           queues[qp]);
            ret = OPAL_ERR_BAD_PARAM;
            goto error;
        }
        qp++;
    }

    mca_btl_iwarp_component.num_pp_qps = num_pp_qps;
    mca_btl_iwarp_component.num_srq_qps = num_srq_qps;
    mca_btl_iwarp_component.num_qps = num_pp_qps + num_srq_qps;

    mca_btl_iwarp_component.qp_infos = (mca_btl_iwarp_qp_info_t*)
        malloc(sizeof(mca_btl_iwarp_qp_info_t) *
                mca_btl_iwarp_component.num_qps);
    if (NULL == mca_btl_iwarp_component.qp_infos) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto error;
    }

    qp = 0;
#define P(N) (((N) > count) ? NULL : params[(N)])
    while (queues[qp] != NULL) {
        int count;
        int32_t rd_low, rd_num;
        params = opal_argv_split_with_empty(queues[qp], ',');
        count = opal_argv_count(params);

        if ('P' == params[0][0]) {
            int32_t rd_win, rd_rsv;
            if (count < 3 || count > 6) {
                opal_show_help("help-mpi-btl-iwarp.txt",
                               "invalid pp qp specification", true,
                               opal_process_info.nodename, queues[qp]);
                ret = OPAL_ERR_BAD_PARAM;
                goto error;
            }
            mca_btl_iwarp_component.qp_infos[qp].type = MCA_BTL_IWARP_PP_QP;
            mca_btl_iwarp_component.qp_infos[qp].size = atoi_param(P(1), 0);
            rd_num = atoi_param(P(2), 256);
            /* by default set rd_low to be 3/4 of rd_num */
            rd_low = atoi_param(P(3), rd_num - (rd_num / 4));
            rd_win = atoi_param(P(4), (rd_num - rd_low) * 2);

            if (0 >= rd_win) {
                opal_show_help("help-mpi-btl-iwarp.txt",
                               "invalid pp qp specification", true,
                               opal_process_info.nodename, queues[qp]);
                ret = OPAL_ERR_BAD_PARAM;
                goto error;
            }

            rd_rsv = atoi_param(P(5), (rd_num * 2) / rd_win);

            BTL_VERBOSE(("pp: rd_num is %d rd_low is %d rd_win %d rd_rsv %d",
                         rd_num, rd_low, rd_win, rd_rsv));

            /* Calculate the smallest freelist size that can be allowed */
            if (rd_num + rd_rsv > min_freelist_size) {
                min_freelist_size = rd_num + rd_rsv;
            }

            mca_btl_iwarp_component.qp_infos[qp].u.pp_qp.rd_win = rd_win;
            mca_btl_iwarp_component.qp_infos[qp].u.pp_qp.rd_rsv = rd_rsv;
            if ((rd_num - rd_low) > rd_win) {
                opal_show_help("help-mpi-btl-iwarp.txt", "non optimal rd_win",
                        true, rd_win, rd_num - rd_low);
            }
        } else {
            int32_t sd_max, rd_init, srq_limit;
            if (count < 3 || count > 7) {
                opal_show_help("help-mpi-btl-iwarp.txt",
                               "invalid srq specification", true,
                               opal_process_info.nodename, queues[qp]);
                ret = OPAL_ERR_BAD_PARAM;
                goto error;
            }
            mca_btl_iwarp_component.qp_infos[qp].type = MCA_BTL_IWARP_SRQ_QP;
            mca_btl_iwarp_component.qp_infos[qp].size = atoi_param(P(1), 0);
            rd_num = atoi_param(P(2), 256);
            /* by default set rd_low to be 3/4 of rd_num */
            rd_low = atoi_param(P(3), rd_num - (rd_num / 4));
            sd_max = atoi_param(P(4), rd_low / 4);
            /* rd_init is initial value for rd_curr_num of all SRQs, 1/4 of rd_num by default */
            rd_init = atoi_param(P(5), rd_num / 4);
            /* by default set srq_limit to be 3/16 of rd_init (it's 1/4 of rd_low_local,
               the value of rd_low_local we calculate in create_srq function) */
            srq_limit = atoi_param(P(6), (rd_init - (rd_init / 4)) / 4);

            /* If we set srq_limit less or greater than rd_init
               (init value for rd_curr_num) => we receive the IBV_EVENT_SRQ_LIMIT_REACHED
               event immediately and the value of rd_curr_num will be increased */

            /* If we set srq_limit to zero, but size of SRQ greater than 1 => set it to be 1 */
            if((0 == srq_limit) && (1 < rd_num)) {
                srq_limit = 1;
            }

            BTL_VERBOSE(("srq: rd_num is %d rd_low is %d sd_max is %d rd_max is %d srq_limit is %d",
                         rd_num, rd_low, sd_max, rd_init, srq_limit));

            /* Calculate the smallest freelist size that can be allowed */
            if (rd_num > min_freelist_size) {
                min_freelist_size = rd_num;
            }

            if (rd_num < rd_init) {
                opal_show_help("help-mpi-btl-iwarp.txt", "rd_num must be >= rd_init",
                        true, opal_process_info.nodename, queues[qp]);
                ret = OPAL_ERR_BAD_PARAM;
                goto error;
            }

            if (rd_num < srq_limit) {
                opal_show_help("help-mpi-btl-iwarp.txt", "srq_limit must be > rd_num",
                        true, opal_process_info.nodename, queues[qp]);
                ret = OPAL_ERR_BAD_PARAM;
                goto error;
            }

            mca_btl_iwarp_component.qp_infos[qp].u.srq_qp.sd_max = sd_max;
            mca_btl_iwarp_component.qp_infos[qp].u.srq_qp.rd_init = rd_init;
            mca_btl_iwarp_component.qp_infos[qp].u.srq_qp.srq_limit = srq_limit;
        }

        if (rd_num <= rd_low) {
            opal_show_help("help-mpi-btl-iwarp.txt", "rd_num must be > rd_low",
                    true, opal_process_info.nodename, queues[qp]);
            ret = OPAL_ERR_BAD_PARAM;
            goto error;
        }
        mca_btl_iwarp_component.qp_infos[qp].rd_num = rd_num;
        mca_btl_iwarp_component.qp_infos[qp].rd_low = rd_low;
        opal_argv_free(params);
        qp++;
    }
    params = NULL;

    /* Sanity check some sizes */

    max_qp_size = mca_btl_iwarp_component.qp_infos[mca_btl_iwarp_component.num_qps - 1].size;
    max_size_needed = (mca_btl_iwarp_module.super.btl_eager_limit >
                       mca_btl_iwarp_module.super.btl_max_send_size) ?
        mca_btl_iwarp_module.super.btl_eager_limit :
        mca_btl_iwarp_module.super.btl_max_send_size;

    if (max_qp_size < max_size_needed) {
        mca_base_var_source_t eager_source = MCA_BASE_VAR_SOURCE_DEFAULT;
        mca_base_var_source_t max_send_source = MCA_BASE_VAR_SOURCE_DEFAULT;

        (void) get_var_source ("max_send_size", &max_send_source);
        (void) get_var_source ("eager_limit", &eager_source);

        /* the largest queue pair is too small for either the max send size or eager
         * limit. check where we got the max_send_size and eager_limit and adjust if
         * the user did not specify one or the other. */
        if (mca_btl_iwarp_module.super.btl_eager_limit > max_qp_size &&
            MCA_BASE_VAR_SOURCE_DEFAULT == eager_source) {
            mca_btl_iwarp_module.super.btl_eager_limit = max_qp_size;
        }

        if (mca_btl_iwarp_module.super.btl_max_send_size > max_qp_size &&
            MCA_BASE_VAR_SOURCE_DEFAULT == max_send_source) {
            mca_btl_iwarp_module.super.btl_max_send_size = max_qp_size;
        }

        max_size_needed = (mca_btl_iwarp_module.super.btl_eager_limit >
                       mca_btl_iwarp_module.super.btl_max_send_size) ?
        mca_btl_iwarp_module.super.btl_eager_limit :
        mca_btl_iwarp_module.super.btl_max_send_size;
    }

    if (max_qp_size < max_size_needed) {
        opal_show_help("help-mpi-btl-iwarp.txt",
                       "biggest qp size is too small", true,
                       opal_process_info.nodename, max_qp_size,
                       max_size_needed);
        ret = OPAL_ERR_BAD_PARAM;
        goto error;
    } else if (max_qp_size > max_size_needed) {
        opal_show_help("help-mpi-btl-iwarp.txt",
                       "biggest qp size is too big", true,
                       opal_process_info.nodename, max_qp_size,
                       max_size_needed);
    }

    if (mca_btl_iwarp_component.ib_free_list_max > 0 &&
        min_freelist_size > mca_btl_iwarp_component.ib_free_list_max) {
        opal_show_help("help-mpi-btl-iwarp.txt", "freelist too small", true,
                       opal_process_info.nodename,
                       mca_btl_iwarp_component.ib_free_list_max,
                       min_freelist_size);
        ret = OPAL_ERR_BAD_PARAM;
        goto error;
    }

    mca_btl_iwarp_component.rdma_qp = mca_btl_iwarp_component.num_qps - 1;
    if (mca_btl_iwarp_component.num_qps > smallest_pp_qp) {
        mca_btl_iwarp_component.credits_qp = smallest_pp_qp;
    } else {
        mca_btl_iwarp_component.credits_qp = mca_btl_iwarp_component.num_qps - 1;
    }

    ret = OPAL_SUCCESS;
error:
    if (NULL != params) {
        opal_argv_free(params);
    }

    if (NULL != queues) {
        opal_argv_free(queues);
    }

    return ret;
}

/* read a single integer from a linux module parameters file */
static uint64_t read_module_param(char *file, uint64_t value, uint64_t max)
{
    int fd = open(file, O_RDONLY);
    char buffer[64];
    uint64_t ret;
    int rc;

    if (0 > fd) {
        return value;
    }

    rc = read (fd, buffer, 64);

    close (fd);

    if (0 == rc) {
        return value;
    }

    errno = 0;
    ret = strtoull(buffer, NULL, 10);

    if (ret > max) {
        /* NTH: probably should report a bogus value */
        ret = max;
    }

    return (0 == errno) ? ret : value;
}

/* calculate memory registation limits */
static uint64_t calculate_total_mem (void)
{
    hwloc_obj_t machine;
    int rc;
    uint64_t mem, *mptr;
    opal_process_name_t wildcard_rank;

    /* first try to retrieve it from PMIx as it may have
     * been provided */
    wildcard_rank.jobid = OPAL_PROC_MY_NAME.jobid;
    wildcard_rank.vpid = OPAL_VPID_WILDCARD;
    mptr = &mem;
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, OPAL_PMIX_AVAIL_PHYS_MEMORY,
                                   &wildcard_rank, &mptr, OPAL_UINT64);
    if (OPAL_SUCCESS == rc) {
        return mem;
    }

    /* couldn't find it */
    return 0;
}


static uint64_t calculate_max_reg (const char *device_name)
{
    struct stat statinfo;
    uint64_t mtts_per_seg = 1;
    uint64_t num_mtt = 1 << 19;
    uint64_t reserved_mtt = 0;
    uint64_t max_reg, mem_total;

    mem_total = calculate_total_mem ();

    /* On older OFED(<2.0), may need to turn off this parameter*/
    if (mca_btl_iwarp_component.allow_max_memory_registration) {
        max_reg = 2 * mem_total;
        /* Limit us to 87.5% of the registered memory (some fluff for QPs,
        file systems, etc) */
        return (max_reg * 7) >> 3;
    }

    /* Default to being able to register everything (to ensure that
       max_reg is initialized in all cases) */
    max_reg = mem_total;
    if (!strncmp(device_name, "mlx5", 4)) {
        max_reg = 2 * mem_total;

    } else if (!strncmp(device_name, "mlx4", 4)) {
        if (0 == stat("/sys/module/mlx4_core/parameters/log_num_mtt", &statinfo)) {
            mtts_per_seg = 1ull << read_module_param("/sys/module/mlx4_core/parameters/log_mtts_per_seg", 1, 63);
            num_mtt = 1ull << read_module_param("/sys/module/mlx4_core/parameters/log_mtts_per_seg", 1, 63);
            if (1 == num_mtt) {
                /* NTH: is 19 a minimum? when log_num_mtt is set to 0 use 19 */
                num_mtt = 1 << 19;
                max_reg = (num_mtt - reserved_mtt) * opal_getpagesize () * mtts_per_seg;
            } else  {
                max_reg = (num_mtt - reserved_mtt) * opal_getpagesize () * mtts_per_seg;
            }
        }

    } else if (!strncmp(device_name, "mthca", 5)) {
        if (0 == stat("/sys/module/ib_mthca/parameters/num_mtt", &statinfo)) {
            mtts_per_seg = 1ull << read_module_param("/sys/module/ib_mthca/parameters/log_mtts_per_seg", 1, 63);
            num_mtt = read_module_param("/sys/module/ib_mthca/parameters/num_mtt", 1 << 20, (uint64_t) -1);
            reserved_mtt = read_module_param("/sys/module/ib_mthca/parameters/fmr_reserved_mtts", 0, (uint64_t) -1);

            max_reg = (num_mtt - reserved_mtt) * opal_getpagesize () * mtts_per_seg;
        } else {
            max_reg = mem_total;
        }

    } else {
        /* Need to update to determine the registration limit for this
           configuration */
        max_reg = mem_total;
    }

    /* Print a warning if we can't register more than 75% of physical
       memory.  Abort if the abort_not_enough_reg_mem MCA param was
       set. */
    if (max_reg < mem_total * 3 / 4) {
        char *action;

        if (mca_btl_iwarp_component.abort_not_enough_reg_mem) {
            action = "Your MPI job will now abort.";
        } else {
            action = "Your MPI job will continue, but may be behave poorly and/or hang.";
        }
        opal_show_help("help-mpi-btl-iwarp.txt", "reg mem limit low", true,
                       opal_process_info.nodename, (unsigned long)(max_reg >> 20),
                       (unsigned long)(mem_total >> 20), action);
        return 0;  /* signal that we can't have enough memory */
    }

    /* Limit us to 87.5% of the registered memory (some fluff for QPs,
       file systems, etc) */
    return (max_reg * 7) >> 3;
}

static int init_one_device(opal_list_t *btl_list, struct ibv_device* ib_dev)
{
    mca_rcache_base_resources_t rcache_resources;
    mca_btl_iwarp_device_t *device;
    uint8_t i, k = 0;
    int ret = -1, port_cnt;
    opal_btl_iwarp_ini_values_t values, default_values;
    int *allowed_ports = NULL;
    bool need_search;
    struct ibv_context *dev_context = NULL;

    /* Open up the device */
    dev_context = ibv_open_device(ib_dev);
    if (NULL == dev_context) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    /* Find out if this device supports RC QPs */
    if (OPAL_SUCCESS != opal_common_verbs_qp_test(dev_context,
                                                  OPAL_COMMON_VERBS_FLAGS_RC)) {
        ibv_close_device(dev_context);
        BTL_VERBOSE(("iwarp: RC QPs not supported -- skipping %s",
                     ibv_get_device_name(ib_dev)));
        ++num_devices_intentionally_ignored;
        return OPAL_ERR_NOT_SUPPORTED;
    }

    device = OBJ_NEW(mca_btl_iwarp_device_t);
    if(NULL == device){
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        ibv_close_device(dev_context);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    device->mem_reg_active = 0;
    device->mem_reg_max_total = calculate_max_reg(ibv_get_device_name(ib_dev));
    device->mem_reg_max = device->mem_reg_max_total;
    if(( 0 == device->mem_reg_max) && mca_btl_iwarp_component.abort_not_enough_reg_mem) {
        return OPAL_ERROR;
    }

    device->ib_dev = ib_dev;
    device->ib_dev_context = dev_context;
    device->ib_pd = NULL;
    device->device_btls = OBJ_NEW(opal_pointer_array_t);
    if (OPAL_SUCCESS != opal_pointer_array_init(device->device_btls, 2, INT_MAX, 2)) {
        BTL_ERROR(("Failed to initialize device_btls array: %s:%d", __FILE__, __LINE__));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if(NULL == device->ib_dev_context){
        BTL_ERROR(("error obtaining device context for %s errno says %s",
                    ibv_get_device_name(device->ib_dev), strerror(errno)));
        goto error;
    }
#if HAVE_DECL_IBV_EXP_QUERY_DEVICE
    device->ib_exp_dev_attr.comp_mask = IBV_EXP_DEVICE_ATTR_RESERVED - 1;
    if(ibv_exp_query_device(device->ib_dev_context, &device->ib_exp_dev_attr)){
        BTL_ERROR(("error obtaining device attributes for %s errno says %s",
                    ibv_get_device_name(device->ib_dev), strerror(errno)));
        goto error;
    }
#endif
    if(ibv_query_device(device->ib_dev_context, &device->ib_dev_attr)){
        BTL_ERROR(("error obtaining device attributes for %s errno says %s",
                    ibv_get_device_name(device->ib_dev), strerror(errno)));
        goto error;
    }
    /* If mca_btl_if_include/exclude were specified, get usable ports */
    allowed_ports = (int*)malloc(device->ib_dev_attr.phys_port_cnt * sizeof(int));
    if (NULL == allowed_ports) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto error;
    }

    port_cnt = get_port_list(device, allowed_ports);
    if (0 == port_cnt) {
        ret = OPAL_SUCCESS;
        ++num_devices_intentionally_ignored;
        goto error;
    }

    /* Load in vendor/part-specific device parameters.  Note that even if
       we don't find values for this vendor/part, "values" will be set
       indicating that it does not have good values */
    ret = opal_btl_iwarp_ini_query(device->ib_dev_attr.vendor_id,
                                    device->ib_dev_attr.vendor_part_id,
                                    &values);
    if (OPAL_SUCCESS != ret &&
        OPAL_ERR_NOT_FOUND != ret) {
        /* If we get a serious error, propagate it upwards */
        goto error;
    }
    if (OPAL_ERR_NOT_FOUND == ret) {
        /* If we didn't find a matching device in the INI files, output a
           warning that we're using default values (unless overridden
           that we don't want to see these warnings) */
        if (mca_btl_iwarp_component.warn_no_device_params_found) {
            opal_show_help("help-mpi-btl-iwarp.txt",
                           "no device params found", true,
                           opal_process_info.nodename,
                           ibv_get_device_name(device->ib_dev),
                           device->ib_dev_attr.vendor_id,
                           device->ib_dev_attr.vendor_part_id);
        }
    }

    /* If we're supposed to ignore devices of this vendor/part ID,
       then do so */
    if (values.ignore_device_set && values.ignore_device) {
        BTL_VERBOSE(("device %s skipped; ignore_device=1",
                     ibv_get_device_name(device->ib_dev)));
        ret = OPAL_SUCCESS;
        ++num_devices_intentionally_ignored;
        goto error;
    }

    /* Note that even if we don't find default values, "values" will
       be set indicating that it does not have good values */
    ret = opal_btl_iwarp_ini_query(0, 0, &default_values);
    if (OPAL_SUCCESS != ret &&
        OPAL_ERR_NOT_FOUND != ret) {
        /* If we get a serious error, propagate it upwards */
        goto error;
    }

    /* If we did find values for this device (or in the defaults
       section), handle them */
    merge_values(&values, &default_values);
    /*  If MCA param was set, use it. If not, check the INI file
        or default to IBV_MTU_1024 */
    if (0 < mca_btl_iwarp_component.ib_mtu) {
        device->mtu = mca_btl_iwarp_component.ib_mtu;
    } else if (values.mtu_set) {
        switch (values.mtu) {
        case 256:
            device->mtu = IBV_MTU_256;
            break;
        case 512:
            device->mtu = IBV_MTU_512;
            break;
        case 1024:
            device->mtu = IBV_MTU_1024;
            break;
        case 2048:
            device->mtu = IBV_MTU_2048;
            break;
        case 4096:
            device->mtu = IBV_MTU_4096;
            break;
        default:
            BTL_ERROR(("invalid MTU value specified in INI file (%d); ignored", values.mtu));
            device->mtu = IBV_MTU_1024 ;
            break;
        }
    } else {
        device->mtu = IBV_MTU_1024 ;
    }

    /* Allocate the protection domain for the device */
    device->ib_pd = ibv_alloc_pd(device->ib_dev_context);
    if(NULL == device->ib_pd){
        BTL_ERROR(("error allocating protection domain for %s errno says %s",
                    ibv_get_device_name(device->ib_dev), strerror(errno)));
        goto error;
    }

    /* Figure out what the max_inline_data value should be for all
       ports and QPs on this device */
    need_search = false;
    if(-2 != mca_btl_iwarp_component.ib_max_inline_data) {
        /* User has explicitly set btl_iwarp_max_inline_data MCA parameter
           Per setup in _mca.c, we know that the MCA param value is guaranteed
           to be >= -1 */
        if (-1 == mca_btl_iwarp_component.ib_max_inline_data) {
            need_search = true;
        } else {
            device->max_inline_data = (uint32_t)
                mca_btl_iwarp_component.ib_max_inline_data;
        }
    } else if (values.max_inline_data_set) {
        if (-1 == values.max_inline_data) {
            need_search = true;
        } else if (values.max_inline_data >= 0) {
            device->max_inline_data = (uint32_t) values.max_inline_data;
        } else {
            if(default_values.max_inline_data_set &&
               default_values.max_inline_data >= -1) {
                BTL_ERROR(("Invalid max_inline_data value specified "
                           "in INI file (%d); using default value (%d)",
                            values.max_inline_data,
                            default_values.max_inline_data));
                device->max_inline_data = (uint32_t)
                    default_values.max_inline_data;
            } else {
                BTL_ERROR(("Invalid max_inline_data value specified "
                           "in INI file (%d)", values.max_inline_data));
                ret = OPAL_ERR_BAD_PARAM;
                goto error;
            }
        }
    }

    /* If we don't have a set max inline data size, search for it */
    if (need_search) {
        opal_common_verbs_find_max_inline(device->ib_dev,
                                          device->ib_dev_context,
                                          device->ib_pd,
                                          &device->max_inline_data);
    }

    /* Should we use RDMA for short / eager messages?  First check MCA
       param, then check INI file values. */
    if (mca_btl_iwarp_component.use_eager_rdma >= 0) {
        device->use_eager_rdma = mca_btl_iwarp_component.use_eager_rdma;
    } else if (values.use_eager_rdma_set) {
        device->use_eager_rdma = values.use_eager_rdma;
    }
    /* Eager RDMA is not currently supported with progress threads */
    if (device->use_eager_rdma && OPAL_ENABLE_PROGRESS_THREADS) {
        device->use_eager_rdma = 0;
        opal_show_help("help-mpi-btl-iwarp.txt",
                       "eager RDMA and progress threads", true);
    }

    asprintf (&rcache_resources.cache_name, "verbs.%" PRIu64, device->ib_dev_attr.node_guid);
    rcache_resources.reg_data = (void*)device;
    rcache_resources.sizeof_reg = sizeof(mca_btl_iwarp_reg_t);
    rcache_resources.register_mem = iwarp_reg_mr;
    rcache_resources.deregister_mem = iwarp_dereg_mr;
    device->rcache =
        mca_rcache_base_module_create (mca_btl_iwarp_component.ib_rcache_name,
                                       device, &rcache_resources);
    if (NULL == device->rcache) {
        /* Don't print an error message here -- we'll get one from
           mpool_create anyway */
         goto error;
    }

    device->mpool = mca_mpool_base_module_lookup (mca_btl_iwarp_component.ib_mpool_hints);
    if (NULL == device->mpool) {
        goto error;
    }

#if OPAL_ENABLE_PROGRESS_THREADS
    device->ib_channel = ibv_create_comp_channel(device->ib_dev_context);
    if (NULL == device->ib_channel) {
        BTL_ERROR(("error creating channel for %s errno says %s",
                    ibv_get_device_name(device->ib_dev),
                    strerror(errno)));
        goto error;
    }
#endif

    ret = OPAL_SUCCESS;

    /* Note ports are 1 based (i >= 1) */
    for(k = 0; k < port_cnt; k++){
        struct ibv_port_attr ib_port_attr;
        i = allowed_ports[k];
        if(ibv_query_port(device->ib_dev_context, i, &ib_port_attr)){
            BTL_ERROR(("error getting port attributes for device %s "
                        "port number %d errno says %s",
                        ibv_get_device_name(device->ib_dev), i, strerror(errno)));
            break;
        }
        if(IBV_PORT_ACTIVE == ib_port_attr.state) {
            if (ib_port_attr.active_mtu < device->mtu){
                device->mtu = ib_port_attr.active_mtu;
            }
            ret = init_one_port(btl_list, device, i, &ib_port_attr);
            if (OPAL_SUCCESS != ret) {
                /* Out of bounds error indicates that we hit max btl number
                 * don't propagate the error to the caller */
                if (OPAL_ERR_VALUE_OUT_OF_BOUNDS == ret) {
                    ret = OPAL_SUCCESS;
                }
                break;
            }
        }
    }
    free(allowed_ports);
    allowed_ports = NULL;

    /* If we made a BTL, check APM status and return.  Otherwise, fall
       through and destroy everything */
    if (device->btls > 0) {

        /* Check to ensure that all devices used in this process have
           compatible receive_queues values (we check elsewhere to see
           if all devices used in other processes in this job have
           compatible receive_queues values).

           Not only is the check complex, but the reasons behind what
           it does (and does not do) are complex.  Before explaining
           the code below, here's some notes:

           1. The iwarp BTL component only supports 1 value of the
              receive_queues between all of its modules.

              --> This could be changed to allow every module to have
                  its own receive_queues.  But that would be a big
                  deal; no one has time to code this up right now.

           2. The receive_queues value can be specified either as an
              MCA parameter or in the INI file.  Specifying the value
              as an MCA parameter overrides all INI file values
              (meaning: that MCA param value will be used for all
              iwarp BTL modules in the process).

           Effectively, the first device through init_one_device()
           gets to decide what the receive_queues will be for the all
           modules in this process.  This is an unfortunate artifact
           of the iwarp BTL startup sequence (see below for more
           details).  The first device will choose the receive_queues
           value from: (in priority order):

           1. If the btl_iwarp_receive_queues MCA param was
              specified, use that.
           2. If this device has a receive_queues value specified in
              the INI file, use that.
           3. Otherwise, use the default MCA param value for
              btl_iwarp_receive_queues.

           If any successive device has a different value specified in
           the INI file, we show_help and return up the stack that
           this device failed.

           In the case that the user does not specify a
           mca_btl_iwarp_receive_queues value, the short description
           of what is allowed is that either a) no devices specify a
           receive_queues value in the INI file (in which case we use
           the default MCA param value), b) all devices specify the
           same receive_queues value in the INI value, or c) some/all
           devices specify the same receive_queues value in the INI
           value as the default MCA param value.

           Let's take some sample cases to explain this more clearly...

           THESE ARE THE "GOOD" CASES
           --------------------------

           Case 1: no INI values
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: no receive_queues in INI file
           - device 2: no receive_queues in INI file
           --> use receive_queues value A with all devices

           Case 2: all INI values the same (same as default)
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: receive_queues value A in the INI file
           - device 1: receive_queues value A in the INI file
           - device 2: receive_queues value A in the INI file
           --> use receive_queues value A with all devices

           Case 3: all INI values the same (but different than default)
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: receive_queues value B in the INI file
           - device 1: receive_queues value B in the INI file
           - device 2: receive_queues value B in the INI file
           --> use receive_queues value B with all devices

           Case 4: some INI unspecified, but rest same as default
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: receive_queues value A in the INI file
           - device 1: no receive_queues in INI file
           - device 2: receive_queues value A in the INI file
           --> use receive_queues value A with all devices

           Case 5: some INI unspecified (including device 0), but rest same as default
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: no receive_queues in INI file
           - device 2: receive_queues value A in the INI file
           --> use receive_queues value A with all devices

           Case 6: different default/INI values, but MCA param is specified
           - MCA parameter: value D
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: receive_queues value B in INI file
           - device 2: receive_queues value C in INI file
           --> use receive_queues value D with all devices

           What this means is that this selection process is
           unfortunately tied to the order of devices.  :-( Device 0
           effectively sets what the receive_queues value will be for
           that process.  If any later device disagrees, that's
           problematic and we have to error/abort.

           ALL REMAINING CASES WILL FAIL
           -----------------------------

           Case 7: one INI value (different than default)
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: receive_queues value B in INI file
           - device 1: no receive_queues in INI file
           - device 2: no receive_queues in INI file
           --> Jeff thinks that it would be great to use
               receive_queues value B with all devices.  However, it
               shares one of the problems cited in case 8, below.  So
               we need to fail this scenario; print an error and
               abort.

           Case 8: one INI value, different than default
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: receive_queues value B in INI file
           - device 2: no receive_queues in INI file

           --> Jeff thinks that it would be great to use
               receive_queues value B with all devices.  However, it
               has (at least) 2 problems:

               1. The check for local receive_queue compatibility is
                  done here in init_one_device().  By the time we call
                  init_one_device() for device 1, we have already
                  called init_one_device() for device 0, meaning that
                  device 0's QPs have already been created and setup
                  using the MCA parameter's default receive_queues
                  value.  So if device 1 *changes* the
                  component.receive_queues value, then device 0 and
                  device 1 now have different receive_queue sets (more
                  specifically: the QPs setup for device 0 are now
                  effectively lost).  This is Bad.

                  It would be great if we didn't have this restriction
                  -- either by letting each module have its own
                  receive_queues value or by scanning all devices and
                  figuring out a final receive_queues value *before*
                  actually setting up any QPs.  But that's not the
                  current flow of the code (patches would be greatly
                  appreciated here, of course!).  Unfortunately, no
                  one has time to code this up right now, so we're
                  leaving this as explicitly documented for some
                  future implementer...

               2. Conside a scenario with server 1 having RNIC A/subnet
                  X, and server 2 having RNIC B/subnet X and RNIC
                  C/subnet Y.  And let's assume:

                  Server 1:
                  RNIC A: no receive_queues in INI file

                  Server 2:
                  RNIC B: no receive_queues in INI file
                  RNIC C: receive_queues specified in INI file

                  A will therefore use the default receive_queues
                  value.  B and C will use C's INI receive_queues.
                  But note that modex [currently] only sends around
                  vendor/part IDs for OpenFabrics devices -- not the
                  actual receive_queues value (it was felt that
                  including the final receive_queues string value in
                  the modex would dramatically increase the size of
                  the modex).  So processes on server 1 will get the
                  vendor/part ID for RNIC B, look it up in the INI
                  file, see that it has no receive_queues value
                  specified, and then assume that it uses the default
                  receive_queues value.  Hence, procs on server 1 will
                  try to connect RNIC A-->RNIC B with the wrong
                  receive_queues value.  Bad.  Further, the error
                  won't be discovered by checks like this because A
                  won't check D's receive_queues because D is on a
                  different subnet.

                  This could be fixed, of course; either by a) send
                  the final receive_queues value in the modex (perhaps
                  compressing or encoding it so that it can be much
                  shorter than the string -- the current vendor/part
                  ID stuff takes 8 bytes for each device), or b)
                  replicating the determination process of each host
                  in each process (i.e., procs on server 1 would see
                  both B and C, and use them both to figure out what
                  the "final" receive_queues value is for B).
                  Unfortunately, no one has time to code this up right
                  now, so we're leaving this as explicitly documented
                  for some future implementer...

               Because of both of these problems, this case is
               problematic and must fail with a show_help error.

           Case 9: two devices with same INI value (different than default)
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: receive_queues value B in INI file
           - device 2: receive_queues value B in INI file
           --> per case 8, fail with a show_help message.

           Case 10: two devices with different INI values
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: receive_queues value B in INI file
           - device 2: receive_queues value C in INI file
           --> per case 8, fail with a show_help message.

        */

        {
            /* we need to read this MCA param at this point in case someone
             * altered it via MPI_T */
            mca_base_var_source_t source;

            if (OPAL_SUCCESS != (ret = get_var_source ("receive_queues", &source))) {
                BTL_ERROR(("mca_base_var_get_value failed to get value for receive_queues: %s:%d",
                           __FILE__, __LINE__));
                goto error;
            }

            mca_btl_iwarp_component.receive_queues_source = source;
        }

        /* If the MCA param was specified, skip all the checks */
        if (MCA_BASE_VAR_SOURCE_DEFAULT != mca_btl_iwarp_component.receive_queues_source) {
            goto good;
        }

        /* If we're the first device and we have a receive_queues
           value from the INI file *that is different than the
           already-existing default value*, then set the component to
           use that. */
        if (0 == mca_btl_iwarp_component.devices_count) {
            if (NULL != values.receive_queues &&
                0 != strcmp(values.receive_queues,
                            mca_btl_iwarp_component.receive_queues)) {
                if (NULL != mca_btl_iwarp_component.receive_queues) {
                    free(mca_btl_iwarp_component.receive_queues);
                }
                mca_btl_iwarp_component.receive_queues =
                    strdup(values.receive_queues);
                mca_btl_iwarp_component.receive_queues_source =
                    BTL_IWARP_RQ_SOURCE_DEVICE_INI;
            }
        }

        /* If we're not the first device, then we have to conform to
           either the default value if the first device didn't set
           anything, or to whatever the first device decided. */
        else {
            /* In all cases, if this device has a receive_queues value
               in the INI, then it must agree with
               component.receive_queues. */
            if (NULL != values.receive_queues) {
                if (0 != strcmp(values.receive_queues,
                                mca_btl_iwarp_component.receive_queues)) {
                    opal_show_help("help-mpi-btl-iwarp.txt",
                                   "locally conflicting receive_queues", true,
                                   opal_install_dirs.opaldatadir,
                                   opal_process_info.nodename,
                                   ibv_get_device_name(receive_queues_device->ib_dev),
                                   receive_queues_device->ib_dev_attr.vendor_id,
                                   receive_queues_device->ib_dev_attr.vendor_part_id,
                                   mca_btl_iwarp_component.receive_queues,
                                   ibv_get_device_name(device->ib_dev),
                                   device->ib_dev_attr.vendor_id,
                                   device->ib_dev_attr.vendor_part_id,
                                   values.receive_queues);
                    ret = OPAL_ERR_RESOURCE_BUSY;
                    goto error;
                }
            }

            /* If this device doesn't have an INI receive_queues
               value, then if the component.receive_queues value came
               from the default, we're ok.  But if the
               component.receive_queues value came from the 1st
               device's INI file, we must error. */
            else if ((mca_base_var_source_t) BTL_IWARP_RQ_SOURCE_DEVICE_INI ==
                mca_btl_iwarp_component.receive_queues_source) {
                opal_show_help("help-mpi-btl-iwarp.txt",
                               "locally conflicting receive_queues", true,
                               opal_install_dirs.opaldatadir,
                               opal_process_info.nodename,
                               ibv_get_device_name(receive_queues_device->ib_dev),
                               receive_queues_device->ib_dev_attr.vendor_id,
                               receive_queues_device->ib_dev_attr.vendor_part_id,
                               mca_btl_iwarp_component.receive_queues,
                               ibv_get_device_name(device->ib_dev),
                               device->ib_dev_attr.vendor_id,
                               device->ib_dev_attr.vendor_part_id,
                               mca_btl_iwarp_component.default_recv_qps);
                ret = OPAL_ERR_RESOURCE_BUSY;
                goto error;
            }
        }

        receive_queues_device = device;

    good:
        mca_btl_iwarp_component.devices_count++;
        return OPAL_SUCCESS;
    }

error:
    if (OPAL_SUCCESS != ret) {
        opal_show_help("help-mpi-btl-iwarp.txt",
                       "error in device init", true,
                       opal_process_info.nodename,
                       ibv_get_device_name(device->ib_dev));
    }

    if (NULL != allowed_ports) {
        free(allowed_ports);
    }
    OBJ_RELEASE(device);
    return ret;
}

static int finish_btl_init(mca_btl_iwarp_module_t *iwarp_btl)
{
    int qp;
    iwarp_btl->num_peers = 0;

    /* Initialize module state */
    OBJ_CONSTRUCT(&iwarp_btl->ib_lock, opal_mutex_t);

    /* setup the qp structure */
    iwarp_btl->qps = (mca_btl_iwarp_module_qp_t*)
        calloc(mca_btl_iwarp_component.num_qps,
                sizeof(mca_btl_iwarp_module_qp_t));
    if (NULL == iwarp_btl->qps) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* setup all the qps */
    for (qp = 0; qp < mca_btl_iwarp_component.num_qps; qp++) {
        if (!BTL_IWARP_QP_TYPE_PP(qp)) {
            OBJ_CONSTRUCT(&iwarp_btl->qps[qp].u.srq_qp.pending_frags[0],
                    opal_list_t);
            OBJ_CONSTRUCT(&iwarp_btl->qps[qp].u.srq_qp.pending_frags[1],
                    opal_list_t);
            iwarp_btl->qps[qp].u.srq_qp.sd_credits =
                mca_btl_iwarp_component.qp_infos[qp].u.srq_qp.sd_max;
            iwarp_btl->qps[qp].u.srq_qp.srq = NULL;
        }
    }

    /* initialize the memory pool using the device */
    iwarp_btl->super.btl_mpool = iwarp_btl->device->mpool;

    iwarp_btl->eager_rdma_channels = 0;

    iwarp_btl->eager_rdma_frag_size = OPAL_ALIGN(
            sizeof(mca_btl_iwarp_header_t) +
            sizeof(mca_btl_iwarp_header_coalesced_t) +
            sizeof(mca_btl_iwarp_control_header_t) +
            sizeof(mca_btl_iwarp_footer_t) +
            iwarp_btl->super.btl_eager_limit,
            mca_btl_iwarp_component.buffer_alignment, size_t);

    opal_output_verbose(1, opal_btl_base_framework.framework_output,
                        "[rank=%d] iwarp: using port %s:%d",
                        OPAL_PROC_MY_NAME.vpid,
                        ibv_get_device_name(iwarp_btl->device->ib_dev),
                        iwarp_btl->port_num);
    return OPAL_SUCCESS;
}

struct dev_distance {
    struct ibv_device *ib_dev;
    float distance;
};

static int compare_distance(const void *p1, const void *p2)
{
    const struct dev_distance *d1 = (const struct dev_distance *) p1;
    const struct dev_distance *d2 = (const struct dev_distance *) p2;

    if (d1->distance > (d2->distance+EPS)) {
        return 1;
    } else if ((d1->distance + EPS) < d2->distance) {
        return -1;
    } else {
        return 0;
    }
}

static float get_ib_dev_distance(struct ibv_device *dev)
{
    /* If we don't have hwloc, we'll default to a distance of 0,
       because we have no way of measuring. */
    float distance = 0;
    float a, b;
    int i;
    hwloc_cpuset_t my_cpuset = NULL, ibv_cpuset = NULL;
    hwloc_obj_t my_obj, ibv_obj, node_obj;
    struct hwloc_distances_s *hwloc_distances = NULL;

    /* Override any distance logic so all devices are used */
    if (0 != mca_btl_iwarp_component.ignore_locality ||
        OPAL_SUCCESS != opal_hwloc_base_get_topology()) {
        return distance;
    }

#if HWLOC_API_VERSION >= 0x20000
    unsigned int j, distances_nr = 1;
    int ibvindex, myindex;
#endif

    if (NULL == hwloc_distances) {
        #if HWLOC_API_VERSION < 0x20000
            hwloc_distances =
                (struct hwloc_distances_s*)hwloc_get_whole_distance_matrix_by_type(opal_hwloc_topology,
                                                                                   HWLOC_OBJ_NODE);
            /* If we got no info, just return 0 */
            if (NULL == hwloc_distances || NULL == hwloc_distances->latency) {
                goto out;
            }

        #else
            if (0 != hwloc_distances_get_by_type(opal_hwloc_topology, HWLOC_OBJ_NODE,
                                                 &distances_nr, &hwloc_distances,
                                                 HWLOC_DISTANCES_KIND_MEANS_LATENCY, 0) || 0 == distances_nr) {
                hwloc_distances = NULL;
                goto out;
            }
        #endif
    }

    /* Next, find the NUMA node where this IBV device is located */
    ibv_cpuset = hwloc_bitmap_alloc();
    if (NULL == ibv_cpuset) {
        goto out;
    }
    if (0 != hwloc_ibv_get_device_cpuset(opal_hwloc_topology, dev, ibv_cpuset)) {
        goto out;
    }
    ibv_obj = hwloc_get_obj_covering_cpuset(opal_hwloc_topology, ibv_cpuset);
    if (NULL == ibv_obj) {
        goto out;
    }

    opal_output_verbose(5, opal_btl_base_framework.framework_output,
                        "hwloc_distances->nbobjs=%d", hwloc_distances->nbobjs);
#if HWLOC_API_VERSION < 0x20000
    for (i = 0; i < (int)(2 *  hwloc_distances->nbobjs); i++) {
        opal_output_verbose(5, opal_btl_base_framework.framework_output,
                            "hwloc_distances->latency[%d]=%f", i, hwloc_distances->latency[i]);
    }
#else
    for (i = 0; i < (int)hwloc_distances->nbobjs; i++) {
        opal_output_verbose(5, opal_btl_base_framework.framework_output,
                            "hwloc_distances->values[%d]=%"PRIu64, i, hwloc_distances->values[i]);
    }
#endif

    /* If ibv_obj is a NUMA node or below, we're good. */
    switch (ibv_obj->type) {
    case HWLOC_OBJ_NODE:
    case HWLOC_OBJ_SOCKET:
#if HWLOC_API_VERSION < 0x20000
    case HWLOC_OBJ_CACHE:
#else
    case HWLOC_OBJ_L1CACHE:
    case HWLOC_OBJ_L2CACHE:
    case HWLOC_OBJ_L3CACHE:
    case HWLOC_OBJ_L4CACHE:
    case HWLOC_OBJ_L5CACHE:
#endif
    case HWLOC_OBJ_CORE:
    case HWLOC_OBJ_PU:
        while (NULL != ibv_obj && ibv_obj->type != HWLOC_OBJ_NODE) {
            ibv_obj = ibv_obj->parent;
        }
        break;

    default:
        /* If it's above a NUMA node, then I don't know how to compute
           the distance... */
        opal_output_verbose(5, opal_btl_base_framework.framework_output, "ibv_obj->type set to NULL");
        ibv_obj = NULL;
        break;
    }

    /* If we don't have an object for this ibv device, give up */
    if (NULL == ibv_obj) {
        goto out;
    }
    #if HWLOC_API_VERSION >= 0x20000
        /* the new matrix format isn't quite as friendly, so we have to
         * do an exhaustive search to find the index of this object
         * in that array */
        ibvindex = -1;
        for (j=0; j < distances_nr; j++) {
            if (ibv_obj == hwloc_distances->objs[j]) {
                ibvindex = j;
                break;
            }
        }
        if (-1 == ibvindex) {
            OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
            goto out;
        }
    #endif

    opal_output_verbose(5, opal_btl_base_framework.framework_output,
                        "ibv_obj->logical_index=%d", ibv_obj->logical_index);
    /* This function is only called if the process is bound, so let's
       find out where we are bound to.  For the moment, we only care
       about the NUMA node to which we are bound. */
    my_cpuset = hwloc_bitmap_alloc();
    if (NULL == my_cpuset) {
        goto out;
    }
    if (0 != hwloc_get_cpubind(opal_hwloc_topology, my_cpuset, 0)) {
        goto out;
    }
    my_obj = hwloc_get_obj_covering_cpuset(opal_hwloc_topology, my_cpuset);
    if (NULL == my_obj) {
        goto out;
    }

    /* If my_obj is a NUMA node or below, we're good. */
    switch (my_obj->type) {
    case HWLOC_OBJ_NODE:
    case HWLOC_OBJ_SOCKET:
    #if HWLOC_API_VERSION < 0x20000
        case HWLOC_OBJ_CACHE:
    #else
        case HWLOC_OBJ_L1CACHE:
        case HWLOC_OBJ_L2CACHE:
        case HWLOC_OBJ_L3CACHE:
        case HWLOC_OBJ_L4CACHE:
        case HWLOC_OBJ_L5CACHE:
    #endif
    case HWLOC_OBJ_CORE:
    case HWLOC_OBJ_PU:
        while (NULL != my_obj && my_obj->type != HWLOC_OBJ_NODE) {
            my_obj = my_obj->parent;
        }
        if (NULL != my_obj) {
            opal_output_verbose(5, opal_btl_base_framework.framework_output,
                                "my_obj->logical_index=%d", my_obj->logical_index);
            /* Distance may be asymetrical, so calculate both of them
               and take the max */
            #if HWLOC_API_VERSION < 0x20000
                a = hwloc_distances->latency[my_obj->logical_index +
                                             (ibv_obj->logical_index *
                                              hwloc_distances->nbobjs)];
                b = hwloc_distances->latency[ibv_obj->logical_index +
                                             (my_obj->logical_index *
                                              hwloc_distances->nbobjs)];
            #else
                /* the new matrix format isn't quite as friendly, so we have to
                 * do an exhaustive search to find the index of this object
                 * in that array */
                myindex = -1;
                for (j=0; j < distances_nr; j++) {
                    if (my_obj == hwloc_distances->objs[j]) {
                        myindex = j;
                        break;
                    }
                }
                if (-1 == myindex) {
                    OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
                    goto out;
                }
                a = (float)hwloc_distances->values[myindex + (ibvindex * hwloc_distances->nbobjs)];
                b = (float)hwloc_distances->values[ibvindex + (myindex * hwloc_distances->nbobjs)];
            #endif
            distance = (a > b) ? a : b;
        }
        break;

    default:
        /* If the obj is above a NUMA node, then we're bound to more than
           one NUMA node.  Find the max distance. */
        i = 0;
        for (node_obj = hwloc_get_obj_inside_cpuset_by_type(opal_hwloc_topology,
                                                            ibv_obj->cpuset,
                                                            HWLOC_OBJ_NODE, i);
             NULL != node_obj;
             node_obj = hwloc_get_obj_inside_cpuset_by_type(opal_hwloc_topology,
                                                            ibv_obj->cpuset,
                                                            HWLOC_OBJ_NODE, ++i)) {
            #if HWLOC_API_VERSION < 0x20000
                a = hwloc_distances->latency[node_obj->logical_index +
                                             (ibv_obj->logical_index *
                                              hwloc_distances->nbobjs)];
                b = hwloc_distances->latency[ibv_obj->logical_index +
                                             (node_obj->logical_index *
                                              hwloc_distances->nbobjs)];
            #else
                unsigned int j;
                j = node_obj->logical_index + (ibv_obj->logical_index * hwloc_distances->nbobjs);
                if (j < distances_nr) {
                    a = (float)hwloc_distances->values[j];
                } else {
                    goto out;
                }
                j = ibv_obj->logical_index + (node_obj->logical_index * hwloc_distances->nbobjs);
                if (j < distances_nr) {
                    b = (float)hwloc_distances->values[j];
                } else {
                    goto out;
                }
            #endif
            a = (a > b) ? a : b;
            distance = (a > distance) ? a : distance;
        }
        break;
    }

 out:
    if (NULL != ibv_cpuset) {
        hwloc_bitmap_free(ibv_cpuset);
    }
    if (NULL != my_cpuset) {
        hwloc_bitmap_free(my_cpuset);
    }

#if HWLOC_API_VERSION >= 0x20000
    if (NULL != hwloc_distances) {
        hwloc_distances_release(opal_hwloc_topology, hwloc_distances);
    }
#endif
    return distance;
}

static struct dev_distance *
sort_devs_by_distance(struct ibv_device **ib_devs, int count)
{
    int i;
    struct dev_distance *devs = (struct dev_distance *) malloc(count * sizeof(struct dev_distance));
    if (NULL == devs) {
        return NULL;
    }

    for (i = 0; i < count; i++) {
        devs[i].ib_dev = ib_devs[i];
        opal_output_verbose(5, opal_btl_base_framework.framework_output,
                            "Checking distance from this process to device=%s", ibv_get_device_name(ib_devs[i]));
        /* If we're not bound, just assume that the device is close. */
        devs[i].distance = 0;
        if (opal_process_info.cpuset) {
            /* If this process is bound to one or more PUs, we can get
               an accurate distance. */
            devs[i].distance = get_ib_dev_distance(ib_devs[i]);
        }
        opal_output_verbose(5, opal_btl_base_framework.framework_output,
                            "Process is %s: distance to device is %f",
                            (opal_process_info.cpuset ? "bound" : "not bound"), devs[i].distance);
    }

    qsort(devs, count, sizeof(struct dev_distance), compare_distance);

    return devs;
}


/*
 *  IB component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */

static mca_btl_base_module_t**
btl_iwarp_component_init(int *num_btl_modules,
                          bool enable_progress_threads,
                          bool enable_mpi_threads)
{
    struct ibv_device **ib_devs;
    mca_btl_base_module_t** btls = NULL;
    int i, ret, num_devs, length;
    opal_list_t btl_list;
    mca_btl_iwarp_module_t * iwarp_btl;
    mca_btl_base_selected_module_t* ib_selected;
    opal_list_item_t* item;
    mca_btl_iwarp_frag_init_data_t *init_data;
    struct dev_distance *dev_sorted;
    float distance;
    int index;
    bool found;
    mca_base_var_source_t source;
    int list_count = 0;

    /* initialization */
    *num_btl_modules = 0;
    num_devs = 0;

    /* If we got this far, then setup the memory alloc hook (because
       we're most likely going to be using this component). The hook
       is to be set up as early as possible in this function since we
       want most of the allocated resources be aligned.
     */
    opal_memory->memoryc_set_alignment(32, mca_btl_iwarp_module.super.btl_eager_limit);

    /* Per https://svn.open-mpi.org/trac/ompi/ticket/1305, check to
       see if $sysfsdir/class/infiniband exists.  If it does not,
       assume that the RDMA hardware drivers are not loaded, and
       therefore we don't want OpenFabrics verbs support in this OMPI
       job.  No need to print a warning. */
    if (!opal_common_verbs_check_basics()) {
        goto no_btls;
    }

    /* Read in INI files with device-specific parameters */
    if (OPAL_SUCCESS != (ret = opal_btl_iwarp_ini_init())) {
        goto no_btls;
    }

    index = mca_base_var_find("ompi", "btl", "iwarp", "max_inline_data");
    if (index >= 0) {
        if (OPAL_SUCCESS == mca_base_var_get_value(index, NULL, &source, NULL)) {
            if (-1 == mca_btl_iwarp_component.ib_max_inline_data  &&
                MCA_BASE_VAR_SOURCE_DEFAULT == source) {
                /* If the user has not explicitly set this MCA parameter
                   use max_inline_data value specified in the
                   device-specific parameters INI file */
                mca_btl_iwarp_component.ib_max_inline_data = -2;
            }
        }
    }

    OBJ_CONSTRUCT(&mca_btl_iwarp_component.send_free_coalesced, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_iwarp_component.send_user_free, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_iwarp_component.recv_user_free, opal_free_list_t);

    init_data = (mca_btl_iwarp_frag_init_data_t *) malloc(sizeof(mca_btl_iwarp_frag_init_data_t));
    if (NULL == init_data) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }

    init_data->order = mca_btl_iwarp_component.rdma_qp;
    init_data->list = &mca_btl_iwarp_component.send_user_free;

    /* Align fragments on 8-byte boundaries (instead of 2) to fix bus errors that
       occur on some 32-bit platforms. Depending on the size of the fragment this
       will waste 2-6 bytes of space per frag. In most cases this shouldn't waste
       any space. */
    if (OPAL_SUCCESS != opal_free_list_init (
                &mca_btl_iwarp_component.send_user_free,
                sizeof(mca_btl_iwarp_put_frag_t), 8,
                OBJ_CLASS(mca_btl_iwarp_put_frag_t),
                0, 0,
                mca_btl_iwarp_component.ib_free_list_num,
                mca_btl_iwarp_component.ib_free_list_max,
                mca_btl_iwarp_component.ib_free_list_inc,
                NULL, 0, NULL, mca_btl_iwarp_frag_init, init_data)) {
        goto no_btls;
    }

    init_data = (mca_btl_iwarp_frag_init_data_t *) malloc(sizeof(mca_btl_iwarp_frag_init_data_t));
    if (NULL == init_data) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }

    init_data->order = mca_btl_iwarp_component.rdma_qp;
    init_data->list = &mca_btl_iwarp_component.recv_user_free;

    if(OPAL_SUCCESS != opal_free_list_init (
                &mca_btl_iwarp_component.recv_user_free,
                sizeof(mca_btl_iwarp_get_frag_t), 8,
                OBJ_CLASS(mca_btl_iwarp_get_frag_t),
                0, 0,
                mca_btl_iwarp_component.ib_free_list_num,
                mca_btl_iwarp_component.ib_free_list_max,
                mca_btl_iwarp_component.ib_free_list_inc,
                NULL, 0, NULL, mca_btl_iwarp_frag_init, init_data)) {
        goto no_btls;
    }

    init_data = (mca_btl_iwarp_frag_init_data_t *) malloc(sizeof(mca_btl_iwarp_frag_init_data_t));
    if (NULL == init_data) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }
    length = sizeof(mca_btl_iwarp_coalesced_frag_t);

    init_data->list = &mca_btl_iwarp_component.send_free_coalesced;

    if(OPAL_SUCCESS != opal_free_list_init (
                &mca_btl_iwarp_component.send_free_coalesced,
                length, 8, OBJ_CLASS(mca_btl_iwarp_coalesced_frag_t),
                0, 0, mca_btl_iwarp_component.ib_free_list_num,
                mca_btl_iwarp_component.ib_free_list_max,
                mca_btl_iwarp_component.ib_free_list_inc,
                NULL, 0, NULL, mca_btl_iwarp_frag_init, init_data)) {
        goto no_btls;
    }

    /* If fork support is requested, try to enable it */
    if (OPAL_SUCCESS != (ret = opal_common_verbs_fork_test())) {
        goto no_btls;
    }

    /* Parse the include and exclude lists, checking for errors */
    mca_btl_iwarp_component.if_include_list =
        mca_btl_iwarp_component.if_exclude_list =
        mca_btl_iwarp_component.if_list = NULL;

    if (NULL != mca_btl_iwarp_component.if_include)
      list_count++;
    if (NULL != mca_btl_iwarp_component.if_exclude)
      list_count++;
    if (NULL != mca_btl_iwarp_component.ipaddr_include)
      list_count++;
    if (NULL != mca_btl_iwarp_component.ipaddr_exclude)
      list_count++;

    if (list_count > 1) {
        opal_show_help("help-mpi-btl-iwarp.txt",
                       "specified include and exclude", true,
                       NULL == mca_btl_iwarp_component.if_include ?
                        "<not specified>" : mca_btl_iwarp_component.if_include,
                       NULL == mca_btl_iwarp_component.if_exclude ?
                        "<not specified>" : mca_btl_iwarp_component.if_exclude,
                       NULL == mca_btl_iwarp_component.ipaddr_include ?
                        "<not specified>" :mca_btl_iwarp_component.ipaddr_include,
                       NULL == mca_btl_iwarp_component.ipaddr_exclude ?
                         "<not specified>" :mca_btl_iwarp_component.ipaddr_exclude,
                       NULL);
        goto no_btls;
    } else if (NULL != mca_btl_iwarp_component.if_include) {
        mca_btl_iwarp_component.if_include_list =
            opal_argv_split(mca_btl_iwarp_component.if_include, ',');
        mca_btl_iwarp_component.if_list =
            opal_argv_copy(mca_btl_iwarp_component.if_include_list);
    } else if (NULL != mca_btl_iwarp_component.if_exclude) {
        mca_btl_iwarp_component.if_exclude_list =
            opal_argv_split(mca_btl_iwarp_component.if_exclude, ',');
        mca_btl_iwarp_component.if_list =
            opal_argv_copy(mca_btl_iwarp_component.if_exclude_list);
    }

    ib_devs = opal_ibv_get_device_list(&num_devs);

    if(0 == num_devs || NULL == ib_devs) {
        mca_btl_base_error_no_nics("OpenFabrics (iwarp)", "device");
        goto no_btls;
    }

    dev_sorted = sort_devs_by_distance(ib_devs, num_devs);
    if (NULL == dev_sorted) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }

    OBJ_CONSTRUCT(&btl_list, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_iwarp_component.ib_lock, opal_mutex_t);

    distance = dev_sorted[0].distance;
    for (found = false, i = 0;
         i < num_devs && (-1 == mca_btl_iwarp_component.ib_max_btls ||
                mca_btl_iwarp_component.ib_num_btls <
                mca_btl_iwarp_component.ib_max_btls); i++) {
        if (0 != mca_btl_iwarp_component.ib_num_btls &&
            (dev_sorted[i].distance - distance) > EPS) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                                "[rank=%d] iwarp: skipping device %s; it is too far away",
                                OPAL_PROC_MY_NAME.vpid,
                                ibv_get_device_name(dev_sorted[i].ib_dev));
            break;
        }

        /* Only take devices that match the type specified by
           btl_iwarp_device_type */
        switch (mca_btl_iwarp_component.device_type) {
        case BTL_IWARP_DT_IWARP:
            if (IBV_TRANSPORT_IB == dev_sorted[i].ib_dev->transport_type) {
                BTL_VERBOSE(("iwarp: only taking iwarp devices -- skipping %s",
                             ibv_get_device_name(dev_sorted[i].ib_dev)));
                continue;
            }
            break;
        }

        found = true;
        ret = init_one_device(&btl_list, dev_sorted[i].ib_dev);
        if (OPAL_ERR_NOT_SUPPORTED == ret) {
            ++num_devices_intentionally_ignored;
            continue;
        } else if (OPAL_SUCCESS != ret) {
            free(dev_sorted);
            goto no_btls;
        }
    }
    free(dev_sorted);
    if (!found) {
        opal_show_help("help-mpi-btl-iwarp.txt", "no devices right type",
                       true, opal_process_info.nodename,
                       ((BTL_IWARP_DT_IB == mca_btl_iwarp_component.device_type) ?
                        "InfiniBand" :
                        (BTL_IWARP_DT_IWARP == mca_btl_iwarp_component.device_type) ?
                        "iWARP" : "<any>"));
        goto no_btls;
    }

    /* If we got back from checking all the devices and find that
       there are still items in the component.if_list, that means that
       they didn't exist.  Show an appropriate warning if the warning
       was not disabled. */

    if (0 != opal_argv_count(mca_btl_iwarp_component.if_list) &&
        mca_btl_iwarp_component.warn_nonexistent_if) {
        char *str = opal_argv_join(mca_btl_iwarp_component.if_list, ',');
        opal_show_help("help-mpi-btl-iwarp.txt", "nonexistent port",
                       true, opal_process_info.nodename,
                       ((NULL != mca_btl_iwarp_component.if_include) ?
                        "in" : "ex"), str);
        free(str);
    }

    if(0 == mca_btl_iwarp_component.ib_num_btls) {
        /* If there were unusable devices that weren't specifically
           ignored, warn about it */
        if (num_devices_intentionally_ignored < num_devs) {
            opal_show_help("help-mpi-btl-iwarp.txt",
                           "no active ports found", true,
                           opal_process_info.nodename);
        }
        goto no_btls;
    }

    /* Now that we know we have devices and ports that we want to use,
       init CPC components */
    if (OPAL_SUCCESS != (ret = opal_btl_iwarp_connect_base_init())) {
        goto no_btls;
    }

    /* Setup the BSRQ QP's based on the final value of
       mca_btl_iwarp_component.receive_queues. */
    if (OPAL_SUCCESS != setup_qps()) {
        goto no_btls;
    }
    if (mca_btl_iwarp_component.num_srq_qps > 0) {
        opal_hash_table_t *srq_addr_table = &mca_btl_iwarp_component.srq_manager.srq_addr_table;
        if(OPAL_SUCCESS != opal_hash_table_init(
                srq_addr_table, (mca_btl_iwarp_component.num_srq_qps) *
                                 mca_btl_iwarp_component.ib_num_btls)) {
            BTL_ERROR(("SRQ internal error. Failed to allocate SRQ addr hash table"));
            goto no_btls;
        }
    }

    /* Allocate space for btl modules */
    mca_btl_iwarp_component.iwarp_btls =
        (mca_btl_iwarp_module_t **) malloc(sizeof(mca_btl_iwarp_module_t*) *
                mca_btl_iwarp_component.ib_num_btls);
    if(NULL == mca_btl_iwarp_component.iwarp_btls) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }
    btls = (struct mca_btl_base_module_t **)
        malloc(mca_btl_iwarp_component.ib_num_btls *
               sizeof(struct mca_btl_base_module_t*));
    if(NULL == btls) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }

    /* Copy the btl module structs into a contiguous array and fully
       initialize them */
    i = 0;
    while (NULL != (item = opal_list_remove_first(&btl_list))) {
        ib_selected = (mca_btl_base_selected_module_t*)item;
        iwarp_btl = (mca_btl_iwarp_module_t*)ib_selected->btl_module;

        /* Search for a CPC that can handle this port */
        ret = opal_btl_iwarp_connect_base_select_for_local_port(iwarp_btl);
        /* If we get NOT_SUPPORTED, then no CPC was found for this
           port.  But that's not a fatal error -- just keep going;
           let's see if we find any usable iwarp modules or not. */
        if (OPAL_ERR_NOT_SUPPORTED == ret) {
            continue;
        } else if (OPAL_SUCCESS != ret) {
            /* All others *are* fatal.  Note that we already did a
               show_help in the lower layer */
            goto no_btls;
        }

        if (mca_btl_iwarp_component.max_hw_msg_size > 0 &&
            (uint32_t)mca_btl_iwarp_component.max_hw_msg_size > iwarp_btl->ib_port_attr.max_msg_sz) {
            BTL_ERROR(("max_hw_msg_size (%" PRIu32 ") is larger than hw max message size (%" PRIu32 ")",
                mca_btl_iwarp_component.max_hw_msg_size, iwarp_btl->ib_port_attr.max_msg_sz));
        }

        mca_btl_iwarp_component.iwarp_btls[i] = iwarp_btl;
        OBJ_RELEASE(ib_selected);
        btls[i] = &iwarp_btl->super;
        if (finish_btl_init(iwarp_btl) != OPAL_SUCCESS) {
            goto no_btls;
        }
        ++i;
    }
    /* If we got nothing, then error out */
    if (0 == i) {
        goto no_btls;
    }
    /* Otherwise reset to the number of iwarp modules that we
       actually got */
    mca_btl_iwarp_component.ib_num_btls = i;

    btl_iwarp_modex_send();

    *num_btl_modules = mca_btl_iwarp_component.ib_num_btls;
    opal_ibv_free_device_list(ib_devs);
    if (NULL != mca_btl_iwarp_component.if_include_list) {
        opal_argv_free(mca_btl_iwarp_component.if_include_list);
        mca_btl_iwarp_component.if_include_list = NULL;
    }
    if (NULL != mca_btl_iwarp_component.if_exclude_list) {
        opal_argv_free(mca_btl_iwarp_component.if_exclude_list);
        mca_btl_iwarp_component.if_exclude_list = NULL;
    }

    mca_btl_iwarp_component.memory_registration_verbose = opal_output_open(NULL);
    opal_output_set_verbosity (mca_btl_iwarp_component.memory_registration_verbose,
                               mca_btl_iwarp_component.memory_registration_verbose_level);

    /* setup the fork warning message as we are sensitive
     * to memory corruption issues when fork is called
     */
    opal_warn_fork();
    return btls;

 no_btls:
    /* If we fail early enough in the setup, we just modex around that
       there are no iwarp BTL's in this process and return NULL. */

    mca_btl_iwarp_component.ib_num_btls = 0;
    btl_iwarp_modex_send();
    if (NULL != btls) {
        free(btls);
    }
    return NULL;
}

/*
 * Progress the no_credits_pending_frags lists on all qp's
 */
static int progress_no_credits_pending_frags(mca_btl_base_endpoint_t *ep)
{
    int qp, pri, rc, len;
    opal_list_item_t *frag;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);

    /* Traverse all QPs and all priorities */
    for (qp = 0; qp < mca_btl_iwarp_component.num_qps; ++qp) {
        for (pri = 0; pri < 2; ++pri) {
            /* Note that entries in the no_credits_pending_frags list
               may be eager RDMA or send fragments.  So be sure to
               check that we have at least 1 RDMA or send credit.

               This loop needs a little explaining.  :-\

               In the body of the loop, we call _endpoint_post_send().
               The frag will either be successfully sent, or it will
               be [re]added to the no_credit_pending_frags list.  So
               if we keep trying to drain the no_credits_pending_frag
               list, we could end up in an infinite loop.  So instead,
               we get the initial length of the list and ensure to run
               through every entry at least once.  This attempts to
               send *every* frag once and catches the case where a
               frag may be on the RDMA list, but because of
               coalescing, is now too big for RDMA and defaults over
               to sending -- but then we're out of send credits, so it
               doesn't go.  But if we *do* still have some RDMA
               credits and there are RDMA frags on the list behind
               this now-too-big frag, they'll get a chance to go.

               Specifically, the condition in this for loop is as follows:

               - len > 0: ensure to go through all entries in the list once
               - the 2nd part of the conditional checks to see if we
                 have any credits at all.  Specifically, do we have
                 any RDMA credits or any send credits, *or* are we on
                 an SRQ, in which case we define that we *always* have
                 credits (because the hardware will continually
                 retransmit for us).
            */
            for (len = opal_list_get_size(&ep->qps[qp].no_credits_pending_frags[pri]);
                 len > 0 &&
                     (ep->eager_rdma_remote.tokens > 0 ||
                      ep->qps[qp].u.pp_qp.sd_credits > 0 ||
                      !BTL_IWARP_QP_TYPE_PP(qp)); --len) {
                frag = opal_list_remove_first(&ep->qps[qp].no_credits_pending_frags[pri]);
                assert (NULL != frag);

                /* If _endpoint_post_send() fails because of
                   RESOURCE_BUSY, then the frag was re-added to the
                   no_credits_pending list.  Specifically: either the
                   frag was initially an RDMA frag, but there were no
                   RDMA credits so it fell through the trying to send,
                   but we had no send credits and therefore re-added
                   the frag to the no_credits list, or the frag was a
                   send frag initially (and the same sequence
                   occurred, starting at the send frag out-of-credits
                   scenario).  In this case, just continue and try the
                   rest of the frags in the list.

                   If it fails because of another error, return the
                   error upward. */
                rc = mca_btl_iwarp_endpoint_post_send(ep, to_send_frag(frag));
                if (OPAL_UNLIKELY(OPAL_SUCCESS != rc &&
                                  OPAL_ERR_RESOURCE_BUSY != rc)) {
                    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
                    return rc;
                }
            }
        }
    }

    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
    return OPAL_SUCCESS;
}

void mca_btl_iwarp_frag_progress_pending_put_get(mca_btl_base_endpoint_t *ep,
        const int qp)
{
    mca_btl_iwarp_module_t* iwarp_btl = ep->endpoint_btl;
    opal_list_item_t *frag;
    size_t i, len = opal_list_get_size(&ep->pending_get_frags);
    int rc;

    for(i = 0; i < len && ep->qps[qp].qp->sd_wqe > 0 && ep->get_tokens > 0; i++) {
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        frag = opal_list_remove_first(&(ep->pending_get_frags));
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if (NULL == frag)
            break;
        rc = mca_btl_iwarp_get_internal ((mca_btl_base_module_t *)iwarp_btl, ep,
                                          to_get_frag(frag));
        if (OPAL_ERR_OUT_OF_RESOURCE == rc) {
            OPAL_THREAD_LOCK(&ep->endpoint_lock);
            opal_list_prepend (&ep->pending_get_frags, frag);
            OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
            break;
        }
    }

    len = opal_list_get_size(&ep->pending_put_frags);
    for(i = 0; i < len && ep->qps[qp].qp->sd_wqe > 0; i++) {
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        frag = opal_list_remove_first(&(ep->pending_put_frags));
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if (NULL == frag)
            break;
        rc = mca_btl_iwarp_put_internal ((mca_btl_base_module_t*)iwarp_btl, ep,
                                          to_put_frag(frag));
        if (OPAL_ERR_OUT_OF_RESOURCE == rc) {
            OPAL_THREAD_LOCK(&ep->endpoint_lock);
            opal_list_prepend (&ep->pending_put_frags, frag);
            OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
            break;
        }
    }
}

static int btl_iwarp_handle_incoming(mca_btl_iwarp_module_t *iwarp_btl,
                                         mca_btl_iwarp_endpoint_t *ep,
                                         mca_btl_iwarp_recv_frag_t *frag,
                                         size_t byte_len)
{
    mca_btl_base_descriptor_t *des = &to_base_frag(frag)->base;
    mca_btl_iwarp_header_t *hdr = frag->hdr;
    int rqp = to_base_frag(frag)->base.order, cqp;
    uint16_t rcredits = 0, credits;
    bool is_credit_msg;

    if(ep->nbo) {
        BTL_IWARP_HEADER_NTOH(*hdr);
    }

    /* advance the segment address past the header and subtract from the
     * length.*/
    des->des_segments->seg_len = byte_len - sizeof(mca_btl_iwarp_header_t);

    if(OPAL_LIKELY(!(is_credit_msg = is_credit_message(frag)))) {
        /* call registered callback */
        mca_btl_active_message_callback_t* reg;

        reg = mca_btl_base_active_message_trigger + hdr->tag;
        reg->cbfunc( &iwarp_btl->super, hdr->tag, des, reg->cbdata );

        if(MCA_BTL_IWARP_RDMA_FRAG(frag)) {
            cqp = (hdr->credits >> 11) & 0x0f;
            hdr->credits &= 0x87ff;
        } else {
            cqp = rqp;
        }
        if(BTL_IWARP_IS_RDMA_CREDITS(hdr->credits)) {
            rcredits = BTL_IWARP_CREDITS(hdr->credits);
            hdr->credits = 0;
        }
    } else {
        mca_btl_iwarp_rdma_credits_header_t *chdr =
            (mca_btl_iwarp_rdma_credits_header_t *) des->des_segments->seg_addr.pval;
        if(ep->nbo) {
            BTL_IWARP_RDMA_CREDITS_HEADER_NTOH(*chdr);
        }
        cqp = chdr->qpn;
        rcredits = chdr->rdma_credits;
    }

    credits = hdr->credits;

    if(hdr->cm_seen)
         OPAL_THREAD_ADD_FETCH32(&ep->qps[cqp].u.pp_qp.cm_sent, -hdr->cm_seen);

    /* Now return fragment. Don't touch hdr after this point! */
    if(MCA_BTL_IWARP_RDMA_FRAG(frag)) {
        mca_btl_iwarp_eager_rdma_local_t *erl = &ep->eager_rdma_local;
        OPAL_THREAD_LOCK(&erl->lock);
        MCA_BTL_IWARP_RDMA_MAKE_REMOTE(frag->ftr);
        while(erl->tail != erl->head) {
            mca_btl_iwarp_recv_frag_t *tf;
            tf = MCA_BTL_IWARP_GET_LOCAL_RDMA_FRAG(ep, erl->tail);
            if(MCA_BTL_IWARP_RDMA_FRAG_LOCAL(tf))
                break;
            OPAL_THREAD_ADD_FETCH32(&erl->credits, 1);
            MCA_BTL_IWARP_RDMA_NEXT_INDEX(erl->tail);
        }
        OPAL_THREAD_UNLOCK(&erl->lock);
    } else {
        if (is_cts_message(frag)) {
            /* If this was a CTS, free it here (it was
               malloc'ed+ibv_reg_mr'ed -- so it should *not* be
               FRAG_RETURN'ed). */
            int rc = opal_btl_iwarp_connect_base_free_cts(ep);
            if (OPAL_SUCCESS != rc) {
                return rc;
            }
        } else {
            /* Otherwise, FRAG_RETURN it and repost if necessary */
            MCA_BTL_IB_FRAG_RETURN(frag);
            if (BTL_IWARP_QP_TYPE_PP(rqp)) {
                if (OPAL_UNLIKELY(is_credit_msg)) {
                    OPAL_THREAD_ADD_FETCH32(&ep->qps[cqp].u.pp_qp.cm_received, 1);
                } else {
                    OPAL_THREAD_ADD_FETCH32(&ep->qps[rqp].u.pp_qp.rd_posted, -1);
                }
                mca_btl_iwarp_endpoint_post_rr(ep, cqp);
            } else {
                mca_btl_iwarp_module_t *btl = ep->endpoint_btl;
                OPAL_THREAD_ADD_FETCH32(&btl->qps[rqp].u.srq_qp.rd_posted, -1);
                mca_btl_iwarp_post_srr(btl, rqp);
            }
        }
    }

    assert((cqp != MCA_BTL_NO_ORDER && BTL_IWARP_QP_TYPE_PP(cqp)) || !credits);

    /* If we got any credits (RDMA or send), then try to progress all
       the no_credits_pending_frags lists */
    if (rcredits > 0) {
        OPAL_THREAD_ADD_FETCH32(&ep->eager_rdma_remote.tokens, rcredits);
    }
    if (credits > 0) {
        OPAL_THREAD_ADD_FETCH32(&ep->qps[cqp].u.pp_qp.sd_credits, credits);
    }
    if (rcredits + credits > 0) {
        int rc;

        if (OPAL_SUCCESS !=
            (rc = progress_no_credits_pending_frags(ep))) {
            return rc;
        }
    }

    send_credits(ep, cqp);

    return OPAL_SUCCESS;
}


static char* btl_iwarp_component_status_to_string(enum ibv_wc_status status)
{
    switch(status) {
    case IBV_WC_SUCCESS:
        return "SUCCESS";
        break;
    case IBV_WC_LOC_LEN_ERR:
        return "LOCAL LENGTH ERROR";
        break;
    case IBV_WC_LOC_QP_OP_ERR:
        return "LOCAL QP OPERATION ERROR";
        break;
    case IBV_WC_LOC_PROT_ERR:
        return "LOCAL PROTOCOL ERROR";
        break;
    case IBV_WC_WR_FLUSH_ERR:
        return "WORK REQUEST FLUSHED ERROR";
        break;
    case IBV_WC_MW_BIND_ERR:
        return "MEMORY WINDOW BIND ERROR";
        break;
    case IBV_WC_BAD_RESP_ERR:
        return "BAD RESPONSE ERROR";
        break;
    case IBV_WC_LOC_ACCESS_ERR:
        return "LOCAL ACCESS ERROR";
        break;
    case IBV_WC_REM_INV_REQ_ERR:
        return "INVALID REQUEST ERROR";
        break;
    case IBV_WC_REM_ACCESS_ERR:
        return "REMOTE ACCESS ERROR";
        break;
    case IBV_WC_REM_OP_ERR:
        return "REMOTE OPERATION ERROR";
        break;
    case IBV_WC_RETRY_EXC_ERR:
        return "RETRY EXCEEDED ERROR";
        break;
    case IBV_WC_RNR_RETRY_EXC_ERR:
        return "RECEIVER NOT READY RETRY EXCEEDED ERROR";
        break;
    case IBV_WC_LOC_RDD_VIOL_ERR:
        return "LOCAL RDD VIOLATION ERROR";
        break;
    case IBV_WC_REM_INV_RD_REQ_ERR:
        return "INVALID READ REQUEST ERROR";
        break;
    case IBV_WC_REM_ABORT_ERR:
        return "REMOTE ABORT ERROR";
        break;
    case IBV_WC_INV_EECN_ERR:
        return "INVALID EECN ERROR";
        break;
    case IBV_WC_INV_EEC_STATE_ERR:
        return "INVALID EEC STATE ERROR";
        break;
    case IBV_WC_FATAL_ERR:
        return "FATAL ERROR";
        break;
    case IBV_WC_RESP_TIMEOUT_ERR:
        return "RESPONSE TIMEOUT ERROR";
        break;
    case IBV_WC_GENERAL_ERR:
        return "GENERAL ERROR";
        break;
    default:
        return "STATUS UNDEFINED";
        break;
    }
}

static void
progress_pending_frags_wqe(mca_btl_base_endpoint_t *ep, const int qpn)
{
    int ret;
    opal_list_item_t *frag;
    mca_btl_iwarp_qp_t *qp = ep->qps[qpn].qp;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);
    for(int i = 0; i < 2; i++) {
       while(qp->sd_wqe > 0) {
            mca_btl_base_endpoint_t *tmp_ep;
            frag = opal_list_remove_first(&ep->qps[qpn].no_wqe_pending_frags[i]);
            if(NULL == frag)
                break;
            assert(0 == frag->opal_list_item_refcount);
            tmp_ep = to_com_frag(frag)->endpoint;
            ret = mca_btl_iwarp_endpoint_post_send(tmp_ep, to_send_frag(frag));
            if (OPAL_SUCCESS != ret) {
                /* NTH: this handles retrying if we are out of credits but other errors are not
                 * handled (maybe abort?). */
                if (OPAL_ERR_RESOURCE_BUSY != ret) {
                    opal_list_prepend (&ep->qps[qpn].no_wqe_pending_frags[i], (opal_list_item_t *) frag);
                }
                break;
            }
       }
    }
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
}

static void progress_pending_frags_srq(mca_btl_iwarp_module_t* iwarp_btl,
        const int qp)
{
    opal_list_item_t *frag;
    int i;

    assert(BTL_IWARP_QP_TYPE_SRQ(qp));

    for(i = 0; i < 2; i++) {
        while(iwarp_btl->qps[qp].u.srq_qp.sd_credits > 0) {
            OPAL_THREAD_LOCK(&iwarp_btl->ib_lock);
            frag = opal_list_remove_first(
                    &iwarp_btl->qps[qp].u.srq_qp.pending_frags[i]);
            OPAL_THREAD_UNLOCK(&iwarp_btl->ib_lock);

            if(NULL == frag)
                break;

            mca_btl_iwarp_endpoint_send(to_com_frag(frag)->endpoint,
                    to_send_frag(frag));
        }
    }
}

static char *cq_name[] = {"HP CQ", "LP CQ"};
static void handle_wc(mca_btl_iwarp_device_t* device, const uint32_t cq,
        struct ibv_wc *wc)
{
    static int flush_err_printed[] = {0, 0};
    mca_btl_iwarp_com_frag_t* frag;
    mca_btl_base_descriptor_t *des;
    mca_btl_iwarp_endpoint_t* endpoint;
    mca_btl_iwarp_module_t *iwarp_btl = NULL;
    const opal_proc_t* remote_proc = NULL;
    int qp, btl_ownership;
    int n;

    des = (mca_btl_base_descriptor_t*)(uintptr_t)wc->wr_id;
    frag = to_com_frag(des);

    /* For receive fragments "order" contains QP idx the fragment was posted
     * to. For send fragments "order" contains QP idx the fragment was send
     * through */
    qp = des->order;

    if (IBV_WC_RECV == wc->opcode && (wc->wc_flags & IBV_WC_WITH_IMM)) {
#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        wc->imm_data = ntohl(wc->imm_data);
#endif
        frag->endpoint = (mca_btl_iwarp_endpoint_t*)
            opal_pointer_array_get_item(device->endpoints, wc->imm_data);
    }

    endpoint = frag->endpoint;

    assert (NULL != endpoint);

    iwarp_btl = endpoint->endpoint_btl;

    if(wc->status != IBV_WC_SUCCESS) {
        OPAL_OUTPUT((-1, "Got WC: ERROR"));
        goto error;
    }

    /* Handle work completions */
    switch(wc->opcode) {
        case IBV_WC_RDMA_READ:
        case IBV_WC_COMP_SWAP:
        case IBV_WC_FETCH_ADD:
            OPAL_OUTPUT((-1, "Got WC: RDMA_READ or RDMA_WRITE"));

            OPAL_THREAD_ADD_FETCH32(&endpoint->get_tokens, 1);

            mca_btl_iwarp_get_frag_t *get_frag = to_get_frag(des);

            /* check if atomic result needs to be byte swapped (mlx5) */
            if (iwarp_btl->atomic_ops_be && IBV_WC_RDMA_READ != wc->opcode) {
                *((int64_t *) frag->sg_entry.addr) = ntoh64 (*((int64_t *) frag->sg_entry.addr));
            }

            get_frag->cb.func (&iwarp_btl->super, endpoint, (void *)(intptr_t) frag->sg_entry.addr,
                               get_frag->cb.local_handle, get_frag->cb.context, get_frag->cb.data,
                               OPAL_SUCCESS);
            /* fall through */
        case IBV_WC_RDMA_WRITE:
            if (MCA_BTL_IWARP_FRAG_SEND_USER == iwarp_frag_type(des)) {
                mca_btl_iwarp_put_frag_t *put_frag = to_put_frag(des);

                put_frag->cb.func (&iwarp_btl->super, endpoint, (void *)(intptr_t) frag->sg_entry.addr,
                                   put_frag->cb.local_handle, put_frag->cb.context, put_frag->cb.data,
                                   OPAL_SUCCESS);
                put_frag->cb.func = NULL;
            }
            /* fall through */
        case IBV_WC_SEND:
            OPAL_OUTPUT((-1, "Got WC: RDMA_WRITE or SEND"));
            if(iwarp_frag_type(des) == MCA_BTL_IWARP_FRAG_SEND) {
                opal_list_item_t *i;
                while((i = opal_list_remove_first(&to_send_frag(des)->coalesced_frags))) {
                    btl_ownership = (to_base_frag(i)->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                        to_base_frag(i)->base.des_cbfunc(&iwarp_btl->super, endpoint,
                                &to_base_frag(i)->base, OPAL_SUCCESS);
                    if( btl_ownership ) {
                        mca_btl_iwarp_free(&iwarp_btl->super, &to_base_frag(i)->base);
                    }
                }
            }
            /* Process a completed send/put/get */
            btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
            if (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
                des->des_cbfunc(&iwarp_btl->super, endpoint, des, OPAL_SUCCESS);
            }
            if( btl_ownership ) {
                mca_btl_iwarp_free(&iwarp_btl->super, des);
            }

            /* return send wqe */
            qp_put_wqe(endpoint, qp);

            /* return wqes that were sent before this frag */
            n = qp_frag_to_wqe(endpoint, qp, to_com_frag(des));

            if(IBV_WC_SEND == wc->opcode && !BTL_IWARP_QP_TYPE_PP(qp)) {
                OPAL_THREAD_ADD_FETCH32(&iwarp_btl->qps[qp].u.srq_qp.sd_credits, 1+n);

                /* new SRQ credit available. Try to progress pending frags*/
                progress_pending_frags_srq(iwarp_btl, qp);
            }
            /* new wqe or/and get token available. Try to progress pending frags */
            progress_pending_frags_wqe(endpoint, qp);
            mca_btl_iwarp_frag_progress_pending_put_get(endpoint, qp);
            break;
        case IBV_WC_RECV:
            OPAL_OUTPUT((-1, "Got WC: RDMA_RECV, qp %d, src qp %d, WR ID %" PRIx64,
                         wc->qp_num, wc->src_qp, wc->wr_id));

            /* Process a RECV */
            if(btl_iwarp_handle_incoming(iwarp_btl, endpoint, to_recv_frag(frag),
                        wc->byte_len) != OPAL_SUCCESS) {
                iwarp_btl->error_cb(&iwarp_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                                     NULL, NULL);
                break;
            }

            /* decide if it is time to setup an eager rdma channel */
            if(!endpoint->eager_rdma_local.base.pval && endpoint->use_eager_rdma &&
                    wc->byte_len < mca_btl_iwarp_component.eager_limit &&
                    iwarp_btl->eager_rdma_channels <
                    mca_btl_iwarp_component.max_eager_rdma &&
                    OPAL_THREAD_ADD_FETCH32(&endpoint->eager_recv_count, 1) ==
                    mca_btl_iwarp_component.eager_rdma_threshold) {
                mca_btl_iwarp_endpoint_connect_eager_rdma(endpoint);
            }
            break;
        default:
            BTL_ERROR(("Unhandled work completion opcode is %d", wc->opcode));
            if(iwarp_btl)
                iwarp_btl->error_cb(&iwarp_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                                     NULL, NULL);
            break;
    }

    return;

error:
    if(endpoint->endpoint_proc && endpoint->endpoint_proc->proc_opal)
        remote_proc = endpoint->endpoint_proc->proc_opal;

    /* For iWARP, the TCP connection is tied to the QP once the QP is
     * in RTS.  And destroying the QP is thus tied to connection
     * teardown for iWARP.  To destroy the connection in iWARP you
     * must move the QP out of RTS, either into CLOSING for a nice
     * graceful close (e.g., via rdma_disconnect()), or to ERROR if
     * you want to be rude (e.g., just destroying the QP without
     * disconnecting first).  In both cases, all pending non-completed
     * SQ and RQ WRs will automatically be flushed.
     */
    if (IBV_WC_WR_FLUSH_ERR == wc->status &&
        IBV_TRANSPORT_IWARP == device->ib_dev->transport_type) {
        return;
    }

    if(IBV_WC_WR_FLUSH_ERR != wc->status || !flush_err_printed[cq]++) {
        BTL_PEER_ERROR(remote_proc, ("error polling %s with status %s "
                    "status number %d for wr_id %" PRIx64 " opcode %d  vendor error %d qp_idx %d",
                    cq_name[cq], btl_iwarp_component_status_to_string(wc->status),
                    wc->status, wc->wr_id,
                    wc->opcode, wc->vendor_err, qp));
    }

    if (IBV_WC_RNR_RETRY_EXC_ERR == wc->status ||
        IBV_WC_RETRY_EXC_ERR == wc->status) {
        const char *peer_hostname;
        peer_hostname = opal_get_proc_hostname(endpoint->endpoint_proc->proc_opal);
        const char *device_name =
            ibv_get_device_name(endpoint->qps[qp].qp->lcl_qp->context->device);

        if (IBV_WC_RNR_RETRY_EXC_ERR == wc->status) {
            // The show_help checker script gets confused if the topic
            // is an inline logic check, so separate it into two calls
            // to show_help.
            if (BTL_IWARP_QP_TYPE_PP(qp)) {
                opal_show_help("help-mpi-btl-iwarp.txt",
                               "pp rnr retry exceeded",
                               true,
                               opal_process_info.nodename,
                               device_name,
                               peer_hostname);
            } else {
                opal_show_help("help-mpi-btl-iwarp.txt",
                               "srq rnr retry exceeded",
                               true,
                               opal_process_info.nodename,
                               device_name,
                               peer_hostname);
            }
        } else if (IBV_WC_RETRY_EXC_ERR == wc->status) {
            opal_show_help("help-mpi-btl-iwarp.txt",
                           "pp retry exceeded", true,
                           opal_process_info.nodename,
                           device_name, peer_hostname);
        }
    }

    if(iwarp_btl)
        iwarp_btl->error_cb(&iwarp_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                             (struct opal_proc_t*)remote_proc, NULL);
}

static int poll_device(mca_btl_iwarp_device_t* device, int count)
{
    int ne = 0, cq;
    uint32_t hp_iter = 0;
    struct ibv_wc wc[MCA_BTL_IWARP_CQ_POLL_BATCH_DEFAULT];
    int i;

    device->pollme = false;
    for(cq = 0; cq < 2 && hp_iter < mca_btl_iwarp_component.cq_poll_progress;)
    {
        ne = ibv_poll_cq(device->ib_cq[cq], mca_btl_iwarp_component.cq_poll_batch, wc);
        if(0 == ne) {
            /* don't check low prio cq if there was something in high prio cq,
             * but for each cq_poll_ratio hp cq polls poll lp cq once */
            if(count && device->hp_cq_polls)
                break;
            cq++;
            device->hp_cq_polls = mca_btl_iwarp_component.cq_poll_ratio;
            continue;
        }

        if(ne < 0)
            goto error;

        count++;

        if(BTL_IWARP_HP_CQ == cq) {
            device->pollme = true;
            hp_iter++;
            device->hp_cq_polls--;
        }

        for (i = 0; i < ne; i++)
            handle_wc(device, cq, &wc[i]);
    }

    return count;
error:
    BTL_ERROR(("error polling %s with %d errno says %s", cq_name[cq], ne,
                strerror(errno)));
    return count;
}

#if OPAL_ENABLE_PROGRESS_THREADS
void* mca_btl_iwarp_progress_thread(opal_object_t* arg)
{
    opal_thread_t* thread = (opal_thread_t*)arg;
    mca_btl_iwarp_device_t* device = thread->t_arg;
    struct ibv_cq *ev_cq;
    void *ev_ctx;

    /* This thread enter in a cancel enabled state */
    pthread_setcancelstate( PTHREAD_CANCEL_ENABLE, NULL );
    pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS, NULL );

    opal_output(-1, "WARNING: the iwarp btl progress thread code *does not yet work*.  Your run is likely to hang, crash, break the kitchen sink, and/or eat your cat.  You have been warned.");

    while (device->progress) {
#if 0
        while(ompi_progress_threads()) {
            while(ompi_progress_threads())
                sched_yield();
            usleep(100); /* give app a chance to re-enter library */
        }
#endif

        if(ibv_get_cq_event(device->ib_channel, &ev_cq, &ev_ctx))
            BTL_ERROR(("Failed to get CQ event with error %s",
                        strerror(errno)));
        if(ibv_req_notify_cq(ev_cq, 0)) {
            BTL_ERROR(("Couldn't request CQ notification with error %s",
                        strerror(errno)));
        }

        ibv_ack_cq_events(ev_cq, 1);

        while(poll_device(device, 0));
    }

    return PTHREAD_CANCELED;
}
#endif

static int progress_one_device(mca_btl_iwarp_device_t *device)
{
    int i, c, count = 0, ret;
    mca_btl_iwarp_recv_frag_t* frag;
    mca_btl_iwarp_endpoint_t* endpoint;
    uint32_t non_eager_rdma_endpoints = 0;

    c = device->eager_rdma_buffers_count;
    non_eager_rdma_endpoints += (device->non_eager_rdma_endpoints + device->pollme);

    for(i = 0; i < c; i++) {
        endpoint = device->eager_rdma_buffers[i];

        if(!endpoint)
            continue;

        OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
        frag = MCA_BTL_IWARP_GET_LOCAL_RDMA_FRAG(endpoint,
                endpoint->eager_rdma_local.head);

        if(MCA_BTL_IWARP_RDMA_FRAG_LOCAL(frag)) {
            uint32_t size;
            mca_btl_iwarp_module_t *btl = endpoint->endpoint_btl;

            opal_atomic_mb();

            if(endpoint->nbo) {
                BTL_IWARP_FOOTER_NTOH(*frag->ftr);
            }
            size = MCA_BTL_IWARP_RDMA_FRAG_GET_SIZE(frag->ftr);
#if OPAL_ENABLE_DEBUG
            if (frag->ftr->seq != endpoint->eager_rdma_local.seq)
                BTL_ERROR(("Eager RDMA wrong SEQ: received %d expected %d",
                           frag->ftr->seq,
                           endpoint->eager_rdma_local.seq));
            endpoint->eager_rdma_local.seq++;
#endif
            MCA_BTL_IWARP_RDMA_NEXT_INDEX(endpoint->eager_rdma_local.head);

            OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
            frag->hdr = (mca_btl_iwarp_header_t*)(((char*)frag->ftr) -
                size - BTL_IWARP_FTR_PADDING(size) + sizeof(mca_btl_iwarp_footer_t));
            to_base_frag(frag)->segment.seg_addr.pval =
                ((unsigned char* )frag->hdr) + sizeof(mca_btl_iwarp_header_t);

            ret = btl_iwarp_handle_incoming(btl, to_com_frag(frag)->endpoint,
                    frag, size - sizeof(mca_btl_iwarp_footer_t));
            if (ret != OPAL_SUCCESS) {
                btl->error_cb(&btl->super, MCA_BTL_ERROR_FLAGS_FATAL, NULL, NULL);
                return 0;
            }

            count++;
        } else
            OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
    }

    device->eager_rdma_polls--;

    if(0 == count || non_eager_rdma_endpoints != 0 || !device->eager_rdma_polls) {
        count += poll_device(device, count);
        device->eager_rdma_polls = mca_btl_iwarp_component.eager_rdma_poll_ratio;
    }

    return count;
}

/*
 *  IB component progress.
 */
static int btl_iwarp_component_progress(void)
{
    int i;
    int count = 0;

    if(OPAL_UNLIKELY(mca_btl_iwarp_component.use_async_event_thread &&
            mca_btl_iwarp_component.error_counter)) {
        goto error;
    }

    for(i = 0; i < mca_btl_iwarp_component.devices_count; i++) {
        mca_btl_iwarp_device_t *device =
            (mca_btl_iwarp_device_t *) opal_pointer_array_get_item(&mca_btl_iwarp_component.devices, i);
        if (NULL != device) {
            count += progress_one_device(device);
        }
    }

    return count;

error:
    /* Set the fatal counter to zero */
    mca_btl_iwarp_component.error_counter = 0;
    /* Lets find all error events */
    for(i = 0; i < mca_btl_iwarp_component.ib_num_btls; i++) {
        mca_btl_iwarp_module_t* iwarp_btl =
            mca_btl_iwarp_component.iwarp_btls[i];
        if(iwarp_btl->device->got_fatal_event) {
            iwarp_btl->error_cb(&iwarp_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                                 NULL, NULL);
        }
        if(iwarp_btl->device->got_port_event) {
            /* These are non-fatal so just ignore it. */
            iwarp_btl->device->got_port_event = false;
        }
    }
    return count;
}

int mca_btl_iwarp_post_srr(mca_btl_iwarp_module_t* iwarp_btl, const int qp)
{
    int rd_low_local = iwarp_btl->qps[qp].u.srq_qp.rd_low_local;
    int rd_curr_num = iwarp_btl->qps[qp].u.srq_qp.rd_curr_num;
    int num_post, i, rc;
    struct ibv_recv_wr *bad_wr, *wr_list = NULL, *wr = NULL;

    assert(!BTL_IWARP_QP_TYPE_PP(qp));

    OPAL_THREAD_LOCK(&iwarp_btl->ib_lock);
    if(iwarp_btl->qps[qp].u.srq_qp.rd_posted > rd_low_local) {
        OPAL_THREAD_UNLOCK(&iwarp_btl->ib_lock);
        return OPAL_SUCCESS;
    }
    num_post = rd_curr_num - iwarp_btl->qps[qp].u.srq_qp.rd_posted;

    if (0 == num_post) {
        OPAL_THREAD_UNLOCK(&iwarp_btl->ib_lock);
        return OPAL_SUCCESS;
    }

    for(i = 0; i < num_post; i++) {
        opal_free_list_item_t* item;
        item = opal_free_list_wait (&iwarp_btl->device->qps[qp].recv_free);
        to_base_frag(item)->base.order = qp;
        to_com_frag(item)->endpoint = NULL;
        if(NULL == wr)
            wr = wr_list = &to_recv_frag(item)->rd_desc;
        else
            wr = wr->next = &to_recv_frag(item)->rd_desc;
    }

    wr->next = NULL;

    rc = ibv_post_srq_recv(iwarp_btl->qps[qp].u.srq_qp.srq, wr_list, &bad_wr);
    if(OPAL_LIKELY(0 == rc)) {
        struct ibv_srq_attr srq_attr;

        OPAL_THREAD_ADD_FETCH32(&iwarp_btl->qps[qp].u.srq_qp.rd_posted, num_post);

        if(true == iwarp_btl->qps[qp].u.srq_qp.srq_limit_event_flag) {
            srq_attr.max_wr = iwarp_btl->qps[qp].u.srq_qp.rd_curr_num;
            srq_attr.max_sge = 1;
            srq_attr.srq_limit = mca_btl_iwarp_component.qp_infos[qp].u.srq_qp.srq_limit;

            iwarp_btl->qps[qp].u.srq_qp.srq_limit_event_flag = false;
            if(ibv_modify_srq(iwarp_btl->qps[qp].u.srq_qp.srq, &srq_attr, IBV_SRQ_LIMIT)) {
                BTL_ERROR(("Failed to request limit event for srq on  %s.  "
                   "Fatal error, stoping asynch event thread",
                   ibv_get_device_name(iwarp_btl->device->ib_dev)));

                OPAL_THREAD_UNLOCK(&iwarp_btl->ib_lock);
                return OPAL_ERROR;
            }
        }

        OPAL_THREAD_UNLOCK(&iwarp_btl->ib_lock);
        return OPAL_SUCCESS;
    }

    for(i = 0; wr_list && wr_list != bad_wr; i++, wr_list = wr_list->next);

    BTL_ERROR(("error posting receive descriptors to shared receive "
                "queue %d (%d from %d)", qp, i, num_post));

    OPAL_THREAD_UNLOCK(&iwarp_btl->ib_lock);
    return OPAL_ERROR;
}


struct mca_btl_iwarp_event_t {
    opal_event_t super;
    void *(*fn)(void *);
    void *arg;
    opal_event_t *event;
};

typedef struct mca_btl_iwarp_event_t mca_btl_iwarp_event_t;

static void *mca_btl_iwarp_run_once_cb (int fd, int flags, void *context)
{
    mca_btl_iwarp_event_t *event = (mca_btl_iwarp_event_t *) context;
    void *ret;

    ret = event->fn (event->arg);
    opal_event_del (&event->super);
    free (event);
    return ret;
}

int mca_btl_iwarp_run_in_main (void *(*fn)(void *), void *arg)
{
    mca_btl_iwarp_event_t *event = malloc (sizeof (mca_btl_iwarp_event_t));

    if (OPAL_UNLIKELY(NULL == event)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    event->fn = fn;
    event->arg = arg;

    opal_event_set (opal_sync_event_base, &event->super, -1, OPAL_EV_READ,
                    mca_btl_iwarp_run_once_cb, event);

    opal_event_active (&event->super, OPAL_EV_READ, 1);

    return OPAL_SUCCESS;
}
