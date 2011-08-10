/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <rdma/winverbs.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <malloc.h>
#include <stddef.h>

#include "opal/mca/event/event.h"
#include "opal/align.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/sys/timer.h"
#include "opal/sys/atomic.h"
#include "opal/util/argv.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal_stdint.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/notifier/notifier.h"

#include "ompi/constants.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/runtime/mpiruntime.h"

#include "btl_wv.h"
#include "btl_wv_frag.h"
#include "btl_wv_endpoint.h"
#include "btl_wv_eager_rdma.h"
#include "btl_wv_proc.h"
#include "btl_wv_ini.h"
#include "btl_wv_mca.h"
#include "connect/base.h"
#include "ompi/runtime/params.h"

/*
 * Local functions
 */
static int btl_wv_component_register(void);
static int btl_wv_component_open(void);
static int btl_wv_component_close(void);
static mca_btl_base_module_t **btl_wv_component_init(int*, bool, bool);
static int btl_wv_component_progress(void);

/*
 * Local variables
 */
static mca_btl_wv_device_t *receive_queues_device = NULL;
IWVProvider *prov;

mca_btl_wv_component_t mca_btl_wv_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "wv", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            btl_wv_component_open,  /* component open */
            btl_wv_component_close,  /* component close */
            NULL, /* component query */
            btl_wv_component_register, /* component register */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        btl_wv_component_init,
        btl_wv_component_progress,
    }
};

static enum wv_mtu wv_convert_mtu(UINT32 mtu)
{
    switch (mtu) {
    case 256:    return WV_MTU_256;
    case 512:    return WV_MTU_512;
    case 1024:    return WV_MTU_1024;
    case 2048:    return WV_MTU_2048;
    case 4096:    return WV_MTU_4096;
    default:    return (wv_mtu) mtu;
    }
}

static int btl_wv_component_register(void)
{
    int ret;

    /* register IB component parameters */
    ret = btl_wv_register_mca_params();

    mca_btl_wv_component.max_send_size =
        mca_btl_wv_module.super.btl_max_send_size;
    mca_btl_wv_component.eager_limit =
        mca_btl_wv_module.super.btl_eager_limit;

    return OMPI_SUCCESS;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */
static int btl_wv_component_open(void)
{
    /* initialize state */
    mca_btl_wv_component.ib_num_btls = 0;
    mca_btl_wv_component.wv_btls = NULL;
    OBJ_CONSTRUCT(&mca_btl_wv_component.devices, opal_pointer_array_t);
    mca_btl_wv_component.devices_count = 0;
    mca_btl_wv_component.cpc_explicitly_defined = false;
    mca_btl_wv_component.default_recv_qps = NULL;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_wv_component.ib_procs, opal_list_t);

    srand48(getpid() * time(NULL));
    return ret;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

static int btl_wv_component_close(void)
{
    int rc = OMPI_SUCCESS;
    ompi_btl_wv_connect_base_finalize();
    ompi_btl_wv_ini_finalize();
    if (NULL != mca_btl_wv_component.receive_queues) {
        free(mca_btl_wv_component.receive_queues);
    }

    if (NULL != mca_btl_wv_component.default_recv_qps) {
        free(mca_btl_wv_component.default_recv_qps);
    }

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
 *  Register local wv port information with the modex so that it
 *  can be shared with all other peers.
 */
static int btl_wv_modex_send(void)
{
    int rc, i, j;
    int modex_message_size;
    char *message, *offset;
    size_t size, msg_size;
    ompi_btl_wv_connect_base_module_t *cpc;

    opal_output(-1, "Starting to modex send");
    if (0 == mca_btl_wv_component.ib_num_btls) {
        return 0;
    }
    modex_message_size = offsetof(mca_btl_wv_modex_message_t, end);

    /* The message is packed into multiple parts:
     * 1. a uint8_t indicating the number of modules (ports) in the message
     * 2. for each module:
     *    a. the common module data
     *    b. a uint8_t indicating how many CPCs follow
     *    c. for each CPC:
     *       a. a uint8_t indicating the index of the CPC in the all[]
     *          array in btl_wv_connect_base.c
     *       b. a uint8_t indicating the priority of this CPC
     *       c. a uint8_t indicating the length of the blob to follow
     *       d. a blob that is only meaningful to that CPC
     */
    msg_size = 
        /* uint8_t for number of modules in the message */
        1 +
        /* For each module: */
        mca_btl_wv_component.ib_num_btls * 
        (
         /* Common module data */
         modex_message_size + 
         /* uint8_t for how many CPCs follow */
         1
         );
    /* For each module, add in the size of the per-CPC data */
    for (i = 0; i < mca_btl_wv_component.ib_num_btls; i++) {
        for (j = 0; 
             j < mca_btl_wv_component.wv_btls[i]->num_cpcs;
             ++j) {
            msg_size += 
                /* uint8_t for the index of the CPC */
                1 +
                /* uint8_t for the CPC's priority */
                1 + 
                /* uint8_t for the blob length */
                1 +
                /* blob length */
                mca_btl_wv_component.wv_btls[i]->cpcs[j]->data.cbm_modex_message_len;
        }
    }
    message = (char *) malloc(msg_size);
    if (NULL == message) {
        BTL_ERROR(("Failed malloc"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Pack the number of modules */
    offset = message;
    pack8(&offset, mca_btl_wv_component.ib_num_btls);
    opal_output(-1, "modex sending %d btls (packed: %d, offset now at %d)", mca_btl_wv_component.ib_num_btls, *((uint8_t*) message), (int) (offset - message));

    /* Pack each of the modules */
    for (i = 0; i < mca_btl_wv_component.ib_num_btls; i++) {

        /* Pack the modex common message struct.  */
        size = modex_message_size;

        (mca_btl_wv_component.wv_btls[i]->port_info).vendor_id =
            (mca_btl_wv_component.wv_btls[i]->device->ib_dev_attr).VendorId;

        (mca_btl_wv_component.wv_btls[i]->port_info).vendor_part_id =
            (mca_btl_wv_component.wv_btls[i]->device->ib_dev_attr).VendorPartId;

        (mca_btl_wv_component.wv_btls[i]->port_info).transport_type = MCA_BTL_WV_TRANSPORT_IB;

        memcpy(offset, 
               &(mca_btl_wv_component.wv_btls[i]->port_info), 
               size);
        opal_output(-1, "modex packed btl port modex message: 0x%" PRIx64 ", %d, %d (size: %d)",
                    mca_btl_wv_component.wv_btls[i]->port_info.subnet_id,
                    mca_btl_wv_component.wv_btls[i]->port_info.mtu,
                    mca_btl_wv_component.wv_btls[i]->port_info.lid,
                    (int) size);
                    
        offset += size;
        opal_output(-1, "modex packed btl %d: modex message, offset now %d",
                    i, (int) (offset -message));

        /* Pack the number of CPCs that follow */
        pack8(&offset, 
              mca_btl_wv_component.wv_btls[i]->num_cpcs);
        opal_output(-1, "modex packed btl %d: to pack %d cpcs (packed: %d, offset now %d)",
                    i, mca_btl_wv_component.wv_btls[i]->num_cpcs,
                    *((uint8_t*) (offset - 1)), (int) (offset-message));

        /* Pack each CPC */
        for (j = 0; 
             j < mca_btl_wv_component.wv_btls[i]->num_cpcs;
             ++j) {
            uint8_t u8;

            cpc = mca_btl_wv_component.wv_btls[i]->cpcs[j];
            opal_output(-1, "modex packed btl %d: packing cpc %s", 
                        i, cpc->data.cbm_component->cbc_name);
            /* Pack the CPC index */
            u8 = ompi_btl_wv_connect_base_get_cpc_index(cpc->data.cbm_component);
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
    rc = ompi_modex_send(&mca_btl_wv_component.super.btl_version,
                         message, msg_size);
    free(message);
    opal_output(-1, "Modex sent!  %d calculated, %d actual\n", (int) msg_size, (int) (offset - message));

    return rc;
}

/*
 * Active Message Callback function on control message.
 */

static void btl_wv_control(mca_btl_base_module_t* btl,
                           mca_btl_base_tag_t tag, mca_btl_base_descriptor_t* des,
        void* cbdata)
{
    /* don't return credits used for control messages */
    mca_btl_wv_module_t *obtl = (mca_btl_wv_module_t*)btl;
    mca_btl_wv_endpoint_t* ep = to_com_frag(des)->endpoint;
    mca_btl_wv_control_header_t *ctl_hdr =
        (mca_btl_wv_control_header_t *) to_base_frag(des)->segment.seg_addr.pval;
    mca_btl_wv_eager_rdma_header_t *rdma_hdr;
    mca_btl_wv_header_coalesced_t *clsc_hdr =
        (mca_btl_wv_header_coalesced_t*)(ctl_hdr + 1);
    mca_btl_active_message_callback_t* reg;
    size_t len = des->des_dst->seg_len - sizeof(*ctl_hdr);

    switch (ctl_hdr->type) {
    case MCA_BTL_WV_CONTROL_CREDITS:
        assert(0); /* Credit message is handled elsewhere */
        break;
    case MCA_BTL_WV_CONTROL_RDMA:
       rdma_hdr = (mca_btl_wv_eager_rdma_header_t*)ctl_hdr;

       BTL_VERBOSE(("prior to NTOH received  rkey %" PRIu32 
                    ", rdma_start.lval %" PRIx64 ", pval %p, ival %" PRIu32,
                    rdma_hdr->rkey,
                    rdma_hdr->rdma_start.lval,
                    rdma_hdr->rdma_start.pval,
                    rdma_hdr->rdma_start.ival
                  ));

       if(ep->nbo) {
           BTL_WV_EAGER_RDMA_CONTROL_HEADER_NTOH(*rdma_hdr);
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
       ep->eager_rdma_remote.tokens=mca_btl_wv_component.eager_rdma_num - 1;
       break;
    case MCA_BTL_WV_CONTROL_COALESCED:
        while(len > 0) {
            size_t skip;
            mca_btl_base_descriptor_t tmp_des;
            mca_btl_base_segment_t tmp_seg;

            assert(len >= sizeof(*clsc_hdr));

            if(ep->nbo)
                BTL_WV_HEADER_COALESCED_NTOH(*clsc_hdr);

            skip = (sizeof(*clsc_hdr) + clsc_hdr->alloc_size);

            tmp_des.des_dst = &tmp_seg;
            tmp_des.des_dst_cnt = 1;
            tmp_seg.seg_addr.pval = clsc_hdr + 1;
            tmp_seg.seg_len = clsc_hdr->size;

            /* call registered callback */
            reg = mca_btl_base_active_message_trigger + clsc_hdr->tag;
            reg->cbfunc( &obtl->super, clsc_hdr->tag, &tmp_des, reg->cbdata );
            len -= skip;
            clsc_hdr = (mca_btl_wv_header_coalesced_t*)
                (((unsigned char*)clsc_hdr) + skip);
        }
       break;
    case MCA_BTL_WV_CONTROL_CTS:
        OPAL_OUTPUT((-1, "received CTS from %s (buffer %p): posted recvs %d, sent cts %d",
                     ep->endpoint_proc->proc_ompi->proc_hostname,
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
            if (!ep->endpoint_cts_sent) {
                mca_btl_wv_endpoint_send_cts(ep);
            }
            mca_btl_wv_endpoint_connected(ep);
        }
        break;
    default:
        BTL_ERROR(("Unknown message type received by BTL"));
       break;
    }
}

static int wv_reg_mr(void *reg_data, void *base, size_t size,
                     mca_mpool_base_registration_t *reg)
{
    HRESULT hr;
    mca_btl_wv_device_t *device = (mca_btl_wv_device_t*)reg_data;
    mca_btl_wv_reg_t *wv_reg = (mca_btl_wv_reg_t*)reg;

    wv_reg->mr = (struct wv_mr*)malloc(sizeof(wv_mr));
    wv_reg->mr->context = device->ib_dev_context;
    wv_reg->mr->pd = device->ib_pd;
    wv_reg->mr->addr = base;
    wv_reg->mr->length = size;
    hr = device->ib_pd->handle->RegisterMemory(base, size, WV_ACCESS_LOCAL_WRITE| \
             WV_ACCESS_REMOTE_WRITE|WV_ACCESS_REMOTE_READ, NULL,
             (WV_MEMORY_KEYS *) &wv_reg->mr->lkey);
    wv_reg->mr->rkey = ntohl(wv_reg->mr->rkey);
    if (NULL == wv_reg->mr) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static int wv_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg)
{
    HRESULT hr;
    mca_btl_wv_reg_t *wv_reg = (mca_btl_wv_reg_t*)reg;
    hr = wv_reg->mr->pd->handle->DeregisterMemory(wv_reg->mr->lkey,NULL);
    if(SUCCEEDED(hr)) {
        free(wv_reg->mr);
    }else {
        BTL_ERROR(("%s: error unpinning wv memory errno says %s",
                    __func__, strerror(errno)));
        return OMPI_ERROR;
    }
    wv_reg->mr = NULL;
    return OMPI_SUCCESS;
}

static inline int param_register_int(const char* param_name, int default_value)
{
    int param_value = default_value;
    int id = mca_base_param_register_int("btl", "wv", param_name, NULL,
             default_value);
    mca_base_param_lookup_int(id, &param_value);
    return param_value;
}

static int init_one_port(opal_list_t *btl_list, mca_btl_wv_device_t *device,
                         uint8_t port_num, uint16_t pkey_index,
                         WV_PORT_ATTRIBUTES *ib_port_attr) 
{
    HRESULT hr;
    uint16_t lid, i, lmc, lmc_step;
    mca_btl_wv_module_t *wv_btl;
    mca_btl_base_selected_module_t *ib_selected;
    union wv_gid gid;
    uint64_t subnet_id;

    hr = device->ib_dev_context->device_if->QueryGid(port_num,0,(WV_GID*)&gid);
    if(FAILED(hr)) {
        BTL_ERROR(("query_gid failed (%s:%d)\n",
                   device->ib_dev->name, port_num));
        return OMPI_ERR_NOT_FOUND;
    }
    subnet_id = ntoh64(gid.global.subnet_prefix);
    BTL_VERBOSE(("my IB-only subnet_id for HCA %s port %d is %016" PRIx64, 
                 device->ib_dev->name, port_num, subnet_id));
    if(mca_btl_wv_component.ib_num_btls > 0 &&
            IB_DEFAULT_GID_PREFIX == subnet_id &&
            mca_btl_wv_component.warn_default_gid_prefix) {
        orte_show_help("help-mpi-btl-wv.txt", "default subnet prefix",
                true, orte_process_info.nodename);
    }

    lmc = (1 << ib_port_attr->Lmc);
    lmc_step = 1;

    if (0 != mca_btl_wv_component.max_lmc &&
        mca_btl_wv_component.max_lmc < lmc) {
        lmc = mca_btl_wv_component.max_lmc;
    }

    for(lid = ntohs(ib_port_attr->Lid);
            lid < ntohs(ib_port_attr->Lid) + lmc; lid += lmc_step){
        for(i = 0; i < mca_btl_wv_component.btls_per_lid; i++){
            char param[40];

            wv_btl = (mca_btl_wv_module_t *) malloc(sizeof(mca_btl_wv_module_t));
            if(NULL == wv_btl) {
                BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            memcpy(wv_btl, &mca_btl_wv_module,
                    sizeof(mca_btl_wv_module));
            memcpy(&wv_btl->ib_port_attr, ib_port_attr,
                    sizeof(WV_PORT_ATTRIBUTES));
            ib_selected = OBJ_NEW(mca_btl_base_selected_module_t);
            ib_selected->btl_module = (mca_btl_base_module_t*) wv_btl;
            wv_btl->device = device;
            wv_btl->port_num = (uint8_t) port_num;
            wv_btl->pkey_index = pkey_index;
            wv_btl->lid = lid;
            wv_btl->apm_port = 0;
            wv_btl->src_path_bits = lid - ntohs(ib_port_attr->Lid);
            wv_btl->port_info.subnet_id = subnet_id;
            wv_btl->port_info.mtu = device->mtu;
            wv_btl->port_info.lid = lid;
            wv_btl->cpcs = NULL;
            wv_btl->num_cpcs = 0;

            mca_btl_base_active_message_trigger[MCA_BTL_TAG_IB].cbfunc = btl_wv_control;
            mca_btl_base_active_message_trigger[MCA_BTL_TAG_IB].cbdata = NULL;

            /* Check bandwidth configured for this device */
            sprintf(param, "bandwidth_%s", device->ib_dev->name);
            wv_btl->super.btl_bandwidth =
                param_register_int(param, wv_btl->super.btl_bandwidth);

            /* Check bandwidth configured for this device/port */
            sprintf(param, "bandwidth_%s:%d", device->ib_dev->name,
                    port_num);
            wv_btl->super.btl_bandwidth =
                param_register_int(param, wv_btl->super.btl_bandwidth);

            /* Check bandwidth configured for this device/port/LID */
            sprintf(param, "bandwidth_%s:%d:%d",
                    device->ib_dev->name, port_num, lid);
            wv_btl->super.btl_bandwidth =
                param_register_int(param, wv_btl->super.btl_bandwidth);

            /* Check latency configured for this device */
            sprintf(param, "latency_%s", device->ib_dev->name);
            wv_btl->super.btl_latency =
                param_register_int(param, wv_btl->super.btl_latency);

            /* Check latency configured for this device/port */
            sprintf(param, "latency_%s:%d", device->ib_dev->name,
                    port_num);
            wv_btl->super.btl_latency =
                param_register_int(param, wv_btl->super.btl_latency);

            /* Check latency configured for this device/port/LID */
            sprintf(param, "latency_%s:%d:%d", device->ib_dev->name,
                    port_num, lid);
            wv_btl->super.btl_latency =
                param_register_int(param, wv_btl->super.btl_latency);

            /* Auto-detect the port bandwidth */
            if (0 == wv_btl->super.btl_bandwidth) {
                /* To calculate the bandwidth available on this port,
                   we have to look up the values corresponding to
                   port->active_speed and port->active_width.  These
                   are enums corresponding to the IB spec.  Overall
                   forumula is 80% of the reported speed (to get the
                   true link speed) times the number of links. */
                switch (ib_port_attr->ActiveSpeed) {
                case 1:
                    /* 2.5Gbps * 0.8, in megabits */
                    wv_btl->super.btl_bandwidth = 2000;
                    break;
                case 2:
                    /* 5.0Gbps * 0.8, in megabits */
                    wv_btl->super.btl_bandwidth = 4000;
                    break;
                case 4:
                    /* 10.0Gbps * 0.8, in megabits */
                    wv_btl->super.btl_bandwidth = 8000;
                    break;
                default:
                    /* Who knows?  Declare this port unreachable (do
                       *not* return ERR_VALUE_OF_OUT_OF_BOUNDS; that
                       is reserved for when we exceed the number of
                       allowable BTLs). */
                    return OMPI_ERR_UNREACH;
                }
                switch (ib_port_attr->ActiveWidth) {
                case 1:
                    /* 1x */
                    /* unity */
                    break;
                case 2:
                    /* 4x */
                    wv_btl->super.btl_bandwidth *= 4;
                    break;
                case 4:
                    /* 8x */
                    wv_btl->super.btl_bandwidth *= 8;
                    break;
                case 8:
                    /* 12x */
                    wv_btl->super.btl_bandwidth *= 12;
                    break;
                default:
                    /* Who knows?  Declare this port unreachable (do
                       *not* return ERR_VALUE_OF_OUT_OF_BOUNDS; that
                       is reserved for when we exceed the number of
                       allowable BTLs). */
                    return OMPI_ERR_UNREACH;
                }
            }
            opal_list_append(btl_list, (opal_list_item_t*) ib_selected);
            opal_pointer_array_add(device->device_btls, (void*) wv_btl);
            ++device->btls;
            ++mca_btl_wv_component.ib_num_btls;
            if (-1 != mca_btl_wv_component.ib_max_btls &&
                mca_btl_wv_component.ib_num_btls >=
                mca_btl_wv_component.ib_max_btls) {
                return OMPI_ERR_VALUE_OUT_OF_BOUNDS;
            }
        }
    }
    return OMPI_SUCCESS;
}

static void device_construct(mca_btl_wv_device_t *device)
{
    device->ib_dev = NULL;
    device->ib_dev_context = NULL;
    device->ib_pd = NULL;
    device->mpool = NULL;
    device->btls = 0;
    device->ib_cq[BTL_WV_HP_CQ] = NULL;
    device->ib_cq[BTL_WV_LP_CQ] = NULL;
    device->cq_size[BTL_WV_HP_CQ] = 0;
    device->cq_size[BTL_WV_LP_CQ] = 0;
    device->non_eager_rdma_endpoints = 0;
    device->hp_cq_polls = mca_btl_wv_component.cq_poll_ratio;
    device->eager_rdma_polls = mca_btl_wv_component.eager_rdma_poll_ratio;
    device->pollme = true;
    device->eager_rdma_buffers_count = 0;
    device->eager_rdma_buffers = NULL;
    device->qps = NULL;

    OBJ_CONSTRUCT(&device->device_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&device->send_free_control, ompi_free_list_t);
    device->max_inline_data = 0;
}

static void device_destruct(mca_btl_wv_device_t *device)
{
    int i;
    struct wverbs_context *vcontext;

    if(device->eager_rdma_buffers) {
        int i;
        for(i = 0; i < device->eager_rdma_buffers_count; i++)
            if(device->eager_rdma_buffers[i])
                OBJ_RELEASE(device->eager_rdma_buffers[i]);
        free(device->eager_rdma_buffers);
    }

    if (NULL != device->qps) {
        for (i = 0; i < mca_btl_wv_component.num_qps; i++) {
            OBJ_DESTRUCT(&device->qps[i].send_free);
            OBJ_DESTRUCT(&device->qps[i].recv_free);
        }
        free(device->qps);
    }

    OBJ_DESTRUCT(&device->send_free_control);

    /* Release CQs */
    if(device->ib_cq[BTL_WV_HP_CQ] != NULL) {
        device->ib_cq[BTL_WV_HP_CQ]->handle->Release();
        free(device->ib_cq[BTL_WV_HP_CQ]);
    }

    if(device->ib_cq[BTL_WV_LP_CQ] != NULL) {
        device->ib_cq[BTL_WV_LP_CQ]->handle->Release();
        free(device->ib_cq[BTL_WV_LP_CQ]); 
    }

    if (OMPI_SUCCESS != mca_mpool_base_module_destroy(device->mpool)) {
        BTL_VERBOSE(("Failed to release mpool"));
        goto device_error;
    }
    device->ib_pd->handle->Release();
    free(device->ib_pd);

    OBJ_DESTRUCT(&device->device_lock);

    vcontext = CONTAINING_RECORD(device->ib_dev_context, struct wverbs_context, context);
    vcontext->closing = 1;
    device->ib_dev_context->device_if->Release();
    free(vcontext->port);
    free(vcontext);

    BTL_VERBOSE(("device was successfully released"));
    return;
device_error:
    BTL_VERBOSE(("Failed to destroy device resources"));
}

OBJ_CLASS_INSTANCE(mca_btl_wv_device_t, opal_object_t, device_construct,
        device_destruct);

static int prepare_device_for_use(mca_btl_wv_device_t *device)
{
    mca_btl_wv_frag_init_data_t *init_data;
    int rc, qp, length;

    device->endpoints = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(device->endpoints, 10, INT_MAX, 10);
    opal_pointer_array_add(&mca_btl_wv_component.devices, device);
    if (mca_btl_wv_component.max_eager_rdma > 0 &&
        device->use_eager_rdma) {
        device->eager_rdma_buffers =
            (mca_btl_base_endpoint_t **) calloc(mca_btl_wv_component.max_eager_rdma * device->btls,
                                            sizeof(mca_btl_wv_endpoint_t*));
        if(NULL == device->eager_rdma_buffers) {
            BTL_ERROR(("Memory allocation fails"));
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    init_data = (mca_btl_wv_frag_init_data_t *) malloc(sizeof(mca_btl_wv_frag_init_data_t));
    length = sizeof(mca_btl_wv_header_t) +
        sizeof(mca_btl_wv_footer_t) +
        sizeof(mca_btl_wv_eager_rdma_header_t);

    init_data->order = MCA_BTL_NO_ORDER;
    init_data->list = &device->send_free_control;

    rc = ompi_free_list_init_ex_new(&device->send_free_control,
                sizeof(mca_btl_wv_send_control_frag_t), opal_cache_line_size,
                OBJ_CLASS(mca_btl_wv_send_control_frag_t), length,
                mca_btl_wv_component.buffer_alignment,
                mca_btl_wv_component.ib_free_list_num, -1,
                mca_btl_wv_component.ib_free_list_inc,
                device->mpool, mca_btl_wv_frag_init,
                init_data);
    if (OMPI_SUCCESS != rc) {
        /* If we're "out of memory", this usually means that we ran
           out of registered memory, so show that error message */
        if (OMPI_ERR_OUT_OF_RESOURCE == OPAL_SOS_GET_ERROR_CODE(rc) ||
            OMPI_ERR_TEMP_OUT_OF_RESOURCE == OPAL_SOS_GET_ERROR_CODE(rc)) {
            errno = ENOMEM;
            mca_btl_wv_show_init_error(__FILE__, __LINE__,
                                       "ompi_free_list_init_ex_new",
                                       device->ib_dev->name);
        }
        return rc;
    }

    /* setup all the qps */
    for(qp = 0; qp < mca_btl_wv_component.num_qps; qp++) {
        init_data = (mca_btl_wv_frag_init_data_t *) malloc(sizeof(mca_btl_wv_frag_init_data_t));
        /* Initialize pool of send fragments */
        length = sizeof(mca_btl_wv_header_t) +
            sizeof(mca_btl_wv_header_coalesced_t) +
            sizeof(mca_btl_wv_control_header_t) +
            sizeof(mca_btl_wv_footer_t) +
            mca_btl_wv_component.qp_infos[qp].size;

        init_data->order = qp;
        init_data->list = &device->qps[qp].send_free;

        rc = ompi_free_list_init_ex_new(init_data->list,
                    sizeof(mca_btl_wv_send_frag_t), opal_cache_line_size,
                    OBJ_CLASS(mca_btl_wv_send_frag_t), length,
                    mca_btl_wv_component.buffer_alignment,
                    mca_btl_wv_component.ib_free_list_num,
                    mca_btl_wv_component.ib_free_list_max,
                    mca_btl_wv_component.ib_free_list_inc,
                    device->mpool, mca_btl_wv_frag_init,
                                        init_data);
        if (OMPI_SUCCESS != rc) {
            /* If we're "out of memory", this usually means that we
               ran out of registered memory, so show that error
               message */
            if (OMPI_ERR_OUT_OF_RESOURCE == OPAL_SOS_GET_ERROR_CODE(rc) ||
                OMPI_ERR_TEMP_OUT_OF_RESOURCE == OPAL_SOS_GET_ERROR_CODE(rc)) {
                errno = ENOMEM;
                mca_btl_wv_show_init_error(__FILE__, __LINE__,
                                           "ompi_free_list_init_ex_new",
                                           device->ib_dev->name);
            }
            return OMPI_ERROR;
        }

        init_data = (mca_btl_wv_frag_init_data_t *) malloc(sizeof(mca_btl_wv_frag_init_data_t));
        length = sizeof(mca_btl_wv_header_t) +
            sizeof(mca_btl_wv_header_coalesced_t) +
            sizeof(mca_btl_wv_control_header_t) +
            sizeof(mca_btl_wv_footer_t) +
            mca_btl_wv_component.qp_infos[qp].size;

        init_data->order = qp;
        init_data->list = &device->qps[qp].recv_free;

        if(OMPI_SUCCESS != ompi_free_list_init_ex_new(init_data->list,
                    sizeof(mca_btl_wv_recv_frag_t), opal_cache_line_size,
                    OBJ_CLASS(mca_btl_wv_recv_frag_t),
                    length, mca_btl_wv_component.buffer_alignment,
                    mca_btl_wv_component.ib_free_list_num,
                    mca_btl_wv_component.ib_free_list_max,
                    mca_btl_wv_component.ib_free_list_inc,
                    device->mpool, mca_btl_wv_frag_init,
                    init_data)) {
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

static int get_port_list(mca_btl_wv_device_t *device, int *allowed_ports)
{
    int i, j, k, num_ports = 0;
    const char *dev_name;
    char *name;

    dev_name = device->ib_dev->name;

    name = (char*) malloc(strlen(dev_name) + 4);
    if (NULL == name) {
        return 0;
    }

    /* Assume that all ports are allowed.  num_ports will be adjusted
       below to reflect whether this is true or not. */
    for (i = 1; i <= device->ib_dev_attr.PhysPortCount; ++i) {
        allowed_ports[num_ports++] = i;
    }
    num_ports = 0;
    if (NULL != mca_btl_wv_component.if_include_list) {
        /* If only the device name is given (eg. mtdevice0,mtdevice1) use all
           ports */
        i = 0;
        while (mca_btl_wv_component.if_include_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_btl_wv_component.if_include_list[i])) {
                            num_ports = device->ib_dev_attr.PhysPortCount;
                goto done;
            }
            ++i;
        }
        /* Include only requested ports on the device */
        for (i = 1; i <= device->ib_dev_attr.PhysPortCount; ++i) {
            sprintf(name,"%s:%d",dev_name,i);
            for (j = 0;
                 NULL != mca_btl_wv_component.if_include_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_btl_wv_component.if_include_list[j])) {
                    allowed_ports[num_ports++] = i;
                    break;
                }
            }
        }
    } else if (NULL != mca_btl_wv_component.if_exclude_list) {
        /* If only the device name is given (eg. mtdevice0,mtdevice1) exclude
           all ports */
        i = 0;
        while (mca_btl_wv_component.if_exclude_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_btl_wv_component.if_exclude_list[i])) {
                num_ports = 0;
                goto done;
            }
            ++i;
        }
        /* Exclude the specified ports on this device */
        for (i = 1; i <= device->ib_dev_attr.PhysPortCount; ++i) {
            sprintf(name,"%s:%d",dev_name,i);
            for (j = 0;
                 NULL != mca_btl_wv_component.if_exclude_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_btl_wv_component.if_exclude_list[j])) {
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
        num_ports = device->ib_dev_attr.PhysPortCount;
    }

done:

    /* Remove the following from the error-checking if_list:
       - bare device name
       - device name suffixed with port number */
    if (NULL != mca_btl_wv_component.if_list) {
        for (i = 0; NULL != mca_btl_wv_component.if_list[i]; ++i) {

            /* Look for raw device name */
            if (0 == strcmp(mca_btl_wv_component.if_list[i], dev_name)) {
                j = opal_argv_count(mca_btl_wv_component.if_list);
                opal_argv_delete(&j, &(mca_btl_wv_component.if_list),
                                 i, 1);
                --i;
            }
        }
        for (i = 1; i <= device->ib_dev_attr.PhysPortCount; ++i) {
            sprintf(name, "%s:%d", dev_name, i);
            for (j = 0; NULL != mca_btl_wv_component.if_list[j]; ++j) {
                if (0 == strcmp(mca_btl_wv_component.if_list[j], name)) {
                    k = opal_argv_count(mca_btl_wv_component.if_list);
                    opal_argv_delete(&k, &(mca_btl_wv_component.if_list),
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
static void merge_values(ompi_btl_wv_ini_values_t *target,
                         ompi_btl_wv_ini_values_t *src)
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

static bool inline is_credit_message(const mca_btl_wv_recv_frag_t *frag)
{
    mca_btl_wv_control_header_t* chdr =
        (mca_btl_wv_control_header_t *) to_base_frag(frag)->segment.seg_addr.pval;
    return (MCA_BTL_TAG_BTL == frag->hdr->tag) &&
        (MCA_BTL_WV_CONTROL_CREDITS == chdr->type);
}

static bool inline is_cts_message(const mca_btl_wv_recv_frag_t *frag)
{
    mca_btl_wv_control_header_t* chdr =
        (mca_btl_wv_control_header_t *) to_base_frag(frag)->segment.seg_addr.pval;
    return (MCA_BTL_TAG_BTL == frag->hdr->tag) &&
        (MCA_BTL_WV_CONTROL_CTS == chdr->type);
}

static int32_t atoi_param(char *param, int32_t dflt)
{
    if (NULL == param || '\0' == param[0]) {
        return dflt ? dflt : 1;
    }

    return atoi(param);
}

static void init_apm_port(mca_btl_wv_device_t *device, int port, uint16_t lid)
{
    int index;
    struct mca_btl_wv_module_t *btl;
    for(index = 0; index < device->btls; index++) {
        btl = (mca_btl_wv_module_t *) opal_pointer_array_get_item(device->device_btls, index);
        /* Ok, we already have btl for the fist port,
         * second one will be used for APM */
        btl->apm_port = port;
        btl->port_info.apm_lid = lid + btl->src_path_bits;
        mca_btl_wv_component.apm_ports++;
        BTL_VERBOSE(("APM-PORT: Setting alternative port - %d, lid - %d"
                    ,port ,lid));
    }
}

static int setup_qps(void)
{
    char **queues, **params = NULL;
    int  num_pp_qps = 0, num_srq_qps = 0, qp = 0; 
    uint32_t max_qp_size, max_size_needed;
    int32_t min_freelist_size = 0;
    int smallest_pp_qp = 0, ret = OMPI_ERROR;

    queues = opal_argv_split(mca_btl_wv_component.receive_queues, ':');
    if (0 == opal_argv_count(queues)) {
        orte_show_help("help-mpi-btl-wv.txt",
                       "no qps in receive_queues", true,
                       orte_process_info.nodename, 
                       mca_btl_wv_component.receive_queues);
        ret = OMPI_ERROR;
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
        }else {
            orte_show_help("help-mpi-btl-wv.txt",
                           "invalid qp type in receive_queues", true,
                           orte_process_info.nodename, 
                           mca_btl_wv_component.receive_queues,
                           queues[qp]);
            ret = OMPI_ERR_BAD_PARAM;
            goto error;
        }
        qp++;
    }
    mca_btl_wv_component.num_pp_qps = num_pp_qps;
    mca_btl_wv_component.num_srq_qps = num_srq_qps;
    mca_btl_wv_component.num_qps = num_pp_qps + num_srq_qps; 

    mca_btl_wv_component.qp_infos = (mca_btl_wv_qp_info_t*)
        malloc(sizeof(mca_btl_wv_qp_info_t) *
                mca_btl_wv_component.num_qps);

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
                orte_show_help("help-mpi-btl-wv.txt",
                               "invalid pp qp specification", true,
                               orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }
            mca_btl_wv_component.qp_infos[qp].type = MCA_BTL_WV_PP_QP;
            mca_btl_wv_component.qp_infos[qp].size = atoi_param(P(1), 0);
            rd_num = atoi_param(P(2), 256);
            /* by default set rd_low to be 3/4 of rd_num */
            rd_low = atoi_param(P(3), rd_num - (rd_num / 4));
            rd_win = atoi_param(P(4), (rd_num - rd_low) * 2);
            rd_rsv = atoi_param(P(5), (rd_num * 2) / rd_win);

            BTL_VERBOSE(("pp: rd_num is %d rd_low is %d rd_win %d rd_rsv %d",
                         rd_num, rd_low, rd_win, rd_rsv));

            /* Calculate the smallest freelist size that can be allowed */
            if (rd_num + rd_rsv > min_freelist_size) {
                min_freelist_size = rd_num + rd_rsv;
            }

            mca_btl_wv_component.qp_infos[qp].u.pp_qp.rd_win = rd_win;
            mca_btl_wv_component.qp_infos[qp].u.pp_qp.rd_rsv = rd_rsv;
            if ((rd_num - rd_low) > rd_win) {
                orte_show_help("help-mpi-btl-wv.txt", "non optimal rd_win",
                        true, rd_win, rd_num - rd_low);
            }
        } else {
            int32_t sd_max, rd_init, srq_limit;
            if (count < 3 || count > 7) {
                orte_show_help("help-mpi-btl-wv.txt",
                               "invalid srq specification", true,
                               orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }
            mca_btl_wv_component.qp_infos[qp].type = MCA_BTL_WV_SRQ_QP;
            mca_btl_wv_component.qp_infos[qp].size = atoi_param(P(1), 0);
            rd_num = atoi_param(P(2), 256);
            /* by default set rd_low to be 3/4 of rd_num */
            rd_low = atoi_param(P(3), rd_num - (rd_num / 4));
            sd_max = atoi_param(P(4), rd_low / 4);
            /* rd_init is initial value for rd_curr_num of all SRQs, 1/4 of rd_num by default */
            rd_init = atoi_param(P(5), rd_num / 4);
            /* by default set srq_limit to be 3/16 of rd_init (it's 1/4 of rd_low_local,
               the value of rd_low_local we calculate in create_srq function) */
            srq_limit = atoi_param(P(6), (rd_init - (rd_init / 4)) / 4);
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
                orte_show_help("help-mpi-btl-wv.txt", "rd_num must be >= rd_init",
                        true, orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }

            if (rd_num < srq_limit) {
                orte_show_help("help-mpi-btl-wv.txt", "srq_limit must be > rd_num",
                        true, orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }

            mca_btl_wv_component.qp_infos[qp].u.srq_qp.sd_max = sd_max;
            mca_btl_wv_component.qp_infos[qp].u.srq_qp.rd_init = rd_init;
            mca_btl_wv_component.qp_infos[qp].u.srq_qp.srq_limit = srq_limit;
        }

        if (rd_num <= rd_low) {
            orte_show_help("help-mpi-btl-wv.txt", "rd_num must be > rd_low",
                    true, orte_process_info.nodename, queues[qp]);
            ret = OMPI_ERR_BAD_PARAM;
            goto error;
        }
        mca_btl_wv_component.qp_infos[qp].rd_num = rd_num;
        mca_btl_wv_component.qp_infos[qp].rd_low = rd_low;
        opal_argv_free(params);
        qp++;
    }
    params = NULL;

    /* Sanity check some sizes */

    max_qp_size = mca_btl_wv_component.qp_infos[mca_btl_wv_component.num_qps - 1].size;
    max_size_needed = (mca_btl_wv_module.super.btl_eager_limit >
                       mca_btl_wv_module.super.btl_max_send_size) ?
        mca_btl_wv_module.super.btl_eager_limit :
        mca_btl_wv_module.super.btl_max_send_size;
    if (max_qp_size < max_size_needed) {
        orte_show_help("help-mpi-btl-wv.txt",
                       "biggest qp size is too small", true,
                       orte_process_info.nodename, max_qp_size,
                       max_size_needed);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    } else if (max_qp_size > max_size_needed) {
        orte_show_help("help-mpi-btl-wv.txt",
                       "biggest qp size is too big", true,
                       orte_process_info.nodename, max_qp_size,
                       max_size_needed);
    }

    if (mca_btl_wv_component.ib_free_list_max > 0 &&
        min_freelist_size > mca_btl_wv_component.ib_free_list_max) {
        orte_show_help("help-mpi-btl-wv.txt", "freelist too small", true,
                       orte_process_info.nodename,
                       mca_btl_wv_component.ib_free_list_max,
                       min_freelist_size);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    }

    mca_btl_wv_component.rdma_qp = mca_btl_wv_component.num_qps - 1;
    mca_btl_wv_component.credits_qp = smallest_pp_qp;

    ret = OMPI_SUCCESS;
error:
    if (NULL != params) {
        opal_argv_free(params);
    }

    if (NULL != queues) {
        opal_argv_free(queues);
    }

    return ret;
}

static int init_one_device(opal_list_t *btl_list, struct wv_device* ib_dev)
{
    struct mca_mpool_base_resources_t mpool_resources;
    mca_btl_wv_device_t *device;
    uint8_t i, k = 0;
    int ret = -1, port_cnt;
    ompi_btl_wv_ini_values_t values, default_values;
    int *allowed_ports = NULL;
    bool need_search;
    struct wverbs_device *vdev;
    struct wverbs_context *vcontext;
    HRESULT hr;

    device = OBJ_NEW(mca_btl_wv_device_t);
    if(NULL == device){
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    device->ib_dev = ib_dev;

    vdev = CONTAINING_RECORD(ib_dev, struct wverbs_device, device);
    vcontext = (struct wverbs_context*)malloc(sizeof(struct wverbs_context));
    if (vcontext == NULL) {
        return NULL;
    }

    memcpy(&vcontext->device, vdev, sizeof(struct wverbs_device));
    vcontext->context.device = &vcontext->device.device;
    vcontext->event_port = NULL;
    vcontext->closing = 0;

    vcontext->port = (struct wverbs_port*)malloc(vdev->phys_port_cnt*sizeof(struct wverbs_port));

    hr = prov->OpenDevice(vdev->guid, &vcontext->context.device_if);
    if (FAILED(hr)) {
        goto error;
    }

    for (i = 0; i < vdev->phys_port_cnt; i++) {
        vcontext->port[i].port_num = (uint8_t) i + 1;
        vcontext->port[i].event_flag = 0;
        vcontext->context.device_if->Notify(vcontext->port[i].port_num,
                                            &vcontext->port[i].comp_entry.Overlap,
                                            &vcontext->port[i].event_flag);
    }
    device->ib_dev_context = &vcontext->context;
    
    device->ib_pd = NULL;
    device->device_btls = OBJ_NEW(opal_pointer_array_t);
    if (OPAL_SUCCESS != opal_pointer_array_init(device->device_btls, 2, INT_MAX, 2)) {
        BTL_ERROR(("Failed to initialize device_btls array: %s:%d", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if(NULL == device->ib_dev_context){
        BTL_ERROR(("error obtaining device context for %s errno says %s",
                   device->ib_dev->name, strerror(errno)));
        goto error;
    }

    hr = device->ib_dev_context->device_if->Query(&device->ib_dev_attr);
    if (FAILED(hr)) {
        BTL_ERROR(("error obtaining device attributes for %s errno says %s",
                   device->ib_dev->name, strerror(errno)));
        goto error;
    }            

    /* If mca_btl_if_include/exclude were specified, get usable ports */
    allowed_ports = (int*)malloc(device->ib_dev_attr.PhysPortCount * sizeof(int));
    port_cnt = get_port_list(device, allowed_ports);
    if (0 == port_cnt) {
        free(allowed_ports);
        ret = OMPI_SUCCESS;
        goto error;
    }

    /* Load in vendor/part-specific device parameters.  Note that even if
       we don't find values for this vendor/part, "values" will be set
       indicating that it does not have good values */
    ret = ompi_btl_wv_ini_query(device->ib_dev_attr.VendorId,
                                device->ib_dev_attr.VendorPartId,
                                &values);
    if (OMPI_SUCCESS != ret &&
        OMPI_ERR_NOT_FOUND != OPAL_SOS_GET_ERROR_CODE(ret)) {
        /* If we get a serious error, propagate it upwards */
        goto error;
    }
    if (OMPI_ERR_NOT_FOUND == OPAL_SOS_GET_ERROR_CODE(ret)) {
        /* If we didn't find a matching device in the INI files, output a
           warning that we're using default values (unless overridden
           that we don't want to see these warnings) */
        if (mca_btl_wv_component.warn_no_device_params_found) {
            orte_show_help("help-mpi-btl-wv.txt",
                           "no device params found", true,
                           orte_process_info.nodename,
                           device->ib_dev->name,
                           device->ib_dev_attr.VendorId,
                           device->ib_dev_attr.VendorPartId);
        }
    }
    /* Note that even if we don't find default values, "values" will
       be set indicating that it does not have good values */
    ret = ompi_btl_wv_ini_query(0, 0, &default_values);
    if (OMPI_SUCCESS != ret &&
        OMPI_ERR_NOT_FOUND != OPAL_SOS_GET_ERROR_CODE(ret)) {
        /* If we get a serious error, propagate it upwards */
        goto error;
    }

    /* If we did find values for this device (or in the defaults
       section), handle them */
    merge_values(&values, &default_values);
    if (values.mtu_set) {
        switch (values.mtu) {
        case 256:
            device->mtu = WV_MTU_256; 
            break;
        case 512:
            device->mtu = WV_MTU_512;
            break;
        case 1024:
            device->mtu = WV_MTU_1024;
            break;
        case 2048:
            device->mtu = WV_MTU_2048;
            break;
        case 4096:
            device->mtu = WV_MTU_4096;
            break;
        default:
            BTL_ERROR(("invalid MTU value specified in INI file (%d); ignored", values.mtu));
            device->mtu = mca_btl_wv_component.ib_mtu;
            break;
        }
    } else {
        device->mtu = mca_btl_wv_component.ib_mtu;
    }

    device->ib_pd = (struct wv_pd*)malloc(sizeof(wv_pd));
    hr = device->ib_dev_context->device_if->AllocateProtectionDomain(&device->ib_pd->handle);
    device->ib_pd->context = device->ib_dev_context;
    
    if(NULL == device->ib_pd){
        BTL_ERROR(("error allocating protection domain for %s errno says %s",
                   device->ib_dev->name, strerror(errno)));
        goto error;
    }

    /* Figure out what the max_inline_data value should be for all
       ports and QPs on this device */
    need_search = false;
    if(-2 != mca_btl_wv_component.ib_max_inline_data) {
        /* User has explicitly set btl_wv_max_inline_data MCA parameter 
           Per setup in _mca.c, we know that the MCA param value is guaranteed
           to be >= -1 */
        if (-1 == mca_btl_wv_component.ib_max_inline_data) {
            need_search = true;
        } else {
            device->max_inline_data = (uint32_t) 
                mca_btl_wv_component.ib_max_inline_data;
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
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            } 
        }
    }
    /* Should we use RDMA for short / eager messages?  First check MCA
       param, then check INI file values. */
    if (mca_btl_wv_component.use_eager_rdma >= 0) {
        device->use_eager_rdma = mca_btl_wv_component.use_eager_rdma;
    } else if (values.use_eager_rdma_set) {
        device->use_eager_rdma = values.use_eager_rdma;
    }

    mpool_resources.reg_data = (void*)device;
    mpool_resources.sizeof_reg = sizeof(mca_btl_wv_reg_t);
    mpool_resources.register_mem = wv_reg_mr;
    mpool_resources.deregister_mem = wv_dereg_mr;
    device->mpool =
        mca_mpool_base_module_create(mca_btl_wv_component.ib_mpool_name,
                device, &mpool_resources);
    if(NULL == device->mpool){
        /* Don't print an error message here -- we'll get one from
           mpool_create anyway (OPAL_SOS would be good here...) */
         goto error;
    }

    ret = OMPI_SUCCESS;
    for(k = 0; k < port_cnt; k++){
        WV_PORT_ATTRIBUTES ib_port_attr;
        HRESULT hr;
        i = allowed_ports[k];
        hr = device->ib_dev_context->device_if->QueryPort(i, &ib_port_attr);
        if (FAILED(hr)) {
            BTL_ERROR(("error getting port attributes for device %s "
                        "port number %d errno says %s",
                        device->ib_dev->name, i, strerror(errno)));
            break;
        }
        if(WvPortActive == ib_port_attr.State) {
        /* Select the lower of the HCA and port active speed. With QLogic
               HCAs that are capable of 4K MTU we had an issue when connected
               to switches with 2K MTU. This fix is valid for other IB vendors
               as well. */
            if (wv_convert_mtu(ib_port_attr.ActiveMtu) < device->mtu){
                device->mtu = wv_convert_mtu(ib_port_attr.ActiveMtu);
            }
            if (mca_btl_wv_component.apm_ports && device->btls > 0) {
                init_apm_port(device, i, ntohs(ib_port_attr.Lid));
                break;
            }
            if (0 == mca_btl_wv_component.ib_pkey_val) {
                ret = init_one_port(btl_list, device, i, 0, &ib_port_attr);
            } else {
                uint16_t pkey,j;
                for (j = 0; j < (uint16_t)device->ib_dev_attr.MaxPkeys; j++) {
                    {
                        HRESULT hr = 0;
                        hr = device->ib_dev_context->device_if->QueryPkey(i,(UINT16)j,&pkey);
                        if(FAILED(hr)) {
                            BTL_ERROR(("error getting pkey for index %d, device %s "
                                       "port number %d errno says %s",
                                       j, device->ib_dev->name, i, strerror(errno)));
                        }
                    }
                    pkey = ntohs(pkey) & MCA_BTL_IB_PKEY_MASK;
                    if(pkey == mca_btl_wv_component.ib_pkey_val){
                        ret = init_one_port(btl_list, device, i, j, &ib_port_attr);
                        break;
                    }
                }
            }
            if (OMPI_SUCCESS != ret) {
                /* Out of bounds error indicates that we hit max btl number
                 * don't propagate the error to the caller */
                if (OMPI_ERR_VALUE_OUT_OF_BOUNDS == OPAL_SOS_GET_ERROR_CODE(ret)) {
                    ret = OMPI_SUCCESS;
                }
                break;
            }
        }
    }
    free(allowed_ports);

    /* If we made a BTL, check APM status and return.  Otherwise, fall
       through and destroy everything */
    if (device->btls > 0) {
        /* if apm was enabled it should be > 1 */
        if (1 == mca_btl_wv_component.apm_ports) {
            orte_show_help("help-mpi-btl-wv.txt",
                           "apm not enough ports", true);
            mca_btl_wv_component.apm_ports = 0;
        }

        /* Check to ensure that all devices used in this process have
           compatible receive_queues values (we check elsewhere to see
           if all devices used in other processes in this job have
           compatible receive_queues values).

           Not only is the check complex, but the reasons behind what
           it does (and does not do) are complex.  Before explaining
           the code below, here's some notes:

           1. The wv BTL component only supports 1 value of the
              receive_queues between all of its modules.

              --> This could be changed to allow every module to have
                  its own receive_queues.  But that would be a big
                  deal; no one has time to code this up right now.

           2. The receive_queues value can be specified either as an
              MCA parameter or in the INI file.  Specifying the value
              as an MCA parameter overrides all INI file values
              (meaning: that MCA param value will be used for all
              wv BTL modules in the process).

           Effectively, the first device through init_one_device()
           gets to decide what the receive_queues will be for the all
           modules in this process.  This is an unfortunate artifact
           of the wv BTL startup sequence (see below for more
           details).  The first device will choose the receive_queues
           value from: (in priority order): 

           1. If the btl_wv_receive_queues MCA param was
              specified, use that.
           2. If this device has a receive_queues value specified in
              the INI file, use that.
           3. Otherwise, use the default MCA param value for
              btl_wv_receive_queues.

           If any successive device has a different value specified in
           the INI file, we show_help and return up the stack that
           this device failed.

           In the case that the user does not specify a
           mca_btl_wv_receive_queues value, the short description
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

               2. Conside a scenario with server 1 having HCA A/subnet
                  X, and server 2 having HCA B/subnet X and HCA
                  C/subnet Y.  And let's assume:

                  Server 1:
                  HCA A: no receive_queues in INI file

                  Server 2:
                  HCA B: no receive_queues in INI file
                  HCA C: receive_queues specified in INI file
                
                  A will therefore use the default receive_queues
                  value.  B and C will use C's INI receive_queues.
                  But note that modex [currently] only sends around
                  vendor/part IDs for OpenFabrics devices -- not the
                  actual receive_queues value (it was felt that
                  including the final receive_queues string value in
                  the modex would dramatically increase the size of
                  the modex).  So processes on server 1 will get the
                  vendor/part ID for HCA B, look it up in the INI
                  file, see that it has no receive_queues value
                  specified, and then assume that it uses the default
                  receive_queues value.  Hence, procs on server 1 will
                  try to connect HCA A-->HCA B with the wrong
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

        /* If the MCA param was specified, skip all the checks */
        if (BTL_WV_RQ_SOURCE_MCA ==
            mca_btl_wv_component.receive_queues_source) {
            goto good;
        }

        /* If we're the first device and we have a receive_queues
           value from the INI file *that is different than the
           already-existing default value*, then set the component to
           use that. */
        if (0 == mca_btl_wv_component.devices_count) {
            if (NULL != values.receive_queues &&
                0 != strcmp(values.receive_queues,
                            mca_btl_wv_component.receive_queues)) {
                if (NULL != mca_btl_wv_component.receive_queues) {
                    free(mca_btl_wv_component.receive_queues);
                }
                mca_btl_wv_component.receive_queues =
                    strdup(values.receive_queues);
                mca_btl_wv_component.receive_queues_source =
                    BTL_WV_RQ_SOURCE_DEVICE_INI;
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
                                mca_btl_wv_component.receive_queues)) {
                    orte_show_help("help-mpi-btl-wv.txt",
                                   "locally conflicting receive_queues", true,
                                   opal_install_dirs.pkgdatadir,
                                   orte_process_info.nodename,
                                   receive_queues_device->ib_dev->name,
                                   receive_queues_device->ib_dev_attr.VendorId,
                                   receive_queues_device->ib_dev_attr.VendorPartId,
                                   mca_btl_wv_component.receive_queues,
                                   device->ib_dev->name,
                                   device->ib_dev_attr.VendorId,
                                   device->ib_dev_attr.VendorPartId,
                                   values.receive_queues);
                    ret = OMPI_ERR_RESOURCE_BUSY;
                    goto error;
                }
            }

            /* If this device doesn't have an INI receive_queues
               value, then if the component.receive_queues value came
               from the default, we're ok.  But if the
               component.receive_queues value came from the 1st
               device's INI file, we must error. */
            else if (BTL_WV_RQ_SOURCE_DEVICE_INI ==
                mca_btl_wv_component.receive_queues_source) {
                orte_show_help("help-mpi-btl-wv.txt",
                               "locally conflicting receive_queues", true,
                               opal_install_dirs.pkgdatadir,
                               orte_process_info.nodename,
                               receive_queues_device->ib_dev->name,
                               receive_queues_device->ib_dev_attr.VendorId,
                               receive_queues_device->ib_dev_attr.VendorPartId,
                               mca_btl_wv_component.receive_queues,
                               device->ib_dev->name,
                               device->ib_dev_attr.VendorId,
                               device->ib_dev_attr.VendorPartId,
                               mca_btl_wv_component.default_recv_qps);
                ret = OMPI_ERR_RESOURCE_BUSY;
                goto error;
            }
        }

        receive_queues_device = device;

    good:
        mca_btl_wv_component.devices_count++;
        return OMPI_SUCCESS;
    }

error:
    if (device->mpool) {
        mca_mpool_base_module_destroy(device->mpool);
    }
    if (device->ib_pd) {
        device->ib_pd->handle->Release();
        free(device->ib_pd);
    }

    if (OMPI_SUCCESS != ret) {
        orte_show_help("help-mpi-btl-wv.txt",
                       "error in device init", true, 
                       orte_process_info.nodename,
                       device->ib_dev->name);
    }
    device->ib_dev_context->device_if->Release();
    OBJ_RELEASE(device);
    return ret;
}

static int finish_btl_init(mca_btl_wv_module_t *wv_btl)
{
    int qp;
    wv_btl->num_peers = 0;

    /* Initialize module state */
    OBJ_CONSTRUCT(&wv_btl->ib_lock, opal_mutex_t);

    /* setup the qp structure */
    wv_btl->qps = (mca_btl_wv_module_qp_t*)
        calloc(mca_btl_wv_component.num_qps,
                sizeof(mca_btl_wv_module_qp_t));

    /* setup all the qps */
    for (qp = 0; qp < mca_btl_wv_component.num_qps; qp++) {
        if (!BTL_WV_QP_TYPE_PP(qp)) {
            OBJ_CONSTRUCT(&wv_btl->qps[qp].u.srq_qp.pending_frags[0],
                    opal_list_t);
            OBJ_CONSTRUCT(&wv_btl->qps[qp].u.srq_qp.pending_frags[1],
                    opal_list_t);
            wv_btl->qps[qp].u.srq_qp.sd_credits =
                mca_btl_wv_component.qp_infos[qp].u.srq_qp.sd_max;
            wv_btl->qps[qp].u.srq_qp.srq = NULL;
        }
    }

    /* initialize the memory pool using the device */
    wv_btl->super.btl_mpool = wv_btl->device->mpool;

    wv_btl->eager_rdma_channels = 0;

    wv_btl->eager_rdma_frag_size = OPAL_ALIGN(
            sizeof(mca_btl_wv_header_t) +
            sizeof(mca_btl_wv_header_coalesced_t) +
            sizeof(mca_btl_wv_control_header_t) +
            sizeof(mca_btl_wv_footer_t) +
            wv_btl->super.btl_eager_limit,
            mca_btl_wv_component.buffer_alignment, size_t);

    return OMPI_SUCCESS;
}

static struct wv_device **wv_get_device_list_compat(int *num_devs)
{
    struct wv_device **ib_devs;
    WV_DEVICE_ATTRIBUTES attr;
    struct wverbs_device *dev_array;
    NET64 *guid;
    SIZE_T size, cnt;
    HRESULT hr;
    
    hr = WvGetObject(IID_IWVProvider,(LPVOID*)&prov);
    if(FAILED(hr))
        goto err1;
    cnt = 0;
    size = sizeof(NET64);
    while ((size / sizeof(NET64)) > cnt) {
        if (cnt > 0) {
            free(guid);
        }
        cnt = size / sizeof(NET64);
        guid = (NET64*)malloc(cnt*sizeof(NET64)) ;
        if (guid == NULL) {
            goto err1;
        }
        hr = prov->QueryDeviceList(guid, &size);
        if (FAILED(hr)) {
            goto err2;
        }
    }
    size /= sizeof(NET64);
    dev_array = (struct wverbs_device*) malloc(size*sizeof(wverbs_device));
    ib_devs = (struct wv_device**)malloc((size + 1)*sizeof(wv_device));
    if (dev_array == NULL || ib_devs == NULL) {
        goto err2;
    }
    for (cnt = 0; cnt < size; cnt++) {
        ib_devs[cnt] = &dev_array[cnt].device;
        hr = prov->QueryDevice(guid[cnt], &attr);
        if (FAILED(hr)) {
            goto err3;
        }
        sprintf(dev_array[cnt].device.name, "ib_device%d", cnt);
        dev_array[cnt].device.node_type = WV_NODE_UNKNOWN;
        dev_array[cnt].device.transport_type = (WV_DEVICE_TYPE) attr.DeviceType;
        dev_array[cnt].guid = guid[cnt];
        dev_array[cnt].phys_port_cnt = attr.PhysPortCount;
    }
    ib_devs[cnt] = NULL;
    *num_devs = (int) size;
    return ib_devs;
err3:
    free(ib_devs);
err2:
    free(guid);
err1:
    return NULL;
}

static void wv_free_device_list_compat(struct wv_device **ib_devs)
{
    free(ib_devs);
}

static opal_carto_graph_t *host_topo;

static int get_ib_dev_distance(struct wv_device *dev)
{
    opal_paffinity_base_cpu_set_t cpus;
    opal_carto_base_node_t *device_node;
    int min_distance = -1, i, num_processors;
    const char *device = dev->name;

    if(opal_paffinity_base_get_processor_info(&num_processors) != OMPI_SUCCESS) {
        num_processors = 100; /* Choose something big enough */
    }

    device_node = opal_carto_base_find_node(host_topo, device);

    /* no topology info for device found. Assume that it is close */
    if(NULL == device_node)
        return 0;

    OPAL_PAFFINITY_CPU_ZERO(cpus);
    opal_paffinity_base_get(&cpus);

    for (i = 0; i < num_processors; i++) {
        opal_carto_base_node_t *slot_node;
        int distance, socket, core;
        char *slot;

        if(!OPAL_PAFFINITY_CPU_ISSET(i, cpus))
            continue;

        opal_paffinity_base_get_map_to_socket_core(i, &socket, &core);
        asprintf(&slot, "slot%d", socket);

        slot_node = opal_carto_base_find_node(host_topo, slot);

        free(slot);

        if(NULL == slot_node)
            return 0;

        distance = opal_carto_base_spf(host_topo, slot_node, device_node);

        if(distance < 0)
            return 0;

        if(min_distance < 0 || min_distance > distance)
            min_distance = distance;
    }

    return min_distance;
}

struct dev_distance {
    struct wv_device *ib_dev;
    int distance;
};

static int compare_distance(const void *p1, const void *p2)
{
    const struct dev_distance *d1 = (const struct dev_distance *) p1;
    const struct dev_distance *d2 = (const struct dev_distance *) p2;

    return d1->distance - d2->distance;
}

static struct dev_distance *
sort_devs_by_distance(struct wv_device **ib_devs, int count)
{
    int i;
    struct dev_distance *devs = (struct dev_distance *) malloc(count * sizeof(struct dev_distance));

    /* 
     * if we have found more than one device, search for the shortest path.
     * otherwise, just use the one we got
     */
    if( count == 1) {
        devs[0].ib_dev = ib_devs[0];
        devs[0].distance = 0;

        return devs;
    }
    opal_carto_base_get_host_graph(&host_topo, "Infiniband");

    for (i = 0; i < count; i++) {
        devs[i].ib_dev = ib_devs[i];
        devs[i].distance = get_ib_dev_distance(ib_devs[i]);
    }

    qsort(devs, count, sizeof(struct dev_distance), compare_distance);

    opal_carto_base_free_graph(host_topo);

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
btl_wv_component_init(int *num_btl_modules,
                      bool enable_progress_threads,
                      bool enable_mpi_threads)
{
    struct wv_device **ib_devs;
    mca_btl_base_module_t** btls;
    int i, ret, num_devs, length;
    opal_list_t btl_list;
    mca_btl_wv_module_t * wv_btl;
    mca_btl_base_selected_module_t* ib_selected;
    opal_list_item_t* item;
    mca_btl_wv_frag_init_data_t *init_data;
    struct dev_distance *dev_sorted;
    int distance;
    int index, value;
    bool found;
    mca_base_param_source_t source;
    int list_count = 0;

    /* initialization */
    *num_btl_modules = 0;
    num_devs = 0;

    /* Read in INI files with device-specific parameters */
    if (OMPI_SUCCESS != (ret = ompi_btl_wv_ini_init())) {
        goto no_btls;
    }

    /* Init CPC components */
    if (OMPI_SUCCESS != (ret = ompi_btl_wv_connect_base_init())) {
        goto no_btls;
    }
    /* If we are using ptmalloc2 and there are no posix threads
       available, this will cause memory corruption.  Refuse to run.
       Right now, ptmalloc2 is the only memory manager that we have on
       OS's that support OpenFabrics that provide both FREE and MUNMAP
       support, so the following test is [currently] good enough... */
    value = opal_mem_hooks_support_level();
    /* If we have a memory manager available, and
       mpi_leave_pinned==-1, then unless the user explicitly set
       mpi_leave_pinned_pipeline==0, then set mpi_leave_pinned to 1.

       We have a memory manager if we have both FREE and MUNMAP
       support */
    if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) == 
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) & value)) {
        ret = 0;
        index = mca_base_param_find("mpi", NULL, "leave_pinned");
        if (index >= 0) {
            if (OPAL_SUCCESS == mca_base_param_lookup_int(index, &value) &&
                -1 == value) {
                ++ret;
            }
        }
        index = mca_base_param_find("mpi", NULL, "leave_pinned_pipeline");
        if (index >= 0) {
            if (OPAL_SUCCESS == mca_base_param_lookup_int(index, &value) &&
                OPAL_SUCCESS == mca_base_param_lookup_source(index, &source, 
                                                             NULL)) {
                if (0 == value && MCA_BASE_PARAM_SOURCE_DEFAULT == source) {
                    ++ret;
                }
            }
        }
        /* If we were good on both parameters, then set leave_pinned=1 */
        if (2 == ret) {
            ompi_mpi_leave_pinned = 1;
            ompi_mpi_leave_pinned_pipeline = 0;
        }
    }
    index = mca_base_param_find("btl", "wv", "max_inline_data");
    if (index >= 0) {
        if (OPAL_SUCCESS == mca_base_param_lookup_source(index, &source,
                                                         NULL)) {
            if (-1 == mca_btl_wv_component.ib_max_inline_data  && 
                MCA_BASE_PARAM_SOURCE_DEFAULT == source) {
                /* If the user has not explicitly set this MCA parameter
                   use max_inline_data value specified in the 
                   device-specific parameters INI file */
                mca_btl_wv_component.ib_max_inline_data = -2;
            }
        }
    }
    index = mca_base_param_find("btl","wv","flags");
    if (index >= 0) { 
        if (OPAL_SUCCESS == mca_base_param_lookup_int(index, &value)) { 
            if (value & MCA_BTL_FLAGS_GET) { 
                /* Until GET flow is fixed - we do not support GET 
                   in wv btl. */ 
                BTL_ERROR(("WV btl does not support GET flag")); 
            } 
        } 
    }      
    OBJ_CONSTRUCT(&mca_btl_wv_component.send_free_coalesced, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_wv_component.send_user_free, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_wv_component.recv_user_free, ompi_free_list_t);

    init_data = (mca_btl_wv_frag_init_data_t *) malloc(sizeof(mca_btl_wv_frag_init_data_t));

    init_data->order = mca_btl_wv_component.rdma_qp;
    init_data->list = &mca_btl_wv_component.send_user_free;

    if (OMPI_SUCCESS != ompi_free_list_init_ex_new(
                &mca_btl_wv_component.send_user_free,
                sizeof(mca_btl_wv_put_frag_t), 2,
                OBJ_CLASS(mca_btl_wv_put_frag_t),
                0, 0,
                mca_btl_wv_component.ib_free_list_num,
                mca_btl_wv_component.ib_free_list_max,
                mca_btl_wv_component.ib_free_list_inc,
                NULL, mca_btl_wv_frag_init, init_data)) {
        goto no_btls;
    }

    init_data = (mca_btl_wv_frag_init_data_t *) malloc(sizeof(mca_btl_wv_frag_init_data_t));

    init_data->order = mca_btl_wv_component.rdma_qp;
    init_data->list = &mca_btl_wv_component.recv_user_free;

    if(OMPI_SUCCESS != ompi_free_list_init_ex_new(
                &mca_btl_wv_component.recv_user_free,
                sizeof(mca_btl_wv_get_frag_t), 2,
                OBJ_CLASS(mca_btl_wv_get_frag_t),
                0, 0,
                mca_btl_wv_component.ib_free_list_num,
                mca_btl_wv_component.ib_free_list_max,
                mca_btl_wv_component.ib_free_list_inc,
                NULL, mca_btl_wv_frag_init, init_data)) {
        goto no_btls;
    }

    init_data = (mca_btl_wv_frag_init_data_t *) malloc(sizeof(mca_btl_wv_frag_init_data_t));
    length = sizeof(mca_btl_wv_coalesced_frag_t);

    init_data->list = &mca_btl_wv_component.send_free_coalesced;

    if(OMPI_SUCCESS != ompi_free_list_init_ex(
                &mca_btl_wv_component.send_free_coalesced,
                length, 2, OBJ_CLASS(mca_btl_wv_coalesced_frag_t),
                mca_btl_wv_component.ib_free_list_num,
                mca_btl_wv_component.ib_free_list_max,
                mca_btl_wv_component.ib_free_list_inc,
                NULL, mca_btl_wv_frag_init, init_data)) {
        goto no_btls;
    }

    /* Parse the include and exclude lists, checking for errors */
    mca_btl_wv_component.if_include_list =
        mca_btl_wv_component.if_exclude_list =
        mca_btl_wv_component.if_list = NULL;

    if (NULL != mca_btl_wv_component.if_include)
      list_count++;
    if (NULL != mca_btl_wv_component.if_exclude)
      list_count++;
    if (NULL != mca_btl_wv_component.ipaddr_include)
      list_count++;
    if (NULL != mca_btl_wv_component.ipaddr_exclude)
      list_count++;

    if (list_count > 1) {
        orte_show_help("help-mpi-btl-wv.txt",
                       "specified include and exclude", true,
                       NULL == mca_btl_wv_component.if_include ?
                        "<not specified>" : mca_btl_wv_component.if_include,
                       NULL == mca_btl_wv_component.if_exclude ?
                        "<not specified>" : mca_btl_wv_component.if_exclude,
                       NULL == mca_btl_wv_component.ipaddr_include ?
                        "<not specified>" :mca_btl_wv_component.ipaddr_include,
                       NULL == mca_btl_wv_component.ipaddr_exclude ?
                         "<not specified>" :mca_btl_wv_component.ipaddr_exclude,
                       NULL);
        goto no_btls;
    } else if (NULL != mca_btl_wv_component.if_include) {
        mca_btl_wv_component.if_include_list =
            opal_argv_split(mca_btl_wv_component.if_include, ',');
        mca_btl_wv_component.if_list =
            opal_argv_copy(mca_btl_wv_component.if_include_list);
    } else if (NULL != mca_btl_wv_component.if_exclude) {
        mca_btl_wv_component.if_exclude_list =
            opal_argv_split(mca_btl_wv_component.if_exclude, ',');
        mca_btl_wv_component.if_list =
            opal_argv_copy(mca_btl_wv_component.if_exclude_list);
    }

    ib_devs = wv_get_device_list_compat(&num_devs); 

    if(0 == num_devs || NULL == ib_devs) {
        mca_btl_base_error_no_nics("OpenFabrics (wv)", "device");
        goto no_btls;
    }

    dev_sorted = sort_devs_by_distance(ib_devs, num_devs);

    OBJ_CONSTRUCT(&btl_list, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_wv_component.ib_lock, opal_mutex_t);
    distance = dev_sorted[0].distance;
    for (found = false, i = 0; 
         i < num_devs && (-1 == mca_btl_wv_component.ib_max_btls ||
                mca_btl_wv_component.ib_num_btls <
                mca_btl_wv_component.ib_max_btls); i++) {
        if (distance != dev_sorted[i].distance) {
            break;
        }

        found = true;
        if (OMPI_SUCCESS !=
            (ret = init_one_device(&btl_list, dev_sorted[i].ib_dev))) {
            free(dev_sorted);
            goto no_btls;
        }
    }
    free(dev_sorted);
    if (!found) {
        orte_show_help("help-mpi-btl-wv.txt", "no devices right type",
                       true, orte_process_info.nodename,
                       ((BTL_WV_DT_IB == mca_btl_wv_component.device_type) ?
                        "InfiniBand" :
                        (BTL_WV_DT_IWARP == mca_btl_wv_component.device_type) ?
                        "iWARP" : "<any>"));
        goto no_btls;
    }

    /* If we got back from checking all the devices and find that
       there are still items in the component.if_list, that means that
       they didn't exist.  Show an appropriate warning if the warning
       was not disabled. */

    if (0 != opal_argv_count(mca_btl_wv_component.if_list) &&
        mca_btl_wv_component.warn_nonexistent_if) {
        char *str = opal_argv_join(mca_btl_wv_component.if_list, ',');
        orte_show_help("help-mpi-btl-wv.txt", "nonexistent port",
                       true, orte_process_info.nodename,
                       ((NULL != mca_btl_wv_component.if_include) ?
                        "in" : "ex"), str);
        free(str);
    }

    if(0 == mca_btl_wv_component.ib_num_btls) {
        orte_show_help("help-mpi-btl-wv.txt",
                "no active ports found", true, orte_process_info.nodename);
        goto no_btls;
    }

    /* Setup the BSRQ QP's based on the final value of
       mca_btl_wv_component.receive_queues. */
    if (OMPI_SUCCESS != setup_qps()) {
        goto no_btls;
    }

    /* Allocate space for btl modules */
    mca_btl_wv_component.wv_btls =
        (mca_btl_wv_module_t **) malloc(sizeof(mca_btl_wv_module_t*) *
                mca_btl_wv_component.ib_num_btls);
    if(NULL == mca_btl_wv_component.wv_btls) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }
    btls = (struct mca_btl_base_module_t **)
        malloc(mca_btl_wv_component.ib_num_btls *
               sizeof(struct mca_btl_base_module_t*));
    if(NULL == btls) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }

    /* Copy the btl module structs into a contiguous array and fully
       initialize them */
    i = 0;
    while (NULL != (item = opal_list_remove_first(&btl_list))) {
        mca_btl_wv_device_t *device;
        int qp_index;

        ib_selected = (mca_btl_base_selected_module_t*)item;
        wv_btl = (mca_btl_wv_module_t*)ib_selected->btl_module;
        device = wv_btl->device;

        /* Search for a CPC that can handle this port */
        ret = ompi_btl_wv_connect_base_select_for_local_port(wv_btl);
        /* If we get NOT_SUPPORTED, then no CPC was found for this
           port.  But that's not a fatal error -- just keep going;
           let's see if we find any usable wv modules or not. */
        if (OMPI_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(ret)) {
            continue;
        } else if (OMPI_SUCCESS != ret) {
            /* All others *are* fatal.  Note that we already did a
               show_help in the lower layer */
            goto no_btls;
        }

        if (mca_btl_wv_component.max_hw_msg_size > 0 && 
            mca_btl_wv_component.max_hw_msg_size > wv_btl->ib_port_attr.MaxMessageSize) { 
                BTL_ERROR(("max_hw_msg_size (%d) is larger than hw max message size (%d)", 
                    mca_btl_wv_component.max_hw_msg_size, wv_btl->ib_port_attr.MaxMessageSize)); 
        } 
     
        mca_btl_wv_component.wv_btls[i] = wv_btl;
        OBJ_RELEASE(ib_selected);
        btls[i] = &wv_btl->super;
        if (finish_btl_init(wv_btl) != OMPI_SUCCESS) {
            goto no_btls;
        }
        ++i;

        /* For each btl module that we made - find every
           base device that doesn't have device->qps setup on it yet (remember
           that some modules may share the same device, so when going through
           to loop, we may hit a device that was already setup earlier in
           the loop). 
           
           We may to call for prepare_device_for_use() only after adding the btl
           to mca_btl_wv_component.wv_btls, since the prepare_device_for_use 
           adds device to async thread that require access to 
           mca_btl_wv_component.wv_btls.
        */

        if (NULL == device->qps) {
            /* Setup the device qps info */
            device->qps = (mca_btl_wv_device_qp_t*)
                calloc(mca_btl_wv_component.num_qps,
                       sizeof(mca_btl_wv_device_qp_t));
            for (qp_index = 0; qp_index < mca_btl_wv_component.num_qps; qp_index++) {
                OBJ_CONSTRUCT(&device->qps[qp_index].send_free, ompi_free_list_t);
                OBJ_CONSTRUCT(&device->qps[qp_index].recv_free, ompi_free_list_t);
            }

            /* Do finial init on device */
            ret = prepare_device_for_use(device);
            if (OMPI_SUCCESS != ret) {
                orte_show_help("help-mpi-btl-wv.txt",
                               "error in device init", true, 
                               orte_process_info.nodename,
                               device->ib_dev->name);
                goto no_btls;
            }
        }
    }
    /* If we got nothing, then error out */
    if (0 == i) {
        goto no_btls;
    }
    /* Otherwise reset to the number of wv modules that we
       actually got */
    mca_btl_wv_component.ib_num_btls = i;

    btl_wv_modex_send();

    *num_btl_modules = mca_btl_wv_component.ib_num_btls;
    wv_free_device_list_compat(ib_devs); 
    if (NULL != mca_btl_wv_component.if_include_list) {
        opal_argv_free(mca_btl_wv_component.if_include_list);
        mca_btl_wv_component.if_include_list = NULL;
    }
    if (NULL != mca_btl_wv_component.if_exclude_list) {
        opal_argv_free(mca_btl_wv_component.if_exclude_list);
        mca_btl_wv_component.if_exclude_list = NULL;
    }
    
    /* setup the fork warning message as we are sensitive
     * to memory corruption issues when fork is called
     */
    ompi_warn_fork();
    
    return btls;

 no_btls:
    mca_btl_wv_component.ib_num_btls = 0;
    btl_wv_modex_send();
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
    for (qp = 0; qp < mca_btl_wv_component.num_qps; ++qp) {
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
                      !BTL_WV_QP_TYPE_PP(qp)); --len) {
                frag = opal_list_remove_first(&ep->qps[qp].no_credits_pending_frags[pri]);

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
                rc = mca_btl_wv_endpoint_post_send(ep, to_send_frag(frag));
                if (OPAL_UNLIKELY(OMPI_SUCCESS != rc &&
                                  OMPI_ERR_RESOURCE_BUSY != OPAL_SOS_GET_ERROR_CODE(rc))) {
                    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
                    return rc;
                }
            }
        }
    }

    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
    return OMPI_SUCCESS;
}

void mca_btl_wv_frag_progress_pending_put_get(mca_btl_base_endpoint_t *ep,
                                              const int qp)
{
    mca_btl_wv_module_t* wv_btl = ep->endpoint_btl;
    opal_list_item_t *frag;
    size_t i, len = opal_list_get_size(&ep->pending_get_frags);
    int rc;
    for(i = 0; i < len && ep->qps[qp].qp->sd_wqe > 0 && ep->get_tokens > 0; i++)
    {
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        frag = opal_list_remove_first(&(ep->pending_get_frags));
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(NULL == frag)
            break;
        rc = mca_btl_wv_get((mca_btl_base_module_t *)wv_btl, ep, 
                                &to_base_frag(frag)->base); 
        if(OMPI_ERR_OUT_OF_RESOURCE == OPAL_SOS_GET_ERROR_CODE(rc))            
            break;
    }

    len = opal_list_get_size(&ep->pending_put_frags);
    for(i = 0; i < len && ep->qps[qp].qp->sd_wqe > 0; i++) {
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        frag = opal_list_remove_first(&(ep->pending_put_frags));
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(NULL == frag)
            break;
        rc = mca_btl_wv_put((mca_btl_base_module_t *)wv_btl, ep, 
                                &to_base_frag(frag)->base); 
        if(OMPI_ERR_OUT_OF_RESOURCE == OPAL_SOS_GET_ERROR_CODE(rc))
            break;
    }
}

static int btl_wv_handle_incoming(mca_btl_wv_module_t *wv_btl,
                                         mca_btl_wv_endpoint_t *ep,
                                         mca_btl_wv_recv_frag_t *frag,
                                         size_t byte_len)
{
    mca_btl_base_descriptor_t *des = &to_base_frag(frag)->base;
    mca_btl_wv_header_t *hdr = frag->hdr;
    int rqp = to_base_frag(frag)->base.order, cqp;
    uint16_t rcredits = 0, credits;
    bool is_credit_msg;

    if(ep->nbo) {
        BTL_WV_HEADER_NTOH(*hdr);
    }

    /* advance the segment address past the header and subtract from the
     * length.*/
    des->des_dst->seg_len = byte_len - sizeof(mca_btl_wv_header_t);

    if(OPAL_LIKELY(!(is_credit_msg = is_credit_message(frag)))) {
        /* call registered callback */
        mca_btl_active_message_callback_t* reg;
        reg = mca_btl_base_active_message_trigger + hdr->tag;
        reg->cbfunc( &wv_btl->super, hdr->tag, des, reg->cbdata );
        if(MCA_BTL_WV_RDMA_FRAG(frag)) {
            cqp = (hdr->credits >> 11) & 0x0f;
            hdr->credits &= 0x87ff;
        } else {
            cqp = rqp;
        }
        if(BTL_WV_IS_RDMA_CREDITS(hdr->credits)) {
            rcredits = BTL_WV_CREDITS(hdr->credits);
            hdr->credits = 0;
        }
    } else {
        mca_btl_wv_rdma_credits_header_t *chdr =
            (mca_btl_wv_rdma_credits_header_t *) des->des_dst->seg_addr.pval;
        if(ep->nbo) {
            BTL_WV_RDMA_CREDITS_HEADER_NTOH(*chdr);
        }
        cqp = chdr->qpn;
        rcredits = chdr->rdma_credits;
    }

    credits = hdr->credits;

    if(hdr->cm_seen)
        OPAL_THREAD_ADD32(&ep->qps[cqp].u.pp_qp.cm_sent, -hdr->cm_seen);

    /* Now return fragment. Don't touch hdr after this point! */
    if(MCA_BTL_WV_RDMA_FRAG(frag)) {
        mca_btl_wv_eager_rdma_local_t *erl = &ep->eager_rdma_local;
        OPAL_THREAD_LOCK(&erl->lock);
        MCA_BTL_WV_RDMA_MAKE_REMOTE(frag->ftr);
        while(erl->tail != erl->head) {
            mca_btl_wv_recv_frag_t *tf;
            tf = MCA_BTL_WV_GET_LOCAL_RDMA_FRAG(ep, erl->tail);
            if(MCA_BTL_WV_RDMA_FRAG_LOCAL(tf))
                break;
            OPAL_THREAD_ADD32(&erl->credits, 1);
            MCA_BTL_WV_RDMA_NEXT_INDEX(erl->tail);
        }
        OPAL_THREAD_UNLOCK(&erl->lock);
    } else {
        if (is_cts_message(frag)) {
            /* If this was a CTS, free it here (it was
               malloc'ed+reg_mr'ed -- so it should *not* be
               FRAG_RETURN'ed). */
            int rc = ompi_btl_wv_connect_base_free_cts(ep);
            if (OMPI_SUCCESS != rc) {
                return rc;
            }
        } else {
            /* Otherwise, FRAG_RETURN it and repost if necessary */
            MCA_BTL_IB_FRAG_RETURN(frag);
            if (BTL_WV_QP_TYPE_PP(rqp)) {
                if (OPAL_UNLIKELY(is_credit_msg)) {
                    OPAL_THREAD_ADD32(&ep->qps[cqp].u.pp_qp.cm_received, 1);
                } else {
                    OPAL_THREAD_ADD32(&ep->qps[rqp].u.pp_qp.rd_posted, -1);
                }
                mca_btl_wv_endpoint_post_rr(ep, cqp);
            } else {
                mca_btl_wv_module_t *btl = ep->endpoint_btl;
                OPAL_THREAD_ADD32(&btl->qps[rqp].u.srq_qp.rd_posted, -1);
                mca_btl_wv_post_srr(btl, rqp);
            }
        }
    }

    assert((cqp != MCA_BTL_NO_ORDER && BTL_WV_QP_TYPE_PP(cqp)) || !credits);

    /* If we got any credits (RDMA or send), then try to progress all
       the no_credits_pending_frags lists */
    if (rcredits > 0) {
        OPAL_THREAD_ADD32(&ep->eager_rdma_remote.tokens, rcredits);
    }
    if (credits > 0) {
        OPAL_THREAD_ADD32(&ep->qps[cqp].u.pp_qp.sd_credits, credits);
    }
    if (rcredits + credits > 0) {
        int rc;

        if (OMPI_SUCCESS != 
            (rc = progress_no_credits_pending_frags(ep))) {
            return rc;
        }
    }

    send_credits(ep, cqp);

    return OMPI_SUCCESS;
}

static char* btl_wv_component_status_to_string(WV_WC_STATUS status)
{
    switch(status) {
    case WvWcSuccess:
        return "SUCCESS";
        break;
    case WvWcLocalLengthError:
        return "LOCAL LENGTH ERROR";
        break;
    case WvWcLocalOpError:
        return "LOCAL QP OPERATION ERROR";
        break;
    case WvWcLocalProtectionError:
        return "LOCAL PROTOCOL ERROR";
        break;
    case WvWcFlushed:
        return "WORK REQUEST FLUSHED ERROR";
        break;
    case WvWcMwBindError:
        return "MEMORY WINDOW BIND ERROR";
        break;
    case WvWcBadResponse:
        return "BAD RESPONSE ERROR";
        break;
    case WvWcLocalAccessError:
        return "LOCAL ACCESS ERROR";
        break;
    case WvWcRemoteInvalidRequest:
        return "INVALID REQUEST ERROR";
        break;
    case WvWcRemoteAccessError:
        return "REMOTE ACCESS ERROR";
        break;
    case WvWcRemoteOpError:
        return "REMOTE OPERATION ERROR";
        break;
    case -2:
        return "RETRY EXCEEDED ERROR";
        break;
    case WvWcRnrRetryError:
        return "RECEIVER NOT READY RETRY EXCEEDED ERROR";
        break;
    case -5:
        return "LOCAL RDD VIOLATION ERROR";
        break;
    case -6:
        return "INVALID READ REQUEST ERROR";
        break;
    case -3:
        return "REMOTE ABORT ERROR";
        break;
    case -7:
        return "INVALID EECN ERROR";
        break;
    case -8:
        return "INVALID EEC STATE ERROR";
        break;
    case -1:
        return "FATAL ERROR";
        break;
    case WvWcTimeoutRetryError:
        return "RESPONSE TIMEOUT ERROR";
        break;
    case WvWcError:
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
    int i;
    opal_list_item_t *frag;
    mca_btl_wv_qp_t *qp = ep->qps[qpn].qp;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);
    for(i = 0; i < 2; i++) {
       while(qp->sd_wqe > 0) {
            mca_btl_base_endpoint_t *tmp_ep;
            frag = opal_list_remove_first(&ep->qps[qpn].no_wqe_pending_frags[i]);
            if(NULL == frag)
                break;
            tmp_ep = to_com_frag(frag)->endpoint;
            mca_btl_wv_endpoint_post_send(tmp_ep, to_send_frag(frag));
       }
    }
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
}

static void progress_pending_frags_srq(mca_btl_wv_module_t* wv_btl,
                                       const int qp)
{
    opal_list_item_t *frag;
    int i;

    assert(BTL_WV_QP_TYPE_SRQ(qp));
    for(i = 0; i < 2; i++) {
        while(wv_btl->qps[qp].u.srq_qp.sd_credits > 0) {
            OPAL_THREAD_LOCK(&wv_btl->ib_lock);
            frag = opal_list_remove_first(
                    &wv_btl->qps[qp].u.srq_qp.pending_frags[i]);
            OPAL_THREAD_UNLOCK(&wv_btl->ib_lock);

            if(NULL == frag)
                break;

            mca_btl_wv_endpoint_send(to_com_frag(frag)->endpoint,
                    to_send_frag(frag));
        }
    }
}

static char *cq_name[] = {"HP CQ", "LP CQ"};
static void handle_wc(mca_btl_wv_device_t* device, const uint32_t cq,
                      WV_COMPLETION *wc)  
{
    static int flush_err_printed[] = {0, 0};
    mca_btl_wv_com_frag_t* frag;
    mca_btl_base_descriptor_t *des;
    mca_btl_wv_endpoint_t* endpoint;
    mca_btl_wv_module_t *wv_btl = NULL;
    ompi_proc_t* remote_proc = NULL;
    int qp, btl_ownership;

    des = (mca_btl_base_descriptor_t*)(uintptr_t)wc->WrId;  
    frag = to_com_frag(des);

    /* For receive fragments "order" contains QP idx the fragment was posted
     * to. For send fragments "order" contains QP idx the fragment was send
     * through */
    qp = des->order;
    endpoint = frag->endpoint;

    if(endpoint)
        wv_btl = endpoint->endpoint_btl;

    if(wc->Status != WvWcSuccess) {
        OPAL_OUTPUT((-1, "Got WC: ERROR"));
        goto error;
    }

    /* Handle work completions */
    switch(wc->Opcode) {
        case WvRdmaRead:
            OPAL_OUTPUT((-1, "Got WC: RDMA_READ"));
            OPAL_THREAD_ADD32(&endpoint->get_tokens, 1);
            /* fall through */

        case WvRdmaWrite:
        case WvSend:
            OPAL_OUTPUT((-1, "Got WC: RDMA_WRITE or SEND"));
            if(wv_frag_type(des) == MCA_BTL_WV_FRAG_SEND) {
                opal_list_item_t *i;
                while((i = opal_list_remove_first(&to_send_frag(des)->coalesced_frags))) {
                    btl_ownership = (to_base_frag(i)->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
#if BTL_WV_FAILOVER_ENABLED
                    if (to_base_frag(i)->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
#endif
                        to_base_frag(i)->base.des_cbfunc(&wv_btl->super, endpoint,
                                &to_base_frag(i)->base, OMPI_SUCCESS);
#if BTL_WV_FAILOVER_ENABLED
                    }
#endif
                    if( btl_ownership ) {
                        mca_btl_wv_free(&wv_btl->super, &to_base_frag(i)->base);
                    }
                }
            }
            /* Process a completed send/put/get */
            btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
            if (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
                des->des_cbfunc(&wv_btl->super, endpoint, des,OMPI_SUCCESS);
            }
            if( btl_ownership ) {
                mca_btl_wv_free(&wv_btl->super, des);
            }

            /* return send wqe */
            qp_put_wqe(endpoint, qp);

            if(WvSend == wc->Opcode && !BTL_WV_QP_TYPE_PP(qp)) {
                OPAL_THREAD_ADD32(&wv_btl->qps[qp].u.srq_qp.sd_credits, 1);

                /* new SRQ credit available. Try to progress pending frags*/
                progress_pending_frags_srq(wv_btl, qp);
            }
            /* new wqe or/and get token available. Try to progress pending frags */
            progress_pending_frags_wqe(endpoint, qp);
            mca_btl_wv_frag_progress_pending_put_get(endpoint, qp);
            break;
        case WvReceive:
            OPAL_OUTPUT((-1, "Got WC: RDMA_RECV, qp %d, src qp %d, WR ID %" PRIx64,
                         ((wv_qp *)wc->reserved)->qp_num, wc->SourceQPn, wc->WrId));

            if(wc->Flags & WV_WC_IMMEDIATE) { 
                endpoint = (mca_btl_wv_endpoint_t*)
                    opal_pointer_array_get_item(device->endpoints, wc->ImmediateData);
                frag->endpoint = endpoint;
                wv_btl = endpoint->endpoint_btl;
            }

            /* Process a RECV */
            if(btl_wv_handle_incoming(wv_btl, endpoint, to_recv_frag(frag),
                wc->Length) != OMPI_SUCCESS) {
                wv_btl->error_cb(&wv_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                                 NULL, NULL);
                break;
            }

            /* decide if it is time to setup an eager rdma channel */
            if(!endpoint->eager_rdma_local.base.pval && endpoint->use_eager_rdma &&
                    wc->Length < mca_btl_wv_component.eager_limit &&
                    wv_btl->eager_rdma_channels <
                    mca_btl_wv_component.max_eager_rdma &&
                    OPAL_THREAD_ADD32(&endpoint->eager_recv_count, 1) ==
                    mca_btl_wv_component.eager_rdma_threshold) {
                mca_btl_wv_endpoint_connect_eager_rdma(endpoint);
            }
            break;
        default:
            BTL_ERROR(("Unhandled work completion opcode is %d", wc->Opcode));
            if(wv_btl)
                wv_btl->error_cb(&wv_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                                     NULL, NULL);
            break;
    }

    return;

error:
    if(endpoint && endpoint->endpoint_proc && endpoint->endpoint_proc->proc_ompi)
        remote_proc = endpoint->endpoint_proc->proc_ompi;

    if(WvWcFlushed != wc->Status || !flush_err_printed[cq]++) {
        BTL_PEER_ERROR(remote_proc, ("error polling %s with status %s "
                       "status number %d for wr_id %" PRIx64 " opcode %d  vendor error %d qp_idx %d",
                       cq_name[cq], btl_wv_component_status_to_string(wc->Status),
                       wc->Status, wc->WrId, 
                       wc->Opcode, wc->VendorCode, qp));
        orte_notifier.log_peer(ORTE_NOTIFIER_CRIT, ORTE_ERR_COMM_FAILURE,
                               remote_proc ? &remote_proc->proc_name : NULL,
                               "\n\tIB polling %s with status %s "
                               "status number %d for wr_id %llu opcode %d vendor error %d qp_idx %d",
                               cq_name[cq], btl_wv_component_status_to_string(wc->Status),
                               wc->Status, wc->WrId, 
                               wc->Opcode, wc->VendorCode, qp);
    }

    if (WvWcRnrRetryError == wc->Status ||
        -2 == wc->Status) {
        char *peer_hostname = 
            (NULL != endpoint->endpoint_proc->proc_ompi->proc_hostname) ?
            endpoint->endpoint_proc->proc_ompi->proc_hostname : 
            "<unknown -- please run with mpi_keep_peer_hostnames=1>";
        const char *device_name = 
            (endpoint->qps[qp].qp->lcl_qp->context->device->name); 

        if (WvWcRnrRetryError == wc->Status) {
            orte_show_help("help-mpi-btl-wv.txt",
                           BTL_WV_QP_TYPE_PP(qp) ? 
                           "pp rnr retry exceeded" : 
                           "srq rnr retry exceeded", true,
                           orte_process_info.nodename, device_name,
                           peer_hostname);
            orte_notifier.show_help(ORTE_NOTIFIER_CRIT, ORTE_ERR_COMM_FAILURE,
                                    "help-mpi-btl-wv.txt",
                                    BTL_WV_QP_TYPE_PP(qp) ? 
                                    "pp rnr retry exceeded" : 
                                    "srq rnr retry exceeded",
                                    orte_process_info.nodename, device_name,
                                    peer_hostname);
        } else if (-2 == wc->Status) {
            orte_show_help("help-mpi-btl-wv.txt", 
                           "pp retry exceeded", true,
                           orte_process_info.nodename,
                           device_name, peer_hostname);
            orte_notifier.show_help(ORTE_NOTIFIER_CRIT, ORTE_ERR_COMM_FAILURE,
                                    "help-mpi-btl-wv.txt", 
                                    "pp retry exceeded",
                                    orte_process_info.nodename,
                                    device_name, peer_hostname);
        }
    }

#if BTL_WV_FAILOVER_ENABLED
    mca_btl_wv_handle_endpoint_error(wv_btl, des, qp,
                                         remote_proc, endpoint);
#else
    if(wv_btl)
        wv_btl->error_cb(&wv_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                             remote_proc, NULL);
#endif
}

static int poll_device(mca_btl_wv_device_t* device, int count)
{
    int ne = 0, cq;
    uint32_t hp_iter = 0;
    WV_COMPLETION wc;
    device->pollme = false;
    for(cq = 0; cq < 2 && hp_iter < mca_btl_wv_component.cq_poll_progress;)
    {
        ne = (int)device->ib_cq[cq]->handle->Poll(&wc,1);
        if(0 == ne) {
            /* don't check low prio cq if there was something in high prio cq,
             * but for each cq_poll_ratio hp cq polls poll lp cq once */
            if(count && device->hp_cq_polls)
                break;
            cq++;
            device->hp_cq_polls = mca_btl_wv_component.cq_poll_ratio;
            continue;
        }
        if(ne < 0)
            goto error;
        count++;
        if(BTL_WV_HP_CQ == cq) {
            device->pollme = true;
            hp_iter++;
            device->hp_cq_polls--;
        }
        handle_wc(device, cq, &wc);
    }

    return count;
error:
    BTL_ERROR(("error polling %s with %d errno says %s", cq_name[cq], ne,
                strerror(errno)));
    return count;
}

static int progress_one_device(mca_btl_wv_device_t *device)
{
    int i, c, count = 0, ret;
    mca_btl_wv_recv_frag_t* frag;
    mca_btl_wv_endpoint_t* endpoint;
    uint32_t non_eager_rdma_endpoints = 0;
    c = device->eager_rdma_buffers_count;
    non_eager_rdma_endpoints += (device->non_eager_rdma_endpoints + device->pollme);
    for(i = 0; i < c; i++) {
        endpoint = device->eager_rdma_buffers[i];
        if(!endpoint)
            continue;
        OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
        frag = MCA_BTL_WV_GET_LOCAL_RDMA_FRAG(endpoint,
                endpoint->eager_rdma_local.head);
        if(MCA_BTL_WV_RDMA_FRAG_LOCAL(frag)) {
            uint32_t size;
            mca_btl_wv_module_t *btl = endpoint->endpoint_btl;
            if(endpoint->nbo) {
                BTL_WV_FOOTER_NTOH(*frag->ftr);
            }
            size = MCA_BTL_WV_RDMA_FRAG_GET_SIZE(frag->ftr);
#if OPAL_ENABLE_DEBUG
            if (frag->ftr->seq != endpoint->eager_rdma_local.seq)
                BTL_ERROR(("Eager RDMA wrong SEQ: received %d expected %d",
                           frag->ftr->seq,
                           endpoint->eager_rdma_local.seq));
            endpoint->eager_rdma_local.seq++;
#endif
            MCA_BTL_WV_RDMA_NEXT_INDEX(endpoint->eager_rdma_local.head);
            OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
            frag->hdr = (mca_btl_wv_header_t*)(((char*)frag->ftr) -
                    size + sizeof(mca_btl_wv_footer_t));
            to_base_frag(frag)->segment.seg_addr.pval =
                ((unsigned char* )frag->hdr) + sizeof(mca_btl_wv_header_t);
            ret = btl_wv_handle_incoming(btl, to_com_frag(frag)->endpoint,
                    frag, size - sizeof(mca_btl_wv_footer_t));
            if (ret != OMPI_SUCCESS) {
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
        device->eager_rdma_polls = mca_btl_wv_component.eager_rdma_poll_ratio;
    }

    return count;
}

/*
 *  IB component progress.
 */
static int btl_wv_component_progress(void)
{
    int i;
    int count = 0;
    for(i = 0; i < mca_btl_wv_component.devices_count; i++) {
        mca_btl_wv_device_t *device =
            (mca_btl_wv_device_t *) opal_pointer_array_get_item(&mca_btl_wv_component.devices, i);
        count += progress_one_device(device);
    }

    return count;
}

int mca_btl_wv_post_srr(mca_btl_wv_module_t* wv_btl, const int qp)
{
    int rd_low_local = wv_btl->qps[qp].u.srq_qp.rd_low_local;
    int rd_curr_num = wv_btl->qps[qp].u.srq_qp.rd_curr_num;
    int num_post, i,rc;
    struct wv_recv_wr *wr = NULL;
    HRESULT hr = 0;
    assert(!BTL_WV_QP_TYPE_PP(qp));
    OPAL_THREAD_LOCK(&wv_btl->ib_lock);
    if(wv_btl->qps[qp].u.srq_qp.rd_posted > rd_low_local) {
        OPAL_THREAD_UNLOCK(&wv_btl->ib_lock);
        return OMPI_SUCCESS;
    }
    num_post = rd_curr_num - wv_btl->qps[qp].u.srq_qp.rd_posted;
    if (0 == num_post) {
        OPAL_THREAD_UNLOCK(&wv_btl->ib_lock);
        return OMPI_SUCCESS;
    }
    for(i = 0; i < num_post; i++) {
        ompi_free_list_item_t* item;
        OMPI_FREE_LIST_WAIT(&wv_btl->device->qps[qp].recv_free, item, rc);
        to_base_frag(item)->base.order = qp;
        to_com_frag(item)->endpoint = NULL;
        wr = &to_recv_frag(item)->rd_desc;
        hr = wv_btl->qps[qp].u.srq_qp.srq->handle->PostReceive(wr->wr_id,
             wr->sg_list, wr->num_sge);
    }
    if(SUCCEEDED(hr)) {
        struct wv_srq_attr srq_attr;
        OPAL_THREAD_ADD32(&wv_btl->qps[qp].u.srq_qp.rd_posted, num_post);
        if(true == wv_btl->qps[qp].u.srq_qp.srq_limit_event_flag) {
            srq_attr.max_wr = wv_btl->qps[qp].u.srq_qp.rd_curr_num;
            srq_attr.max_sge = 1;
            srq_attr.srq_limit = mca_btl_wv_component.qp_infos[qp].u.srq_qp.srq_limit;
            wv_btl->qps[qp].u.srq_qp.srq_limit_event_flag = false;
            SIZE_T max_wr,max_sge,srq_limit;
            hr = wv_btl->qps[qp].u.srq_qp.srq->handle->Query(&max_wr,&max_sge,&srq_limit);
            hr = wv_btl->qps[qp].u.srq_qp.srq->handle->Modify(max_wr,srq_limit);
            if(FAILED(hr)) {
                OPAL_THREAD_UNLOCK(&wv_btl->ib_lock);
                return OMPI_ERROR;
            }
        }
        OPAL_THREAD_UNLOCK(&wv_btl->ib_lock);
        return OMPI_SUCCESS;
    }
    OPAL_THREAD_UNLOCK(&wv_btl->ib_lock);

    return OMPI_ERROR;
}
