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
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
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

#include <infiniband/verbs.h>
#include <errno.h>
#include <string.h>   /* for strerror()*/
#include <unistd.h>

#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/include/opal/align.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/sys/timer.h"
#include "opal/sys/atomic.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/mca/paffinity/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/sys_info.h"

#include "ompi/proc/proc.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/datatype/convertor.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "btl_openib.h"
#include "btl_openib_frag.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_eager_rdma.h"
#include "btl_openib_proc.h"
#include "btl_openib_ini.h"
#include "btl_openib_mca.h"
#include "btl_openib_xrc.h"
#if OMPI_HAVE_THREADS
#include "btl_openib_async.h"
#endif
#include "connect/base.h"

/*
 * Local functions
 */
static int btl_openib_component_open(void);
static int btl_openib_component_close(void);
static mca_btl_base_module_t **btl_openib_component_init(int*, bool, bool);
static int btl_openib_component_progress(void);

mca_btl_openib_component_t mca_btl_openib_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_1,

            "openib", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            btl_openib_component_open,  /* component open */
            btl_openib_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        btl_openib_component_init,
        btl_openib_component_progress,
    }
};

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */
int btl_openib_component_open(void)
{
    int ret;

    /* initialize state */
    mca_btl_openib_component.ib_num_btls = 0;
    mca_btl_openib_component.openib_btls = NULL;
    OBJ_CONSTRUCT(&mca_btl_openib_component.hcas, opal_pointer_array_t);
    mca_btl_openib_component.hcas_count = 0;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_procs, opal_list_t);

    /* register IB component parameters */
    ret = btl_openib_register_mca_params();

    mca_btl_openib_component.max_send_size =
        mca_btl_openib_module.super.btl_max_send_size;
    mca_btl_openib_component.eager_limit =
        mca_btl_openib_module.super.btl_eager_limit;

    srand48(getpid() * time(NULL));
    return ret;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

static int btl_openib_component_close(void)
{
    /* Close down the connect pseudo component */
    if (NULL != ompi_btl_openib_connect.bcf_finalize) {
        ompi_btl_openib_connect.bcf_finalize();
    }

    ompi_btl_openib_ini_finalize();
    return OMPI_SUCCESS;
}

/*
 *  Register OPENIB  port information. The MCA framework
 *  will make this available to all peers.
 */
static int btl_openib_modex_send(void)
{
    int    rc, i;
    char *message, *offset;
    uint32_t size, size_save;
    size_t msg_size;

    /* The message is packed into 2 parts:
     * 1. a uint32_t indicating the number of ports in the message
     * 2. for each port:
     *    a. the port data
     *    b. a uint32_t indicating a string length
     *    c. the string cpc list for that port, length specified by 2b.
     */
    msg_size = sizeof(uint32_t) + mca_btl_openib_component.ib_num_btls * (sizeof(uint32_t) + sizeof(mca_btl_openib_port_info_t));
    for (i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        msg_size += strlen(mca_btl_openib_component.openib_btls[i]->port_info.cpclist);
    }

    if (0 == msg_size) {
        return 0;
    }

    message = malloc(msg_size);
    if (NULL == message) {
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Pack the number of ports */
    size = mca_btl_openib_component.ib_num_btls;
#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    size = htonl(size);
#endif
    memcpy(message, &size, sizeof(size));
    offset = message + sizeof(size);

    /* Pack each of the ports */
    for (i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        /* Pack the port struct */
        memcpy(offset, &mca_btl_openib_component.openib_btls[i]->port_info, sizeof(mca_btl_openib_port_info_t));
#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
        MCA_BTL_OPENIB_PORT_INFO_HTON(*(mca_btl_openib_port_info_t *)offset);
#endif
        offset += sizeof(mca_btl_openib_port_info_t);

        /* Pack the strlen of the cpclist */
        size = size_save =
            strlen(mca_btl_openib_component.openib_btls[i]->port_info.cpclist);
#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
        size = htonl(size);
#endif
        memcpy(offset, &size, sizeof(size));
        offset += sizeof(size);

        /* Pack the string */
        memcpy(offset,
               mca_btl_openib_component.openib_btls[i]->port_info.cpclist,
               size_save);
        offset += size_save;
    }

    rc = ompi_modex_send(&mca_btl_openib_component.super.btl_version,
                         message, msg_size);
    free(message);

    return rc;
}

/*
 * Active Message Callback function on control message.
 */

static void btl_openib_control(mca_btl_base_module_t* btl,
        mca_btl_base_tag_t tag, mca_btl_base_descriptor_t* des,
        void* cbdata)
{
    /* don't return credits used for control messages */
    mca_btl_openib_module_t *obtl = (mca_btl_openib_module_t*)btl;
    mca_btl_openib_endpoint_t* ep = to_com_frag(des)->endpoint;
    mca_btl_openib_control_header_t *ctl_hdr =
        to_base_frag(des)->segment.seg_addr.pval;
    mca_btl_openib_eager_rdma_header_t *rdma_hdr;
    mca_btl_openib_header_coalesced_t *clsc_hdr =
        (mca_btl_openib_header_coalesced_t*)(ctl_hdr + 1);
    mca_btl_active_message_callback_t* reg;
    size_t len = des->des_dst->seg_len - sizeof(*ctl_hdr);

    switch (ctl_hdr->type) {
    case MCA_BTL_OPENIB_CONTROL_CREDITS:
        assert(0); /* Credit message is handled elsewhere */
        break;
    case MCA_BTL_OPENIB_CONTROL_RDMA:
       rdma_hdr = (mca_btl_openib_eager_rdma_header_t*)ctl_hdr;

       BTL_VERBOSE(("prior to NTOH received  rkey %lu, rdma_start.lval %llu, pval %p, ival %u\n",
                  rdma_hdr->rkey,
                  (unsigned long) rdma_hdr->rdma_start.lval,
                  rdma_hdr->rdma_start.pval,
                  rdma_hdr->rdma_start.ival
                  ));

       if(ep->nbo) {
           BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_NTOH(*rdma_hdr);
       }

       BTL_VERBOSE(("received  rkey %lu, rdma_start.lval %llu, pval %p,"
                   " ival %u\n", rdma_hdr->rkey,
                   (unsigned long) rdma_hdr->rdma_start.lval,
                  rdma_hdr->rdma_start.pval, rdma_hdr->rdma_start.ival));

       if (ep->eager_rdma_remote.base.pval) {
           BTL_ERROR(("Got RDMA connect twice!"));
           return;
       }
       ep->eager_rdma_remote.rkey = rdma_hdr->rkey;
       ep->eager_rdma_remote.base.lval = rdma_hdr->rdma_start.lval;
       ep->eager_rdma_remote.tokens=mca_btl_openib_component.eager_rdma_num - 1;
       break;
    case MCA_BTL_OPENIB_CONTROL_COALESCED:
        while(len > 0) {
            size_t skip;
            mca_btl_base_descriptor_t tmp_des;
            mca_btl_base_segment_t tmp_seg;

            assert(len >= sizeof(*clsc_hdr));

            if(ep->nbo)
                BTL_OPENIB_HEADER_COALESCED_NTOH(*clsc_hdr);

            skip = (sizeof(*clsc_hdr) + clsc_hdr->alloc_size);

            tmp_des.des_dst = &tmp_seg;
            tmp_des.des_dst_cnt = 1;
            tmp_seg.seg_addr.pval = clsc_hdr + 1;
            tmp_seg.seg_len = clsc_hdr->size;

            /* call registered callback */
            reg = mca_btl_base_active_message_trigger + clsc_hdr->tag;
            reg->cbfunc( &obtl->super, clsc_hdr->tag, &tmp_des, reg->cbdata );
            len -= skip;
            clsc_hdr = (mca_btl_openib_header_coalesced_t*)
                (((unsigned char*)clsc_hdr) + skip);
        }
       break;
    default:
        BTL_ERROR(("Unknown message type received by BTL"));
       break;
    }
}

static int openib_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg)
{
    mca_btl_openib_hca_t *hca = (mca_btl_openib_hca_t*)reg_data;
    mca_btl_openib_reg_t *openib_reg = (mca_btl_openib_reg_t*)reg;

    openib_reg->mr = ibv_reg_mr(hca->ib_pd, base, size, IBV_ACCESS_LOCAL_WRITE |
            IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ);

    if(NULL == openib_reg->mr)
        return OMPI_ERR_OUT_OF_RESOURCE;

    return OMPI_SUCCESS;
}

static int openib_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg)
{
    mca_btl_openib_reg_t *openib_reg = (mca_btl_openib_reg_t*)reg;

    if(openib_reg->mr != NULL) {
        if(ibv_dereg_mr(openib_reg->mr)) {
            BTL_ERROR(("%s: error unpinning openib memory errno says %s\n",
                       __func__, strerror(errno)));
            return OMPI_ERROR;
        }
    }
    openib_reg->mr = NULL;
    return OMPI_SUCCESS;
}
static inline int param_register_int(const char* param_name, int default_value)
{
    int param_value = default_value;
    int id = mca_base_param_register_int("btl", "openib", param_name, NULL,
            default_value);
    mca_base_param_lookup_int(id, &param_value);
    return param_value;
}

#if OMPI_HAVE_THREADS
static int start_async_event_thread(void)
{
    /* Set the fatal counter to zero */
    mca_btl_openib_component.fatal_counter = 0;

    /* Create pipe for communication with async event thread */
    if(pipe(mca_btl_openib_component.async_pipe)) {
        BTL_ERROR(("Failed to create pipe for communication with "
                    "async event thread"));
        return OMPI_ERROR;
    }

    /* Starting async event thread for the component */
    if(pthread_create(&mca_btl_openib_component.async_thread, NULL,
                (void*(*)(void*))btl_openib_async_thread, NULL)) {
        BTL_ERROR(("Failed to create async event thread"));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
#endif

static int init_one_port(opal_list_t *btl_list, mca_btl_openib_hca_t *hca,
                         uint8_t port_num, uint16_t pkey_index,
                         struct ibv_port_attr *ib_port_attr)
{
    uint16_t lid, i, lmc, lmc_step;
    mca_btl_openib_module_t *openib_btl;
    mca_btl_base_selected_module_t *ib_selected;
    union ibv_gid gid;
    uint64_t subnet_id;

    ibv_query_gid(hca->ib_dev_context, port_num, 0, &gid);
    subnet_id = ntoh64(gid.global.subnet_prefix);
    BTL_VERBOSE(("my subnet_id is %016x\n", subnet_id));

    if(mca_btl_openib_component.ib_num_btls > 0 &&
            IB_DEFAULT_GID_PREFIX == subnet_id &&
            mca_btl_openib_component.warn_default_gid_prefix) {
        opal_show_help("help-mpi-btl-openib.txt", "default subnet prefix",
                true, orte_system_info.nodename);
    }

    lmc = (1 << ib_port_attr->lmc);
    lmc_step = 1;

    if (0 != mca_btl_openib_component.max_lmc &&
        mca_btl_openib_component.max_lmc < lmc) {
        lmc = mca_btl_openib_component.max_lmc;
    }

#if OMPI_HAVE_THREADS
    /* APM support */
    if (lmc > 1){
        if (-1 == mca_btl_openib_component.apm) {
            lmc_step = lmc;
        } else if (0 == lmc % (mca_btl_openib_component.apm + 1)) {
            lmc_step = mca_btl_openib_component.apm + 1;
        } else {
            opal_show_help("help-mpi-btl-openib.txt", "apm with wrong lmc",true,
                    mca_btl_openib_component.apm, lmc);
            return OMPI_ERROR;
        }
    } else {
        if (mca_btl_openib_component.apm) {
            /* Disable apm and report warning */
            mca_btl_openib_component.apm = 0;
            opal_show_help("help-mpi-btl-openib.txt", "apm without lmc",true);
        }
    }
#endif

    for(lid = ib_port_attr->lid;
            lid < ib_port_attr->lid + lmc; lid += lmc_step){
        for(i = 0; i < mca_btl_openib_component.btls_per_lid; i++){
            char param[40];
            int rc;

            openib_btl = malloc(sizeof(mca_btl_openib_module_t));
            if(NULL == openib_btl) {
                BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            memcpy(openib_btl, &mca_btl_openib_module,
                    sizeof(mca_btl_openib_module));
            memcpy(&openib_btl->ib_port_attr, ib_port_attr,
                    sizeof(struct ibv_port_attr));
            ib_selected = OBJ_NEW(mca_btl_base_selected_module_t);
            ib_selected->btl_module = (mca_btl_base_module_t*) openib_btl;
            openib_btl->hca = hca;
            openib_btl->port_num = (uint8_t) port_num;
            openib_btl->pkey_index = pkey_index;
            openib_btl->lid = lid;
            openib_btl->apm_lmc_max = lmc_step ;
            openib_btl->src_path_bits = lid - ib_port_attr->lid;
            /* store the subnet for multi-nic support */
            openib_btl->port_info.subnet_id = subnet_id;
            openib_btl->port_info.mtu = hca->mtu;
#if HAVE_XRC
            /* This code is protected with ifdef because we don't want to send
             * extra bytes during OOB */
            if(MCA_BTL_XRC_ENABLED) {
                openib_btl->port_info.lid = lid;
            }
#endif
            rc = ompi_btl_openib_connect_base_query(&openib_btl->port_info.cpclist, hca);
            if (OMPI_SUCCESS != rc) {
                continue;
            }

            mca_btl_base_active_message_trigger[MCA_BTL_TAG_IB].cbfunc = btl_openib_control;
            mca_btl_base_active_message_trigger[MCA_BTL_TAG_IB].cbdata = NULL;

            /* Check bandwidth configured for this HCA */
            sprintf(param, "bandwidth_%s", ibv_get_device_name(hca->ib_dev));
            openib_btl->super.btl_bandwidth =
                param_register_int(param, openib_btl->super.btl_bandwidth);

            /* Check bandwidth configured for this HCA/port */
            sprintf(param, "bandwidth_%s:%d", ibv_get_device_name(hca->ib_dev),
                    port_num);
            openib_btl->super.btl_bandwidth =
                param_register_int(param, openib_btl->super.btl_bandwidth);

            /* Check bandwidth configured for this HCA/port/LID */
            sprintf(param, "bandwidth_%s:%d:%d",
                    ibv_get_device_name(hca->ib_dev), port_num, lid);
            openib_btl->super.btl_bandwidth =
                param_register_int(param, openib_btl->super.btl_bandwidth);

            /* Check latency configured for this HCA */
            sprintf(param, "latency_%s", ibv_get_device_name(hca->ib_dev));
            openib_btl->super.btl_latency =
                param_register_int(param, openib_btl->super.btl_latency);

            /* Check latency configured for this HCA/port */
            sprintf(param, "latency_%s:%d", ibv_get_device_name(hca->ib_dev),
                    port_num);
            openib_btl->super.btl_latency =
                param_register_int(param, openib_btl->super.btl_latency);

            /* Check latency configured for this HCA/port/LID */
            sprintf(param, "latency_%s:%d:%d", ibv_get_device_name(hca->ib_dev),
                    port_num, lid);
            openib_btl->super.btl_latency =
                param_register_int(param, openib_btl->super.btl_latency);

            /* Auto-detect the port bandwidth */
            if (0 == openib_btl->super.btl_bandwidth) {
                /* To calculate the bandwidth available on this port,
                   we have to look up the values corresponding to
                   port->active_speed and port->active_width.  These
                   are enums corresponding to the IB spec.  Overall
                   forumula is 80% of the reported speed (to get the
                   true link speed) times the number of links. */
                switch (ib_port_attr->active_speed) {
                case 1:
                    /* 2.5Gbps * 0.8, in megabits */
                    openib_btl->super.btl_bandwidth = 2000;
                    break;
                case 2:
                    /* 5.0Gbps * 0.8, in megabits */
                    openib_btl->super.btl_bandwidth = 4000;
                    break;
                case 4:
                    /* 10.0Gbps * 0.8, in megabits */
                    openib_btl->super.btl_bandwidth = 8000;
                    break;
                default:
                    /* Who knows?  Declare this port unreachable (do
                       *not* return ERR_VALUE_OF_OUT_OF_BOUNDS; that
                       is reserved for when we exceed the number of
                       allowable BTLs). */
                    return OMPI_ERR_UNREACH;
                }
                switch (ib_port_attr->active_width) {
                case 1:
                    /* 1x */
                    /* unity */
                    break;
                case 2:
                    /* 4x */
                    openib_btl->super.btl_bandwidth *= 4;
                    break;
                case 4:
                    /* 8x */
                    openib_btl->super.btl_bandwidth *= 8;
                    break;
                case 8:
                    /* 12x */
                    openib_btl->super.btl_bandwidth *= 12;
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
            hca->btls++;
            ++mca_btl_openib_component.ib_num_btls;
            if (-1 != mca_btl_openib_component.ib_max_btls &&
                mca_btl_openib_component.ib_num_btls >=
                mca_btl_openib_component.ib_max_btls) {
                return OMPI_ERR_VALUE_OUT_OF_BOUNDS;
            }
        }
    }

    return OMPI_SUCCESS;
}

static void hca_construct(mca_btl_openib_hca_t *hca)
{
    int i;

    hca->ib_dev = NULL;
    hca->ib_dev_context = NULL;
    hca->mpool = NULL;
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    hca->ib_channel = NULL;
#endif
    hca->btls = 0;
    hca->ib_cq[BTL_OPENIB_HP_CQ] = NULL;
    hca->ib_cq[BTL_OPENIB_LP_CQ] = NULL;
    hca->cq_size[BTL_OPENIB_HP_CQ] = 0;
    hca->cq_size[BTL_OPENIB_LP_CQ] = 0;
    hca->non_eager_rdma_endpoints = 0;
    hca->hp_cq_polls = mca_btl_openib_component.cq_poll_ratio;
    hca->eager_rdma_polls = mca_btl_openib_component.eager_rdma_poll_ratio;
    hca->pollme = true;
    hca->eager_rdma_buffers_count = 0;
    hca->eager_rdma_buffers = NULL;
#if HAVE_XRC
    hca->xrc_fd = -1;
#endif
    hca->qps = (mca_btl_openib_hca_qp_t*)calloc(mca_btl_openib_component.num_qps,
            sizeof(mca_btl_openib_hca_qp_t));
    OBJ_CONSTRUCT(&hca->hca_lock, opal_mutex_t);
    for(i = 0; i < mca_btl_openib_component.num_qps; i++) {
        OBJ_CONSTRUCT(&hca->qps[i].send_free, ompi_free_list_t);
        OBJ_CONSTRUCT(&hca->qps[i].recv_free, ompi_free_list_t);
    }
    OBJ_CONSTRUCT(&hca->send_free_control, ompi_free_list_t);
}

static void hca_destruct(mca_btl_openib_hca_t *hca)
{
    int i;

    if(hca->eager_rdma_buffers) {
        int i;
        for(i = 0; i < hca->eager_rdma_buffers_count; i++)
            if(hca->eager_rdma_buffers[i])
                OBJ_RELEASE(hca->eager_rdma_buffers[i]);
        free(hca->eager_rdma_buffers);
    }
    OBJ_DESTRUCT(&hca->hca_lock);
    for(i = 0; i < mca_btl_openib_component.num_qps; i++) {
        OBJ_DESTRUCT(&hca->qps[i].send_free);
        OBJ_DESTRUCT(&hca->qps[i].recv_free);
    }
    OBJ_DESTRUCT(&hca->send_free_control);
    if(hca->qps)
        free(hca->qps);
}

OBJ_CLASS_INSTANCE(mca_btl_openib_hca_t, opal_object_t, hca_construct,
        hca_destruct);

static int prepare_hca_for_use(mca_btl_openib_hca_t *hca)
{
    mca_btl_openib_frag_init_data_t *init_data;
    int qp, length;

#if OMPI_HAVE_THREADS
    if(mca_btl_openib_component.use_async_event_thread) {
        if(0 == mca_btl_openib_component.async_thread) {
            /* async thread is not yet started, so start it here */
            if(start_async_event_thread() != OMPI_SUCCESS)
                return OMPI_ERROR;
        }
        hca->got_fatal_event = false;
        if (write(mca_btl_openib_component.async_pipe[1],
                    &hca->ib_dev_context->async_fd, sizeof(int))<0){
            BTL_ERROR(("Failed to write to pipe [%d]",errno));
            return OMPI_ERROR;
        }
    }
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* Prepare data for thread, but not starting it */
    OBJ_CONSTRUCT(&hca->thread, opal_thread_t);
    hca->thread.t_run = mca_btl_openib_progress_thread;
    hca->thread.t_arg = hca;
    hca->progress = false;
#endif
#endif

    hca->endpoints = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(hca->endpoints, 10, INT_MAX, 10);
    opal_pointer_array_add(&mca_btl_openib_component.hcas, hca);
    if(mca_btl_openib_component.max_eager_rdma > 0 &&
            mca_btl_openib_component.use_eager_rdma &&
            hca->use_eager_rdma) {
        hca->eager_rdma_buffers =
            calloc(mca_btl_openib_component.max_eager_rdma * hca->btls,
                    sizeof(mca_btl_openib_endpoint_t*));
        if(NULL == hca->eager_rdma_buffers) {
            BTL_ERROR(("Memory allocation fails\n"));
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));
    length = sizeof(mca_btl_openib_header_t) +
        sizeof(mca_btl_openib_footer_t) +
        sizeof(mca_btl_openib_eager_rdma_header_t);

    init_data->order = MCA_BTL_NO_ORDER;
    init_data->list = &hca->send_free_control;

    if(OMPI_SUCCESS != ompi_free_list_init_ex_new(
                &hca->send_free_control,
                sizeof(mca_btl_openib_send_control_frag_t), CACHE_LINE_SIZE,
                OBJ_CLASS(mca_btl_openib_send_control_frag_t), length,
                mca_btl_openib_component.buffer_alignment,
                mca_btl_openib_component.ib_free_list_num, -1,
                mca_btl_openib_component.ib_free_list_inc,
                hca->mpool, mca_btl_openib_frag_init,
                init_data)) {
        return OMPI_ERROR;
    }

    /* setup all the qps */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));
        /* Initialize pool of send fragments */
        length = sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t) +
            sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_footer_t) +
            mca_btl_openib_component.qp_infos[qp].size;

        init_data->order = qp;
        init_data->list = &hca->qps[qp].send_free;

        if(OMPI_SUCCESS != ompi_free_list_init_ex_new(init_data->list,
                    sizeof(mca_btl_openib_send_frag_t), CACHE_LINE_SIZE,
                    OBJ_CLASS(mca_btl_openib_send_frag_t), length,
                    mca_btl_openib_component.buffer_alignment,
                    mca_btl_openib_component.ib_free_list_num,
                    mca_btl_openib_component.ib_free_list_max,
                    mca_btl_openib_component.ib_free_list_inc,
                    hca->mpool, mca_btl_openib_frag_init,
                    init_data)) {
            return OMPI_ERROR;
        }

        init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));
        length = sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t) +
            sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_footer_t) +
            mca_btl_openib_component.qp_infos[qp].size;

        init_data->order = qp;
        init_data->list = &hca->qps[qp].recv_free;

        if(OMPI_SUCCESS != ompi_free_list_init_ex_new(init_data->list,
                    sizeof(mca_btl_openib_recv_frag_t), CACHE_LINE_SIZE,
                    OBJ_CLASS(mca_btl_openib_recv_frag_t),
                    length, mca_btl_openib_component.buffer_alignment,
                    mca_btl_openib_component.ib_free_list_num,
                    mca_btl_openib_component.ib_free_list_max,
                    mca_btl_openib_component.ib_free_list_inc,
                    hca->mpool, mca_btl_openib_frag_init,
                    init_data)) {
            return OMPI_ERROR;
        }
    }

    mca_btl_openib_component.hcas_count++;
    return OMPI_SUCCESS;
}

static int
get_port_list(mca_btl_openib_hca_t *hca, int *allowed_ports)
{
    int i, j, k, num_ports = 0;
    const char *dev_name;
    char *name;

    dev_name = ibv_get_device_name(hca->ib_dev);
    name = (char*) malloc(strlen(dev_name) + 4);
    if (NULL == name) {
        return 0;
    }

    /* Assume that all ports are allowed.  num_ports will be adjusted
       below to reflect whether this is true or not. */
    for (i = 1; i <= hca->ib_dev_attr.phys_port_cnt; ++i) {
        allowed_ports[num_ports++] = i;
    }
    num_ports = 0;
    if (NULL != mca_btl_openib_component.if_include_list) {
        /* If only the HCA name is given (eg. mthca0,mthca1) use all
           ports */
        i = 0;
        while (mca_btl_openib_component.if_include_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_btl_openib_component.if_include_list[i])) {
                num_ports = hca->ib_dev_attr.phys_port_cnt;
                goto done;
            }
            ++i;
        }
        /* Include only requested ports on the HCA */
        for (i = 1; i <= hca->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name,"%s:%d",dev_name,i);
            for (j = 0;
                 NULL != mca_btl_openib_component.if_include_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_btl_openib_component.if_include_list[j])) {
                    allowed_ports[num_ports++] = i;
                    break;
                }
            }
        }
    } else if (NULL != mca_btl_openib_component.if_exclude_list) {
        /* If only the HCA name is given (eg. mthca0,mthca1) exclude
           all ports */
        i = 0;
        while (mca_btl_openib_component.if_exclude_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_btl_openib_component.if_exclude_list[i])) {
                num_ports = 0;
                goto done;
            }
            ++i;
        }
        /* Exclude the specified ports on this HCA */
        for (i = 1; i <= hca->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name,"%s:%d",dev_name,i);
            for (j = 0;
                 NULL != mca_btl_openib_component.if_exclude_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_btl_openib_component.if_exclude_list[j])) {
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
        num_ports = hca->ib_dev_attr.phys_port_cnt;
    }

done:

    /* Remove the following from the error-checking if_list:
       - bare device name
       - device name suffixed with port number */
    if (NULL != mca_btl_openib_component.if_list) {
        for (i = 0; NULL != mca_btl_openib_component.if_list[i]; ++i) {

            /* Look for raw device name */
            if (0 == strcmp(mca_btl_openib_component.if_list[i], dev_name)) {
                j = opal_argv_count(mca_btl_openib_component.if_list);
                opal_argv_delete(&j, &(mca_btl_openib_component.if_list),
                                 i, 1);
                --i;
            }
        }
        for (i = 1; i <= hca->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name, "%s:%d", dev_name, i);
            for (j = 0; NULL != mca_btl_openib_component.if_list[j]; ++j) {
                if (0 == strcmp(mca_btl_openib_component.if_list[j], name)) {
                    k = opal_argv_count(mca_btl_openib_component.if_list);
                    opal_argv_delete(&k, &(mca_btl_openib_component.if_list),
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

static void merge_values(ompi_btl_openib_ini_values_t *target,
                         ompi_btl_openib_ini_values_t *src)
{
    if (!target->mtu_set && src->mtu_set) {
        target->mtu = src->mtu;
        target->mtu_set = true;
    }

    if (!target->use_eager_rdma_set && src->use_eager_rdma_set) {
        target->use_eager_rdma = src->use_eager_rdma;
        target->use_eager_rdma_set = true;
    }
}

static bool inline is_credit_message(const mca_btl_openib_recv_frag_t *frag)
{
    mca_btl_openib_control_header_t* chdr =
        to_base_frag(frag)->segment.seg_addr.pval;
    return (MCA_BTL_TAG_BTL == frag->hdr->tag) &&
        (MCA_BTL_OPENIB_CONTROL_CREDITS == chdr->type);
}

static int init_one_hca(opal_list_t *btl_list, struct ibv_device* ib_dev)
{
    struct mca_mpool_base_resources_t mpool_resources;
    mca_btl_openib_hca_t *hca;
    uint8_t i, k = 0;
    int ret = -1, port_cnt;
    ompi_btl_openib_ini_values_t values, default_values;
    int *allowed_ports = NULL;

    hca = OBJ_NEW(mca_btl_openib_hca_t);
    if(NULL == hca){
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    hca->ib_dev = ib_dev;
    hca->ib_dev_context = ibv_open_device(ib_dev);

    if(NULL == hca->ib_dev_context){
        BTL_ERROR(("error obtaining device context for %s errno says %s\n",
                    ibv_get_device_name(ib_dev), strerror(errno)));
        goto error;
    }

    if(ibv_query_device(hca->ib_dev_context, &hca->ib_dev_attr)){
        BTL_ERROR(("error obtaining device attributes for %s errno says %s\n",
                    ibv_get_device_name(ib_dev), strerror(errno)));
        goto error;
    }
    /* If mca_btl_if_include/exclude were specified, get usable ports */
    allowed_ports = (int*)malloc(hca->ib_dev_attr.phys_port_cnt * sizeof(int));
    port_cnt = get_port_list(hca, allowed_ports);
    if(0 == port_cnt) {
        ret = OMPI_SUCCESS;
        goto error;
    }
#if HAVE_XRC
    /* if user configured to run with XRC qp and the device don't support it -
     * we should ignore this hca. Maybe we have other one that have XRC support
     */
    if (!(hca->ib_dev_attr.device_cap_flags & IBV_DEVICE_XRC) &&
            mca_btl_openib_component.num_xrc_qps > 0) {
        opal_show_help("help-mpi-btl-openib.txt",
                "XRC on device without XRC support", true,
                mca_btl_openib_component.num_xrc_qps,
                ibv_get_device_name(ib_dev),
                orte_system_info.nodename);
        ret = OMPI_SUCCESS;
        goto error;
    }
#endif
    /* Load in vendor/part-specific HCA parameters.  Note that even if
       we don't find values for this vendor/part, "values" will be set
       indicating that it does not have good values */
    ret = ompi_btl_openib_ini_query(hca->ib_dev_attr.vendor_id,
                                    hca->ib_dev_attr.vendor_part_id,
                                    &values);
    if (OMPI_SUCCESS != ret && OMPI_ERR_NOT_FOUND != ret) {
        /* If we get a serious error, propagate it upwards */
        goto error;
    }
    if (OMPI_ERR_NOT_FOUND == ret) {
        /* If we didn't find a matching HCA in the INI files, output a
           warning that we're using default values (unless overridden
           that we don't want to see these warnings) */
        if (mca_btl_openib_component.warn_no_hca_params_found) {
            opal_show_help("help-mpi-btl-openib.txt",
                           "no hca params found", true,
                           orte_system_info.nodename,
                           hca->ib_dev_attr.vendor_id,
                           hca->ib_dev_attr.vendor_part_id);
        }
    }
    /* Note that even if we don't find default values, "values" will
       be set indicating that it does not have good values */
    ret = ompi_btl_openib_ini_query(0, 0, &default_values);
    if (OMPI_SUCCESS != ret && OMPI_ERR_NOT_FOUND != ret) {
        /* If we get a serious error, propagate it upwards */
        goto error;
    }

    /* If we did find values for this HCA (or in the defaults
       section), handle them */
    merge_values(&values, &default_values);
    if (values.mtu_set) {
        switch (values.mtu) {
        case 256:
            hca->mtu = IBV_MTU_256;
            break;
        case 512:
            hca->mtu = IBV_MTU_512;
            break;
        case 1024:
            hca->mtu = IBV_MTU_1024;
            break;
        case 2048:
            hca->mtu = IBV_MTU_2048;
            break;
        case 4096:
            hca->mtu = IBV_MTU_4096;
            break;
        default:
            BTL_ERROR(("invalid MTU value specified in INI file (%d); ignored\n", values.mtu));
            hca->mtu = mca_btl_openib_component.ib_mtu;
            break;
        }
    } else {
        hca->mtu = mca_btl_openib_component.ib_mtu;
    }

    /* If "use eager rdma" was set, then enable it on this HCA */
    if (values.use_eager_rdma_set) {
        hca->use_eager_rdma = values.use_eager_rdma;
    }

    /* Allocate the protection domain for the HCA */
    hca->ib_pd = ibv_alloc_pd(hca->ib_dev_context);
    if(NULL == hca->ib_pd){
        BTL_ERROR(("error allocating protection domain for %s errno says %s\n",
                    ibv_get_device_name(ib_dev), strerror(errno)));
        goto error;
    }

    if (MCA_BTL_XRC_ENABLED) {
        if (OMPI_SUCCESS != mca_btl_openib_open_xrc_domain(hca)) {
            BTL_ERROR(("XRC Internal error. Failed to open xrc domain"));
            goto error;
        }
    }

    mpool_resources.reg_data = (void*)hca;
    mpool_resources.sizeof_reg = sizeof(mca_btl_openib_reg_t);
    mpool_resources.register_mem = openib_reg_mr;
    mpool_resources.deregister_mem = openib_dereg_mr;
    hca->mpool =
        mca_mpool_base_module_create(mca_btl_openib_component.ib_mpool_name,
                hca, &mpool_resources);
    if(NULL == hca->mpool){
         BTL_ERROR(("error creating IB memory pool for %s errno says %s\n",
                     ibv_get_device_name(ib_dev), strerror(errno)));
         goto error;
    }

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    hca->ib_channel = ibv_create_comp_channel(hca->ib_dev_context);
    if (NULL == hca->ib_channel) {
        BTL_ERROR(("error creating channel for %s errno says %s\n",
                    ibv_get_device_name(hca->ib_dev),
                    strerror(errno)));
        goto error;
    }
#endif

    ret = OMPI_SUCCESS;

    /* Note ports are 1 based (i >= 1) */
    for(k = 0; k < port_cnt; k++){
        struct ibv_port_attr ib_port_attr;
        i = allowed_ports[k];
        if(ibv_query_port(hca->ib_dev_context, i, &ib_port_attr)){
            BTL_ERROR(("error getting port attributes for device %s "
                        "port number %d errno says %s",
                        ibv_get_device_name(ib_dev), i, strerror(errno)));
            break;
        }
        if(IBV_PORT_ACTIVE == ib_port_attr.state){

            if (0 == mca_btl_openib_component.ib_pkey_val) {
                ret = init_one_port(btl_list, hca, i, mca_btl_openib_component.ib_pkey_ix,
                                    &ib_port_attr);
            }
            else {
                uint16_t pkey,j;
                for (j=0; j < hca->ib_dev_attr.max_pkeys; j++) {
                    ibv_query_pkey(hca->ib_dev_context, i, j, &pkey);
                    pkey=ntohs(pkey);
                    if(pkey == mca_btl_openib_component.ib_pkey_val){
                        ret = init_one_port(btl_list, hca, i, j, &ib_port_attr);
                        break;
                    }
                }
            }
            if (OMPI_SUCCESS != ret) {
                /* Out of bounds error indicates that we hit max btl number
                 * don't propagate the error to the caller */
                if(OMPI_ERR_VALUE_OUT_OF_BOUNDS == ret)
                    ret = OMPI_SUCCESS;
                break;
            }
        }
    }

    /* If we made a BTL, we're done.  Otherwise, fall through and
       destroy everything */
    if (hca->btls > 0) {
        ret = prepare_hca_for_use(hca);
        if(OMPI_SUCCESS == ret)
            return OMPI_SUCCESS;
    }

error:
#if defined(OMPI_HAVE_THREADS) && OMPI_ENABLE_PROGRESS_THREADS == 1
    if(hca->ib_channel)
        ibv_destroy_comp_channel(hca->ib_channel);
#endif
    if(hca->mpool)
        mca_mpool_base_module_destroy(hca->mpool);
    if (MCA_BTL_XRC_ENABLED) {
        if(OMPI_SUCCESS != mca_btl_openib_close_xrc_domain(hca)) {
            BTL_ERROR(("XRC Internal error. Failed to close xrc domain"));
        }
    }
    if(hca->ib_pd)
        ibv_dealloc_pd(hca->ib_pd);
    if(hca->ib_dev_context)
        ibv_close_device(hca->ib_dev_context);
    OBJ_RELEASE(hca);
    return ret;
}

static int finish_btl_init(mca_btl_openib_module_t *openib_btl)
{
    int qp;
    openib_btl->num_peers = 0;

    /* Initialize module state */
    OBJ_CONSTRUCT(&openib_btl->ib_lock, opal_mutex_t);

    /* setup the qp structure */
    openib_btl->qps = (mca_btl_openib_module_qp_t*)
        calloc(mca_btl_openib_component.num_qps,
                sizeof(mca_btl_openib_module_qp_t));

    /* setup all the qps */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        if(!BTL_OPENIB_QP_TYPE_PP(qp)) {
            OBJ_CONSTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[0],
                    opal_list_t);
            OBJ_CONSTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[1],
                    opal_list_t);
            openib_btl->qps[qp].u.srq_qp.sd_credits =
                mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
        }
    }

    /* initialize the memory pool using the hca */
    openib_btl->super.btl_mpool = openib_btl->hca->mpool;

    openib_btl->eager_rdma_channels = 0;

    openib_btl->eager_rdma_frag_size = OPAL_ALIGN(
            sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t) +
            sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_footer_t) +
            openib_btl->super.btl_eager_limit,
            mca_btl_openib_component.buffer_alignment, size_t);

    return OMPI_SUCCESS;
}

static struct ibv_device **ibv_get_device_list_compat(int *num_devs)
{
    struct ibv_device **ib_devs;

#ifdef HAVE_IBV_GET_DEVICE_LIST
    ib_devs = ibv_get_device_list(num_devs);
#else
    struct dlist *dev_list;
    struct ibv_device *ib_dev;
    *num_devs = 0;

    /* Determine the number of hca's available on the host */
    dev_list = ibv_get_devices();
    if (NULL == dev_list)
        return NULL;

    dlist_start(dev_list);

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        (*num_devs)++;

    /* Allocate space for the ib devices */
    ib_devs = (struct ibv_device**)malloc(*num_devs * sizeof(struct ibv_dev*));
    if(NULL == ib_devs) {
        *num_devs = 0;
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return NULL;
    }

    dlist_start(dev_list);

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        *(++ib_devs) =  ib_dev;
#endif

    return ib_devs;
}

static void ibv_free_device_list_compat(struct ibv_device **ib_devs)
{
#ifdef HAVE_IBV_GET_DEVICE_LIST
    ibv_free_device_list(ib_devs);
#else
    free(ib_devs);
#endif
}

static opal_carto_graph_t *host_topo;

static int get_ib_dev_distance(struct ibv_device *dev)
{
    opal_paffinity_base_cpu_set_t cpus;
    opal_carto_base_node_t *hca_node;
    int min_distance = -1, i, max_proc_id;
    const char *hca = ibv_get_device_name(dev);

    if(opal_paffinity_base_max_processor_id(&max_proc_id) != OMPI_SUCCESS)
        max_proc_id = 100; /* Choose something big enough */

    hca_node = carto_base_find_node(host_topo, hca);

    /* no topology info for HCA found. Assume that it is close */
    if(NULL == hca_node)
        return 0;

    OPAL_PAFFINITY_CPU_ZERO(cpus);
    opal_paffinity_base_get(&cpus);

    for(i = 0; i < max_proc_id; i++) {
        opal_carto_base_node_t *slot_node;
        int distance, socket, core;
        char *slot;

        if(!OPAL_PAFFINITY_CPU_ISSET(i, cpus))
            continue;

        opal_paffinity_base_map_to_socket_core(i, &socket, &core);
        asprintf(&slot, "slot%d", socket);

        slot_node = carto_base_find_node(host_topo, slot);

        free(slot);

        if(NULL == slot_node)
            return 0;

        distance = carto_base_spf(host_topo, slot_node, hca_node);

        if(distance < 0)
            return 0;

        if(min_distance < 0 || min_distance < distance)
            min_distance = distance;
    }

    return min_distance;
}

struct dev_distance {
    struct ibv_device *ib_dev;
    int distance;
};

static int compare_distance(const void *p1, const void *p2)
{
    const struct dev_distance *d1 = p1;
    const struct dev_distance *d2 = p2;

    return d1->distance - d2->distance;
}

static struct dev_distance *
sort_devs_by_distance(struct ibv_device **ib_devs, int count)
{
    int i;
    struct dev_distance *devs = malloc(count * sizeof(struct dev_distance));

    carto_base_get_host_graph(&host_topo, "Infiniband");

    for(i = 0; i < count; i++) {
        devs[i].ib_dev = ib_devs[i];
        devs[i].distance = get_ib_dev_distance(ib_devs[i]);
    }

    qsort(devs, count, sizeof(struct dev_distance), compare_distance);

    carto_base_free_graph(host_topo);

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
btl_openib_component_init(int *num_btl_modules,
                          bool enable_progress_threads,
                          bool enable_mpi_threads)
{
    struct ibv_device **ib_devs;
    mca_btl_base_module_t** btls;
    int i, ret, num_devs, length;
    opal_list_t btl_list;
    mca_btl_openib_module_t * openib_btl;
    mca_btl_base_selected_module_t* ib_selected;
    opal_list_item_t* item;
    unsigned short seedv[3];
    mca_btl_openib_frag_init_data_t *init_data;
    struct dev_distance *dev_sorted;
    int distance;

    /* initialization */
    *num_btl_modules = 0;
    num_devs = 0;

    seedv[0] = orte_process_info.my_name->vpid;
    seedv[1] = opal_sys_timer_get_cycles();
    seedv[2] = opal_sys_timer_get_cycles();
    seed48(seedv);

    /* Read in INI files with HCA-specific parameters */
    if (OMPI_SUCCESS != (ret = ompi_btl_openib_ini_init())) {
        goto no_btls;
    }

    if(MCA_BTL_XRC_ENABLED) {
        OBJ_CONSTRUCT(&mca_btl_openib_component.ib_addr_table,
                opal_hash_table_t);
    }

    OBJ_CONSTRUCT(&mca_btl_openib_component.send_free_coalesced, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_openib_component.send_user_free, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_openib_component.recv_user_free, ompi_free_list_t);

    init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));

    init_data->order = mca_btl_openib_component.rdma_qp;
    init_data->list = &mca_btl_openib_component.send_user_free;

    if(OMPI_SUCCESS != ompi_free_list_init_ex_new(
                &mca_btl_openib_component.send_user_free,
                sizeof(mca_btl_openib_put_frag_t), 2,
                OBJ_CLASS(mca_btl_openib_put_frag_t),
                0, 0,
                mca_btl_openib_component.ib_free_list_num,
                mca_btl_openib_component.ib_free_list_max,
                mca_btl_openib_component.ib_free_list_inc,
                NULL, mca_btl_openib_frag_init, init_data)) {
        goto no_btls;
    }

    init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));

    init_data->order = mca_btl_openib_component.rdma_qp;
    init_data->list = &mca_btl_openib_component.recv_user_free;

    if(OMPI_SUCCESS != ompi_free_list_init_ex_new(
                &mca_btl_openib_component.recv_user_free,
                sizeof(mca_btl_openib_get_frag_t), 2,
                OBJ_CLASS(mca_btl_openib_get_frag_t),
                0, 0,
                mca_btl_openib_component.ib_free_list_num,
                mca_btl_openib_component.ib_free_list_max,
                mca_btl_openib_component.ib_free_list_inc,
                NULL, mca_btl_openib_frag_init, init_data)) {
        goto no_btls;
    }

    init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));
    length = sizeof(mca_btl_openib_coalesced_frag_t);

    init_data->list = &mca_btl_openib_component.send_free_coalesced;

    if(OMPI_SUCCESS != ompi_free_list_init_ex(
                &mca_btl_openib_component.send_free_coalesced,
                length, 2, OBJ_CLASS(mca_btl_openib_coalesced_frag_t),
                mca_btl_openib_component.ib_free_list_num,
                mca_btl_openib_component.ib_free_list_max,
                mca_btl_openib_component.ib_free_list_inc,
                NULL, mca_btl_openib_frag_init, init_data)) {
        goto no_btls;
    }

    /* If we want fork support, try to enable it */
#ifdef HAVE_IBV_FORK_INIT
    if (0 != mca_btl_openib_component.want_fork_support) {
        if (0 != ibv_fork_init()) {
            /* If the want_fork_support MCA parameter is >0, then the
               user was specifically asking for fork support and we
               couldn't provide it.  So print an error and deactivate
               this BTL. */
            if (mca_btl_openib_component.want_fork_support > 0) {
                opal_show_help("help-mpi-btl-openib.txt",
                               "ibv_fork_init fail", true,
                               orte_system_info.nodename);
                goto no_btls;
            }
        }
    }
#endif

    /* Parse the include and exclude lists, checking for errors */
    mca_btl_openib_component.if_include_list =
        mca_btl_openib_component.if_exclude_list =
        mca_btl_openib_component.if_list = NULL;
    if (NULL != mca_btl_openib_component.if_include &&
        NULL != mca_btl_openib_component.if_exclude) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "specified include and exclude", true,
                       mca_btl_openib_component.if_include,
                       mca_btl_openib_component.if_exclude, NULL);
        goto no_btls;
    } else if (NULL != mca_btl_openib_component.if_include) {
        mca_btl_openib_component.if_include_list =
            opal_argv_split(mca_btl_openib_component.if_include, ',');
        mca_btl_openib_component.if_list =
            opal_argv_copy(mca_btl_openib_component.if_include_list);
    } else if (NULL != mca_btl_openib_component.if_exclude) {
        mca_btl_openib_component.if_exclude_list =
            opal_argv_split(mca_btl_openib_component.if_exclude, ',');
        mca_btl_openib_component.if_list =
            opal_argv_copy(mca_btl_openib_component.if_exclude_list);
    }

    ib_devs = ibv_get_device_list_compat(&num_devs);

    if(0 == num_devs || NULL == ib_devs) {
        mca_btl_base_error_no_nics("OpenIB", "HCA");
        goto no_btls;
    }

    dev_sorted = sort_devs_by_distance(ib_devs, num_devs);

    /* We must loop through all the hca id's, get their handles and
       for each hca we query the number of ports on the hca and set up
       a distinct btl module for each hca port */

    OBJ_CONSTRUCT(&btl_list, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_lock, opal_mutex_t);
#if OMPI_HAVE_THREADS
    mca_btl_openib_component.async_thread = 0;
#endif
    for(i = 0; i < num_devs && (-1 == mca_btl_openib_component.ib_max_btls ||
                mca_btl_openib_component.ib_num_btls <
                mca_btl_openib_component.ib_max_btls); i++) {
        if(0 == mca_btl_openib_component.ib_num_btls)
            distance = dev_sorted[i].distance;
        else if(distance != dev_sorted[i].distance)
            break;

        if(OMPI_SUCCESS !=
           (ret = init_one_hca(&btl_list, dev_sorted[i].ib_dev)))
            break;
    }

    if(ret != OMPI_SUCCESS) {
        opal_show_help("help-mpi-btl-openib.txt",
                "error in hca init", true, orte_system_info.nodename);
    }

    free(dev_sorted);

    /* If we got back from checking all the HCAs and find that there
       are still items in the component.if_list, that means that they
       didn't exist.  Show an appropriate warning if the warning was
       not disabled. */

    if (0 != opal_argv_count(mca_btl_openib_component.if_list) &&
        mca_btl_openib_component.warn_nonexistent_if) {
        char *str = opal_argv_join(mca_btl_openib_component.if_list, ',');
        opal_show_help("help-mpi-btl-openib.txt", "nonexistent port",
                       true, orte_system_info.nodename,
                       ((NULL != mca_btl_openib_component.if_include) ?
                        "in" : "ex"), str);
        free(str);
    }

    if(0 == mca_btl_openib_component.ib_num_btls) {
        opal_show_help("help-mpi-btl-openib.txt",
                "no active ports found", true, orte_system_info.nodename);
        return NULL;
    }

    /* Allocate space for btl modules */
    mca_btl_openib_component.openib_btls =
        malloc(sizeof(mca_btl_openib_module_t*) *
                mca_btl_openib_component.ib_num_btls);
    if(NULL == mca_btl_openib_component.openib_btls) {
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return NULL;
    }
    btls = (struct mca_btl_base_module_t **)
        malloc(mca_btl_openib_component.ib_num_btls *
               sizeof(struct mca_btl_base_module_t*));
    if(NULL == btls) {
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return NULL;
    }

    /* Copy the btl module structs into a contiguous array and fully
       initialize them */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++){
        item = opal_list_remove_first(&btl_list);
        ib_selected = (mca_btl_base_selected_module_t*)item;
        mca_btl_openib_component.openib_btls[i] =
            (mca_btl_openib_module_t*)ib_selected->btl_module;
        OBJ_RELEASE(ib_selected);
        openib_btl = mca_btl_openib_component.openib_btls[i];
        btls[i] = &openib_btl->super;
        if(finish_btl_init(openib_btl) != OMPI_SUCCESS)
            return NULL;
     }

    btl_openib_modex_send();

    *num_btl_modules = mca_btl_openib_component.ib_num_btls;
    ibv_free_device_list_compat(ib_devs);
    if (NULL != mca_btl_openib_component.if_include_list) {
        opal_argv_free(mca_btl_openib_component.if_include_list);
        mca_btl_openib_component.if_include_list = NULL;
    }
    if (NULL != mca_btl_openib_component.if_exclude_list) {
        opal_argv_free(mca_btl_openib_component.if_exclude_list);
        mca_btl_openib_component.if_exclude_list = NULL;
    }
    return btls;

 no_btls:
    /* If we fail early enough in the setup, we just modex around that
       there are no openib BTL's in this process and return NULL. */

    if (MCA_BTL_XRC_ENABLED)
        OBJ_DESTRUCT(&mca_btl_openib_component.ib_addr_table);

    mca_btl_openib_component.ib_num_btls = 0;
    btl_openib_modex_send();
    return NULL;
}

static void progress_pending_eager_rdma(mca_btl_base_endpoint_t *ep)
{
    int qp;
    opal_list_item_t *frag;

    /* Go over all QPs and try to send high prio packets over eager rdma
     * channel */
    OPAL_THREAD_LOCK(&ep->endpoint_lock);
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
       while(ep->qps[qp].qp->sd_wqe > 0 && ep->eager_rdma_remote.tokens > 0) {
            frag = opal_list_remove_first(&ep->qps[qp].pending_frags[0]);
            if(NULL == frag)
                break;
            mca_btl_openib_endpoint_post_send(ep, to_send_frag(frag));
       }
       if(ep->eager_rdma_remote.tokens == 0)
           break;
    }
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
}

static inline int
get_enpoint_credits(mca_btl_base_endpoint_t *ep, const int qp)
{
    return BTL_OPENIB_QP_TYPE_PP(qp) ? ep->qps[qp].u.pp_qp.sd_credits : 1;
}

static void progress_pending_frags_pp(mca_btl_base_endpoint_t *ep, const int qp)
{
    int i;
    opal_list_item_t *frag;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);
    for(i = 0; i < 2; i++) {
       while((get_enpoint_credits(ep, qp) +
                   (1 - i) * ep->eager_rdma_remote.tokens) > 0) {
            frag = opal_list_remove_first(&ep->qps[qp].pending_frags[i]);
            if(NULL == frag)
                break;
            mca_btl_openib_endpoint_post_send(ep, to_send_frag(frag));
       }
    }
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
}

void mca_btl_openib_frag_progress_pending_put_get(mca_btl_base_endpoint_t *ep,
        const int qp)
{
    mca_btl_openib_module_t* openib_btl = ep->endpoint_btl;
    opal_list_item_t *frag;
    size_t i, len = opal_list_get_size(&ep->pending_get_frags);

    for(i = 0; i < len && ep->qps[qp].qp->sd_wqe > 0 && ep->get_tokens > 0; i++)
    {
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        frag = opal_list_remove_first(&(ep->pending_get_frags));
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(NULL == frag)
            break;
        if(mca_btl_openib_get((mca_btl_base_module_t *)openib_btl, ep,
                    &to_base_frag(frag)->base) == OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }

    len = opal_list_get_size(&ep->pending_put_frags);
    for(i = 0; i < len && ep->qps[qp].qp->sd_wqe > 0; i++) {
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        frag = opal_list_remove_first(&(ep->pending_put_frags));
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(NULL == frag)
            break;
        if(mca_btl_openib_put((mca_btl_base_module_t*)openib_btl, ep,
                    &to_base_frag(frag)->base) == OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }
}

static int btl_openib_handle_incoming(mca_btl_openib_module_t *openib_btl,
                                         mca_btl_openib_endpoint_t *ep,
                                         mca_btl_openib_recv_frag_t *frag,
                                         size_t byte_len)
{
    mca_btl_base_descriptor_t *des = &to_base_frag(frag)->base;
    mca_btl_openib_header_t *hdr = frag->hdr;
    int rqp = to_base_frag(frag)->base.order, cqp;
    uint16_t rcredits = 0, credits;
    bool is_credit_msg;

    if(ep->nbo) {
        BTL_OPENIB_HEADER_NTOH(*hdr);
    }

    /* advance the segment address past the header and subtract from the
     * length.*/
    des->des_dst->seg_len = byte_len - sizeof(mca_btl_openib_header_t);

    if(OPAL_LIKELY(!(is_credit_msg = is_credit_message(frag)))) {
        /* call registered callback */
        mca_btl_active_message_callback_t* reg;
        reg = mca_btl_base_active_message_trigger + hdr->tag;
        reg->cbfunc( &openib_btl->super, hdr->tag, des, reg->cbdata );
        if(MCA_BTL_OPENIB_RDMA_FRAG(frag)) {
            cqp = (hdr->credits >> 11) & 0x0f;
            hdr->credits &= 0x87ff;
        } else {
            cqp = rqp;
        }
        if(BTL_OPENIB_IS_RDMA_CREDITS(hdr->credits)) {
            rcredits = BTL_OPENIB_CREDITS(hdr->credits);
            hdr->credits = 0;
        }
    } else {
        mca_btl_openib_rdma_credits_header_t *chdr=des->des_dst->seg_addr.pval;
        if(ep->nbo) {
            BTL_OPENIB_RDMA_CREDITS_HEADER_NTOH(*chdr);
        }
        cqp = chdr->qpn;
        rcredits = chdr->rdma_credits;
    }

    credits = hdr->credits;

    if(hdr->cm_seen)
         OPAL_THREAD_ADD32(&ep->qps[cqp].u.pp_qp.cm_sent, -hdr->cm_seen);

    /* Now return fragment. Don't touch hdr after this point! */
    if(MCA_BTL_OPENIB_RDMA_FRAG(frag)) {
        mca_btl_openib_eager_rdma_local_t *erl = &ep->eager_rdma_local;
        OPAL_THREAD_LOCK(&erl->lock);
        MCA_BTL_OPENIB_RDMA_MAKE_REMOTE(frag->ftr);
        while(erl->tail != erl->head) {
            mca_btl_openib_recv_frag_t *tf;
            tf = MCA_BTL_OPENIB_GET_LOCAL_RDMA_FRAG(ep, erl->tail);
            if(MCA_BTL_OPENIB_RDMA_FRAG_LOCAL(tf))
                break;
            OPAL_THREAD_ADD32(&erl->credits, 1);
            MCA_BTL_OPENIB_RDMA_NEXT_INDEX(erl->tail);
        }
        OPAL_THREAD_UNLOCK(&erl->lock);
    } else {
        MCA_BTL_IB_FRAG_RETURN(frag);
        if(BTL_OPENIB_QP_TYPE_PP(rqp)) {
            if(OPAL_UNLIKELY(is_credit_msg))
                OPAL_THREAD_ADD32(&ep->qps[cqp].u.pp_qp.cm_received, 1);
            else
                OPAL_THREAD_ADD32(&ep->qps[rqp].u.pp_qp.rd_posted, -1);
            mca_btl_openib_endpoint_post_rr(ep, cqp);
        } else {
            mca_btl_openib_module_t *btl = ep->endpoint_btl;
            OPAL_THREAD_ADD32(&btl->qps[rqp].u.srq_qp.rd_posted, -1);
            mca_btl_openib_post_srr(btl, rqp);
        }
    }

    if(rcredits > 0) {
        OPAL_THREAD_ADD32(&ep->eager_rdma_remote.tokens, rcredits);
        progress_pending_eager_rdma(ep);
    }

    assert((cqp != MCA_BTL_NO_ORDER && BTL_OPENIB_QP_TYPE_PP(cqp)) || !credits);

    if(credits) {
        OPAL_THREAD_ADD32(&ep->qps[cqp].u.pp_qp.sd_credits, credits);
        progress_pending_frags_pp(ep, cqp);
    }

    send_credits(ep, cqp);

    return OMPI_SUCCESS;
}

static char* btl_openib_component_status_to_string(enum ibv_wc_status status)
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
    case IBV_WC_LOC_EEC_OP_ERR:
        return "LOCAL EEC OPERATION ERROR";
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
    int i;
    opal_list_item_t *frag;
    mca_btl_openib_qp_t *qp = ep->qps[qpn].qp;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);
    for(i = 0; i < 2; i++) {
       while(qp->sd_wqe > 0) {
            mca_btl_base_endpoint_t *ep;
            OPAL_THREAD_LOCK(&qp->lock);
            frag = opal_list_remove_first(&qp->pending_frags[i]);
            OPAL_THREAD_UNLOCK(&qp->lock);
            if(NULL == frag)
                break;
            ep = to_com_frag(frag)->endpoint;
            mca_btl_openib_endpoint_post_send(ep, to_send_frag(frag));
       }
    }
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
}

static void progress_pending_frags_srq(mca_btl_openib_module_t* openib_btl,
        const int qp)
{
    opal_list_item_t *frag;
    int i;

    assert(BTL_OPENIB_QP_TYPE_SRQ(qp) || BTL_OPENIB_QP_TYPE_XRC(qp));

    for(i = 0; i < 2; i++) {
        while(openib_btl->qps[qp].u.srq_qp.sd_credits > 0) {
            OPAL_THREAD_LOCK(&openib_btl->ib_lock);
            frag = opal_list_remove_first(
                    &openib_btl->qps[qp].u.srq_qp.pending_frags[i]);
            OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);

            if(NULL == frag)
                break;

            mca_btl_openib_endpoint_send(to_com_frag(frag)->endpoint,
                    to_send_frag(frag));
        }
    }
}

static char *cq_name[] = {"HP CQ", "LP CQ"};
static void handle_wc(mca_btl_openib_hca_t* hca, const uint32_t cq,
        struct ibv_wc *wc)
{
    static int flush_err_printed[] = {0, 0};
    mca_btl_openib_com_frag_t* frag;
    mca_btl_base_descriptor_t *des;
    mca_btl_openib_endpoint_t* endpoint;
    mca_btl_openib_module_t *openib_btl = NULL;
    ompi_proc_t* remote_proc = NULL;
    int qp, btl_ownership;

    des = (mca_btl_base_descriptor_t*)(uintptr_t)wc->wr_id;
    frag = to_com_frag(des);

    /* For receive fragments "order" contains QP idx the fragment was posted
     * to. For send fragments "order" contains QP idx the fragment was send
     * through */
    qp = des->order;
    endpoint = frag->endpoint;

    if(endpoint)
        openib_btl = endpoint->endpoint_btl;

    if(wc->status != IBV_WC_SUCCESS)
        goto error;

    /* Handle work completions */
    switch(wc->opcode) {
        case IBV_WC_RDMA_READ:
            OPAL_THREAD_ADD32(&endpoint->get_tokens, 1);
            /* fall through */

        case IBV_WC_RDMA_WRITE:
        case IBV_WC_SEND:
            if(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_SEND) {
                opal_list_item_t *i;
                while((i = opal_list_remove_first(&to_send_frag(des)->coalesced_frags))) {
                    btl_ownership = (to_base_frag(i)->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                    to_base_frag(i)->base.des_cbfunc(&openib_btl->super, endpoint,
                            &to_base_frag(i)->base, OMPI_SUCCESS);
                    if( btl_ownership ) {
                        mca_btl_openib_free(&openib_btl->super, &to_base_frag(i)->base);
                    }
                }
            }
            /* Process a completed send/put/get */
            btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
            des->des_cbfunc(&openib_btl->super, endpoint, des,OMPI_SUCCESS);
            if( btl_ownership ) {
                mca_btl_openib_free(&openib_btl->super, des);
            }

            /* return send wqe */
            qp_put_wqe(endpoint, qp);

            if(IBV_WC_SEND == wc->opcode && !BTL_OPENIB_QP_TYPE_PP(qp)) {
                OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.sd_credits, 1);

                /* new SRQ credit available. Try to progress pending frags*/
                progress_pending_frags_srq(openib_btl, qp);
            }
            /* new wqe or/and get token available. Try to progress pending frags */
            progress_pending_frags_wqe(endpoint, qp);
            mca_btl_openib_frag_progress_pending_put_get(endpoint, qp);
            break;
        case IBV_WC_RECV:
            if(wc->wc_flags & IBV_WC_WITH_IMM) {
                endpoint = (mca_btl_openib_endpoint_t*)
                    opal_pointer_array_get_item(hca->endpoints, wc->imm_data);
                frag->endpoint = endpoint;
                openib_btl = endpoint->endpoint_btl;
            }

            /* Process a RECV */
            if(btl_openib_handle_incoming(openib_btl, endpoint, to_recv_frag(frag),
                        wc->byte_len) != OMPI_SUCCESS) {
                openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL);
                break;
            }

            /* decide if it is time to setup an eager rdma channel */
            if(!endpoint->eager_rdma_local.base.pval && endpoint->use_eager_rdma &&
                    wc->byte_len < mca_btl_openib_component.eager_limit &&
                    openib_btl->eager_rdma_channels <
                    mca_btl_openib_component.max_eager_rdma &&
                    OPAL_THREAD_ADD32(&endpoint->eager_recv_count, 1) ==
                    mca_btl_openib_component.eager_rdma_threshold) {
                mca_btl_openib_endpoint_connect_eager_rdma(endpoint);
            }
            break;
        default:
            BTL_ERROR(("Unhandled work completion opcode is %d", wc->opcode));
            if(openib_btl)
                openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL);
            break;
    }

    return;

error:
    if(endpoint && endpoint->endpoint_proc && endpoint->endpoint_proc->proc_ompi)
        remote_proc = endpoint->endpoint_proc->proc_ompi;

    if(wc->status != IBV_WC_WR_FLUSH_ERR || !flush_err_printed[cq]++) {
        BTL_PEER_ERROR(remote_proc, ("error polling %s with status %s "
                    "status number %d for wr_id %llu opcode %d qp_idx %d",
                    cq_name[cq], btl_openib_component_status_to_string(wc->status),
                    wc->status, wc->wr_id, wc->opcode, qp));
    }

    if(IBV_WC_RETRY_EXC_ERR == wc->status)
        opal_show_help("help-mpi-btl-openib.txt", "btl_openib:retry-exceeded", true);

    if(openib_btl)
        openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL);
}

static int poll_hca(mca_btl_openib_hca_t* hca, int count)
{
    int ne = 0, cq;
    uint32_t hp_iter = 0;
    struct ibv_wc wc;

    hca->pollme = false;
    for(cq = 0; cq < 2 && hp_iter < mca_btl_openib_component.cq_poll_progress;)
    {
        ne = ibv_poll_cq(hca->ib_cq[cq], 1, &wc);
        if(0 == ne) {
            /* don't check low prio cq if there was something in high prio cq,
             * but for each cq_poll_ratio hp cq polls poll lp cq once */
            if(count && hca->hp_cq_polls)
                break;
            cq++;
            hca->hp_cq_polls = mca_btl_openib_component.cq_poll_ratio;
            continue;
        }

        if(ne < 0)
            goto error;

        count++;

        if(BTL_OPENIB_HP_CQ == cq) {
            hca->pollme = true;
            hp_iter++;
            hca->hp_cq_polls--;
        }

        handle_wc(hca, cq, &wc);
    }

    return count;
error:
    BTL_ERROR(("error polling %s with %d errno says %s\n", cq_name[cq], ne,
                strerror(errno)));
    return count;
}

#if OMPI_ENABLE_PROGRESS_THREADS == 1
void* mca_btl_openib_progress_thread(opal_object_t* arg)
{
    opal_thread_t* thread = (opal_thread_t*)arg;
    mca_btl_openib_hca_t* hca = thread->t_arg;
    struct ibv_cq *ev_cq;
    void *ev_ctx;

    /* This thread enter in a cancel enabled state */
    pthread_setcancelstate( PTHREAD_CANCEL_ENABLE, NULL );
    pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS, NULL );

    opal_output(0, "WARNING: the openib btl progress thread code *does not yet work*.  Your run is likely to hang, crash, break the kitchen sink, and/or eat your cat.  You have been warned.");

    while (hca->progress) {
        while(opal_progress_threads()) {
            while(opal_progress_threads())
                sched_yield();
            usleep(100); /* give app a chance to re-enter library */
        }

        if(ibv_get_cq_event(hca->ib_channel, &ev_cq, &ev_ctx))
            BTL_ERROR(("Failed to get CQ event with error %s",
                        strerror(errno)));
        if(ibv_req_notify_cq(ev_cq, 0)) {
            BTL_ERROR(("Couldn't request CQ notification with error %s",
                        strerror(errno)));
        }

        ibv_ack_cq_events(ev_cq, 1);

        while(poll_hca(hca, 0));
    }

    return PTHREAD_CANCELED;
}
#endif

static int progress_one_hca(mca_btl_openib_hca_t *hca)
{
    int i, c, count = 0, ret;
    mca_btl_openib_recv_frag_t* frag;
    mca_btl_openib_endpoint_t* endpoint;
    uint32_t non_eager_rdma_endpoints = 0;

    c = hca->eager_rdma_buffers_count;
    non_eager_rdma_endpoints += (hca->non_eager_rdma_endpoints + hca->pollme);

    for(i = 0; i < c; i++) {
        endpoint = hca->eager_rdma_buffers[i];

        if(!endpoint)
            continue;

        OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
        frag = MCA_BTL_OPENIB_GET_LOCAL_RDMA_FRAG(endpoint,
                endpoint->eager_rdma_local.head);

        if(MCA_BTL_OPENIB_RDMA_FRAG_LOCAL(frag)) {
            uint32_t size;
            mca_btl_openib_module_t *btl = endpoint->endpoint_btl;

            opal_atomic_rmb();

            if(endpoint->nbo) {
                BTL_OPENIB_FOOTER_NTOH(*frag->ftr);
            }
            size = MCA_BTL_OPENIB_RDMA_FRAG_GET_SIZE(frag->ftr);
#if OMPI_ENABLE_DEBUG
            if (frag->ftr->seq != endpoint->eager_rdma_local.seq)
                BTL_ERROR(("Eager RDMA wrong SEQ: received %d expected %d",
                           frag->ftr->seq,
                           endpoint->eager_rdma_local.seq));
            endpoint->eager_rdma_local.seq++;
#endif
            MCA_BTL_OPENIB_RDMA_NEXT_INDEX(endpoint->eager_rdma_local.head);

            OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
            frag->hdr = (mca_btl_openib_header_t*)(((char*)frag->ftr) -
                    size + sizeof(mca_btl_openib_footer_t));
            to_base_frag(frag)->segment.seg_addr.pval =
                ((unsigned char* )frag->hdr) + sizeof(mca_btl_openib_header_t);

            ret = btl_openib_handle_incoming(btl, to_com_frag(frag)->endpoint,
                    frag, size - sizeof(mca_btl_openib_footer_t));
            if (ret != MPI_SUCCESS) {
                btl->error_cb(&btl->super, MCA_BTL_ERROR_FLAGS_FATAL);
                return 0;
            }

            count++;
        } else
            OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
    }

    hca->eager_rdma_polls--;

    if(0 == count || non_eager_rdma_endpoints != 0 || !hca->eager_rdma_polls) {
        count += poll_hca(hca, count);
        hca->eager_rdma_polls = mca_btl_openib_component.eager_rdma_poll_ratio;
    }

    return count;
}

/*
 *  IB component progress.
 */
static int btl_openib_component_progress(void)
{
    int i;
    int count = 0;

#if OMPI_HAVE_THREADS
    if(OPAL_UNLIKELY(mca_btl_openib_component.use_async_event_thread &&
            mca_btl_openib_component.fatal_counter)) {
        goto error;
    }
#endif

    for(i = 0; i < mca_btl_openib_component.hcas_count; i++) {
        mca_btl_openib_hca_t *hca =
            opal_pointer_array_get_item(&mca_btl_openib_component.hcas, i);
        count += progress_one_hca(hca);
    }

    return count;

#if OMPI_HAVE_THREADS
error:
    /* Set the fatal counter to zero */
    mca_btl_openib_component.fatal_counter = 0;
    /* Lets found all fatal events */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        mca_btl_openib_module_t* openib_btl =
            mca_btl_openib_component.openib_btls[i];
        if(openib_btl->hca->got_fatal_event) {
            openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL);
        }
    }
    return count;
#endif
}

int mca_btl_openib_post_srr(mca_btl_openib_module_t* openib_btl, const int qp)
{
    int rd_low = mca_btl_openib_component.qp_infos[qp].rd_low;
    int rd_num = mca_btl_openib_component.qp_infos[qp].rd_num;
    int num_post, i, rc;
    struct ibv_recv_wr *bad_wr, *wr_list = NULL, *wr = NULL;

    assert(!BTL_OPENIB_QP_TYPE_PP(qp));

    OPAL_THREAD_LOCK(&openib_btl->ib_lock);
    if(openib_btl->qps[qp].u.srq_qp.rd_posted > rd_low) {
        OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
        return OMPI_SUCCESS;
    }
    num_post = rd_num - openib_btl->qps[qp].u.srq_qp.rd_posted;

    for(i = 0; i < num_post; i++) {
        ompi_free_list_item_t* item;
        OMPI_FREE_LIST_WAIT(&openib_btl->hca->qps[qp].recv_free, item, rc);
        to_base_frag(item)->base.order = qp;
        to_com_frag(item)->endpoint = NULL;
        if(NULL == wr)
            wr = wr_list = &to_recv_frag(item)->rd_desc;
        else
            wr = wr->next = &to_recv_frag(item)->rd_desc;
    }

    wr->next = NULL;

    rc = ibv_post_srq_recv(openib_btl->qps[qp].u.srq_qp.srq, wr_list, &bad_wr);
    if(OPAL_LIKELY(0 == rc)) {
        OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.rd_posted, num_post);
        OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
        return OMPI_SUCCESS;
    }

    for(i = 0; wr_list && wr_list != bad_wr; i++, wr_list = wr_list->next);

    BTL_ERROR(("error posting receive descriptors to shared receive "
                "queue %d (%d from %d)", qp, i, num_post));

    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
    return OMPI_ERROR;
}
