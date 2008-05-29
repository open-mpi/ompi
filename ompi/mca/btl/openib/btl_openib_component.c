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
 * Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
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
#include <infiniband/driver.h>
#include <errno.h>
#include <string.h>   /* for strerror()*/
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/include/opal/align.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "orte/util/output.h"
#include "opal/sys/timer.h"
#include "opal/sys/atomic.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/installdirs/installdirs.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

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
#include "btl_openib_fd.h"
#if OMPI_HAVE_THREADS
#include "btl_openib_async.h"
#endif
#include "connect/base.h"
#include "btl_openib_iwarp.h"

/*
 * Local functions
 */
static int btl_openib_component_open(void);
static int btl_openib_component_close(void);
static mca_btl_base_module_t **btl_openib_component_init(int*, bool, bool);
static int btl_openib_component_progress(void);

/*
 * Local variables
 */
static mca_btl_openib_hca_t *receive_queues_hca = NULL;

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
    ompi_btl_openib_connect_base_finalize();
    ompi_btl_openib_fd_finalize();
    ompi_btl_openib_ini_finalize();
    if (NULL != mca_btl_openib_component.receive_queues) {
        free(mca_btl_openib_component.receive_queues);
    }
    return OMPI_SUCCESS;
}

static bool check_basics(void)
{
    int rc;
    char *file;
    struct stat s;

    /* Check to see if $sysfsdir/class/infiniband/ exists */
    asprintf(&file, "%s/class/infiniband", ibv_get_sysfs_path());
    if (NULL == file) {
        return false;
    }
    rc = stat(file, &s);
    free(file);
    if (0 != rc || !S_ISDIR(s.st_mode)) {
        return false;
    }

    /* It exists and is a directory -- good enough */
    return true;
}

static void inline pack8(char **dest, uint8_t value)
{
    /* Copy one character */
    **dest = (char) value;
    /* Most the dest ahead one */
    ++*dest;
}

/*
 *  Register local openib port information with the modex so that it
 *  can be shared with all other peers.
 */
static int btl_openib_modex_send(void)
{
    int rc, i, j;
    int modex_message_size;
    mca_btl_openib_modex_message_t dummy;
    char *message, *offset;
    size_t size, msg_size;
    ompi_btl_openib_connect_base_module_t *cpc;

    orte_output(-1, "Starting to modex send");
    if (0 == mca_btl_openib_component.ib_num_btls) {
        return 0;
    }
    modex_message_size = ((char *) &(dummy.end)) - ((char*) &dummy);

    /* The message is packed into multiple parts:
     * 1. a uint8_t indicating the number of modules (ports) in the message
     * 2. for each module:
     *    a. the common module data
     *    b. a uint8_t indicating how many CPCs follow
     *    c. for each CPC:
     *       a. a uint8_t indicating the index of the CPC in the all[]
     *          array in btl_openib_connect_base.c
     *       b. a uint8_t indicating the priority of this CPC
     *       c. a uint8_t indicating the length of the blob to follow
     *       d. a blob that is only meaningful to that CPC
     */
    msg_size = 
        /* uint8_t for number of modules in the message */
        1 +
        /* For each module: */
        mca_btl_openib_component.ib_num_btls * 
        (
         /* Common module data */
         modex_message_size + 
         /* uint8_t for how many CPCs follow */
         1
         );
    /* For each module, add in the size of the per-CPC data */
    for (i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        for (j = 0; 
             j < mca_btl_openib_component.openib_btls[i]->num_cpcs;
             ++j) {
            msg_size += 
                /* uint8_t for the index of the CPC */
                1 +
                /* uint8_t for the CPC's priority */
                1 + 
                /* uint8_t for the blob length */
                1 +
                /* blob length */
                mca_btl_openib_component.openib_btls[i]->cpcs[j]->data.cbm_modex_message_len;
        }
    }
    message = malloc(msg_size);
    if (NULL == message) {
        BTL_ERROR(("Failed malloc"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Pack the number of modules */
    offset = message;
    pack8(&offset, mca_btl_openib_component.ib_num_btls);
    orte_output(-1, "modex sending %d btls (packed: %d, offset now at %d)", mca_btl_openib_component.ib_num_btls, *((uint8_t*) message), (int) (offset - message));

    /* Pack each of the modules */
    for (i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {

        /* Pack the modex common message struct.  */
        size = modex_message_size;
        memcpy(offset, 
               &(mca_btl_openib_component.openib_btls[i]->port_info), 
               size);
        orte_output(-1, "modex packed btl port modex message: %lx, %d, %d (size: %d)",
                    mca_btl_openib_component.openib_btls[i]->port_info.subnet_id,
                    mca_btl_openib_component.openib_btls[i]->port_info.mtu,
                    mca_btl_openib_component.openib_btls[i]->port_info.lid,
                    (int) size);
                    
#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
        MCA_BTL_OPENIB_MODEX_MSG_HTON(*(mca_btl_openib_modex_message_t *)offset);
#endif
        offset += size;
        orte_output(-1, "modex packed btl %d: modex message, offset now %d",
                    i, (int) (offset -message));

        /* Pack the number of CPCs that follow */
        pack8(&offset, 
              mca_btl_openib_component.openib_btls[i]->num_cpcs);
        orte_output(-1, "modex packed btl %d: to pack %d cpcs (packed: %d, offset now %d)",
                    i, mca_btl_openib_component.openib_btls[i]->num_cpcs,
                    *((uint8_t*) (offset - 1)), (int) (offset-message));

        /* Pack each CPC */
        for (j = 0; 
             j < mca_btl_openib_component.openib_btls[i]->num_cpcs;
             ++j) {
            uint8_t u8;

            cpc = mca_btl_openib_component.openib_btls[i]->cpcs[j];
            orte_output(-1, "modex packed btl %d: packing cpc %s", 
                        i, cpc->data.cbm_component->cbc_name);
            /* Pack the CPC index */
            u8 = ompi_btl_openib_connect_base_get_cpc_index(cpc->data.cbm_component);
            pack8(&offset, u8);
            orte_output(-1, "packing btl %d: cpc %d: index %d (packed %d, offset now %d)",
                        i, j, u8, *((uint8_t*) (offset-1)), (int)(offset-message));
            /* Pack the CPC priority */
            pack8(&offset, cpc->data.cbm_priority);
            orte_output(-1, "packing btl %d: cpc %d: priority %d (packed %d, offset now %d)",
                        i, j, cpc->data.cbm_priority, *((uint8_t*) (offset-1)), (int)(offset-message));
            /* Pack the blob length */
            u8 = cpc->data.cbm_modex_message_len;
            pack8(&offset, u8);
            orte_output(-1, "packing btl %d: cpc %d: message len %d (packed %d, offset now %d)",
                        i, j, u8, *((uint8_t*) (offset-1)), (int)(offset-message));
            /* If the blob length is > 0, pack the blob */
            if (u8 > 0) {
                memcpy(offset, cpc->data.cbm_modex_message, u8);
                offset += u8;
                orte_output(-1, "packing btl %d: cpc %d: blob packed %d %x (offset now %d)",
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
    rc = ompi_modex_send(&mca_btl_openib_component.super.btl_version,
                         message, msg_size);
    free(message);
    orte_output(-1, "Modex sent!  %d calculated, %d actual\n", (int) msg_size, (int) (offset - message));

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

       BTL_VERBOSE(("prior to NTOH received  rkey %lu, rdma_start.lval %llu, pval %p, ival %u",
                  rdma_hdr->rkey,
                  (unsigned long) rdma_hdr->rdma_start.lval,
                  rdma_hdr->rdma_start.pval,
                  rdma_hdr->rdma_start.ival
                  ));

       if(ep->nbo) {
           BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_NTOH(*rdma_hdr);
       }

       BTL_VERBOSE(("received  rkey %lu, rdma_start.lval %llu, pval %p,"
                   " ival %u", rdma_hdr->rkey,
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
            BTL_ERROR(("%s: error unpinning openib memory errno says %s",
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

    /* If we have struct ibv_device.transport_type, then we're >= OFED
       v1.2, and the transport could be iWarp or IB.  If we don't have
       that member, then we're < OFED v1.2, and it can only be IB. */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (IBV_TRANSPORT_IWARP == hca->ib_dev->transport_type) {
        subnet_id = mca_btl_openib_get_iwarp_subnet_id(hca->ib_dev);
    } else {
        ibv_query_gid(hca->ib_dev_context, port_num, 0, &gid);
        subnet_id = ntoh64(gid.global.subnet_prefix);
    }
#else
    ibv_query_gid(hca->ib_dev_context, port_num, 0, &gid);
    subnet_id = ntoh64(gid.global.subnet_prefix);
#endif

    BTL_VERBOSE(("my subnet_id is %016x", subnet_id));

    if(mca_btl_openib_component.ib_num_btls > 0 &&
            IB_DEFAULT_GID_PREFIX == subnet_id &&
            mca_btl_openib_component.warn_default_gid_prefix) {
        orte_show_help("help-mpi-btl-openib.txt", "default subnet prefix",
                true, orte_process_info.nodename);
    }

    lmc = (1 << ib_port_attr->lmc);
    lmc_step = 1;

    if (0 != mca_btl_openib_component.max_lmc &&
        mca_btl_openib_component.max_lmc < lmc) {
        lmc = mca_btl_openib_component.max_lmc;
    }

#if OMPI_HAVE_THREADS
    /* APM support -- only meaningful if async event support is
       enabled.  If async events are not enabled, then there's nothing
       to listen for the APM event to load the new path, so it's not
       worth enabling APM.  */
    if (lmc > 1){
        if (-1 == mca_btl_openib_component.apm_lmc) {
            lmc_step = lmc;
            mca_btl_openib_component.apm_lmc = lmc - 1;
        } else if (0 == lmc % (mca_btl_openib_component.apm_lmc + 1)) {
            lmc_step = mca_btl_openib_component.apm_lmc + 1;
        } else {
            orte_show_help("help-mpi-btl-openib.txt", "apm with wrong lmc",true,
                    mca_btl_openib_component.apm_lmc, lmc);
            return OMPI_ERROR;
        }
    } else {
        if (mca_btl_openib_component.apm_lmc) {
            /* Disable apm and report warning */
            mca_btl_openib_component.apm_lmc = 0;
            orte_show_help("help-mpi-btl-openib.txt", "apm without lmc",true);
        }
    }
#endif

    for(lid = ib_port_attr->lid;
            lid < ib_port_attr->lid + lmc; lid += lmc_step){
        for(i = 0; i < mca_btl_openib_component.btls_per_lid; i++){
            char param[40];

            openib_btl = malloc(sizeof(mca_btl_openib_module_t));
            if(NULL == openib_btl) {
                BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
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
            openib_btl->apm_port = 0;
            openib_btl->src_path_bits = lid - ib_port_attr->lid;

            openib_btl->port_info.subnet_id = subnet_id;
            openib_btl->port_info.mtu = hca->mtu;
            openib_btl->port_info.lid = lid;

            openib_btl->cpcs = NULL;
            openib_btl->num_cpcs = 0;

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
            opal_pointer_array_add(hca->hca_btls, (void*) openib_btl);
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
    hca->ib_dev = NULL;
    hca->ib_dev_context = NULL;
    hca->ib_pd = NULL;
    hca->mpool = NULL;
#if OMPI_ENABLE_PROGRESS_THREADS
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
    hca->qps = NULL;
    OBJ_CONSTRUCT(&hca->hca_lock, opal_mutex_t);
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
    OBJ_DESTRUCT(&hca->send_free_control);
    if (NULL != hca->qps) {
        for (i = 0; i < mca_btl_openib_component.num_qps; i++) {
            OBJ_DESTRUCT(&hca->qps[i].send_free);
            OBJ_DESTRUCT(&hca->qps[i].recv_free);
        }
        free(hca->qps);
    }
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

#if HAVE_XRC
    /* if user configured to run with XRC qp and the device doesn't
     * support it - we should ignore this hca. Maybe we have another
     * one that has XRC support
     */
    if (!(hca->ib_dev_attr.device_cap_flags & IBV_DEVICE_XRC) &&
            MCA_BTL_XRC_ENABLED) {
        orte_show_help("help-mpi-btl-openib.txt",
                "XRC on device without XRC support", true,
                mca_btl_openib_component.num_xrc_qps,
                ibv_get_device_name(hca->ib_dev),
                orte_process_info.nodename);
        return OMPI_ERROR;
    }

    if (MCA_BTL_XRC_ENABLED) {
        if (OMPI_SUCCESS != mca_btl_openib_open_xrc_domain(hca)) {
            BTL_ERROR(("XRC Internal error. Failed to open xrc domain"));
            return OMPI_ERROR;
        }
    }
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
            BTL_ERROR(("Memory allocation fails"));
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

/*
 * Prefer values that are already in the target
 */
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

    if (NULL == target->receive_queues && NULL != src->receive_queues) {
        target->receive_queues = strdup(src->receive_queues);
    }
}

static bool inline is_credit_message(const mca_btl_openib_recv_frag_t *frag)
{
    mca_btl_openib_control_header_t* chdr =
        to_base_frag(frag)->segment.seg_addr.pval;
    return (MCA_BTL_TAG_BTL == frag->hdr->tag) &&
        (MCA_BTL_OPENIB_CONTROL_CREDITS == chdr->type);
}

static int32_t atoi_param(char *param, int32_t dflt)
{
    if (NULL == param || '\0' == param[0]) {
        return dflt ? dflt : 1;
    }

    return atoi(param);
}

static void init_apm_port(mca_btl_openib_hca_t *hca, int port, uint16_t lid)
{
    int index;
    struct mca_btl_openib_module_t *btl;
    for(index = 0; index < hca->btls; index++) {
        btl = opal_pointer_array_get_item(hca->hca_btls, index);
        /* Ok, we already have btl for the fist port,
         * second one will be used for APM */
        btl->apm_port = port;
        btl->port_info.apm_lid = lid + btl->src_path_bits;
        mca_btl_openib_component.apm_ports++;
        BTL_VERBOSE(("APM-PORT: Setting alternative port - %d, lid - %d"
                    ,port ,lid));
    }
}

static int setup_qps(void)
{
    char **queues, **params = NULL;
    int num_xrc_qps = 0, num_pp_qps = 0, num_srq_qps = 0, qp = 0;
    uint32_t max_qp_size, max_size_needed;
    int32_t min_freelist_size = 0;
    int smallest_pp_qp = 0, ret = OMPI_ERROR;

    queues = opal_argv_split(mca_btl_openib_component.receive_queues, ':');
    if (0 == opal_argv_count(queues)) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "no qps in receive_queues", true,
                       orte_process_info.nodename, 
                       mca_btl_openib_component.receive_queues);
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
        } else if (0 == strncmp("X,", queues[qp], 2)) {
#if HAVE_XRC
            num_xrc_qps++;
#else
            orte_show_help("help-mpi-btl-openib.txt", "No XRC support", true,
                           orte_process_info.nodename, 
                           mca_btl_openib_component.receive_queues);
            ret = OMPI_ERR_NOT_AVAILABLE;
            goto error;
#endif
        } else {
            orte_show_help("help-mpi-btl-openib.txt",
                           "invalid qp type in receive_queues", true,
                           orte_process_info.nodename, 
                           mca_btl_openib_component.receive_queues,
                           queues[qp]);
            ret = OMPI_ERR_BAD_PARAM;
            goto error;
        }
        qp++;
    }
    /* Current XRC implementation can't used with other QP types - PP
       and SRQ */
    if (num_xrc_qps > 0 && (num_pp_qps > 0 || num_srq_qps > 0)) {
        orte_show_help("help-mpi-btl-openib.txt", "XRC with PP or SRQ", true,
                       orte_process_info.nodename, 
                       mca_btl_openib_component.receive_queues);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    }

    /* Current XRC implementation can't used with btls_per_lid > 1 */
    if (num_xrc_qps > 0 && mca_btl_openib_component.btls_per_lid > 1) {
        orte_show_help("help-mpi-btl-openib.txt", "XRC with BTLs per LID", 
                       true, orte_process_info.nodename, 
                       mca_btl_openib_component.receive_queues, num_xrc_qps);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    }
    mca_btl_openib_component.num_pp_qps = num_pp_qps;
    mca_btl_openib_component.num_srq_qps = num_srq_qps;
    mca_btl_openib_component.num_xrc_qps = num_xrc_qps;
    mca_btl_openib_component.num_qps = num_pp_qps + num_srq_qps + num_xrc_qps;

    mca_btl_openib_component.qp_infos = (mca_btl_openib_qp_info_t*)
        malloc(sizeof(mca_btl_openib_qp_info_t) *
                mca_btl_openib_component.num_qps);

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
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid pp qp specification", true,
                               orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }
            mca_btl_openib_component.qp_infos[qp].type = MCA_BTL_OPENIB_PP_QP;
            mca_btl_openib_component.qp_infos[qp].size = atoi_param(P(1), 0);
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

            mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_win = rd_win;
            mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv = rd_rsv;
            if ((rd_num - rd_low) > rd_win) {
                orte_show_help("help-mpi-btl-openib.txt", "non optimal rd_win",
                        true, rd_win, rd_num - rd_low);
            }
        } else {
            int32_t sd_max;
            if (count < 3 || count > 5) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid srq specification", true,
                               orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }
            mca_btl_openib_component.qp_infos[qp].type = (params[0][0] =='X') ?
                MCA_BTL_OPENIB_XRC_QP : MCA_BTL_OPENIB_SRQ_QP;
            mca_btl_openib_component.qp_infos[qp].size = atoi_param(P(1), 0);
            rd_num = atoi_param(P(2), 256);
            /* by default set rd_low to be 3/4 of rd_num */
            rd_low = atoi_param(P(3), rd_num - (rd_num / 4));
            sd_max = atoi_param(P(4), rd_low / 4);
            BTL_VERBOSE(("srq: rd_num is %d rd_low is %d sd_max is %d",
                         rd_num, rd_low, sd_max));

            /* Calculate the smallest freelist size that can be allowed */
            if (rd_num > min_freelist_size) {
                min_freelist_size = rd_num;
            }

            mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max = sd_max;
        }

        if (rd_num <= rd_low) {
            orte_show_help("help-mpi-btl-openib.txt", "rd_num must be > rd_low",
                    true, orte_process_info.nodename, queues[qp]);
            ret = OMPI_ERR_BAD_PARAM;
            goto error;
        }
        mca_btl_openib_component.qp_infos[qp].rd_num = rd_num;
        mca_btl_openib_component.qp_infos[qp].rd_low = rd_low;
        opal_argv_free(params);
        qp++;
    }
    params = NULL;

    /* Sanity check some sizes */

    max_qp_size = mca_btl_openib_component.qp_infos[mca_btl_openib_component.num_qps - 1].size;
    max_size_needed = (mca_btl_openib_module.super.btl_eager_limit >
                       mca_btl_openib_module.super.btl_max_send_size) ?
        mca_btl_openib_module.super.btl_eager_limit :
        mca_btl_openib_module.super.btl_max_send_size;
    if (max_qp_size < max_size_needed) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "biggest qp size is too small", true,
                       orte_process_info.nodename, max_qp_size,
                       max_size_needed);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    } else if (max_qp_size > max_size_needed) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "biggest qp size is too big", true,
                       orte_process_info.nodename, max_qp_size,
                       max_size_needed);
    }

    if (mca_btl_openib_component.ib_free_list_max > 0 &&
        min_freelist_size > mca_btl_openib_component.ib_free_list_max) {
        orte_show_help("help-mpi-btl-openib.txt", "freelist too small", true,
                       orte_process_info.nodename,
                       mca_btl_openib_component.ib_free_list_max,
                       min_freelist_size);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    }

    mca_btl_openib_component.rdma_qp = mca_btl_openib_component.num_qps - 1;
    mca_btl_openib_component.credits_qp = smallest_pp_qp;

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
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    hca->ib_dev = ib_dev;
    hca->ib_dev_context = ibv_open_device(ib_dev);
    hca->ib_pd = NULL;
    hca->hca_btls = OBJ_NEW(opal_pointer_array_t);
    if (OPAL_SUCCESS != opal_pointer_array_init(hca->hca_btls, 2, INT_MAX, 2)) {
        BTL_ERROR(("Failed to initialize hca_btls array: %s:%d", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if(NULL == hca->ib_dev_context){
        BTL_ERROR(("error obtaining device context for %s errno says %s",
                    ibv_get_device_name(hca->ib_dev), strerror(errno)));
        goto error;
    }

    if(ibv_query_device(hca->ib_dev_context, &hca->ib_dev_attr)){
        BTL_ERROR(("error obtaining device attributes for %s errno says %s",
                    ibv_get_device_name(hca->ib_dev), strerror(errno)));
        goto error;
    }
    /* If mca_btl_if_include/exclude were specified, get usable ports */
    allowed_ports = (int*)malloc(hca->ib_dev_attr.phys_port_cnt * sizeof(int));
    port_cnt = get_port_list(hca, allowed_ports);
    if (0 == port_cnt) {
        free(allowed_ports);
        ret = OMPI_SUCCESS;
        goto error;
    }

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
            orte_show_help("help-mpi-btl-openib.txt",
                           "no hca params found", true,
                           orte_process_info.nodename,
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
            BTL_ERROR(("invalid MTU value specified in INI file (%d); ignored", values.mtu));
            hca->mtu = mca_btl_openib_component.ib_mtu;
            break;
        }
    } else {
        hca->mtu = mca_btl_openib_component.ib_mtu;
    }

    /* If the user specified btl_openib_receive_queues MCA param, it
       overrides all HCA INI params */
    if (BTL_OPENIB_RQ_SOURCE_MCA != 
        mca_btl_openib_component.receive_queues_source && 
        NULL != values.receive_queues) {
        /* If a prior HCA's INI values set a different value for
           receive_queues, this is unsupported (see
           https://svn.open-mpi.org/trac/ompi/ticket/1285) */
        if (BTL_OPENIB_RQ_SOURCE_HCA_INI ==
            mca_btl_openib_component.receive_queues_source) {
            if (0 != strcmp(values.receive_queues, 
                            mca_btl_openib_component.receive_queues)) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "conflicting receive_queues", true,
                               orte_process_info.nodename,
                               ibv_get_device_name(hca->ib_dev),
                               hca->ib_dev_attr.vendor_id,
                               hca->ib_dev_attr.vendor_part_id,
                               values.receive_queues,
                               ibv_get_device_name(receive_queues_hca->ib_dev),
                               receive_queues_hca->ib_dev_attr.vendor_id,
                               receive_queues_hca->ib_dev_attr.vendor_part_id,
                               mca_btl_openib_component.receive_queues,
                               opal_install_dirs.pkgdatadir);
                ret = OMPI_ERR_RESOURCE_BUSY;
                goto error;
            }
        } else {
            if (NULL != mca_btl_openib_component.receive_queues) {
                free(mca_btl_openib_component.receive_queues);
            }
            receive_queues_hca = hca;
            mca_btl_openib_component.receive_queues = 
                strdup(values.receive_queues);
            mca_btl_openib_component.receive_queues_source =
                BTL_OPENIB_RQ_SOURCE_HCA_INI;
        }
    }

    /* If "use eager rdma" was set, then enable it on this HCA */
    if (values.use_eager_rdma_set) {
        hca->use_eager_rdma = values.use_eager_rdma;
    }

#if HAVE_XRC
    /* if user configured to run with XRC qp and the device doesn't
     * support it - we should ignore this hca. Maybe we have another
     * one that has XRC support
     */
    if (!(hca->ib_dev_attr.device_cap_flags & IBV_DEVICE_XRC) &&
            mca_btl_openib_component.num_xrc_qps > 0) {
        orte_show_help("help-mpi-btl-openib.txt",
                "XRC on device without XRC support", true,
                mca_btl_openib_component.num_xrc_qps,
                ibv_get_device_name(hca->ib_dev),
                orte_process_info.nodename);
        ret = OMPI_SUCCESS;
        goto error;
    }
#endif

    /* Allocate the protection domain for the HCA */
    hca->ib_pd = ibv_alloc_pd(hca->ib_dev_context);
    if(NULL == hca->ib_pd){
        BTL_ERROR(("error allocating protection domain for %s errno says %s",
                    ibv_get_device_name(hca->ib_dev), strerror(errno)));
        goto error;
    }

#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED) {
        if (OMPI_SUCCESS != mca_btl_openib_open_xrc_domain(hca)) {
            BTL_ERROR(("XRC Internal error. Failed to open xrc domain"));
            goto error;
        }
    }
#endif

    mpool_resources.reg_data = (void*)hca;
    mpool_resources.sizeof_reg = sizeof(mca_btl_openib_reg_t);
    mpool_resources.register_mem = openib_reg_mr;
    mpool_resources.deregister_mem = openib_dereg_mr;
    hca->mpool =
        mca_mpool_base_module_create(mca_btl_openib_component.ib_mpool_name,
                hca, &mpool_resources);
    if(NULL == hca->mpool){
         BTL_ERROR(("error creating IB memory pool for %s errno says %s",
                     ibv_get_device_name(hca->ib_dev), strerror(errno)));
         goto error;
    }

#if OMPI_ENABLE_PROGRESS_THREADS
    hca->ib_channel = ibv_create_comp_channel(hca->ib_dev_context);
    if (NULL == hca->ib_channel) {
        BTL_ERROR(("error creating channel for %s errno says %s",
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
                        ibv_get_device_name(hca->ib_dev), i, strerror(errno)));
            break;
        }
        if(IBV_PORT_ACTIVE == ib_port_attr.state) {
            if (mca_btl_openib_component.apm_ports && hca->btls > 0) {
                init_apm_port(hca, i, ib_port_attr.lid);
                break;
            }
            if (0 == mca_btl_openib_component.ib_pkey_val) {
                ret = init_one_port(btl_list, hca, i, mca_btl_openib_component.ib_pkey_ix,
                                    &ib_port_attr);
            } else {
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
                if (OMPI_ERR_VALUE_OUT_OF_BOUNDS == ret) {
                    ret = OMPI_SUCCESS;
                }
                break;
            }
        }
    }
    free(allowed_ports);

    /* If we made a BTL, check APM status and return.  Otherwise, fall
       through and destroy everything */
    if (hca->btls > 0) {
        /* if apm was enabled it should be > 1 */
        if (1 == mca_btl_openib_component.apm_ports) {
            orte_show_help("help-mpi-btl-openib.txt",
                           "apm not enough ports", true);
            mca_btl_openib_component.apm_ports = 0;
        }
        return OMPI_SUCCESS;
    }

error:
#if OMPI_ENABLE_PROGRESS_THREADS
    if (hca->ib_channel) {
        ibv_destroy_comp_channel(hca->ib_channel);
    }
#endif
    if (hca->mpool) {
        mca_mpool_base_module_destroy(hca->mpool);
    }
#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED) {
        if(OMPI_SUCCESS != mca_btl_openib_close_xrc_domain(hca)) {
            BTL_ERROR(("XRC Internal error. Failed to close xrc domain"));
        }
    }
#endif
    if (hca->ib_pd) {
        ibv_dealloc_pd(hca->ib_pd);
    }
    if (hca->ib_dev_context) {
        ibv_close_device(hca->ib_dev_context);
    }
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
    for (qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        if (!BTL_OPENIB_QP_TYPE_PP(qp)) {
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
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
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
    int min_distance = -1, i, max_proc_id, num_processors;
    const char *hca = ibv_get_device_name(dev);

    if(opal_paffinity_base_get_processor_info(&num_processors, &max_proc_id) != OMPI_SUCCESS)
        max_proc_id = 100; /* Choose something big enough */

    hca_node = opal_carto_base_find_node(host_topo, hca);

    /* no topology info for HCA found. Assume that it is close */
    if(NULL == hca_node)
        return 0;

    OPAL_PAFFINITY_CPU_ZERO(cpus);
    opal_paffinity_base_get(&cpus);

    for (i = 0; i < max_proc_id; i++) {
        opal_carto_base_node_t *slot_node;
        int distance, socket, core;
        char *slot;

        if(!OPAL_PAFFINITY_CPU_ISSET(i, cpus))
            continue;

        opal_paffinity_base_map_to_socket_core(i, &socket, &core);
        asprintf(&slot, "slot%d", socket);

        slot_node = opal_carto_base_find_node(host_topo, slot);

        free(slot);

        if(NULL == slot_node)
            return 0;

        distance = opal_carto_base_spf(host_topo, slot_node, hca_node);

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

    /* Per https://svn.open-mpi.org/trac/ompi/ticket/1305, check to
       see if $sysfsdir/class/infiniband exists.  If it does not,
       assume that the RDMA hardware drivers are not loaded, and
       therefore we don't want OpenFabrics verbs support in this OMPI
       job.  No need to print a warning. */
    if (!check_basics()) {
        return NULL;
    }
    

    seedv[0] = ORTE_PROC_MY_NAME->vpid;
    seedv[1] = opal_sys_timer_get_cycles();
    seedv[2] = opal_sys_timer_get_cycles();
    seed48(seedv);

    /* Read in INI files with HCA-specific parameters */
    if (OMPI_SUCCESS != (ret = ompi_btl_openib_ini_init())) {
        goto no_btls;
    }

    /* Initialize FD listening */
    if (OMPI_SUCCESS != ompi_btl_openib_fd_init()) {
        goto no_btls;
    }

    /* Init CPC components */
    if (OMPI_SUCCESS != (ret = ompi_btl_openib_connect_base_init())) {
        goto no_btls;
    }

    OBJ_CONSTRUCT(&mca_btl_openib_component.send_free_coalesced, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_openib_component.send_user_free, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_openib_component.recv_user_free, ompi_free_list_t);

    init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));

    init_data->order = mca_btl_openib_component.rdma_qp;
    init_data->list = &mca_btl_openib_component.send_user_free;

    if (OMPI_SUCCESS != ompi_free_list_init_ex_new(
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
                orte_show_help("help-mpi-btl-openib.txt",
                               "ibv_fork_init fail", true,
                               orte_process_info.nodename);
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
        orte_show_help("help-mpi-btl-openib.txt",
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
        mca_btl_base_error_no_nics("OpenFabrics (openib)", "HCA");
        goto no_btls;
    }

    dev_sorted = sort_devs_by_distance(ib_devs, num_devs);

    OBJ_CONSTRUCT(&btl_list, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_lock, opal_mutex_t);
#if OMPI_HAVE_THREADS
    mca_btl_openib_component.async_thread = 0;
#endif
    for (i = 0; i < num_devs && (-1 == mca_btl_openib_component.ib_max_btls ||
                mca_btl_openib_component.ib_num_btls <
                mca_btl_openib_component.ib_max_btls); i++) {
        if (0 == mca_btl_openib_component.ib_num_btls) {
            distance = dev_sorted[i].distance;
        } else if (distance != dev_sorted[i].distance) {
            break;
        }

        if (OMPI_SUCCESS !=
           (ret = init_one_hca(&btl_list, dev_sorted[i].ib_dev)))
            break;
    }

    if (OMPI_SUCCESS != ret) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "error in hca init", true, orte_process_info.nodename,
                       ibv_get_device_name(dev_sorted[i].ib_dev));
        return NULL;
    }

    free(dev_sorted);

    /* If we got back from checking all the HCAs and find that there
       are still items in the component.if_list, that means that they
       didn't exist.  Show an appropriate warning if the warning was
       not disabled. */

    if (0 != opal_argv_count(mca_btl_openib_component.if_list) &&
        mca_btl_openib_component.warn_nonexistent_if) {
        char *str = opal_argv_join(mca_btl_openib_component.if_list, ',');
        orte_show_help("help-mpi-btl-openib.txt", "nonexistent port",
                       true, orte_process_info.nodename,
                       ((NULL != mca_btl_openib_component.if_include) ?
                        "in" : "ex"), str);
        free(str);
    }

    if(0 == mca_btl_openib_component.ib_num_btls) {
        orte_show_help("help-mpi-btl-openib.txt",
                "no active ports found", true, orte_process_info.nodename);
        return NULL;
    }

    /* Setup the BSRQ QP's based on the final value of
       mca_btl_openib_component.receive_queues. */
    if (OMPI_SUCCESS != setup_qps()) {
        return NULL;
    }

    /* For XRC: 
     * from this point we know if MCA_BTL_XRC_ENABLED it true or false */

    /* Init XRC IB Addr hash table */
    if (MCA_BTL_XRC_ENABLED) {
        OBJ_CONSTRUCT(&mca_btl_openib_component.ib_addr_table,
                opal_hash_table_t);
    }

    /* Loop through all the btl modules that we made and find every
       base HCA that doesn't have hca->qps setup on it yet (remember
       that some modules may share the same HCA, so when going through
       to loop, we may hit an HCA that was already setup earlier in
       the loop). */
    for (item = opal_list_get_first(&btl_list);
         opal_list_get_end(&btl_list) != item;
         item = opal_list_get_next(item)) {
        mca_btl_base_selected_module_t *m = 
            (mca_btl_base_selected_module_t*) item;
        mca_btl_openib_hca_t *hca = 
            ((mca_btl_openib_module_t*) m->btl_module)->hca;
        if (NULL == hca->qps) {

            /* Setup the HCA qps info */
            hca->qps = (mca_btl_openib_hca_qp_t*)
                calloc(mca_btl_openib_component.num_qps,
                       sizeof(mca_btl_openib_hca_qp_t));
            for (i = 0; i < mca_btl_openib_component.num_qps; i++) {
                OBJ_CONSTRUCT(&hca->qps[i].send_free, ompi_free_list_t);
                OBJ_CONSTRUCT(&hca->qps[i].recv_free, ompi_free_list_t);
            }

            /* Do finial init on HCA */
            ret = prepare_hca_for_use(hca);
            if (OMPI_SUCCESS != ret) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "error in hca init", true, 
                               orte_process_info.nodename,
                               ibv_get_device_name(hca->ib_dev));
                return NULL;
            }
        }
    }

    /* Allocate space for btl modules */
    mca_btl_openib_component.openib_btls =
        malloc(sizeof(mca_btl_openib_module_t*) *
                mca_btl_openib_component.ib_num_btls);
    if(NULL == mca_btl_openib_component.openib_btls) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        return NULL;
    }
    btls = (struct mca_btl_base_module_t **)
        malloc(mca_btl_openib_component.ib_num_btls *
               sizeof(struct mca_btl_base_module_t*));
    if(NULL == btls) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        return NULL;
    }

    /* Copy the btl module structs into a contiguous array and fully
       initialize them */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++){
        item = opal_list_remove_first(&btl_list);
        ib_selected = (mca_btl_base_selected_module_t*)item;
        openib_btl = (mca_btl_openib_module_t*)ib_selected->btl_module;

        /* Do we have at least one CPC that can handle this
           port? */
        ret = 
            ompi_btl_openib_connect_base_select_for_local_port(openib_btl);
        if (OMPI_SUCCESS != ret) {
            orte_show_help("help-mpi-btl-openib.txt",
                           "failed load cpc", true,
                           orte_process_info.nodename,
                           ibv_get_device_name(openib_btl->hca->ib_dev));
            return NULL;
        }

        mca_btl_openib_component.openib_btls[i] = openib_btl;
        OBJ_RELEASE(ib_selected);
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

    /* Be sure to shut down the fd listener */
    ompi_btl_openib_fd_finalize();

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

    /* For iWARP, the TCP connection is tied to the QP once the QP is
     * in RTS.  And destroying the QP is thus tied to connection
     * teardown for iWARP.  To destroy the connection in iWARP you
     * must move the QP out of RTS, either into CLOSING for a nice
     * graceful close (e.g., via rdma_disconnect()), or to ERROR if
     * you want to be rude (e.g., just destroying the QP without
     * disconnecting first).  In both cases, all pending non-completed
     * SQ and RQ WRs will automatically be flushed.
     */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (IBV_WC_WR_FLUSH_ERR == wc->status && 
        IBV_TRANSPORT_IWARP == hca->ib_dev->transport_type) {
        return;
    }
#endif

    if(IBV_WC_WR_FLUSH_ERR != wc->status || !flush_err_printed[cq]++) {
        BTL_PEER_ERROR(remote_proc, ("error polling %s with status %s "
                    "status number %d for wr_id %llu opcode %d qp_idx %d",
                    cq_name[cq], btl_openib_component_status_to_string(wc->status),
                    wc->status, wc->wr_id, wc->opcode, qp));
    }

    if(IBV_WC_RETRY_EXC_ERR == wc->status)
        orte_show_help("help-mpi-btl-openib.txt", "btl_openib:retry-exceeded", true);

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
    BTL_ERROR(("error polling %s with %d errno says %s", cq_name[cq], ne,
                strerror(errno)));
    return count;
}

#if OMPI_ENABLE_PROGRESS_THREADS
void* mca_btl_openib_progress_thread(opal_object_t* arg)
{
    opal_thread_t* thread = (opal_thread_t*)arg;
    mca_btl_openib_hca_t* hca = thread->t_arg;
    struct ibv_cq *ev_cq;
    void *ev_ctx;

    /* This thread enter in a cancel enabled state */
    pthread_setcancelstate( PTHREAD_CANCEL_ENABLE, NULL );
    pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS, NULL );

    orte_output(-1, "WARNING: the openib btl progress thread code *does not yet work*.  Your run is likely to hang, crash, break the kitchen sink, and/or eat your cat.  You have been warned.");

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
