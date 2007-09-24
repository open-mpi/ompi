/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
#if OMPI_HAVE_THREADS
#include "btl_openib_async.h"
#endif
#include "connect/base.h"


/*
 * Local functions
 */
static int btl_openib_component_open(void);
static int btl_openib_component_close(void);
static int btl_openib_modex_send(void);
static void btl_openib_control(struct mca_btl_base_module_t* btl,
                               mca_btl_base_tag_t tag,
                               mca_btl_base_descriptor_t* descriptor,
                               void* cbdata);
static int init_one_port(opal_list_t *btl_list, mca_btl_openib_hca_t *hca,
                         uint8_t port_num, uint16_t pkey_index,
                         struct ibv_port_attr *ib_port_attr);
static int init_one_hca(opal_list_t *btl_list, struct ibv_device* ib_dev);
static mca_btl_base_module_t **btl_openib_component_init(
    int *num_btl_modules, bool enable_progress_threads,
    bool enable_mpi_threads);
static void merge_values(ompi_btl_openib_ini_values_t *target,
                         ompi_btl_openib_ini_values_t *src);
static int btl_openib_handle_incoming(mca_btl_openib_module_t *openib_btl,
                                         mca_btl_openib_endpoint_t *endpoint,
                                         mca_btl_openib_frag_t *frag, 
                                         size_t byte_len, const int prio);
static char* btl_openib_component_status_to_string(enum ibv_wc_status status);
static int btl_openib_component_progress(void);
static int btl_openib_module_progress(mca_btl_openib_hca_t *hca);
static void btl_openib_frag_progress_pending_pp(
         mca_btl_base_endpoint_t *endpoint,
         const int qp);
static void btl_openib_frag_progress_pending_srq(
         mca_btl_openib_module_t* openib_btl, 
         mca_btl_base_endpoint_t *endpoint,
         const int qp);
static void btl_openib_frag_progress_pending_put_get(
        mca_btl_openib_module_t* openib_btl, mca_btl_base_endpoint_t *endpoint,
        const int qp);

static int openib_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg);
static int openib_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg);
static int get_port_list(mca_btl_openib_hca_t *hca, int *allowed_ports);


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
    OBJ_CONSTRUCT(&mca_btl_openib_component.hcas, ompi_pointer_array_t);
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
    ompi_btl_openib_ini_finalize();
    return OMPI_SUCCESS;
}


/*
 *  Register OPENIB  port information. The MCA framework
 *  will make this available to all peers.
 */

static int btl_openib_modex_send(void)
{
    int         rc, i;
    size_t      size;
    mca_btl_openib_port_info_t *ports = NULL;

    size = mca_btl_openib_component.ib_num_btls * sizeof (mca_btl_openib_port_info_t);
    if (size != 0) {
        ports = (mca_btl_openib_port_info_t *)malloc (size);
        if (NULL == ports) {
            BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for (i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
            mca_btl_openib_module_t *btl = mca_btl_openib_component.openib_btls[i];
            ports[i] = btl->port_info;
#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            MCA_BTL_OPENIB_PORT_INFO_HTON(ports[i]);
#endif
        }
    }
    rc = ompi_modex_send (&mca_btl_openib_component.super.btl_version, ports, size);
    if (NULL != ports) {
        free (ports);
    }
    return rc;
}

/*
 * Active Message Callback function on control message.
 */

static void btl_openib_control(struct mca_btl_base_module_t* btl,
                               mca_btl_base_tag_t tag,
                               mca_btl_base_descriptor_t* descriptor,
                               void* cbdata)
{
    /* dont return credits used for control messages */
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*)descriptor;
    mca_btl_openib_endpoint_t* endpoint = frag->endpoint;
    mca_btl_openib_control_header_t *ctl_hdr = frag->segment.seg_addr.pval;
    mca_btl_openib_eager_rdma_header_t *rdma_hdr;
    mca_btl_openib_rdma_credits_header_t *credits_hdr;
    int qp = frag->qp_idx;
    
    if(BTL_OPENIB_EAGER_RDMA_QP(qp)) {
	    /* if not sent via rdma */
        if(!MCA_BTL_OPENIB_RDMA_FRAG(frag) &&
                ctl_hdr->type == MCA_BTL_OPENIB_CONTROL_CREDITS) {
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_received, 1);
            /* rd_posted don't account for rsv preposts for credit message but
             * receive path decreased it for each message receive no matter if
             * it is credit message or not. So fix rd_posted value here. */
            OPAL_THREAD_ADD32((int32_t*)&endpoint->qps[qp].u.pp_qp.rd_posted, 1);
        }
    } else if (ctl_hdr->type == MCA_BTL_OPENIB_CONTROL_CREDITS) {
        OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_received, 1);
        /* see above */
        OPAL_THREAD_ADD32((int32_t*)&endpoint->qps[qp].u.pp_qp.rd_posted, 1);
    }
    
    switch (ctl_hdr->type) {
    case MCA_BTL_OPENIB_CONTROL_CREDITS:
        credits_hdr = (mca_btl_openib_rdma_credits_header_t*)ctl_hdr;
        if(endpoint->nbo) {
            BTL_OPENIB_RDMA_CREDITS_HEADER_NTOH((*credits_hdr));
        }
        if(credits_hdr->rdma_credits)
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens,
                    credits_hdr->rdma_credits);
        break;
    case MCA_BTL_OPENIB_CONTROL_RDMA:
       rdma_hdr = (mca_btl_openib_eager_rdma_header_t*)ctl_hdr;
       
       BTL_VERBOSE(("prior to NTOH received  rkey %lu, rdma_start.lval %llu, pval %p, ival %u\n", 
                  rdma_hdr->rkey, 
                  (unsigned long) rdma_hdr->rdma_start.lval,
                  rdma_hdr->rdma_start.pval,
                  rdma_hdr->rdma_start.ival
                  ));
       
       if(endpoint->nbo) {
           BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_NTOH((*rdma_hdr));
           
           BTL_VERBOSE(("received  rkey %lu, rdma_start.lval %llu, pval %p, ival %u\n", 
                      rdma_hdr->rkey, 
                      (unsigned long) rdma_hdr->rdma_start.lval,
                      rdma_hdr->rdma_start.pval,
                      rdma_hdr->rdma_start.ival
                      ));
           
       } 
       
       if (endpoint->eager_rdma_remote.base.pval) {
	       BTL_ERROR(("Got RDMA connect twice!"));
	       return;
       }
       endpoint->eager_rdma_remote.rkey =  rdma_hdr->rkey;
       endpoint->eager_rdma_remote.base.lval = rdma_hdr->rdma_start.lval;
       endpoint->eager_rdma_remote.tokens =
           mca_btl_openib_component.eager_rdma_num - 1;
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
            opal_output(mca_btl_base_output, "%s: error unpinning openib memory errno says %s\n",
                    __func__, strerror(errno));
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

static int init_one_port(opal_list_t *btl_list, mca_btl_openib_hca_t *hca,
                         uint8_t port_num, uint16_t pkey_index,
                         struct ibv_port_attr *ib_port_attr)
{
    uint16_t lid, i, lmc;
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

    if (0 != mca_btl_openib_component.max_lmc && 
        mca_btl_openib_component.max_lmc < lmc) {
        lmc = mca_btl_openib_component.max_lmc;
    }

    for(lid = ib_port_attr->lid;
            lid < ib_port_attr->lid + lmc; lid++){
        for(i = 0; i < mca_btl_openib_component.btls_per_lid; i++){
            char param[40];
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
            openib_btl->src_path_bits = lid - ib_port_attr->lid;
            /* store the subnet for multi-nic support */
            openib_btl->port_info.subnet_id = subnet_id;
            openib_btl->port_info.mtu = hca->mtu;
            openib_btl->ib_reg[MCA_BTL_TAG_BTL].cbfunc = btl_openib_control;
            openib_btl->ib_reg[MCA_BTL_TAG_BTL].cbdata = NULL;

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
                    /* Who knows? */
                    return OMPI_ERR_VALUE_OUT_OF_BOUNDS;
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
                    /* Who knows? */
                    return OMPI_ERR_VALUE_OUT_OF_BOUNDS;
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

static int init_one_hca(opal_list_t *btl_list, struct ibv_device* ib_dev)
{
    struct mca_mpool_base_resources_t mpool_resources;
    mca_btl_openib_hca_t *hca;
    uint8_t i, k = 0;
    int ret = -1, port_cnt;
    ompi_btl_openib_ini_values_t values, default_values;
    int *allowed_ports = NULL;
    
    hca = malloc(sizeof(mca_btl_openib_hca_t));
    if(NULL == hca){
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    hca->ib_dev = ib_dev;
    hca->ib_dev_context = ibv_open_device(ib_dev);
    hca->btls = 0;
    hca->ib_cq[BTL_OPENIB_HP_CQ] = NULL;
    hca->ib_cq[BTL_OPENIB_LP_CQ] = NULL;
    hca->cq_users[BTL_OPENIB_HP_CQ] = 0;
    hca->cq_users[BTL_OPENIB_LP_CQ] = 0;
    hca->cq_size[BTL_OPENIB_HP_CQ] = 0;
    hca->cq_size[BTL_OPENIB_LP_CQ] = 0;
    OBJ_CONSTRUCT(&hca->hca_lock, opal_mutex_t); 
    if(NULL == hca->ib_dev_context){ 
        BTL_ERROR(("error obtaining device context for %s errno says %s\n",
                    ibv_get_device_name(ib_dev), strerror(errno))); 
        goto free_hca;
    } 
       
    if(ibv_query_device(hca->ib_dev_context, &hca->ib_dev_attr)){ 
        BTL_ERROR(("error obtaining device attributes for %s errno says %s\n",
                    ibv_get_device_name(ib_dev), strerror(errno))); 
        goto close_hca;
    }
    /* If mca_btl_if_include/exclude were specified, get usable ports */
    allowed_ports = (int*) malloc(hca->ib_dev_attr.phys_port_cnt * sizeof(int));
    port_cnt = get_port_list(hca, allowed_ports);
    if(0 == port_cnt) {
        ret = OMPI_SUCCESS;
        goto close_hca;
    }
    /* Load in vendor/part-specific HCA parameters.  Note that even if
       we don't find values for this vendor/part, "values" will be set
       indicating that it does not have good values */
    ret = ompi_btl_openib_ini_query(hca->ib_dev_attr.vendor_id,
                                    hca->ib_dev_attr.vendor_part_id,
                                    &values);
    if (OMPI_SUCCESS != ret && OMPI_ERR_NOT_FOUND != ret) {
        /* If we get a serious error, propagate it upwards */
        goto close_hca;
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
        goto close_hca;
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

    hca->ib_pd = ibv_alloc_pd(hca->ib_dev_context);
    if(NULL == hca->ib_pd){
        BTL_ERROR(("error allocating pd for %s errno says %s\n",
                    ibv_get_device_name(ib_dev), strerror(errno)));
        goto close_hca;
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
         goto dealloc_pd;
    }
   
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    hca->ib_channel = ibv_create_comp_channel(hca->ib_dev_context);
    if (NULL == hca->ib_channel) {
        BTL_ERROR(("error creating channel for %s errno says %s\n",
                    ibv_get_device_name(hca->ib_dev),
                    strerror(errno)));
        goto mpool_destroy;
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
#if OMPI_HAVE_THREADS
        if (mca_btl_openib_component.use_async_event_thread) {
            hca->got_fatal_event = false;
            if (write(mca_btl_openib_component.async_pipe[1],
                        &hca->ib_dev_context->async_fd,
                        sizeof(int))<0){
                BTL_ERROR(("Failed to write to pipe [%d]",errno));
                goto comp_channel;
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
        orte_pointer_array_init(&hca->endpoints, 10, INT_MAX, 10);
        ompi_pointer_array_add(&mca_btl_openib_component.hcas, hca);
        mca_btl_openib_component.hcas_count++;
        return OMPI_SUCCESS;
    }

comp_channel:
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    ibv_destroy_comp_channel(hca->ib_channel);
mpool_destroy:
#endif
    mca_mpool_base_module_destroy(hca->mpool);
dealloc_pd:
    ibv_dealloc_pd(hca->ib_pd);
close_hca:
    ibv_close_device(hca->ib_dev_context);
    if(NULL != allowed_ports) {
        free(allowed_ports);
    }
free_hca:
    free(hca);
    return ret;
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
    int i, ret, length, num_devs;
    opal_list_t btl_list; 
    mca_btl_openib_module_t * openib_btl; 
    mca_btl_base_selected_module_t* ib_selected; 
    opal_list_item_t* item; 
#if !defined(HAVE_IBV_GET_DEVICE_LIST)
    struct dlist *dev_list; 
    struct ibv_device* ib_dev; 
#endif
    unsigned short seedv[3];

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
#if OMPI_HAVE_THREADS
    /* Set the fatal counter to zero */
    mca_btl_openib_component.fatal_counter = 0;
    /* Create pipe for comunication with async event thread */
    if (mca_btl_openib_component.use_async_event_thread) {
        if (pipe (mca_btl_openib_component.async_pipe)) {
            BTL_ERROR(("Failed to create pipe for comunication with async event thread"));
            return NULL;
        }
    }
#endif

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

#ifdef HAVE_IBV_GET_DEVICE_LIST
    ib_devs = ibv_get_device_list(&num_devs);
#else 
    /* Determine the number of hca's available on the host */
    dev_list = ibv_get_devices(); 
    if (NULL == dev_list) {
        mca_btl_base_error_no_nics("OpenIB", "HCA");
        mca_btl_openib_component.ib_num_btls = 0;
        btl_openib_modex_send();
        return NULL;
    }
    dlist_start(dev_list); 

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        num_devs++; 
#endif

    if(0 == num_devs) { 
        mca_btl_base_error_no_nics("OpenIB", "HCA");
        btl_openib_modex_send();
        return NULL; 
    }
       
#if !defined(HAVE_IBV_GET_DEVICE_LIST)
    /* Allocate space for the ib devices */ 
    ib_devs = (struct ibv_device**) malloc(num_devs * sizeof(struct ibv_dev*));
    if(NULL == ib_devs) {
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return NULL;
    }

    dlist_start(dev_list); 

    i = 0; 
    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        ib_devs[i++] =  ib_dev; 
#endif
            
    /* We must loop through all the hca id's, get their handles and
       for each hca we query the number of ports on the hca and set up
       a distinct btl module for each hca port */ 

    OBJ_CONSTRUCT(&btl_list, opal_list_t); 
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_lock, opal_mutex_t);
    for (i = 0; i < num_devs &&
            (-1 == mca_btl_openib_component.ib_max_btls ||
             mca_btl_openib_component.ib_num_btls <
             mca_btl_openib_component.ib_max_btls); i++){
#if OMPI_HAVE_THREADS
        if (mca_btl_openib_component.use_async_event_thread && 
                0 == i) {
            /* Starting async event thread for the component */
            if (pthread_create(&mca_btl_openib_component.async_thread,NULL,
                        (void*(*)(void*))btl_openib_async_thread,NULL)) {
                BTL_ERROR(("Failed to create async event thread for openib"));
                return NULL;
            }
        }
#endif
        if (OMPI_SUCCESS != (ret = init_one_hca(&btl_list, ib_devs[i]))) {
            break;
        }
    }

    if(ret != OMPI_SUCCESS) {
        opal_show_help("help-mpi-btl-openib.txt",
                "error in hca init", true, orte_system_info.nodename);
    }

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
#if OMPI_HAVE_THREADS
        if (mca_btl_openib_component.use_async_event_thread) {
            int async_command = 0;
            /* signaling to async_tread to stop poll for this hca*/
            if (write(mca_btl_openib_component.async_pipe[1],
                        &async_command,sizeof(int))<0){
                BTL_ERROR(("Failed to write to pipe"));
                return NULL;
            }
            if (pthread_join(mca_btl_openib_component.async_thread, NULL)) {
                BTL_ERROR(("Failed to stop OpenIB async event thread"));
                return NULL;
            }
        }
#endif
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
    btls = (struct mca_btl_base_module_t **)malloc(mca_btl_openib_component.ib_num_btls *
            sizeof(struct mca_btl_base_module_t*));
    if(NULL == btls) {
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return NULL;
    }

    /* Copy the btl module structs into a contiguous array and fully
       initialize them */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++){
        mca_btl_openib_frag_init_data_t* init_data;
        int qp;
        item = opal_list_remove_first(&btl_list); 
        ib_selected = (mca_btl_base_selected_module_t*)item; 
        mca_btl_openib_component.openib_btls[i] = (mca_btl_openib_module_t*)ib_selected->btl_module; 
        OBJ_RELEASE(ib_selected); 
        openib_btl = mca_btl_openib_component.openib_btls[i];
        
        openib_btl->num_peers = 0; 
        
        
        /* Initialize module state */

        OBJ_CONSTRUCT(&openib_btl->ib_lock, opal_mutex_t); 
        
        OBJ_CONSTRUCT(&openib_btl->send_free_control, ompi_free_list_t);
        
        OBJ_CONSTRUCT(&openib_btl->send_user_free, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->recv_user_free, ompi_free_list_t);

        /* setup the qp structure */
        openib_btl->qps = 
            (mca_btl_openib_module_qp_t*) 
            malloc(sizeof(mca_btl_openib_module_qp_t)* 
                   mca_btl_openib_component.num_qps);
        
                   
        /* initialize the memory pool using the hca */ 
        openib_btl->super.btl_mpool = openib_btl->hca->mpool;
        
        length = sizeof(mca_btl_openib_frag_t);
        
        init_data = (mca_btl_openib_frag_init_data_t*) 
            malloc(sizeof(mca_btl_openib_frag_init_data_t));
        
        init_data->length = length;
        init_data->type = MCA_BTL_OPENIB_FRAG_SEND_USER;
        init_data->order = mca_btl_openib_component.rdma_qp;
        init_data->list = &openib_btl->send_user_free;
        
        if(OMPI_SUCCESS != ompi_free_list_init_ex( &openib_btl->send_user_free,
                                                   length,
                                                   2,
                                                   OBJ_CLASS(mca_btl_openib_send_user_frag_t),
                                                   mca_btl_openib_component.ib_free_list_num,
                                                   mca_btl_openib_component.ib_free_list_max,
                                                   mca_btl_openib_component.ib_free_list_inc,
                                                   NULL, 
                                                   mca_btl_openib_frag_init,
                                                   (void*)init_data)) { 
            return NULL;
        }
        init_data = (mca_btl_openib_frag_init_data_t*) 
            malloc(sizeof(mca_btl_openib_frag_init_data_t));
        
        init_data->length = length;
        init_data->type = MCA_BTL_OPENIB_FRAG_RECV_USER;
        init_data->order = mca_btl_openib_component.rdma_qp;
        init_data->list = &openib_btl->recv_user_free;
        
        if(OMPI_SUCCESS  != ompi_free_list_init_ex(&openib_btl->recv_user_free,
                                                   length,
                                                   2,
                                                   OBJ_CLASS(mca_btl_openib_recv_user_frag_t),
                                                   mca_btl_openib_component.ib_free_list_num,
                                                   mca_btl_openib_component.ib_free_list_max,
                                                   mca_btl_openib_component.ib_free_list_inc,
                                                   NULL, 
                                                   mca_btl_openib_frag_init,
                                                   (void*)init_data)) { 
            return NULL;
        }
        

        orte_pointer_array_init(&openib_btl->eager_rdma_buffers, 
                mca_btl_openib_component.max_eager_rdma,
                mca_btl_openib_component.max_eager_rdma, 
                0);
        openib_btl->eager_rdma_buffers_count = 0;

        btls[i] = &openib_btl->super;

        
        openib_btl->eager_rdma_frag_size = OPAL_ALIGN(
                                                      sizeof(mca_btl_openib_header_t) +
                                                      sizeof(mca_btl_openib_footer_t) +
                                                      openib_btl->super.btl_eager_limit,
                                                      mca_btl_openib_component.buffer_alignment, size_t);
        
        
        
        length = sizeof(mca_btl_openib_send_frag_control_t) +
            sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_footer_t) + 
            sizeof(mca_btl_openib_eager_rdma_header_t);
        
        init_data = (mca_btl_openib_frag_init_data_t*) 
            malloc(sizeof(mca_btl_openib_frag_init_data_t));
        init_data->length = sizeof(mca_btl_openib_eager_rdma_header_t);
        init_data->type = MCA_BTL_OPENIB_FRAG_CONTROL;
        init_data->order = mca_btl_openib_component.eager_rdma_qp;
        init_data->list = &openib_btl->send_free_control;
        
        if(OMPI_SUCCESS != ompi_free_list_init_ex(&openib_btl->send_free_control,
                                                  length,
                                                  mca_btl_openib_component.buffer_alignment,
                                                  OBJ_CLASS(mca_btl_openib_send_frag_control_t),
                                                  mca_btl_openib_component.ib_free_list_num,
                                                  -1,
                                                  mca_btl_openib_component.ib_free_list_inc,
                                                  openib_btl->super.btl_mpool,
                                                  mca_btl_openib_frag_init,
                                                  init_data)) { 
            return NULL;
        }
        
           
           /* setup all the qps */ 
        
        for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
            
            OBJ_CONSTRUCT(&openib_btl->qps[qp].send_free, ompi_free_list_t);
            OBJ_CONSTRUCT(&openib_btl->qps[qp].recv_free, ompi_free_list_t);
            openib_btl->qps[qp].type = mca_btl_openib_component.qp_infos[qp].type;
            
                
            if(MCA_BTL_OPENIB_SRQ_QP == openib_btl->qps[qp].type) { 
                OBJ_CONSTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags, opal_list_t);
                openib_btl->qps[qp].u.srq_qp.sd_credits = 
                    mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
            }
            
            
            /* Initialize pool of send fragments */ 
            length = sizeof(mca_btl_openib_send_frag_t) +
                sizeof(mca_btl_openib_header_t) + 
                sizeof(mca_btl_openib_footer_t) + 
                mca_btl_openib_component.qp_infos[qp].size;              
            
            init_data = (mca_btl_openib_frag_init_data_t*) 
                malloc(sizeof(mca_btl_openib_frag_init_data_t));
            
            init_data->length = mca_btl_openib_component.qp_infos[qp].size;
            init_data->type = MCA_BTL_OPENIB_FRAG_SEND;
            init_data->order = qp;
            init_data->list = &openib_btl->qps[qp].send_free;
            

            if(OMPI_SUCCESS != ompi_free_list_init_ex(init_data->list,
                                                      length,
                                                      mca_btl_openib_component.buffer_alignment,
                                                      OBJ_CLASS(mca_btl_openib_send_frag_t),
                                                      mca_btl_openib_component.ib_free_list_num,
                                                      mca_btl_openib_component.ib_free_list_max,
                                                      mca_btl_openib_component.ib_free_list_inc,
                                                      openib_btl->super.btl_mpool, 
                                                      mca_btl_openib_frag_init,
                                                      (void*)init_data)) { 
                return NULL;
            }
               
            length = sizeof(mca_btl_openib_recv_frag_t) +
                sizeof(mca_btl_openib_header_t) +
                sizeof(mca_btl_openib_footer_t) +
                mca_btl_openib_component.qp_infos[qp].size;
            
            init_data = (mca_btl_openib_frag_init_data_t*) 
                malloc(sizeof(mca_btl_openib_frag_init_data_t));
            init_data->length = mca_btl_openib_component.qp_infos[qp].size;
            init_data->type = MCA_BTL_OPENIB_FRAG_RECV;
            init_data->order = qp;
            init_data->list = &openib_btl->qps[qp].recv_free;
            
            if(OMPI_SUCCESS != ompi_free_list_init_ex(init_data->list,
                                                      length, 
                                                      mca_btl_openib_component.buffer_alignment,
                                                      OBJ_CLASS(mca_btl_openib_recv_frag_t),
                                                      mca_btl_openib_component.ib_free_list_num,
                                                      mca_btl_openib_component.ib_free_list_max,
                                                      mca_btl_openib_component.ib_free_list_inc,
                                                      openib_btl->super.btl_mpool, 
                                                      mca_btl_openib_frag_init,
                                                      init_data)) { 
                return NULL;
            }
        }
        
    }

    /* Setup connect module */
    if (OMPI_SUCCESS != ompi_btl_openib_connect_base_select()) {
        return NULL;
    }
    btl_openib_modex_send();

    *num_btl_modules = mca_btl_openib_component.ib_num_btls;
#ifdef HAVE_IBV_GET_DEVICE_LIST
    ibv_free_device_list(ib_devs);
#else
    free(ib_devs);
#endif
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

    mca_btl_openib_component.ib_num_btls = 0;
    btl_openib_modex_send();
    return NULL;
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


static int btl_openib_handle_incoming(mca_btl_openib_module_t *openib_btl,
                                         mca_btl_openib_endpoint_t *endpoint,
                                         mca_btl_openib_frag_t *frag, 
                                         size_t byte_len, const int qp)
{
    if(endpoint->nbo) {
        BTL_OPENIB_HEADER_NTOH((*(frag->hdr)));
    }

    /* advance the segment address past the header and subtract from the
     * length..*/
    frag->segment.seg_len = byte_len - sizeof(mca_btl_openib_header_t);

    /* call registered callback */
    openib_btl->ib_reg[frag->hdr->tag].cbfunc(&openib_btl->super,
            frag->hdr->tag, &frag->base, 
            openib_btl->ib_reg[frag->hdr->tag].cbdata);

    if(BTL_OPENIB_IS_RDMA_CREDITS(frag->hdr->credits) &&
       BTL_OPENIB_CREDITS(frag->hdr->credits) > 0) {
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens,
                          BTL_OPENIB_CREDITS(frag->hdr->credits));
    } else {
        if(MCA_BTL_OPENIB_PP_QP == endpoint->qps[qp].qp_type &&
                frag->hdr->credits > 0) {
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.sd_credits, 
                              frag->hdr->credits);
        }
    }

    if(frag->hdr->cm_seen) {
        OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_sent,
                -frag->hdr->cm_seen);
    }

    /* We may receive credits here so try to progress only things that
     * may be pending because of credit shortage */
    if(MCA_BTL_OPENIB_PP_QP == endpoint->qps[qp].qp_type ||
            BTL_OPENIB_EAGER_RDMA_QP(qp)) {
        btl_openib_frag_progress_pending_pp(endpoint, qp);
   }
    
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

#define BTL_OPENIB_TOKENS(E, P) \
    (((E)->qps[qp].qp_type == MCA_BTL_OPENIB_SRQ_QP) ? 1 : \
     ((E)->qps[(P)].u.pp_qp.sd_credits +     \
      ((BTL_OPENIB_EAGER_RDMA_QP(P))?(E)->eager_rdma_remote.tokens:0)))
static void btl_openib_frag_progress_pending_pp(
        mca_btl_base_endpoint_t *endpoint, const int qp)
{
    
    opal_list_item_t *frag_item;
    mca_btl_openib_frag_t* frag;
    size_t i, len = opal_list_get_size(&endpoint->qps[qp].pending_frags);
    
    /* check to see if we need to progress any pending descriptors */
    for(i = 0; i < len && endpoint->qps[qp].sd_wqe > 0 &&
            BTL_OPENIB_TOKENS(endpoint, qp) > 0; i++) {
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        frag_item = 
            opal_list_remove_first(&(endpoint->qps[qp].pending_frags));
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
            break;
        if(mca_btl_openib_endpoint_send(frag->endpoint, frag) ==
           OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }
}

static void btl_openib_frag_progress_pending_put_get(
        mca_btl_openib_module_t* openib_btl, mca_btl_base_endpoint_t *endpoint,
        const int qp) { 
    opal_list_item_t *frag_item;
    mca_btl_openib_frag_t* frag;
    size_t i, len = opal_list_get_size(&endpoint->pending_get_frags);
    for(i = 0; i < len && endpoint->qps[qp].sd_wqe > 0 &&
            endpoint->get_tokens > 0; i++) {
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        frag_item = opal_list_remove_first(&(endpoint->pending_get_frags));
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
            break;
        if(mca_btl_openib_get((mca_btl_base_module_t *)openib_btl,
                              frag->endpoint, (mca_btl_base_descriptor_t*)frag) ==
           OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }
    
    len = opal_list_get_size(&endpoint->pending_put_frags);
    for(i = 0; i < len && endpoint->qps[qp].sd_wqe > 0; i++) {
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        frag_item = opal_list_remove_first(&(endpoint->pending_put_frags));
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
            break;
        if(mca_btl_openib_put((mca_btl_base_module_t*)openib_btl,
                              frag->endpoint, (mca_btl_base_descriptor_t*)frag) ==
           OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }
}

static void btl_openib_frag_progress_pending_srq(
        mca_btl_openib_module_t* openib_btl, mca_btl_base_endpoint_t *endpoint,
        const int qp)
{
    
    opal_list_item_t *frag_item;
    mca_btl_openib_frag_t* frag;
    size_t i, len;
    
    assert(MCA_BTL_OPENIB_SRQ_QP == endpoint->qps[qp].qp_type);
    
    len = opal_list_get_size(&openib_btl->qps[qp].u.srq_qp.pending_frags);
    for(i = 0; i < len && openib_btl->qps[qp].u.srq_qp.sd_credits > 0; i++) {
        /* dequeue resources due to global flow control */
        OPAL_THREAD_LOCK(&openib_btl->ib_lock);
        frag_item =
            opal_list_remove_first(&openib_btl->qps[qp].u.srq_qp.pending_frags);
        OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
        if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
            break;
        if(mca_btl_openib_endpoint_send(frag->endpoint, frag) ==
           OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }
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

        while(btl_openib_module_progress(hca));
    }

    return PTHREAD_CANCELED;
}
#endif

/*
 *  IB component progress.
 */
static int btl_openib_component_progress(void)
{
    int i, j, c;
    int count = 0, ret;
    mca_btl_openib_frag_t* frag; 
    mca_btl_openib_endpoint_t* endpoint; 
    
#if OMPI_HAVE_THREADS
    if(mca_btl_openib_component.use_async_event_thread &&
            mca_btl_openib_component.fatal_counter) {
        goto error;
    }
#endif

    /* Poll for RDMA completions - if any succeed, we don't process the slower
     * queues.
     */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        mca_btl_openib_module_t* openib_btl = mca_btl_openib_component.openib_btls[i];
        c = openib_btl->eager_rdma_buffers_count;

        for(j = 0; j < c; j++) {
            endpoint = 
                orte_pointer_array_get_item(openib_btl->eager_rdma_buffers, j);

            if(!endpoint) /* shouldn't happen */
                continue;

            OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
            frag = MCA_BTL_OPENIB_GET_LOCAL_RDMA_FRAG (endpoint,
                    endpoint->eager_rdma_local.head);

            if(MCA_BTL_OPENIB_RDMA_FRAG_LOCAL(frag)) {
                uint32_t size;
                int qp;
                opal_atomic_rmb();
                if(endpoint->nbo) {
                    BTL_OPENIB_FOOTER_NTOH((*frag->ftr));
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
                frag->segment.seg_addr.pval = ((unsigned char* )frag->hdr) + 
                    sizeof(mca_btl_openib_header_t);

                ret = btl_openib_handle_incoming(openib_btl,
                        frag->endpoint, frag, 
                        size - sizeof(mca_btl_openib_footer_t),
                                                 frag->qp_idx);
                if (ret != MPI_SUCCESS) { 
                    openib_btl->error_cb(&openib_btl->super, 
                                         MCA_BTL_ERROR_FLAGS_FATAL);
                    return 0;
                }

                OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
                MCA_BTL_OPENIB_RDMA_MAKE_REMOTE(frag->ftr);
                while (endpoint->eager_rdma_local.tail !=
                        endpoint->eager_rdma_local.head) {
                    mca_btl_openib_frag_t *tf;
                    tf = MCA_BTL_OPENIB_GET_LOCAL_RDMA_FRAG(endpoint,
                            endpoint->eager_rdma_local.tail);
                    if (MCA_BTL_OPENIB_RDMA_FRAG_LOCAL (tf))
                        break;
                    OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits, 1);
                    MCA_BTL_OPENIB_RDMA_NEXT_INDEX(endpoint->eager_rdma_local.tail);
                }
                OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);

                qp = mca_btl_openib_component.eager_rdma_qp;
                send_credits(endpoint, qp);
                count++;
            } else
                OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
        }
    }
    if(count) return count;

    for(i = 0; i < mca_btl_openib_component.hcas_count; i++) {
        mca_btl_openib_hca_t *hca =
            ompi_pointer_array_get_item(&mca_btl_openib_component.hcas, i);
        count += btl_openib_module_progress(hca);
    }

    return count;

#if OMPI_HAVE_THREADS
error:
    /* Set the fatal counter to zero */
    mca_btl_openib_component.fatal_counter = 0;
    /* Lets found all fatal events */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        mca_btl_openib_module_t* openib_btl = mca_btl_openib_component.openib_btls[i];
        if(openib_btl->hca->got_fatal_event) {
            openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL); 
        }
    }
    return count;
#endif
}

static int btl_openib_module_progress(mca_btl_openib_hca_t* hca)
{
    static char *cq_name[] = {"HP CQ", "LP CQ"};
    int cq, qp;
    int count = 0,ne = 0, ret;
    mca_btl_openib_frag_t* frag; 
    mca_btl_openib_endpoint_t* endpoint; 
    mca_btl_openib_module_t *openib_btl = NULL;
    struct ibv_wc wc; 

    for(cq = 0; cq < 2; cq++) {
        if(0 == hca->cq_users[cq])
            continue;
        ne = ibv_poll_cq(hca->ib_cq[cq], 1, &wc);
        if(0 == ne) 
            continue;
        if(ne < 0 || wc.status != IBV_WC_SUCCESS) 
            goto error;
        
        frag = (mca_btl_openib_frag_t*) (unsigned long) wc.wr_id;
        qp = frag->base.order;
        endpoint = frag->endpoint;
        if(endpoint)
            openib_btl = endpoint->endpoint_btl;

        /* Handle work completions */
        switch(wc.opcode) {
            case IBV_WC_RDMA_READ:
                OPAL_THREAD_ADD32(&endpoint->get_tokens, 1);
                /* fall through */
        
            case IBV_WC_RDMA_WRITE:
            case IBV_WC_SEND:
                /* Process a completed send/put/get */
                frag->base.des_cbfunc(&openib_btl->super, endpoint, &frag->base,
                        OMPI_SUCCESS); 

                /* return send wqe */
                OPAL_THREAD_ADD32(&endpoint->qps[qp].sd_wqe, 1);

                if(IBV_WC_SEND == wc.opcode &&
                        MCA_BTL_OPENIB_SRQ_QP == endpoint->qps[qp].qp_type) {
                    OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.sd_credits, 1);
                    /* new SRQ credit available. Try to progress pending frags*/
                    btl_openib_frag_progress_pending_srq(openib_btl, endpoint, qp);
                }
                /* new wqe or/and get token available. Try to progress pending frags */
                btl_openib_frag_progress_pending_pp(endpoint, qp);
                btl_openib_frag_progress_pending_put_get(openib_btl, endpoint, qp);

                count++;
                break;
            case IBV_WC_RECV:
                if(wc.wc_flags & IBV_WC_WITH_IMM) {
                    endpoint = (mca_btl_openib_endpoint_t*)
                        orte_pointer_array_get_item(hca->endpoints, wc.imm_data);
                    frag->endpoint = endpoint;
                    openib_btl = endpoint->endpoint_btl;
                }

                /* Process a RECV */ 
                ret = btl_openib_handle_incoming(openib_btl, endpoint, frag, wc.byte_len, qp);
                if (ret != OMPI_SUCCESS) { 
                    openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL);
                    return 0;
                }

                OMPI_FREE_LIST_RETURN(frag->list, (ompi_free_list_item_t*) frag);
                if(MCA_BTL_OPENIB_SRQ_QP == endpoint->qps[qp].qp_type) { 
                    OPAL_THREAD_ADD32((int32_t*)
                            &openib_btl->qps[qp].u.srq_qp.rd_posted, -1);
                    mca_btl_openib_post_srr(openib_btl, 0, qp);
                } else {
                    OPAL_THREAD_ADD32((int32_t*)
                            &endpoint->qps[qp].u.pp_qp.rd_posted, -1);
                    mca_btl_openib_endpoint_post_rr(endpoint, 0, qp);
                }
                count++; 

                send_credits(endpoint, qp);
 
                /* decide if it is time to setup an eager rdma channel */
                if (!endpoint->eager_rdma_local.base.pval &&
                        endpoint->use_eager_rdma &&
                        wc.byte_len < mca_btl_openib_component.eager_limit &&
                        openib_btl->eager_rdma_buffers_count <
                        mca_btl_openib_component.max_eager_rdma &&
                        OPAL_THREAD_ADD32(&endpoint->eager_recv_count, 1) ==
                        mca_btl_openib_component.eager_rdma_threshold) {
                    mca_btl_openib_endpoint_connect_eager_rdma(endpoint); 
                }
                break; 
            default:
                BTL_ERROR(("Unhandled work completion opcode is %d",
                            wc.opcode));
                if(openib_btl)
                    openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL);
                break;
        }
    }
    
    return count;
error:
    if(ne < 0){ 
        BTL_ERROR(("error polling %s with %d errno says %s\n",
                   cq_name[cq], ne, strerror(errno))); 
    } else {
        static int flush_err_printed[] = {0, 0};
        ompi_proc_t* remote_proc = NULL; 
        frag = (mca_btl_openib_frag_t*) (unsigned long) wc.wr_id; 
        if(frag) { 
            endpoint = (mca_btl_openib_endpoint_t*) frag->endpoint; 
            if(endpoint && 
                    endpoint->endpoint_proc && 
                    endpoint->endpoint_proc->proc_ompi) {
                remote_proc = endpoint->endpoint_proc->proc_ompi; 
            }
        }
        if(wc.status != IBV_WC_WR_FLUSH_ERR || !flush_err_printed[cq]++) {
            BTL_PEER_ERROR(remote_proc, ("error polling %s with status %s "
                        "status number %d for wr_id %llu opcode %d qp_idx %d",
                        cq_name[cq],
                        btl_openib_component_status_to_string(wc.status), 
                        wc.status, wc.wr_id, wc.opcode, frag->qp_idx)); 
        }
        if(wc.status == IBV_WC_RETRY_EXC_ERR) { 
            opal_show_help("help-mpi-btl-openib.txt",
                    "btl_openib:retry-exceeded", true);
        }
    }
    if(openib_btl)
        openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL); 
    return count;
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
