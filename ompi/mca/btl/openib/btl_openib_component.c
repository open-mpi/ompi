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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/include/opal/align.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "opal/sys/timer.h"
#include "opal/sys/atomic.h"

#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/sys_info.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/mca/btl/base/base.h"
#include "btl_openib.h"
#include "btl_openib_frag.h"
#include "btl_openib_endpoint.h" 
#include "btl_openib_eager_rdma.h"
#include "btl_openib_proc.h"
#include "btl_openib_ini.h"
#include "btl_openib_mca.h"

#include "ompi/datatype/convertor.h" 
#include "ompi/mca/mpool/mpool.h" 
#include <infiniband/verbs.h> 
#include <infiniband/driver.h>
#include <errno.h> 
#include <string.h>   /* for strerror()*/ 
#include <sys/types.h>
#include <sys/stat.h>

#include "ompi/mca/pml/base/pml_base_module_exchange.h"

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
static void btl_openib_frag_progress_pending(
         mca_btl_openib_module_t* openib_btl, mca_btl_base_endpoint_t *endpoint,
         const int prio);
static int openib_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg);
static int openib_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg);


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
            /* Whether the component is checkpointable or not */

            false
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
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_procs, opal_list_t);

    /* register IB component parameters */
    ret = btl_openib_register_mca_params();

    mca_btl_openib_component.max_send_size = 
        mca_btl_openib_module.super.btl_max_send_size; 
    mca_btl_openib_component.eager_limit = 
        mca_btl_openib_module.super.btl_eager_limit; 

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
            mca_btl_openib_module_t *btl = &mca_btl_openib_component.openib_btls[i];
            ports[i] = btl->port_info;
#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            MCA_BTL_OPENIB_PORT_INFO_HTON(ports[i]);
#endif
        }
    }
    rc = mca_pml_base_modex_send (&mca_btl_openib_component.super.btl_version, ports, size);
    if (NULL != ports) {
        free (ports);
    }
    return rc;
}

/*
 * Callback function on control message.
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

       
    if(frag->size == mca_btl_openib_component.eager_limit) {
	    /* if not sent via rdma */
        if(!MCA_BTL_OPENIB_RDMA_FRAG(frag) &&
                ctl_hdr->type == MCA_BTL_OPENIB_CONTROL_CREDITS) {
             OPAL_THREAD_ADD32(&endpoint->rd_credits[BTL_OPENIB_HP_QP], -1);
        }
    } else {
        OPAL_THREAD_ADD32(&endpoint->rd_credits[BTL_OPENIB_LP_QP], -1);
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
       
       BTL_VERBOSE(("prior to NTOH received  rkey %lu, rdma_start.lval %llu, pval %p, ival %u, frag_t_len %llu\n", 
                  rdma_hdr->rkey, 
                  (unsigned long) rdma_hdr->rdma_start.lval,
                  rdma_hdr->rdma_start.pval,
                  rdma_hdr->rdma_start.ival,
                  (unsigned long) rdma_hdr->frag_t_len
                  ));
       
       if(endpoint->nbo) {
           BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_NTOH((*rdma_hdr));
           
           BTL_VERBOSE(("received  rkey %lu, rdma_start.lval %llu, pval %p, ival %u, frag_t_len %llu\n", 
                      rdma_hdr->rkey, 
                      (unsigned long) rdma_hdr->rdma_start.lval,
                      rdma_hdr->rdma_start.pval,
                      rdma_hdr->rdma_start.ival,
                      (unsigned long) rdma_hdr->frag_t_len
                      ));
           
       } 
       
       if (endpoint->eager_rdma_remote.base.pval) {
	       BTL_ERROR(("Got RDMA connect twice!"));
	       return;
       }
       endpoint->eager_rdma_remote.rkey =  rdma_hdr->rkey;
       endpoint->eager_rdma_remote.base.lval = rdma_hdr->rdma_start.lval;
       endpoint->eager_rdma_remote.frag_t_len = rdma_hdr->frag_t_len;
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
            opal_output(0, "%s: error unpinning openib memory errno says %s\n",
                    __func__, strerror(errno));
            return OMPI_ERROR;
        }
    }
    openib_reg->mr = NULL;
    return OMPI_SUCCESS;
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

static int init_one_hca(opal_list_t *btl_list, struct ibv_device* ib_dev)
{
    struct mca_mpool_base_resources_t mpool_resources;
    mca_btl_openib_hca_t *hca;
    uint8_t i;
    int ret = -1;
    ompi_btl_openib_ini_values_t values, default_values;

    hca = malloc(sizeof(mca_btl_openib_hca_t));
    if(NULL == hca){
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    hca->ib_dev = ib_dev;
    hca->ib_dev_context = ibv_open_device(ib_dev);
    hca->btls = 0;
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
   
    ret = OMPI_SUCCESS; 
    /* Note ports are 1 based hence j = 1 */
    for(i = 1; i <= hca->ib_dev_attr.phys_port_cnt; i++){
        struct ibv_port_attr ib_port_attr;

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
                for (j = 0; j < hca->ib_dev_attr.max_pkeys; j++) {
                    if(ibv_query_pkey(hca->ib_dev_context, i, j, &pkey)){ 
                        BTL_ERROR(("error getting pkey for index %d, device %s " 
                                    "port number %d errno says %s", 
                                    j, ibv_get_device_name(ib_dev), i, strerror(errno))); 
                    }
                    pkey = ntohs(pkey) & MCA_BTL_IB_PKEY_MASK;
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

    if (hca->btls != 0)
        return ret;

    mca_mpool_base_module_destroy(hca->mpool);
dealloc_pd:
    ibv_dealloc_pd(hca->ib_pd);
close_hca:
    ibv_close_device(hca->ib_dev_context);
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
#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST == 0
    struct dlist *dev_list; 
    struct ibv_device* ib_dev; 
#endif
    unsigned short seedv[3];

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
    
    /* openib BTL does not currently support progress threads, so
       disable the component if they were requested */
    if (enable_progress_threads) {
        mca_btl_base_error_no_nics("OpenIB", "HCA");
        mca_btl_openib_component.ib_num_btls = 0;
        btl_openib_modex_send();
        return NULL;
    }

    seedv[0] = orte_process_info.my_name->vpid;
    seedv[1] = opal_sys_timer_get_cycles();
    seedv[2] = opal_sys_timer_get_cycles();
    seed48(seedv);

    /* Read in INI files with HCA-specific parameters */
    if (OMPI_SUCCESS != (ret = ompi_btl_openib_ini_init())) {
        return NULL;
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
                mca_btl_openib_component.ib_num_btls = 0;
                btl_openib_modex_send();
                return NULL;
            } 
        }
    }
#endif

#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST
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
       
#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST == 0
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
        if (OMPI_SUCCESS != (ret = init_one_hca(&btl_list, ib_devs[i]))) {
            break;
        }
    }

    if(ret != OMPI_SUCCESS) {
        opal_show_help("help-mpi-btl-openib.txt",
                "error in hca init", true, orte_system_info.nodename);
    }
       
    if(0 == mca_btl_openib_component.ib_num_btls) {
        opal_show_help("help-mpi-btl-openib.txt",
                "no active ports found", true, orte_system_info.nodename);
        return NULL;
    }

    /* Allocate space for btl modules */
    mca_btl_openib_component.openib_btls =
        malloc(sizeof(mca_btl_openib_module_t) *
                mca_btl_openib_component.ib_num_btls);
    
    if(NULL == mca_btl_openib_component.openib_btls) {
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return NULL;
    }
    btls = malloc(mca_btl_openib_component.ib_num_btls *
            sizeof(struct mca_btl_openib_module_t*));
    if(NULL == btls) {
        BTL_ERROR(("Failed malloc: %s:%d\n", __FILE__, __LINE__));
        return NULL;
    }

    /* Copy the btl module structs into a contiguous array and fully
       initialize them */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++){
        item = opal_list_remove_first(&btl_list); 
        ib_selected = (mca_btl_base_selected_module_t*)item; 
        openib_btl = (mca_btl_openib_module_t*) ib_selected->btl_module; 
        memcpy(&(mca_btl_openib_component.openib_btls[i]), openib_btl,
                sizeof(mca_btl_openib_module_t)); 
        free(openib_btl); 
        OBJ_RELEASE(ib_selected); 

        openib_btl = &mca_btl_openib_component.openib_btls[i];
        openib_btl->rd_num = mca_btl_openib_component.rd_num +
            mca_btl_openib_component.rd_rsv;
        openib_btl->rd_low = mca_btl_openib_component.rd_low;
        openib_btl->num_peers = 0; 
        openib_btl->sd_tokens[BTL_OPENIB_HP_QP] =
            openib_btl->sd_tokens[BTL_OPENIB_LP_QP] = mca_btl_openib_component.srq_sd_max;

        /* Initialize module state */

        OBJ_CONSTRUCT(&openib_btl->pending_frags[BTL_OPENIB_HP_QP], opal_list_t);
        OBJ_CONSTRUCT(&openib_btl->pending_frags[BTL_OPENIB_LP_QP], opal_list_t);
            
        OBJ_CONSTRUCT(&openib_btl->ib_lock, opal_mutex_t); 
        OBJ_CONSTRUCT(&openib_btl->send_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->send_free_max, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->send_free_frag, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->send_free_control, ompi_free_list_t);
        
        OBJ_CONSTRUCT(&openib_btl->recv_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->recv_free_max, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->recv_free_frag, ompi_free_list_t);

        /* initialize the memory pool using the hca */ 
        openib_btl->super.btl_mpool = openib_btl->hca->mpool;

        /* Initialize pool of send fragments */ 
        length = sizeof(mca_btl_openib_frag_t) + 
            sizeof(mca_btl_openib_header_t) + 
            sizeof(mca_btl_openib_footer_t) + 
            openib_btl->super.btl_eager_limit;

        openib_btl->eager_rdma_frag_size = OPAL_ALIGN(length,
                mca_btl_openib_component.buffer_alignment, int);
 
        ompi_free_list_init_ex(&openib_btl->send_free_eager,
                            length,
                            sizeof(mca_btl_openib_frag_t),
                            mca_btl_openib_component.buffer_alignment,
                            OBJ_CLASS(mca_btl_openib_send_frag_eager_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->super.btl_mpool);
        
        ompi_free_list_init_ex(&openib_btl->recv_free_eager,
                            length, 
                            sizeof(mca_btl_openib_frag_t),
                            mca_btl_openib_component.buffer_alignment,
                            OBJ_CLASS(mca_btl_openib_recv_frag_eager_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->super.btl_mpool);

        length = sizeof(mca_btl_openib_frag_t) + 
            sizeof(mca_btl_openib_header_t) + 
            openib_btl->super.btl_max_send_size;
        
        ompi_free_list_init_ex(&openib_btl->send_free_max,
                            length,
                            sizeof(mca_btl_openib_frag_t),
                            mca_btl_openib_component.buffer_alignment,
                            OBJ_CLASS(mca_btl_openib_send_frag_max_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->super.btl_mpool);
                
        /* Initialize pool of receive fragments */
        ompi_free_list_init_ex(&openib_btl->recv_free_max, 
                             length, 
                             sizeof(mca_btl_openib_frag_t),
                             mca_btl_openib_component.buffer_alignment,
                             OBJ_CLASS (mca_btl_openib_recv_frag_max_t),
                             mca_btl_openib_component.ib_free_list_num,
                             mca_btl_openib_component.ib_free_list_max,
                             mca_btl_openib_component.ib_free_list_inc, 
                             openib_btl->super.btl_mpool);

        length = sizeof(mca_btl_openib_frag_t) +
            sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_footer_t) + 
            sizeof(mca_btl_openib_eager_rdma_header_t);

        ompi_free_list_init_ex(&openib_btl->send_free_control,
                            length,
                            sizeof(mca_btl_openib_frag_t),
                            mca_btl_openib_component.buffer_alignment,
                            OBJ_CLASS(mca_btl_openib_send_frag_control_t),
                            mca_btl_openib_component.ib_free_list_num,
                            -1,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->super.btl_mpool);

        length = sizeof(mca_btl_openib_frag_t);

        ompi_free_list_init(&openib_btl->send_free_frag,
                            length, 
                            OBJ_CLASS(mca_btl_openib_send_frag_frag_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            NULL);

        ompi_free_list_init(&openib_btl->recv_free_frag,
                            length, 
                            OBJ_CLASS(mca_btl_openib_recv_frag_frag_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            NULL);

        orte_pointer_array_init(&openib_btl->eager_rdma_buffers, 
                mca_btl_openib_component.max_eager_rdma,
                mca_btl_openib_component.max_eager_rdma, 
                0);
        openib_btl->eager_rdma_buffers_count = 0;

        orte_pointer_array_init(&openib_btl->endpoints, 10, INT_MAX, 100);
        btls[i] = &openib_btl->super;
    }

    /* Post OOB receive to support dynamic connection setup */
    mca_btl_openib_post_recv();
    btl_openib_modex_send();

    *num_btl_modules = mca_btl_openib_component.ib_num_btls;
#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST
    ibv_free_device_list(ib_devs);
#else
    free(ib_devs);
#endif
    return btls;
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
                                         size_t byte_len, const int prio)
{
    ompi_free_list_t *free_list;
    if(endpoint->nbo) {
        BTL_OPENIB_HEADER_NTOH((*(frag->hdr)));
    }
    if(BTL_OPENIB_HP_QP == prio)
        free_list = &openib_btl->recv_free_eager;
    else
        free_list = &openib_btl->recv_free_max;

    /* advance the segment address past the header and subtract from the
     * length..*/
    frag->segment.seg_len = byte_len - sizeof(mca_btl_openib_header_t);

    /* call registered callback */
    openib_btl->ib_reg[frag->hdr->tag].cbfunc(&openib_btl->super,
            frag->hdr->tag, &frag->base, 
            openib_btl->ib_reg[frag->hdr->tag].cbdata);

    if(BTL_OPENIB_IS_RDMA_CREDITS(frag->hdr->credits) &&
            BTL_OPENIB_CREDITS(frag->hdr->credits) > 0)
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens,
            BTL_OPENIB_CREDITS(frag->hdr->credits));
    else
        if(!mca_btl_openib_component.use_srq && frag->hdr->credits > 0)
            OPAL_THREAD_ADD32(&endpoint->sd_tokens[prio],
                frag->hdr->credits);

    if (!MCA_BTL_OPENIB_RDMA_FRAG(frag)) {
        OMPI_FREE_LIST_RETURN(free_list, (ompi_free_list_item_t*) frag);
    } else {
        mca_btl_openib_frag_t *tf;
        OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
        MCA_BTL_OPENIB_RDMA_MAKE_REMOTE(frag->ftr);
        while (endpoint->eager_rdma_local.tail !=
                endpoint->eager_rdma_local.head) {
            tf = MCA_BTL_OPENIB_GET_LOCAL_RDMA_FRAG(endpoint,
                    endpoint->eager_rdma_local.tail);
            if (MCA_BTL_OPENIB_RDMA_FRAG_LOCAL (tf))
                break;
            OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits, 1);
            MCA_BTL_OPENIB_RDMA_NEXT_INDEX(endpoint->eager_rdma_local.tail);
         }
        OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
    }
   
    if (!endpoint->eager_rdma_local.base.pval &&
            endpoint->use_eager_rdma &&
            BTL_OPENIB_HP_QP == prio &&
            openib_btl->eager_rdma_buffers_count <
            mca_btl_openib_component.max_eager_rdma &&
            OPAL_THREAD_ADD32(&endpoint->eager_recv_count, 1) ==
            mca_btl_openib_component.eager_rdma_threshold) {
        mca_btl_openib_endpoint_connect_eager_rdma(endpoint); 
    }

    /* repost receive descriptors if receive not by RDMA */
    if(!MCA_BTL_OPENIB_RDMA_FRAG(frag)) {
        if(mca_btl_openib_component.use_srq) {
            OPAL_THREAD_ADD32((int32_t*)&openib_btl->srd_posted[prio], -1);
            mca_btl_openib_post_srr(openib_btl, 0, prio);
        } else {
            OPAL_THREAD_ADD32((int32_t*)&endpoint->rd_posted[prio], -1);
            btl_openib_endpoint_post_rr(endpoint, 0, prio);
        }
    }

    /* nothing to progress for SRQ case */
    if(!mca_btl_openib_component.use_srq) {
        btl_openib_frag_progress_pending(openib_btl, endpoint, prio);
    }

    /* check to see if we need to return credits */
    if(btl_openib_check_send_credits(endpoint, prio)) {
        mca_btl_openib_endpoint_send_credits(endpoint, prio);
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
        return "RECEIVER NOT READY RETRY EXCEEEDED ERROR";
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


static inline int btl_openib_frag_progress_one(
        mca_btl_openib_module_t* openib_btl,
        mca_btl_openib_frag_t* frag)
{
    int res;

    switch(frag->wr_desc.sr_desc.opcode) {
        case IBV_WR_SEND:
            res = mca_btl_openib_endpoint_send(frag->endpoint, frag);
            break;
        case IBV_WR_RDMA_WRITE:
            res = mca_btl_openib_put((mca_btl_base_module_t*) openib_btl,
                    frag->endpoint,
                    (mca_btl_base_descriptor_t*) frag);
            break;
        case IBV_WR_RDMA_READ:
            res = mca_btl_openib_get((mca_btl_base_module_t *) openib_btl,
                    frag->endpoint,
                    (mca_btl_base_descriptor_t*) frag);
            break;
        default:
            res = OMPI_ERROR; 
            BTL_ERROR(("error in posting pending operation, "
                        "invalide opcode %d\n", frag->wr_desc.sr_desc.opcode));
            break;
    }

    return res;
}

#define BTL_OPENIB_TOKENS(E, P) ((E)->sd_tokens[(P)] + \
        (((P) == BTL_OPENIB_HP_QP)?(E)->eager_rdma_remote.tokens:0))
static void btl_openib_frag_progress_pending(
        mca_btl_openib_module_t* openib_btl, mca_btl_base_endpoint_t *endpoint,
        const int prio)
{
    
    opal_list_item_t *frag_item;
    mca_btl_openib_frag_t* frag;
    size_t i, len = opal_list_get_size(&endpoint->pending_frags[prio]);

    /* check to see if we need to progress any pending descriptors */
    for(i = 0; i < len && endpoint->sd_wqe[prio] > 0 &&
            BTL_OPENIB_TOKENS(endpoint, prio) > 0; i++) {
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        frag_item = opal_list_remove_first(&(endpoint->pending_frags[prio]));
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
            break;
        if(btl_openib_frag_progress_one(openib_btl, frag) ==
                OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }

    if(BTL_OPENIB_LP_QP == prio) {
        len = opal_list_get_size(&endpoint->pending_get_frags);
        for(i = 0; i < len && endpoint->sd_wqe[BTL_OPENIB_LP_QP] > 0 &&
                endpoint->get_tokens > 0; i++) {
            OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
            frag_item = opal_list_remove_first(&(endpoint->pending_get_frags));
            OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
            if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
                break;
            if(btl_openib_frag_progress_one(openib_btl, frag) ==
                    OMPI_ERR_OUT_OF_RESOURCE)
                break;
        }

        len = opal_list_get_size(&endpoint->pending_put_frags);
        for(i = 0; i < len && endpoint->sd_wqe[BTL_OPENIB_LP_QP] > 0; i++) {
            OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
            frag_item = opal_list_remove_first(&(endpoint->pending_put_frags));
            OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
            if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
                break;
            if(btl_openib_frag_progress_one(openib_btl, frag) ==
                    OMPI_ERR_OUT_OF_RESOURCE)
                break;
        }
    }

    if(!mca_btl_openib_component.use_srq)
        return;

    len = opal_list_get_size(&openib_btl->pending_frags[prio]);
    for(i = 0; i < len && openib_btl->sd_tokens[prio] > 0; i++) {
        /* dequeue resources due to global flow control */
        OPAL_THREAD_LOCK(&openib_btl->ib_lock);
        frag_item = opal_list_remove_first(&openib_btl->pending_frags[prio]);
        OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
        if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
            break;
        if(btl_openib_frag_progress_one(openib_btl, frag) ==
                OMPI_ERR_OUT_OF_RESOURCE)
            break;
     }
}

/*
 *  IB component progress.
 */
static int btl_openib_component_progress(void)
{
    static char *qp_name[] = {"HP", "LP"};
    int i, j, c, qp;
    int count = 0,ne = 0, ret;
    mca_btl_openib_frag_t* frag; 
    mca_btl_openib_endpoint_t* endpoint; 
    struct ibv_wc wc; 
    mca_btl_openib_module_t* openib_btl;

    /* Poll for RDMA completions - if any succeed, we don't process the slower
     * queues.
     */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        mca_btl_openib_module_t* openib_btl = &mca_btl_openib_component.openib_btls[i];

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
                opal_atomic_mb();
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
                        BTL_OPENIB_HP_QP);
                if (ret != MPI_SUCCESS) { 
                    openib_btl->error_cb(&openib_btl->super, 
                                         MCA_BTL_ERROR_FLAGS_FATAL);
                    return 0;
                }
                count++;
            } else
                OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
        }
    }
    if(count) return count;

    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        openib_btl = &mca_btl_openib_component.openib_btls[i];
        
        /* We have two completion queues, one for "high" priority and one for
         * "low". Check high priority before low priority */
        for(qp = 0; qp < 2; qp++) {
            ne = ibv_poll_cq(openib_btl->ib_cq[qp], 1, &wc);

            if(0 == ne)
                continue;

            if(ne < 0 || wc.status != IBV_WC_SUCCESS)
                goto error;

            frag = (mca_btl_openib_frag_t*) (unsigned long) wc.wr_id; 
            endpoint = frag->endpoint;
            /* Handle work completions */
            switch(wc.opcode) {
            case IBV_WC_RDMA_READ:
                assert(BTL_OPENIB_LP_QP == qp);
                OPAL_THREAD_ADD32(&endpoint->get_tokens, 1);
                /* fall through */

            case IBV_WC_RDMA_WRITE:
                if(BTL_OPENIB_LP_QP == qp) {
                    /* process a completed write */
                    frag->base.des_cbfunc(&openib_btl->super, endpoint,
                            &frag->base, OMPI_SUCCESS);

                    /* return send wqe */
                    OPAL_THREAD_ADD32(&endpoint->sd_wqe[qp], 1);

                    /* check for pending frags */
                    btl_openib_frag_progress_pending(openib_btl, endpoint, qp);

                    count++;
                    break;
                }
                /* fall through for high prio QP */
            case IBV_WC_SEND:
                /* Process a completed send */
                frag->base.des_cbfunc(&openib_btl->super, endpoint, &frag->base,
                        OMPI_SUCCESS); 

                /* return send wqe */
                OPAL_THREAD_ADD32(&endpoint->sd_wqe[qp], 1);
                if(mca_btl_openib_component.use_srq)
                    OPAL_THREAD_ADD32(&openib_btl->sd_tokens[qp], 1);
                /* check to see if we need to progress any pending descriptors */
                btl_openib_frag_progress_pending(openib_btl, endpoint, qp);

                /* check to see if we need to return credits */
                if(btl_openib_check_send_credits(endpoint, qp)) {
                    mca_btl_openib_endpoint_send_credits(endpoint, qp);
                }

                count++;
                break;

            case IBV_WC_RECV: 
                if(wc.wc_flags & IBV_WC_WITH_IMM) {
                    endpoint = (mca_btl_openib_endpoint_t*)
                        orte_pointer_array_get_item(openib_btl->endpoints,
                                wc.imm_data);
                    frag->endpoint = endpoint;
                }
                /* Process a RECV */ 
                ret = btl_openib_handle_incoming(openib_btl, endpoint, frag,
                        wc.byte_len, qp);
                if (ret != OMPI_SUCCESS) { 
                    openib_btl->error_cb(&openib_btl->super,
                            MCA_BTL_ERROR_FLAGS_FATAL);
                    return 0;
                }
                count++; 
                break; 

            default:
                BTL_ERROR(("Unhandled work completion opcode is %d",
                            wc.opcode));
                openib_btl->error_cb(&openib_btl->super,
                        MCA_BTL_ERROR_FLAGS_FATAL);
                break;
            }
        }
    }
    return count;
error:
    if(ne < 0){ 
        BTL_ERROR(("error polling %s CQ with %d errno says %s\n",
                    qp_name[qp], ne, strerror(errno))); 
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
        if(wc.status != IBV_WC_WR_FLUSH_ERR || !flush_err_printed[qp]++)
            BTL_PEER_ERROR(remote_proc, ("error polling %s CQ with status %s "
                        "status number %d for wr_id %llu opcode %d",
                        qp_name[qp],
                        btl_openib_component_status_to_string(wc.status), 
                       wc.status, wc.wr_id, wc.opcode)); 
        if(wc.status == IBV_WC_RETRY_EXC_ERR) { 
            opal_show_help("help-mpi-btl-openib.txt",
                    "btl_openib:retry-exceeded", true);
        }
    }
    openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL); 
    return count;
}
