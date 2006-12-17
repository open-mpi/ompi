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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/proc/proc.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/base/base.h" 
#include "btl_mvapi.h"
#include "btl_mvapi_frag.h"
#include "btl_mvapi_endpoint.h" 
#include "btl_mvapi_eager_rdma.h"
#include "btl_mvapi_proc.h"
#include "ompi/mca/btl/base/base.h" 
#include <vapi.h> 
#include <vapi_common.h> 
#include "ompi/datatype/convertor.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "btl_mvapi_endpoint.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"

static int mvapi_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg);
static int mvapi_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg);

mca_btl_mvapi_component_t mca_btl_mvapi_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        
        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_1,

            "mvapi", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_mvapi_component_open,  /* component open */
            mca_btl_mvapi_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_btl_mvapi_component_init,  
        mca_btl_mvapi_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline void mca_btl_mvapi_param_register_string(
                                                       const char* param_name, 
                                                       const char* param_desc,
                                                       const char* default_value, 
                                                       char** out_value)
{
    mca_base_param_reg_string(&mca_btl_mvapi_component.super.btl_version, 
                              param_name, 
                              param_desc, 
                              false, 
                              false, 
                              default_value, 
                              out_value);
}

static inline void  mca_btl_mvapi_param_register_int(
                                                     const char* param_name,
                                                     const char* param_desc,
                                                     int default_value, 
                                                     int* out_value)
{
    mca_base_param_reg_int(&mca_btl_mvapi_component.super.btl_version, 
                           param_name, 
                           param_desc, 
                           false, 
                           false, 
                           default_value, 
                           out_value); 
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_mvapi_component_open(void)
{
    char *msg;
    int tmp_int;

    /* initialize state */
    mca_btl_mvapi_component.ib_num_btls=0;
    mca_btl_mvapi_component.mvapi_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_mvapi_component.ib_procs, opal_list_t);

    /* register IB component parameters */
    mca_btl_mvapi_param_register_int ("max_btls", "maximum number of HCAs/ports to use", 
                                      4, &tmp_int);
    mca_btl_mvapi_component.ib_max_btls = tmp_int;
    mca_btl_mvapi_param_register_int ("free_list_num", "initial size of free lists", 
                                       8, &mca_btl_mvapi_component.ib_free_list_num);
    mca_btl_mvapi_param_register_int ("free_list_max", "maximum size of free lists",
                                       -1, &mca_btl_mvapi_component.ib_free_list_max);
    mca_btl_mvapi_param_register_int ("free_list_inc", "increment size of free lists", 
                                       32, &mca_btl_mvapi_component.ib_free_list_inc);
    mca_btl_mvapi_param_register_string("mpool", "name of the memory pool to be used", 
                                         "rdma", &mca_btl_mvapi_component.ib_mpool_name);
    mca_btl_mvapi_param_register_int("reg_mru_len",  "length of the registration cache most recently used list", 
                                      16, (int*) &mca_btl_mvapi_component.reg_mru_len); 
#ifdef VAPI_FEATURE_SRQ 
    mca_btl_mvapi_param_register_int("use_srq", "if 1 use the IB shared receive queue to post receive descriptors", 
                                     0, (int*) &mca_btl_mvapi_component.use_srq); 
#else
    mca_btl_mvapi_component.use_srq = 0;
#endif
    mca_btl_mvapi_param_register_int("ib_cq_size", "size of the IB completion queue",
                                     10000, (int*) &mca_btl_mvapi_component.ib_cq_size); 
    mca_btl_mvapi_param_register_int("ib_sg_list_size", "size of IB segment list", 
                                     1, (int*) &mca_btl_mvapi_component.ib_sg_list_size); 
    mca_btl_mvapi_param_register_int("ib_pkey_ix", "IB pkey index", 
                                      0, (int*) &mca_btl_mvapi_component.ib_pkey_ix); 
    mca_btl_mvapi_param_register_int("ib_psn", "IB Packet sequence starting number", 
                                      0, (int*) &mca_btl_mvapi_component.ib_psn); 
    mca_btl_mvapi_param_register_int("ib_qp_ous_rd_atom", "IB outstanding atomic reads", 
                                     4, (int*) &mca_btl_mvapi_component.ib_qp_ous_rd_atom); 
    tmp_int = MTU1024;
    asprintf(&msg, "IB MTU, in bytes.  Valid values are: %d=256 bytes, %d=512 bytes, %d=1024 bytes, %d=2048 bytes, %d=4096 bytes.",
             MTU256,
             MTU512,
             MTU1024,
             MTU2048,
             MTU4096);
    mca_btl_mvapi_param_register_int("ib_mtu", msg,
                                     tmp_int, (int*) &mca_btl_mvapi_component.ib_mtu); 
    free(msg);
       
    mca_btl_mvapi_param_register_int("ib_min_rnr_timer", "IB min rnr timer", 
                                      5, (int*) &mca_btl_mvapi_component.ib_min_rnr_timer);
    mca_btl_mvapi_param_register_int("ib_timeout", "IB transmit timeout", 
                                      10, (int*) &mca_btl_mvapi_component.ib_timeout); 
    mca_btl_mvapi_param_register_int("ib_retry_count", "IB transmit retry count",
                                      7, (int*) &mca_btl_mvapi_component.ib_retry_count); 
    mca_btl_mvapi_param_register_int("ib_rnr_retry", "IB rnr retry", 
                                     7, (int*) &mca_btl_mvapi_component.ib_rnr_retry); 
    mca_btl_mvapi_param_register_int("ib_max_rdma_dst_ops", "IB max rdma destination operations", 
                                      4, (int*) &mca_btl_mvapi_component.ib_max_rdma_dst_ops); 
    mca_btl_mvapi_param_register_int("ib_service_level", "IB service level", 
                                      0, (int*) &mca_btl_mvapi_component.ib_service_level); 
    mca_btl_mvapi_param_register_int("ib_static_rate", "IB static rate", 
                                      0, (int*) &mca_btl_mvapi_component.ib_static_rate); 
    mca_btl_mvapi_param_register_int("ib_src_path_bits", "IB source path bits", 
                                      0, (int*) &mca_btl_mvapi_component.ib_src_path_bits); 
        
    mca_btl_mvapi_param_register_int("rd_num", "number of receive descriptors to post to a QP", 
                                      8, (int*) &mca_btl_mvapi_component.rd_num);  
    mca_btl_mvapi_param_register_int("rd_low", "low water mark before reposting occurs", 
                                      6,  (int*) &mca_btl_mvapi_component.rd_low); 
    mca_btl_mvapi_param_register_int("rd_win", "window size at which generate explicity credit message", 
                                      4,  (int*) &mca_btl_mvapi_component.rd_win); 
    mca_btl_mvapi_component.rd_rsv = ((mca_btl_mvapi_component.rd_num<<1)-1) / mca_btl_mvapi_component.rd_win;

    mca_btl_mvapi_param_register_int("srq_rd_max", "Maximum number of receive descriptors posted per SRQ.\n",
                                      1000, (int*) &mca_btl_mvapi_component.srq_rd_max); 
    mca_btl_mvapi_param_register_int("srq_rd_per_peer", "receive descriptors posted per peer, SRQ mode only", 
                                      16, (int*) &mca_btl_mvapi_component.srq_rd_per_peer); 
    mca_btl_mvapi_param_register_int("srq_sd_max", "Maximum number of send descriptors posted per process", 
                                      8,  &mca_btl_mvapi_component.srq_sd_max); 

    mca_btl_mvapi_param_register_int("use_eager_rdma", "user RDMA for eager messages", 
            1, (int*) &mca_btl_mvapi_component.use_eager_rdma);
    if (mca_btl_mvapi_component.use_srq)
        mca_btl_mvapi_component.use_eager_rdma = 0;
    mca_btl_mvapi_param_register_int("eager_rdma_threshold", "Open rdma channel for eager messages after this number of messages received from peer (zero to disable)",
            16, (int*)&mca_btl_mvapi_component.eager_rdma_threshold);
    mca_btl_mvapi_param_register_int("max_eager_rdma", "Maximum number of eager RDMA connections",
            16, (int*)&mca_btl_mvapi_component.max_eager_rdma);
    mca_btl_mvapi_param_register_int("eager_rdma_num", "Number of RDMA buffers for eager messages",
            16, (int*)&mca_btl_mvapi_component.eager_rdma_num);
    mca_btl_mvapi_component.eager_rdma_num+=1;
    mca_btl_mvapi_param_register_int ("exclusivity", "BTL exclusivity", 
                                      MCA_BTL_EXCLUSIVITY_DEFAULT, (int*) &mca_btl_mvapi_module.super.btl_exclusivity);
    mca_btl_mvapi_param_register_int ("eager_limit", "eager send limit", 
                                      (12*1024), &tmp_int); 
    mca_btl_mvapi_module.super.btl_eager_limit = tmp_int;  
    mca_btl_mvapi_param_register_int ("min_send_size", "minimum send size", 
                                      (32*1024), &tmp_int);
    mca_btl_mvapi_module.super.btl_min_send_size = tmp_int;
    mca_btl_mvapi_param_register_int ("max_send_size", "maximum send size", 
                                      (64*1024), &tmp_int);
    mca_btl_mvapi_module.super.btl_max_send_size = tmp_int; 
    mca_btl_mvapi_param_register_int("min_rdma_size", "minimum rdma size", 
                                     1024*1024, &tmp_int);
    mca_btl_mvapi_module.super.btl_min_rdma_size = tmp_int;
    mca_btl_mvapi_param_register_int("max_rdma_size", "maximium rdma size", 
                                     1024*1024, &tmp_int);
    mca_btl_mvapi_module.super.btl_max_rdma_size = tmp_int; 
    mca_btl_mvapi_param_register_int("flags", "BTL flags, SEND=1, PUT=2, GET=4", 
                                     MCA_BTL_FLAGS_PUT | MCA_BTL_FLAGS_GET, (int*) &mca_btl_mvapi_module.super.btl_flags); 
    mca_btl_mvapi_param_register_int("bandwidth", "Approximate maximum bandwidth of interconnect", 
                                      800, (int*) &mca_btl_mvapi_module.super.btl_bandwidth); 
    
    mca_btl_mvapi_component.max_send_size = mca_btl_mvapi_module.super.btl_max_send_size; 
    mca_btl_mvapi_component.eager_limit = mca_btl_mvapi_module.super.btl_eager_limit; 
    
    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_mvapi_component_close(void)
{
    return OMPI_SUCCESS;
}



/*
 *  Register MVAPI port information. The MCA framework
 *  will make this available to all peers.
 */

static int
mca_btl_mvapi_modex_send(void)
{
    int         rc;
    size_t      i;
    size_t      size;
    mca_btl_mvapi_port_info_t *ports = NULL;

    size = mca_btl_mvapi_component.ib_num_btls * sizeof (mca_btl_mvapi_port_info_t);
    if (size != 0) {
        ports = (mca_btl_mvapi_port_info_t *)malloc (size);
        if (NULL == ports) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for (i = 0; i < mca_btl_mvapi_component.ib_num_btls; i++) {
            mca_btl_mvapi_module_t *btl = &mca_btl_mvapi_component.mvapi_btls[i];
            ports[i] = btl->port_info;
        }
    }
    rc = mca_pml_base_modex_send (&mca_btl_mvapi_component.super.btl_version, ports, size);
    if (NULL != ports) {
        free (ports);
    }
    return rc;
}

/*
 * Callback function on control message.
 */

static void mca_btl_mvapi_control(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_tag_t tag,
    mca_btl_base_descriptor_t* descriptor,
    void* cbdata)
{
    /* dont return credits used for control messages */
    mca_btl_mvapi_frag_t* frag = (mca_btl_mvapi_frag_t*)descriptor; 
    mca_btl_mvapi_endpoint_t* endpoint = frag->endpoint;
    mca_btl_mvapi_control_header_t *ctl_hdr = frag->segment.seg_addr.pval;
    mca_btl_mvapi_eager_rdma_header_t *rdma_hdr;

    if(frag->size == mca_btl_mvapi_component.eager_limit) {
        /* if not sent via rdma */
        if (!MCA_BTL_MVAPI_RDMA_FRAG(frag) &&
                ctl_hdr->type == MCA_BTL_MVAPI_CONTROL_NOOP)
            OPAL_THREAD_ADD32(&endpoint->rd_credits_hp, -1);

    } else {
        OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, -1);
    }

    switch (ctl_hdr->type) {
    case MCA_BTL_MVAPI_CONTROL_NOOP:
        break;
    case MCA_BTL_MVAPI_CONTROL_RDMA:
        rdma_hdr = (mca_btl_mvapi_eager_rdma_header_t*)ctl_hdr;
        if (endpoint->eager_rdma_remote.base.pval) {
           BTL_ERROR(("Got RDMA connect twise!\n"));
           return;
        }
        endpoint->eager_rdma_remote.rkey = rdma_hdr->rkey;
        endpoint->eager_rdma_remote.base.pval = rdma_hdr->rdma_start.pval;
        endpoint->eager_rdma_remote.tokens =
            mca_btl_mvapi_component.eager_rdma_num - 1;
        break;
    default:
        BTL_ERROR(("Unknown message type sent by BTL\n"));
        break;
    }
}

static int mvapi_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg)
{
    mca_btl_mvapi_module_t *mvapi_btl = (mca_btl_mvapi_module_t*)reg_data;
    mca_btl_mvapi_reg_t *mvapi_reg = (mca_btl_mvapi_reg_t*)reg;
    VAPI_mrw_t mr_in, mr_out;
    VAPI_ret_t ret;

    memset(&mr_in, 0, sizeof(VAPI_mrw_t));
    memset(&mr_out, 0, sizeof(VAPI_mrw_t));
    mr_in.acl =
        VAPI_EN_LOCAL_WRITE | VAPI_EN_REMOTE_WRITE | VAPI_EN_REMOTE_READ;
    mr_in.pd_hndl = mvapi_btl->ptag;
    mr_in.size = size;
    mr_in.start = (VAPI_virt_addr_t)(MT_virt_addr_t)base;
    mr_in.type = VAPI_MR;
    mvapi_reg->hndl = VAPI_INVAL_HNDL;

    ret = VAPI_register_mr(mvapi_btl->nic, &mr_in, &mvapi_reg->hndl, &mr_out);

    if(ret != VAPI_OK) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    mvapi_reg->l_key = mr_out.l_key;
    mvapi_reg->r_key = mr_out.r_key;
    return OMPI_SUCCESS;
}

static int mvapi_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg)
{
    mca_btl_mvapi_module_t *mvapi_btl = (mca_btl_mvapi_module_t*)reg_data;
    mca_btl_mvapi_reg_t *mvapi_reg = (mca_btl_mvapi_reg_t*)reg;
    VAPI_ret_t ret;

    if(mvapi_reg->hndl != VAPI_INVAL_HNDL) {
        ret = VAPI_deregister_mr(mvapi_btl->nic, mvapi_reg->hndl);
        if(ret != VAPI_OK) {
            opal_output(0, "%s: error unpinning mvapi memory errno says %s\n",
                    __func__, strerror(errno));
            return OMPI_ERROR;
        }
    }
    return OMPI_SUCCESS;
}

/*
 *  IB component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */

mca_btl_base_module_t** mca_btl_mvapi_component_init(int *num_btl_modules, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    VAPI_ret_t vapi_ret;
    VAPI_hca_id_t* hca_ids;
    VAPI_hca_hndl_t hca_hndl; 
    VAPI_hca_vendor_t hca_vendor; 
    VAPI_hca_cap_t hca_cap; 
    VAPI_hca_port_t hca_port; 
    uint32_t num_hcas; 
    mca_btl_base_module_t** btls;
    uint32_t i,j, length;
    struct mca_mpool_base_resources_t hca_pd; 
    opal_list_t btl_list; 
    mca_btl_mvapi_module_t * mvapi_btl; 
    mca_btl_base_selected_module_t* ib_selected; 
    opal_list_item_t* item; 

    /* initialization */
    *num_btl_modules = 0;

    /* mvapi BTL does not currently support progress threads, so
       disable the component if they were requested */
    if (enable_progress_threads) {
        mca_btl_base_error_no_nics("MVAPI", "HCA");
        mca_btl_mvapi_component.ib_num_btls = 0;
        mca_btl_mvapi_modex_send();
        return NULL;
    }

    /* Determine the number of hca's available on the host */
    vapi_ret=EVAPI_list_hcas(0, &num_hcas, NULL);
    if( VAPI_EAGAIN != vapi_ret || 0 == num_hcas ) {
        mca_btl_base_error_no_nics("MVAPI", "HCA");
        mca_btl_mvapi_component.ib_num_btls = 0;
        mca_btl_mvapi_modex_send();
        return NULL;
    }

    /* Allocate space for the hca's */ 
    hca_ids = (VAPI_hca_id_t*) malloc(num_hcas * sizeof(VAPI_hca_id_t));
    if(NULL == hca_ids) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    /* obtain a list of the hca's on this host */ 
    vapi_ret=EVAPI_list_hcas(num_hcas, &num_hcas, hca_ids);
    if( VAPI_OK != vapi_ret ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
        
    /** We must loop through all the hca id's, get there handles and 
        for each hca we query the number of ports on the hca and set up 
        a distinct btl module for each hca port */ 

    OBJ_CONSTRUCT(&btl_list, opal_list_t); 
    OBJ_CONSTRUCT(&mca_btl_mvapi_component.ib_lock, opal_mutex_t);


    for(i = 0; 
        i < num_hcas && mca_btl_mvapi_component.ib_num_btls < mca_btl_mvapi_component.ib_max_btls; 
        i++){  
        vapi_ret = EVAPI_get_hca_hndl(hca_ids[i], &hca_hndl); 
        if(VAPI_OK != vapi_ret) { 
            BTL_ERROR(("error getting hca handle: %s", VAPI_strerror(vapi_ret))); 
            return NULL; 
        } 
        

        vapi_ret = VAPI_query_hca_cap(hca_hndl, &hca_vendor, &hca_cap); 
         if(VAPI_OK != vapi_ret) { 
            BTL_ERROR(("error getting hca properties %s", VAPI_strerror(vapi_ret))); 
            return NULL; 
        } 
         
         
         /* Note ports are 1 based hence j = 1 */
         for(j = 1; j <= hca_cap.phys_port_num; j++){ 
             vapi_ret = VAPI_query_hca_port_prop(hca_hndl, (IB_port_t) j, &hca_port);  
             if(VAPI_OK != vapi_ret) { 
                 BTL_ERROR(("error getting hca port properties %s", VAPI_strerror(vapi_ret))); 
                 return NULL; 
             } 
             
             if( PORT_ACTIVE == hca_port.state ){ 
                 
                 mvapi_btl = (mca_btl_mvapi_module_t*) malloc(sizeof(mca_btl_mvapi_module_t)); 
                 memcpy(mvapi_btl, &mca_btl_mvapi_module, sizeof(mca_btl_mvapi_module));
                 
                 ib_selected = OBJ_NEW(mca_btl_base_selected_module_t); 
                 ib_selected->btl_module = (mca_btl_base_module_t*) mvapi_btl; 
                 memcpy(mvapi_btl->hca_id,   hca_ids[i], sizeof(VAPI_hca_id_t)); 
                 mvapi_btl->nic = hca_hndl; 
                 mvapi_btl->port_id = (IB_port_t) j; 
                 mvapi_btl->port = hca_port; 
                 mvapi_btl->port_info.subnet = hca_port.sm_lid;
                 mvapi_btl->ib_reg[MCA_BTL_TAG_BTL].cbfunc = mca_btl_mvapi_control;
                 mvapi_btl->ib_reg[MCA_BTL_TAG_BTL].cbdata = NULL;
                 
                 opal_list_append(&btl_list, (opal_list_item_t*) ib_selected);
                 if(++mca_btl_mvapi_component.ib_num_btls >= mca_btl_mvapi_component.ib_max_btls)
                     break;
            } 
        }
    }
    
    if(0 == mca_btl_mvapi_component.ib_num_btls){ 
        mca_btl_base_error_no_nics("MVAPI", "HCA");
        mca_btl_mvapi_modex_send();
        return NULL; 
    }
    /* Allocate space for btl modules */
    mca_btl_mvapi_component.mvapi_btls = (mca_btl_mvapi_module_t*) malloc(sizeof(mca_btl_mvapi_module_t) * 
                                                                          mca_btl_mvapi_component.ib_num_btls);
    
    if(NULL == mca_btl_mvapi_component.mvapi_btls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    btls = (struct mca_btl_base_module_t**) 
        malloc(mca_btl_mvapi_component.ib_num_btls * sizeof(struct mca_btl_mvapi_module_t*));
    if(NULL == btls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    
    
    for(i = 0; i < mca_btl_mvapi_component.ib_num_btls; i++){
                 
        
        item = opal_list_remove_first(&btl_list); 
        ib_selected = (mca_btl_base_selected_module_t*)item; 
        mvapi_btl = (mca_btl_mvapi_module_t*) ib_selected->btl_module; 
        memcpy(&(mca_btl_mvapi_component.mvapi_btls[i]), mvapi_btl , sizeof(mca_btl_mvapi_module_t)); 
        free(ib_selected); 
        free(mvapi_btl); 

        mvapi_btl = &mca_btl_mvapi_component.mvapi_btls[i];
        mvapi_btl->rd_num = mca_btl_mvapi_component.rd_num + mca_btl_mvapi_component.rd_rsv;
        mvapi_btl->rd_low = mca_btl_mvapi_component.rd_low;
        mvapi_btl->num_peers = 0; 
        mvapi_btl->sd_tokens_hp = mvapi_btl->sd_tokens_lp = mca_btl_mvapi_component.srq_sd_max;

        /* Initialize module state */

        OBJ_CONSTRUCT(&mvapi_btl->pending_frags_hp, opal_list_t);
        OBJ_CONSTRUCT(&mvapi_btl->pending_frags_lp, opal_list_t);
    

        OBJ_CONSTRUCT(&mvapi_btl->ib_lock, opal_mutex_t); 
        OBJ_CONSTRUCT(&mvapi_btl->send_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&mvapi_btl->send_free_max, ompi_free_list_t);
        OBJ_CONSTRUCT(&mvapi_btl->send_free_frag, ompi_free_list_t);
        
        OBJ_CONSTRUCT(&mvapi_btl->recv_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&mvapi_btl->recv_free_max, ompi_free_list_t);
        
        if(mca_btl_mvapi_module_init(mvapi_btl) != OMPI_SUCCESS) {
            free(hca_ids);
            return NULL;
        }
                         
        hca_pd.reg_data = mvapi_btl;
        hca_pd.sizeof_reg = sizeof(mca_btl_mvapi_reg_t);
        hca_pd.register_mem = mvapi_reg_mr;
        hca_pd.deregister_mem = mvapi_dereg_mr;
        /* initialize the memory pool using the hca */ 
        mvapi_btl->super.btl_mpool = 
            mca_mpool_base_module_create(mca_btl_mvapi_component.ib_mpool_name,
                                         &mvapi_btl->super, 
                                         &hca_pd); 
        
        if(NULL == mvapi_btl->super.btl_mpool) { 
            BTL_ERROR(("error creating vapi memory pool! aborting mvapi btl initialization")); 
            return NULL; 
        }
        /* Initialize pool of send fragments */ 
        
        length = sizeof(mca_btl_mvapi_frag_t) + 
            sizeof(mca_btl_mvapi_header_t) + 
            sizeof(mca_btl_mvapi_footer_t) +
            mvapi_btl->super.btl_eager_limit+ 
            2*MCA_BTL_IB_FRAG_ALIGN; 
       
        mvapi_btl->eager_rdma_frag_size =
            length & ~(2 * MCA_BTL_IB_FRAG_ALIGN - 1);

        ompi_free_list_init(&mvapi_btl->send_free_eager,
                            length, 
                            OBJ_CLASS(mca_btl_mvapi_send_frag_eager_t),
                            2*mvapi_btl->rd_num,
                            mca_btl_mvapi_component.ib_free_list_max,
                            mca_btl_mvapi_component.ib_free_list_inc,
                            mvapi_btl->super.btl_mpool);
        
        ompi_free_list_init(&mvapi_btl->recv_free_eager,
                            length, 
                            OBJ_CLASS(mca_btl_mvapi_recv_frag_eager_t),
                            2*mvapi_btl->rd_num,
                            mca_btl_mvapi_component.ib_free_list_max,
                            mca_btl_mvapi_component.ib_free_list_inc,
                            mvapi_btl->super.btl_mpool);
        
        
        
        length = sizeof(mca_btl_mvapi_frag_t) + 
            sizeof(mca_btl_mvapi_header_t) + 
            mvapi_btl->super.btl_max_send_size+
            2*MCA_BTL_IB_FRAG_ALIGN; 
        
        
        ompi_free_list_init(&mvapi_btl->send_free_max,
                            length, 
                            OBJ_CLASS(mca_btl_mvapi_send_frag_max_t),
                            2*mvapi_btl->rd_num,
                            mca_btl_mvapi_component.ib_free_list_max,
                            mca_btl_mvapi_component.ib_free_list_inc,
                            mvapi_btl->super.btl_mpool);
        
        
        
        /* Initialize pool of receive fragments */
        ompi_free_list_init (&mvapi_btl->recv_free_max, 
                             length, 
                             OBJ_CLASS (mca_btl_mvapi_recv_frag_max_t),
                             2*mvapi_btl->rd_num,
                             mca_btl_mvapi_component.ib_free_list_max,
                             mca_btl_mvapi_component.ib_free_list_inc, 
                             mvapi_btl->super.btl_mpool);

        
        
        length = sizeof(mca_btl_mvapi_frag_t) + 
            sizeof(mca_btl_mvapi_header_t)+ 
            2*MCA_BTL_IB_FRAG_ALIGN; 

        
        
        
        ompi_free_list_init(&mvapi_btl->send_free_frag,
                            length, 
                            OBJ_CLASS(mca_btl_mvapi_send_frag_frag_t),
                            mca_btl_mvapi_component.ib_free_list_num,
                            mca_btl_mvapi_component.ib_free_list_max,
                            mca_btl_mvapi_component.ib_free_list_inc,
                            mvapi_btl->super.btl_mpool);
        
        orte_pointer_array_init(&mvapi_btl->eager_rdma_buffers,
                mca_btl_mvapi_component.max_eager_rdma,
                mca_btl_mvapi_component.max_eager_rdma, 
                0);
        mvapi_btl->eager_rdma_buffers_count = 0;
        OBJ_CONSTRUCT(&mvapi_btl->eager_rdma_lock, opal_mutex_t); 

        /* Initialize the rr_desc_post array for posting of rr*/ 
        mvapi_btl->rr_desc_post = (VAPI_rr_desc_t*) malloc(
            ((mca_btl_mvapi_component.rd_num + mca_btl_mvapi_component.rd_rsv) * sizeof(VAPI_rr_desc_t))); 
        btls[i] = &mvapi_btl->super;
    }

    /* Post OOB receive to support dynamic connection setup */
    mca_btl_mvapi_post_recv();
    
    mca_btl_mvapi_modex_send(); 
    *num_btl_modules = mca_btl_mvapi_component.ib_num_btls;
    free(hca_ids);
    return btls;
}

static int mca_btl_mvapi_handle_incoming_hp(mca_btl_mvapi_module_t *,
        mca_btl_mvapi_endpoint_t *,
        mca_btl_mvapi_frag_t *, 
        size_t);
int mca_btl_mvapi_handle_incoming_hp(
        mca_btl_mvapi_module_t *mvapi_btl,
        mca_btl_mvapi_endpoint_t *endpoint,
        mca_btl_mvapi_frag_t *frag, 
        size_t byte_len)
{
    /* advance the segment address past the header and subtract from the length..*/ 
    frag->segment.seg_len =  byte_len-
        ((unsigned char*) frag->segment.seg_addr.pval -
         (unsigned char*) frag->hdr); 

    /* call registered callback */
    mvapi_btl->ib_reg[frag->hdr->tag].cbfunc(&mvapi_btl->super, frag->hdr->tag,
            &frag->base, mvapi_btl->ib_reg[frag->hdr->tag].cbdata);

    if (!MCA_BTL_MVAPI_RDMA_FRAG(frag)) {
        OMPI_FREE_LIST_RETURN(&(mvapi_btl->recv_free_eager),
                (ompi_free_list_item_t*) frag);
    } else {
        mca_btl_mvapi_frag_t *tf;
        OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
        MCA_BTL_MVAPI_RDMA_MAKE_REMOTE(frag->ftr);
        while (endpoint->eager_rdma_local.tail !=
                endpoint->eager_rdma_local.head) {
             tf = MCA_BTL_MVAPI_GET_LOCAL_RDMA_FRAG(endpoint,
                     endpoint->eager_rdma_local.tail);
             if (MCA_BTL_MVAPI_RDMA_FRAG_LOCAL (tf))
                 break;
             OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits, 1);
             MCA_BTL_MVAPI_RDMA_NEXT_INDEX(endpoint->eager_rdma_local.tail);
        }
        OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
    }

    if (!mca_btl_mvapi_component.use_srq) {
        OPAL_THREAD_ADD32(&endpoint->sd_tokens_hp, frag->hdr->credits);
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens,
                frag->hdr->rdma_credits);
    }
    
    if (mca_btl_mvapi_component.use_eager_rdma &&
            !endpoint->eager_rdma_local.base.pval &&
            mvapi_btl->eager_rdma_buffers_count <
            mca_btl_mvapi_component.max_eager_rdma &&
            OPAL_THREAD_ADD32(&endpoint->eager_recv_count, 1) ==
            mca_btl_mvapi_component.eager_rdma_threshold) 
                mca_btl_mvapi_endpoint_connect_eager_rdma(endpoint); 

    /* repost receive descriptors */
#ifdef VAPI_FEATURE_SRQ
    if(mca_btl_mvapi_component.use_srq) {
        OPAL_THREAD_ADD32((int32_t*) &mvapi_btl->srd_posted_hp, -1);
        MCA_BTL_MVAPI_POST_SRR_HIGH(mvapi_btl, 0);
    } else {
#endif
        if (!MCA_BTL_MVAPI_RDMA_FRAG(frag)) {
            OPAL_THREAD_ADD32((int32_t*) &endpoint->rd_posted_hp, -1);
            MCA_BTL_MVAPI_ENDPOINT_POST_RR_HIGH(endpoint, 0);
        }

        /* check to see if we need to progress any pending desciptors */
        if(endpoint->sd_tokens_hp > 0 ||
                endpoint->eager_rdma_remote.tokens > 0) {

            while(!opal_list_is_empty(&endpoint->pending_frags_hp) &&
                    endpoint->sd_wqe_hp > 0 &&
                    (endpoint->sd_tokens_hp > 0 ||
                     endpoint->eager_rdma_remote.tokens > 0)) {
                opal_list_item_t *frag_item;
                OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
                frag_item = opal_list_remove_first(&(endpoint->pending_frags_hp));
                OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
                if(NULL == (frag = (mca_btl_mvapi_frag_t *) frag_item))
                    break;
                if(OMPI_SUCCESS !=
                        mca_btl_mvapi_endpoint_send(frag->endpoint, frag)) {
                    BTL_ERROR(("error in posting pending send\n"));
                    break;
                }
            }
        }

       /* check to see if we need to return credits */
       if((endpoint->rd_credits_hp >= mca_btl_mvapi_component.rd_win ||
                   endpoint->eager_rdma_local.credits >=
                   mca_btl_mvapi_component.rd_win) &&
               OPAL_THREAD_ADD32(&endpoint->sd_credits_hp, 1) == 1) {
           mca_btl_mvapi_endpoint_send_credits_hp(endpoint);
        }
#ifdef VAPI_FEATURE_SRQ
    }
#endif

    return OMPI_SUCCESS;
}

/*
 *  IB component progress.
 */


int mca_btl_mvapi_component_progress( void )
{
    uint32_t i, j, c;
    int count = 0, hret;
    int32_t credits;
    
    mca_btl_mvapi_frag_t* frag;
    mca_btl_mvapi_endpoint_t* endpoint; 
    /* Poll for RDMA completions - if any succeed, we don't process the slower queues */
    for(i = 0; i < mca_btl_mvapi_component.ib_num_btls; i++) {
        mca_btl_mvapi_module_t* mvapi_btl = &mca_btl_mvapi_component.mvapi_btls[i];

        OPAL_THREAD_LOCK(&mvapi_btl->eager_rdma_lock);
        c = mvapi_btl->eager_rdma_buffers_count;
        OPAL_THREAD_UNLOCK(&mvapi_btl->eager_rdma_lock);

        for(j = 0; j < c; j++) {
            endpoint = 
                orte_pointer_array_get_item(mvapi_btl->eager_rdma_buffers, j);

            if(!endpoint) /* shouldn't happen */
                continue;

            OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
            frag = MCA_BTL_MVAPI_GET_LOCAL_RDMA_FRAG (endpoint,
                    endpoint->eager_rdma_local.head);
            
            if (MCA_BTL_MVAPI_RDMA_FRAG_LOCAL (frag)) {
                uint32_t size = MCA_BTL_MVAPI_RDMA_FRAG_GET_SIZE(frag->ftr);
#if OMPI_ENABLE_DEBUG
                if (frag->ftr->seq != endpoint->eager_rdma_local.seq)
                    BTL_ERROR(("Eager RDMA wrong SEQ: received %d expected %d",
                                frag->ftr->seq, 
                                endpoint->eager_rdma_local.seq));
                endpoint->eager_rdma_local.seq++;
#endif
                MCA_BTL_MVAPI_RDMA_NEXT_INDEX(endpoint->eager_rdma_local.head);

                OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
                frag->hdr = (mca_btl_mvapi_header_t*)(((char*)frag->ftr) - 
                        size + sizeof(mca_btl_mvapi_footer_t));
                frag->segment.seg_addr.pval = ((unsigned char* )frag->hdr) + 
                    sizeof(mca_btl_mvapi_header_t);
                
                hret = mca_btl_mvapi_handle_incoming_hp(mvapi_btl,
                        frag->endpoint, frag, 
                        size - sizeof(mca_btl_mvapi_footer_t));
                if (hret != MPI_SUCCESS)
                    return hret;
                count++;
            } else
                OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
        }
    }
    if (count) return count;

    for(i = 0; i < mca_btl_mvapi_component.ib_num_btls; i++) {
        VAPI_ret_t ret; 
        VAPI_wc_desc_t comp; 
        mca_btl_mvapi_module_t* mvapi_btl = &mca_btl_mvapi_component.mvapi_btls[i];
        /* we have two completion queues, one for "high" priority and one for "low". 
         *   we will check the high priority and process them until there are none left. 
         *   note that low priority messages are only processed one per progress call. 
         */ 
        ret = VAPI_poll_cq(mvapi_btl->nic, mvapi_btl->cq_hndl_hp, &comp); 
        if(VAPI_OK == ret) { 
            if(comp.status != VAPI_SUCCESS) { 
                ompi_proc_t* remote_proc = NULL;
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id;
                if(frag) { 
                    endpoint = (mca_btl_mvapi_endpoint_t*) frag->endpoint; 
                    if(endpoint && 
                       endpoint->endpoint_proc && 
                       endpoint->endpoint_proc->proc_ompi) { 
                        remote_proc = endpoint->endpoint_proc->proc_ompi; 
                    }
                }
                BTL_PEER_ERROR(remote_proc, ("error polling HP CQ with status %s status number %d for Frag : %p", 
                                             VAPI_wc_status_sym(comp.status), 
                                             comp.status, comp.id));  
                if(comp.status == VAPI_RETRY_EXC_ERR) { 
                    opal_show_help("help-mpi-btl-mvapi.txt", "btl_mvapi:retry-exceeded", true);
                }
                return OMPI_ERROR; 
            }
            
            /* Handle work completions */
            switch(comp.opcode) {
            case VAPI_CQE_RQ_RDMA_WITH_IMM: 
                BTL_ERROR(("Got an RDMA with Immediate data!, not supported!")); 
                return OMPI_ERROR; 
           
            case VAPI_CQE_SQ_RDMA_WRITE:
            case VAPI_CQE_SQ_SEND_DATA :

                /* Process a completed send */
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id; 
                endpoint = (mca_btl_mvapi_endpoint_t*) frag->endpoint; 

                /* Process a completed send */
                frag->base.des_cbfunc(&mvapi_btl->super, endpoint, &frag->base, OMPI_SUCCESS); 

                /* return send wqe */
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp, 1);

                /* check to see if we need to progress any pending descriptors */
                while (!opal_list_is_empty(&endpoint->pending_frags_hp) &&
                        endpoint->sd_wqe_hp > 0 && (endpoint->sd_tokens_hp > 0 || endpoint->eager_rdma_remote.tokens > 0)) {
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
                    frag_item = opal_list_remove_first(&(endpoint->pending_frags_hp));
                    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
                    if(NULL == (frag = (mca_btl_mvapi_frag_t *) frag_item))
                        break;
                    if(OMPI_SUCCESS !=  mca_btl_mvapi_endpoint_send(frag->endpoint, frag)) {
                        BTL_ERROR(("error in posting pending send\n"));
                        break;
                    }
                }

                if(!mca_btl_mvapi_component.use_srq) {
                    /* check to see if we need to return credits */
                    if((endpoint->rd_credits_hp >= mca_btl_mvapi_component.rd_win || endpoint->eager_rdma_local.credits >= mca_btl_mvapi_component.rd_win) &&
                        OPAL_THREAD_ADD32(&endpoint->sd_credits_hp, 1) == 1) {
                        mca_btl_mvapi_endpoint_send_credits_hp(endpoint);
                    }
                } else if(OPAL_THREAD_ADD32(&mvapi_btl->sd_tokens_hp, 1)  > 0
                          && !opal_list_is_empty(&mvapi_btl->pending_frags_hp)) {
                    /* dequeue resources due to global flow control */
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&mvapi_btl->ib_lock);
                    frag_item = opal_list_remove_first(&mvapi_btl->pending_frags_hp);
                    OPAL_THREAD_UNLOCK(&mvapi_btl->ib_lock);
                    if(NULL != (frag = (mca_btl_mvapi_frag_t *) frag_item) &&
                       OMPI_SUCCESS !=  mca_btl_mvapi_endpoint_send(frag->endpoint, frag)) {
                        BTL_ERROR(("error in posting pending send\n"));
                    }
                }
                count++;
                break;

            case VAPI_CQE_RQ_SEND_DATA:
                /* process a RECV */
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id;
                hret = mca_btl_mvapi_handle_incoming_hp(mvapi_btl,
                        frag->endpoint, frag, comp.byte_len);
                if (hret != OMPI_SUCCESS)
                    return hret;
                count++;
                break;

            case VAPI_CQE_SQ_RDMA_READ:
            default:
                BTL_ERROR(("Unhandled work completion opcode is %d", comp.opcode));
                break;
            }
        }
        
        ret = VAPI_poll_cq(mvapi_btl->nic, mvapi_btl->cq_hndl_lp, &comp); 
        if(VAPI_OK == ret) { 
            if(comp.status != VAPI_SUCCESS) { 
                BTL_ERROR(("Got error : %s, Vendor code : %d Frag : %p", 
                            VAPI_wc_status_sym(comp.status), 
                            comp.vendor_err_syndrome, comp.id));  
                return OMPI_ERROR; 
            }
            
            /* Handle n/w completions */
            switch(comp.opcode) {
            case VAPI_CQE_RQ_RDMA_WITH_IMM: 
                BTL_ERROR(("Got an RDMA with Immediate data!, not supported!")); 
                return OMPI_ERROR; 
            
            case VAPI_CQE_SQ_SEND_DATA :
                
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id; 
                endpoint = frag->endpoint;

                /* Process a completed send - receiver must return tokens */
                frag->base.des_cbfunc(&mvapi_btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);

                /* return send wqe */
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp, 1);

                /* check to see if we need to progress any pending desciptors */
                while (!opal_list_is_empty(&endpoint->pending_frags_lp) &&
                       endpoint->sd_wqe_lp > 0 && endpoint->sd_tokens_lp > 0) {
                   opal_list_item_t *frag_item;
                   OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
                   frag_item = opal_list_remove_first(&(endpoint->pending_frags_lp));
                   OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
                   if(NULL == (frag = (mca_btl_mvapi_frag_t *) frag_item))
                       break;
                    MCA_BTL_IB_FRAG_PROGRESS(frag);
                }

                if( !mca_btl_mvapi_component.use_srq) {
                    /* check to see if we need to return credits */
                    if( endpoint->rd_credits_lp >= mca_btl_mvapi_component.rd_win &&
                        OPAL_THREAD_ADD32(&endpoint->sd_credits_lp, 1) == 1) {
                        mca_btl_mvapi_endpoint_send_credits_lp(endpoint);
                    }

                /* SRQ case */
                } else if(OPAL_THREAD_ADD32(&mvapi_btl->sd_tokens_lp, 1)  > 0
                          && !opal_list_is_empty(&mvapi_btl->pending_frags_lp)) {
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&mvapi_btl->ib_lock);
                    frag_item = opal_list_remove_first(&mvapi_btl->pending_frags_lp);
                    OPAL_THREAD_UNLOCK(&mvapi_btl->ib_lock);
                    if(NULL != (frag = (mca_btl_mvapi_frag_t *) frag_item)) {
                        MCA_BTL_IB_FRAG_PROGRESS(frag);
                    }
                }
                count++;
                break;

            case VAPI_CQE_SQ_RDMA_READ:

                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id; 
                OPAL_THREAD_ADD32(&frag->endpoint->get_tokens, 1);
                /* fall through */

            case VAPI_CQE_SQ_RDMA_WRITE:

                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id; 
                endpoint = frag->endpoint;

                /* process a completed write */
                frag->base.des_cbfunc(&mvapi_btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);

                /* return send wqe */
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp, 1);

                /* check for pending frags */
                if(!opal_list_is_empty(&endpoint->pending_frags_lp)) {
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
                    frag_item = opal_list_remove_first(&endpoint->pending_frags_lp);
                    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
                    if(NULL != (frag = (mca_btl_mvapi_frag_t *) frag_item)) {
                        MCA_BTL_IB_FRAG_PROGRESS(frag);
                    }
                }
                if (mca_btl_mvapi_component.use_srq &&
                    endpoint->sd_wqe_lp > 0 &&
                   !opal_list_is_empty(&mvapi_btl->pending_frags_lp)) {
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&mvapi_btl->ib_lock);
                    frag_item = opal_list_remove_first(&mvapi_btl->pending_frags_lp);
                    OPAL_THREAD_UNLOCK(&mvapi_btl->ib_lock);
                    if(NULL != (frag = (mca_btl_mvapi_frag_t *) frag_item)) {
                        MCA_BTL_IB_FRAG_PROGRESS(frag);
                    }
                }
                count++;
                break;
                
            case VAPI_CQE_RQ_SEND_DATA:
                
                /* Process a RECV */
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id;
                endpoint = (mca_btl_mvapi_endpoint_t*) frag->endpoint;
                credits = frag->hdr->credits;
                                                                                                                         
                /* advance the segment address past the header and subtract from the length..*/
                frag->segment.seg_len =  comp.byte_len-
                    ((unsigned char*) frag->segment.seg_addr.pval  - (unsigned char*) frag->hdr);
                                                                                                                         
                /* call registered callback */
                mvapi_btl->ib_reg[frag->hdr->tag].cbfunc(&mvapi_btl->super,
                                                          frag->hdr->tag,
                                                          &frag->base,
                                                          mvapi_btl->ib_reg[frag->hdr->tag].cbdata);
                OMPI_FREE_LIST_RETURN(&(mvapi_btl->recv_free_max), (ompi_free_list_item_t*) frag);
                                                                                                                         
#ifdef VAPI_FEATURE_SRQ
                if(mca_btl_mvapi_component.use_srq) {
                    /* repost receive descriptors */
                    OPAL_THREAD_ADD32((int32_t*) &mvapi_btl->srd_posted_lp, -1);
                    MCA_BTL_MVAPI_POST_SRR_LOW(mvapi_btl, 0);
                } else {
#endif
                    /* repost receive descriptors */
                    OPAL_THREAD_ADD32((int32_t*) &endpoint->rd_posted_lp, -1);
                    MCA_BTL_MVAPI_ENDPOINT_POST_RR_LOW(endpoint, 0);
                                                                                                                         
                    /* check to see if we need to progress any pending desciptors */
                    if( OPAL_THREAD_ADD32(&endpoint->sd_tokens_lp, credits) > 0) {
                                                                                                                         
                        while(!opal_list_is_empty(&endpoint->pending_frags_lp) &&
                              endpoint->sd_wqe_lp > 0 && endpoint->sd_tokens_lp > 0) {
                            opal_list_item_t *frag_item;
                            OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
                            frag_item = opal_list_remove_first(&(endpoint->pending_frags_lp));
                            OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
                            if(NULL == (frag = (mca_btl_mvapi_frag_t *) frag_item))
                                break;
                            MCA_BTL_IB_FRAG_PROGRESS(frag);
                        }
                    }
                                                                                                                         
                    /* check to see if we need to return credits */
                    if( endpoint->rd_credits_lp >= mca_btl_mvapi_component.rd_win &&
                        OPAL_THREAD_ADD32(&endpoint->sd_credits_lp, 1) == 1) {
                        mca_btl_mvapi_endpoint_send_credits_lp(endpoint);
                    }
                                                                                                                         
#ifdef VAPI_FEATURE_SRQ
                }
#endif
                count++;
                break;

            default:
                BTL_ERROR(("Errorneous network completion"));
                break;
            }
        }
        
    }
    return count;
}
