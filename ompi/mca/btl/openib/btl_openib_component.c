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
#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "opal/sys/timer.h"

#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/base/base.h" 
#include "btl_openib.h"
#include "btl_openib_frag.h"
#include "btl_openib_endpoint.h" 
#include "btl_openib_eager_rdma.h"
#include "ompi/mca/btl/base/base.h"


#include "ompi/datatype/convertor.h" 
#include "ompi/mca/mpool/mpool.h" 
#include <sysfs/libsysfs.h>  
#include <infiniband/verbs.h> 
#include <errno.h> 
#include <string.h>   /* for strerror()*/ 

#include "ompi/mca/pml/base/pml_base_module_exchange.h"

mca_btl_openib_component_t mca_btl_openib_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_0,

            "openib", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_openib_component_open,  /* component open */
            mca_btl_openib_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_btl_openib_component_init,  
        mca_btl_openib_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline void mca_btl_openib_param_register_string(
                                                        const char* param_name, 
                                                        const char* param_desc,
                                                        const char* default_value, 
                                                        char** out_value)
{
    mca_base_param_reg_string(&mca_btl_openib_component.super.btl_version, 
                              param_name, 
                              param_desc, 
                              false, 
                              false, 
                              default_value, 
                              out_value);
}

static inline void  mca_btl_openib_param_register_int(
                                                      const char* param_name,
                                                      const char* param_desc,
                                                      int default_value, 
                                                      int* out_value)
{
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version, 
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

int mca_btl_openib_component_open(void)
{
    /* initialize state */
    mca_btl_openib_component.ib_num_btls=0;
    mca_btl_openib_component.openib_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_procs, opal_list_t);

    /* register IB component parameters */
    mca_btl_openib_param_register_int ("max_btls", "maximum number of HCAs/ports to use", 
                                      4, &mca_btl_openib_component.ib_max_btls);
    mca_btl_openib_param_register_int ("free_list_num", "intial size of free lists", 
                                       8, &mca_btl_openib_component.ib_free_list_num);
    mca_btl_openib_param_register_int ("free_list_max", "maximum size of free lists",
                                       -1, &mca_btl_openib_component.ib_free_list_max);
    mca_btl_openib_param_register_int ("free_list_inc", "increment size of free lists", 
                                       32, &mca_btl_openib_component.ib_free_list_inc);
    mca_btl_openib_param_register_string("mpool", "name of the memory pool to be used", 
                                         "openib", &mca_btl_openib_component.ib_mpool_name); 
    mca_btl_openib_param_register_int("reg_mru_len",  "length of the registration cache most recently used list", 
                                      16, (int*) &mca_btl_openib_component.reg_mru_len); 
    mca_btl_openib_param_register_int("use_srq", "if 1 use the IB shared receive queue to post receive descriptors", 
                                      0, (int*) &mca_btl_openib_component.use_srq); 
    mca_btl_openib_param_register_int("ib_cq_size", "size of the IB completion queue",
                                      1000, (int*) &mca_btl_openib_component.ib_cq_size); 
    mca_btl_openib_param_register_int("ib_sg_list_size", "size of IB segment list", 
                                      4, (int*) &mca_btl_openib_component.ib_sg_list_size); 
    mca_btl_openib_param_register_int("ib_pkey_ix", "IB pkey index", 
                                      0, (int*) &mca_btl_openib_component.ib_pkey_ix); 
    mca_btl_openib_param_register_int("ib_psn", "IB Packet sequence starting number", 
                                      0, (int*) &mca_btl_openib_component.ib_psn); 
    mca_btl_openib_param_register_int("ib_qp_ous_rd_atom", "IB outstanding atomic reads", 
                                      4, (int*) &mca_btl_openib_component.ib_qp_ous_rd_atom); 
    mca_btl_openib_param_register_int("ib_mtu", "IB MTU", 
                                      IBV_MTU_1024, (int*) &mca_btl_openib_component.ib_mtu); 
    mca_btl_openib_param_register_int("ib_min_rnr_timer", "IB min rnr timer", 
                                      5, (int*) &mca_btl_openib_component.ib_min_rnr_timer);
    mca_btl_openib_param_register_int("ib_timeout", "IB transmit timeout", 
                                      10, (int*) &mca_btl_openib_component.ib_timeout); 
    mca_btl_openib_param_register_int("ib_retry_count", "IB transmit retry count",
                                      7, (int*) &mca_btl_openib_component.ib_retry_count); 
    mca_btl_openib_param_register_int("ib_rnr_retry", "IB rnr retry", 
                                      7, (int*) &mca_btl_openib_component.ib_rnr_retry); 
    mca_btl_openib_param_register_int("ib_max_rdma_dst_ops", "IB max rdma destination operations", 
                                      4, (int*) &mca_btl_openib_component.ib_max_rdma_dst_ops); 
    mca_btl_openib_param_register_int("ib_service_level", "IB service level", 
                                      0, (int*) &mca_btl_openib_component.ib_service_level); 
    mca_btl_openib_param_register_int("ib_static_rate", "IB static rate", 
                                      0, (int*) &mca_btl_openib_component.ib_static_rate); 
    mca_btl_openib_param_register_int("ib_src_path_bits", "IB source path bits", 
                                      0, (int*) &mca_btl_openib_component.ib_src_path_bits); 
    mca_btl_openib_param_register_int ("exclusivity", "BTL exclusivity", 
                                       MCA_BTL_EXCLUSIVITY_DEFAULT, (int*) &mca_btl_openib_module.super.btl_exclusivity);
    mca_btl_openib_param_register_int("rd_num", "number of receive descriptors to post to a QP",
                                      8, (int*) &mca_btl_openib_component.rd_num);
    mca_btl_openib_param_register_int("rd_low", "low water mark before reposting occurs",
                                      6,  (int*) &mca_btl_openib_component.rd_low);
    mca_btl_openib_param_register_int("rd_win", "window size at which generate explicity credit message",
                                      4,  (int*) &mca_btl_openib_component.rd_win);
    mca_btl_openib_component.rd_rsv = ((mca_btl_openib_component.rd_num<<1)-1) / mca_btl_openib_component.rd_win;

    mca_btl_openib_param_register_int("srq_rd_max", "Max number of receive descriptors posted per SRQ.",
                                      1000, (int*) &mca_btl_openib_component.srq_rd_max);
    mca_btl_openib_param_register_int("srq_rd_per_peer", "Number of receive descriptors posted per peer. (SRQ)",
                                      16, (int*) &mca_btl_openib_component.srq_rd_per_peer);
    mca_btl_openib_param_register_int("srq_sd_max", "Maximum number of send descriptors posted. (SRQ)",
                                      8,  &mca_btl_openib_component.srq_sd_max);
    mca_btl_openib_param_register_int("use_eager_rdma", "user RDMA for eager messages", 
                                      0, (int*) &mca_btl_openib_component.use_eager_rdma);
    if (mca_btl_openib_component.use_srq)
        mca_btl_openib_component.use_eager_rdma = 0;
    mca_btl_openib_param_register_int("eager_rdma_threshold", "Open rdma channel for eager messages after this number of messages received from peer", 
                                      100, (int*) &mca_btl_openib_component.eager_rdma_threshold);
    mca_btl_openib_param_register_int("max_eager_rdma", "Maximum number of eager RDMA connections",
                                      16, (int*)&mca_btl_openib_component.max_eager_rdma);
    mca_btl_openib_param_register_int("eager_rdma_num", "Number of RDMA buffers for eager messages",
                                      16, (int*)&mca_btl_openib_component.eager_rdma_num);
    mca_btl_openib_component.eager_rdma_num+=1;
    mca_btl_openib_param_register_int ("eager_limit", "eager send limit", 
                                       (32*1024),(int*) &mca_btl_openib_module.super.btl_eager_limit);  
    mca_btl_openib_param_register_int ("min_send_size", "minimum send size", 
                                       (64*1024),(int*)  &mca_btl_openib_module.super.btl_min_send_size);
    mca_btl_openib_param_register_int ("max_send_size", "maximum send size", 
                                       (64*1024), (int*) &mca_btl_openib_module.super.btl_max_send_size); 
    mca_btl_openib_param_register_int("min_rdma_size", "minimum rdma size", 
                                      1024*1024, (int*) &mca_btl_openib_module.super.btl_min_rdma_size);
    mca_btl_openib_param_register_int("max_rdma_size", "maximium rdma size", 
                                      1024*1024, (int*) &mca_btl_openib_module.super.btl_max_rdma_size); 
    mca_btl_openib_param_register_int("flags", "BTL flags, SEND=0, PUT=1, GET=2", 
                                      MCA_BTL_FLAGS_PUT, (int*) &mca_btl_openib_module.super.btl_flags); 
    
    mca_btl_openib_param_register_int("bandwidth", "Approximate maximum bandwidth of interconnect", 
                                      800, (int*) &mca_btl_openib_module.super.btl_bandwidth); 
    
    mca_btl_openib_component.max_send_size = mca_btl_openib_module.super.btl_max_send_size; 
    mca_btl_openib_component.eager_limit = mca_btl_openib_module.super.btl_eager_limit; 
    
    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_openib_component_close(void)
{
    return OMPI_SUCCESS;
}


/*
 *  Register OPENIB  port information. The MCA framework
 *  will make this available to all peers.
 */

static int
mca_btl_openib_modex_send(void)
{
    int         rc;
    size_t      i;
    size_t      size;
    mca_btl_openib_port_info_t *ports = NULL;

    size = mca_btl_openib_component.ib_num_btls * sizeof (mca_btl_openib_port_info_t);
    if (size != 0) {
        ports = (mca_btl_openib_port_info_t *)malloc (size);
        if (NULL == ports) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for (i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
            mca_btl_openib_module_t *btl = &mca_btl_openib_component.openib_btls[i];
            ports[i] = btl->port_info;
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

static void mca_btl_openib_control(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_tag_t tag,
    mca_btl_base_descriptor_t* descriptor,
    void* cbdata)
{
    /* dont return credits used for control messages */
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*)descriptor;
    mca_btl_openib_endpoint_t* endpoint = frag->endpoint;
    mca_btl_openib_control_header_t *ctl_hdr = frag->segment.seg_addr.pval;
    mca_btl_openib_eager_rdma_header_t *rdma_hdr;

    if(frag->size == mca_btl_openib_component.eager_limit) {
	    /* if not sent via rdma */
        if(!MCA_BTL_OPENIB_RDMA_FRAG(frag) &&
                ctl_hdr->type == MCA_BTL_OPENIB_CONTROL_NOOP) {
             OPAL_THREAD_ADD32(&endpoint->rd_credits_hp, -1);
        }
    } else {
        OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, -1);
    }

    switch (ctl_hdr->type) {
    case MCA_BTL_OPENIB_CONTROL_NOOP:
       break;
    case MCA_BTL_OPENIB_CONTROL_RDMA:
       rdma_hdr = (mca_btl_openib_eager_rdma_header_t*)ctl_hdr;
       if (endpoint->eager_rdma_remote.base.pval) {
	       BTL_ERROR(("Got RDMA connect twise!"));
	       return;
       }
       endpoint->eager_rdma_remote.rkey =  rdma_hdr->rkey;
       endpoint->eager_rdma_remote.base.pval = rdma_hdr->rdma_start.pval;
       endpoint->eager_rdma_remote.tokens =
           mca_btl_openib_component.eager_rdma_num - 1;
       break;
    default:
       BTL_ERROR(("Unknown message type received by BTL"));
       break;
    }
}



/*
 *  IB component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */

mca_btl_base_module_t** mca_btl_openib_component_init(int *num_btl_modules, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    struct ibv_device **ib_devs; 
    uint32_t num_devs; 
    mca_btl_base_module_t** btls;
    uint32_t i,j, length;
    struct mca_mpool_base_resources_t mpool_resources; 
    opal_list_t btl_list; 
    mca_btl_openib_module_t * openib_btl; 
    mca_btl_base_selected_module_t* ib_selected; 
    opal_list_item_t* item; 
    struct dlist *dev_list; 
    struct ibv_device* ib_dev; 
    unsigned short seedv[3];
    

    /* initialization */
    *num_btl_modules = 0;
    num_devs = 0; 

    seedv[0] = orte_process_info.my_name->vpid;
    seedv[1] = opal_sys_timer_get_cycles();
    seedv[2] = opal_sys_timer_get_cycles();
    seed48(seedv);

#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST
    ib_devs = ibv_get_device_list(&num_devs);
#else 
    /* Determine the number of hca's available on the host */
    dev_list = ibv_get_devices(); 
    if (NULL == dev_list) {
        mca_btl_base_error_no_nics("OpenIB", "HCA");
        mca_btl_openib_component.ib_num_btls = 0;
        mca_btl_openib_modex_send();
        return NULL;
    }
    dlist_start(dev_list); 

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        num_devs++; 
#endif

    if(0 == num_devs) { 
        mca_btl_base_error_no_nics("OpenIB", "HCA");
        mca_btl_openib_modex_send();
        return NULL; 
    }
       
#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST == 0
    /* Allocate space for the ib devices */ 
    ib_devs = (struct ibv_device**) malloc(num_devs * sizeof(struct ibv_dev*));
        if(NULL == ib_devs) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    dlist_start(dev_list); 

    i = 0; 
    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        ib_devs[i++] =  ib_dev; 
#endif
            
    /** We must loop through all the hca id's, get there handles and 
        for each hca we query the number of ports on the hca and set up 
        a distinct btl module for each hca port */ 

    OBJ_CONSTRUCT(&btl_list, opal_list_t); 
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_lock, opal_mutex_t);


    for(i = 0; i < num_devs 
            && mca_btl_openib_component.ib_num_btls < mca_btl_openib_component.ib_max_btls; i++){  
        struct ibv_device_attr ib_dev_attr; 
        struct ibv_context* ib_dev_context;
 
        ib_dev = ib_devs[i]; 
        
        ib_dev_context = ibv_open_device(ib_dev); 
        if(!ib_dev_context) { 
            BTL_ERROR((" error obtaining device context for %s errno says %s\n", ibv_get_device_name(ib_dev), strerror(errno))); 
            return NULL; 
        } 
       
        if(ibv_query_device(ib_dev_context, &ib_dev_attr)){ 
            BTL_ERROR(("error obtaining device attributes for %s errno says %s\n", ibv_get_device_name(ib_dev), strerror(errno))); 
            return NULL; 
        } 
        
        
        /* Note ports are 1 based hence j = 1 */
        
        for(j = 1; j <= ib_dev_attr.phys_port_cnt; j++){ 
            struct ibv_port_attr* ib_port_attr; 
            ib_port_attr = (struct ibv_port_attr*) malloc(sizeof(struct ibv_port_attr)); 
            if(ibv_query_port(ib_dev_context, (uint8_t) j, ib_port_attr)){ 
                BTL_ERROR(("error getting port attributes for device %s port number %d errno says %s", 
                          ibv_get_device_name(ib_dev), j, strerror(errno))); 
                return NULL; 
            }

            if( IBV_PORT_ACTIVE == ib_port_attr->state ){ 
                
                openib_btl = (mca_btl_openib_module_t*) malloc(sizeof(mca_btl_openib_module_t)); 
                memcpy(openib_btl, &mca_btl_openib_module, sizeof(mca_btl_openib_module));
                 
                 ib_selected = OBJ_NEW(mca_btl_base_selected_module_t); 
                 ib_selected->btl_module = (mca_btl_base_module_t*) openib_btl; 
                 openib_btl->ib_dev = ib_dev; 
                 openib_btl->ib_dev_context = ib_dev_context; 
                 openib_btl->port_num = (uint8_t) j; 
                 openib_btl->ib_port_attr = ib_port_attr; 
                 openib_btl->port_info.subnet = ib_port_attr->sm_lid; /* store the sm_lid for multi-nic support */
                 openib_btl->ib_reg[MCA_BTL_TAG_BTL].cbfunc = mca_btl_openib_control;
                 openib_btl->ib_reg[MCA_BTL_TAG_BTL].cbdata = NULL;

                 opal_list_append(&btl_list, (opal_list_item_t*) ib_selected);
                 if(++mca_btl_openib_component.ib_num_btls >= mca_btl_openib_component.ib_max_btls)
                     break;
                 
            } 
            else{
                free(ib_port_attr); 
            } 
        }
 
    }
    
    
    /* Allocate space for btl modules */
    mca_btl_openib_component.openib_btls = (mca_btl_openib_module_t*) malloc(sizeof(mca_btl_openib_module_t) * 
                                                                             mca_btl_openib_component.ib_num_btls);
    
    if(NULL == mca_btl_openib_component.openib_btls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    btls = (struct mca_btl_base_module_t**) 
        malloc(mca_btl_openib_component.ib_num_btls * sizeof(struct mca_btl_openib_module_t*));
    if(NULL == btls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    
    
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++){
        item = opal_list_remove_first(&btl_list); 
        ib_selected = (mca_btl_base_selected_module_t*)item; 
        openib_btl = (mca_btl_openib_module_t*) ib_selected->btl_module; 
        memcpy(&(mca_btl_openib_component.openib_btls[i]), openib_btl , sizeof(mca_btl_openib_module_t)); 
        free(ib_selected); 
        free(openib_btl); 

        openib_btl = &mca_btl_openib_component.openib_btls[i];
        openib_btl->rd_num = mca_btl_openib_component.rd_num + mca_btl_openib_component.rd_rsv;
        openib_btl->rd_low = mca_btl_openib_component.rd_low;
        openib_btl->num_peers = 0; 
        openib_btl->sd_tokens_hp = openib_btl->sd_tokens_lp = mca_btl_openib_component.srq_sd_max;

        /* Initialize module state */

        OBJ_CONSTRUCT(&openib_btl->pending_frags_hp, opal_list_t);
        OBJ_CONSTRUCT(&openib_btl->pending_frags_lp, opal_list_t);
            
        OBJ_CONSTRUCT(&openib_btl->ib_lock, opal_mutex_t); 
        OBJ_CONSTRUCT(&openib_btl->send_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->send_free_max, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->send_free_frag, ompi_free_list_t);
        
        OBJ_CONSTRUCT(&openib_btl->recv_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->recv_free_max, ompi_free_list_t);
        
        if(mca_btl_openib_module_init(openib_btl) != OMPI_SUCCESS) {
#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST
	    ibv_free_device_list(ib_devs);
#else
	    free(ib_devs);
#endif
	    return NULL;
        }
	
        mpool_resources.ib_pd = openib_btl->ib_pd; 
                
        /* initialize the memory pool using the hca */ 
        openib_btl->super.btl_mpool = 
            mca_mpool_base_module_create(mca_btl_openib_component.ib_mpool_name,
                                         &openib_btl->super, 
                                         &mpool_resources); 
        
        if(NULL == openib_btl->super.btl_mpool) { 
            BTL_ERROR(("error creating vapi memory pool! aborting openib btl initialization")); 
            return NULL; 
        }

        /* Initialize pool of send fragments */ 
        length = sizeof(mca_btl_openib_frag_t) + 
            sizeof(mca_btl_openib_header_t) + 
            sizeof(mca_btl_openib_footer_t) + 
            openib_btl->super.btl_eager_limit+ 
            2*MCA_BTL_IB_FRAG_ALIGN; 
	
        openib_btl->eager_rdma_frag_size = 
            length & ~(2 * MCA_BTL_IB_FRAG_ALIGN - 1);
 
        ompi_free_list_init(&openib_btl->send_free_eager,
                            length, 
                            OBJ_CLASS(mca_btl_openib_send_frag_eager_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->super.btl_mpool);
        
        ompi_free_list_init(&openib_btl->recv_free_eager,
                            length, 
                            OBJ_CLASS(mca_btl_openib_recv_frag_eager_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->super.btl_mpool);

        length = sizeof(mca_btl_openib_frag_t) + 
            sizeof(mca_btl_openib_header_t) + 
            openib_btl->super.btl_max_send_size +
            2*MCA_BTL_IB_FRAG_ALIGN; 
        

        ompi_free_list_init(&openib_btl->send_free_max,
                            length, 
                            OBJ_CLASS(mca_btl_openib_send_frag_max_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->super.btl_mpool);
                
        /* Initialize pool of receive fragments */
        ompi_free_list_init (&openib_btl->recv_free_max, 
                             length, 
                             OBJ_CLASS (mca_btl_openib_recv_frag_max_t),
                             mca_btl_openib_component.ib_free_list_num,
                             mca_btl_openib_component.ib_free_list_max,
                             mca_btl_openib_component.ib_free_list_inc, openib_btl->super.btl_mpool);

        length = sizeof(mca_btl_openib_frag_t) + 
            sizeof(mca_btl_openib_header_t)+ 
            2*MCA_BTL_IB_FRAG_ALIGN; 

        ompi_free_list_init(&openib_btl->send_free_frag,
                            length, 
                            OBJ_CLASS(mca_btl_openib_send_frag_frag_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->super.btl_mpool);

        orte_pointer_array_init(&openib_btl->eager_rdma_buffers, 
                mca_btl_openib_component.max_eager_rdma,
                mca_btl_openib_component.max_eager_rdma, 
                0);
        openib_btl->eager_rdma_buffers_count = 0;
        OBJ_CONSTRUCT(&openib_btl->eager_rdma_lock, opal_mutex_t); 
        
        /* Initialize the rd_desc_post array for posting of rr*/ 
        openib_btl->rd_desc_post = (struct ibv_recv_wr *) 
            malloc(((mca_btl_openib_component.rd_num + mca_btl_openib_component.rd_rsv) * sizeof(struct ibv_recv_wr))); 
        
        btls[i] = &openib_btl->super;
    }

    /* Post OOB receive to support dynamic connection setup */
    mca_btl_openib_post_recv();
    mca_btl_openib_modex_send();

    *num_btl_modules = mca_btl_openib_component.ib_num_btls;
#if OMPI_MCA_BTL_OPENIB_HAVE_DEVICE_LIST
    ibv_free_device_list(ib_devs);
#else
    free(ib_devs);
#endif
    return btls;
}

static int mca_btl_openib_handle_incoming_hp(mca_btl_openib_module_t *,
        mca_btl_openib_endpoint_t *,
        mca_btl_openib_frag_t *, 
        size_t);
int mca_btl_openib_handle_incoming_hp(
        mca_btl_openib_module_t *openib_btl,
        mca_btl_openib_endpoint_t *endpoint,
        mca_btl_openib_frag_t *frag, 
        size_t byte_len)
{
    /* advance the segment address past the header and subtract from the length..*/ 
    frag->segment.seg_len = byte_len -
        ((unsigned char*)frag->segment.seg_addr.pval -
         (unsigned char*) frag->hdr); 

    /* call registered callback */
    openib_btl->ib_reg[frag->hdr->tag].cbfunc(&openib_btl->super,
            frag->hdr->tag, &frag->base, 
            openib_btl->ib_reg[frag->hdr->tag].cbdata);

    if (!MCA_BTL_OPENIB_RDMA_FRAG(frag)) {
        OMPI_FREE_LIST_RETURN(&(openib_btl->recv_free_eager),
                (opal_list_item_t*) frag); 
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
   
    if (!mca_btl_openib_component.use_srq) {
        OPAL_THREAD_ADD32(&endpoint->sd_tokens_hp, frag->hdr->credits);
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens,
                 frag->hdr->rdma_credits);
    }

    if (mca_btl_openib_component.use_eager_rdma &&
            !endpoint->eager_rdma_local.base.pval &&
            openib_btl->eager_rdma_buffers_count <
            mca_btl_openib_component.max_eager_rdma &&
            OPAL_THREAD_ADD32(&endpoint->eager_recv_count, 1) ==
            mca_btl_openib_component.eager_rdma_threshold) 
                mca_btl_openib_endpoint_connect_eager_rdma(endpoint); 

    /* repost receive descriptors */
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    if(mca_btl_openib_component.use_srq) { 
        OPAL_THREAD_ADD32((int32_t*) &openib_btl->srd_posted_hp, -1); 
        MCA_BTL_OPENIB_POST_SRR_HIGH(openib_btl, 0); 
    } else { 
#endif
        if (!MCA_BTL_OPENIB_RDMA_FRAG(frag)) {
            OPAL_THREAD_ADD32((int32_t*) &endpoint->rd_posted_hp, -1); 
            MCA_BTL_OPENIB_ENDPOINT_POST_RR_HIGH(endpoint, 0);
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
                if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
                    break;
                if(OMPI_SUCCESS != 
                        mca_btl_openib_endpoint_send(frag->endpoint, frag)) {
                    BTL_ERROR(("error in posting pending send\n"));
                    break;
                }
            }
        }

        /* check to see if we need to return credits */
        if((endpoint->rd_credits_hp >= mca_btl_openib_component.rd_win ||
                    endpoint->eager_rdma_local.credits >=
                    mca_btl_openib_component.rd_win) &&
                OPAL_THREAD_ADD32(&endpoint->sd_credits_hp, 1) == 1) {
            mca_btl_openib_endpoint_send_credits_hp(endpoint);
        }

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    }
#endif 

    return OMPI_SUCCESS;
}

/*
 *  IB component progress.
 */

int mca_btl_openib_component_progress()
{
    uint32_t i, j, c;
    int count = 0,ne = 0, ret;
    int32_t credits;
    mca_btl_openib_frag_t* frag; 
    mca_btl_openib_endpoint_t* endpoint; 

    /* Poll for completions */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        
        struct ibv_wc wc; 
        mca_btl_openib_module_t* openib_btl = &mca_btl_openib_component.openib_btls[i];
        
        /* we have two completion queues, one for "high" priority and one for "low". 
         *   we will check the high priority and process them until there are none left. 
         *   note that low priority messages are only processed one per progress call. 
         */

        OPAL_THREAD_LOCK(&openib_btl->eager_rdma_lock);
        c = openib_btl->eager_rdma_buffers_count;
        OPAL_THREAD_UNLOCK(&openib_btl->eager_rdma_lock);

        for(j = 0; j < c; j++) {
            endpoint = 
                orte_pointer_array_get_item(openib_btl->eager_rdma_buffers, j);

            if(!endpoint) /* shouldn't happen */
                continue;

            OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
            frag = MCA_BTL_OPENIB_GET_LOCAL_RDMA_FRAG (endpoint,
                    endpoint->eager_rdma_local.head);

            if (MCA_BTL_OPENIB_RDMA_FRAG_LOCAL (frag)) {
                uint32_t size = MCA_BTL_OPENIB_RDMA_FRAG_GET_SIZE(frag->ftr);
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
                
                ret = mca_btl_openib_handle_incoming_hp(openib_btl,
                        frag->endpoint, frag, 
                        size - sizeof(mca_btl_openib_footer_t));
                if (ret != MPI_SUCCESS)
                    return ret;
                count++;
            } else
                OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
        }

        if(count)
           break;

        ne=ibv_poll_cq(openib_btl->ib_cq_hp, 1, &wc );
        if(ne < 0 ){ 
            BTL_ERROR(("error polling HP CQ with %d errno says %s\n", ne, strerror(errno))); 
            return OMPI_ERROR;
        } 
        else if(1 == ne) { 
            if(wc.status != IBV_WC_SUCCESS) { 
                BTL_ERROR(("error polling HP CQ with status %d for wr_id %llu opcode %d\n", 
                           wc.status, wc.wr_id, wc.opcode)); 
                return OMPI_ERROR;
            }

            /* Handle work completions */
            switch(wc.opcode) {
            case IBV_WC_RECV_RDMA_WITH_IMM: 
                BTL_ERROR(("Got an RDMA with Immediate data Not supported!")); 
                return OMPI_ERROR; 
                
            case IBV_WC_RDMA_WRITE:
            case IBV_WC_SEND :

                frag = (mca_btl_openib_frag_t*) (unsigned long) wc.wr_id; 
                endpoint = frag->endpoint;

                /* Process a completed send */
                frag->base.des_cbfunc(&openib_btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS); 

                /* return send wqe */
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_hp, 1);

                /* check to see if we need to progress any pending desciptors */
                while (!opal_list_is_empty(&endpoint->pending_frags_hp) &&
                        endpoint->sd_wqe_hp > 0 && (endpoint->sd_tokens_hp > 0 || endpoint->eager_rdma_remote.tokens > 0)) {
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
                    frag_item = opal_list_remove_first(&(endpoint->pending_frags_hp));
                    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
                    if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
                        break;
                    if(OMPI_SUCCESS !=  mca_btl_openib_endpoint_send(frag->endpoint, frag)) {
                        BTL_ERROR(("error in posting pending send\n"));
                        break;
                    }
                }

                if(!mca_btl_openib_component.use_srq) {

                    /* check to see if we need to return credits */
                    if((endpoint->rd_credits_hp >= mca_btl_openib_component.rd_win || endpoint->eager_rdma_local.credits >= mca_btl_openib_component.rd_win) &&
                        OPAL_THREAD_ADD32(&endpoint->sd_credits_hp, 1) == 1) {
                        mca_btl_openib_endpoint_send_credits_hp(endpoint);
                    }

                } else if(OPAL_THREAD_ADD32(&openib_btl->sd_tokens_hp, 1)  > 0 
                          && !opal_list_is_empty(&openib_btl->pending_frags_hp)) {

                    /* dequeue resources due to global flow control */
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&openib_btl->ib_lock);
                    frag_item = opal_list_remove_first(&openib_btl->pending_frags_hp);
                    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
                    if(NULL != (frag = (mca_btl_openib_frag_t *) frag_item) &&
                       OMPI_SUCCESS !=  mca_btl_openib_endpoint_send(frag->endpoint, frag)) { 
                        BTL_ERROR(("error in posting pending send\n"));
                    }
                } 

                count++;
                break;

            case IBV_WC_RECV: 
                /* Process a RECV */ 
                frag = (mca_btl_openib_frag_t*) (unsigned long) wc.wr_id;
                ret = mca_btl_openib_handle_incoming_hp(openib_btl,
                        frag->endpoint, frag, wc.byte_len);
                if (ret != OMPI_SUCCESS)
                    return ret;
                count++; 
                break; 

            case IBV_WC_RDMA_READ:
            default:
                BTL_ERROR(("Unhandled work completion opcode is %d", wc.opcode));
                break;
            }
        }
        
        ne=ibv_poll_cq(openib_btl->ib_cq_lp, 1, &wc );
        if(ne < 0){ 
            BTL_ERROR(("error polling LP CQ with %d errno says %s", ne, strerror(errno))); 
            return OMPI_ERROR;
        } 
        else if(1 == ne) {             
            if(wc.status != IBV_WC_SUCCESS) { 
                BTL_ERROR(("error polling LP CQ with status %d for wr_id %llu opcode %d", 
                          wc.status, wc.wr_id, wc.opcode)); 
                return OMPI_ERROR;
            }

            /* Handle n/w completions */
            switch(wc.opcode) {
            case IBV_WC_RECV_RDMA_WITH_IMM: 

                BTL_ERROR(("Got an RDMA with Immediate data Not supported!")); 
                return OMPI_ERROR; 
                
            case IBV_WC_SEND:

                frag = (mca_btl_openib_frag_t*) (unsigned long) wc.wr_id;
                endpoint = frag->endpoint;

                /* Process a completed send - receiver must return tokens */
                frag->base.des_cbfunc(&openib_btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);

                /* return send wqe */
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp, 1);

                /* check to see if we need to progress any pending desciptors */
                while (!opal_list_is_empty(&endpoint->pending_frags_lp) &&
                       endpoint->sd_wqe_lp > 0 && endpoint->sd_tokens_lp > 0) {
                   opal_list_item_t *frag_item;
                   OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
                   frag_item = opal_list_remove_first(&(endpoint->pending_frags_lp));
                   OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
                   if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
                       break;
                    MCA_BTL_IB_FRAG_PROGRESS(frag);
                }

                if( !mca_btl_openib_component.use_srq) {
                    /* check to see if we need to return credits */
                    if( endpoint->rd_credits_lp >= mca_btl_openib_component.rd_win &&
                        OPAL_THREAD_ADD32(&endpoint->sd_credits_lp, 1) == 1) {
                        mca_btl_openib_endpoint_send_credits_lp(endpoint);
                    }

                /* SRQ case */
                } else if(OPAL_THREAD_ADD32(&openib_btl->sd_tokens_lp, 1)  > 0 
                          && !opal_list_is_empty(&openib_btl->pending_frags_lp)) {
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&openib_btl->ib_lock);
                    frag_item = opal_list_remove_first(&openib_btl->pending_frags_lp);
                    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
                    if(NULL != (frag = (mca_btl_openib_frag_t *) frag_item)) {
                        MCA_BTL_IB_FRAG_PROGRESS(frag);
                    }
                } 
                count++;
                break;

            case IBV_WC_RDMA_READ: 
                                    
                frag = (mca_btl_openib_frag_t*) (unsigned long) wc.wr_id;
                OPAL_THREAD_ADD32(&frag->endpoint->get_tokens, 1);
                /* fall through */

            case IBV_WC_RDMA_WRITE: 

                frag = (mca_btl_openib_frag_t*) (unsigned long) wc.wr_id;
                endpoint = frag->endpoint;

                /* process a completed write */
                frag->base.des_cbfunc(&openib_btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);

                /* return send wqe */
                OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp, 1);

                /* check for pending frags */
                if(!opal_list_is_empty(&endpoint->pending_frags_lp)) {
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
                    frag_item = opal_list_remove_first(&endpoint->pending_frags_lp);
                    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
                    if(NULL != (frag = (mca_btl_openib_frag_t *) frag_item)) {
                        MCA_BTL_IB_FRAG_PROGRESS(frag);
                    }
                }
                if (mca_btl_openib_component.use_srq &&
                    endpoint->sd_wqe_lp > 0 &&
                   !opal_list_is_empty(&openib_btl->pending_frags_lp)) {
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&openib_btl->ib_lock);
                    frag_item = opal_list_remove_first(&openib_btl->pending_frags_lp);
                    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
                    if(NULL != (frag = (mca_btl_openib_frag_t *) frag_item)) {
                        MCA_BTL_IB_FRAG_PROGRESS(frag);
                    }
                }
                count++;
                break;
                
            case IBV_WC_RECV: 

                /* Process a RECV */ 
                frag = (mca_btl_openib_frag_t*) (unsigned long) wc.wr_id;
                endpoint = (mca_btl_openib_endpoint_t*) frag->endpoint; 
                credits = frag->hdr->credits;

                /* advance the segment address past the header and subtract from the length..*/ 
                frag->segment.seg_len =  wc.byte_len-
                    ((unsigned char*) frag->segment.seg_addr.pval  - (unsigned char*) frag->hdr); 

                /* call registered callback */
                openib_btl->ib_reg[frag->hdr->tag].cbfunc(&openib_btl->super, 
                                                          frag->hdr->tag, 
                                                          &frag->base, 
                                                          openib_btl->ib_reg[frag->hdr->tag].cbdata);         
                OMPI_FREE_LIST_RETURN(&(openib_btl->recv_free_max), (opal_list_item_t*) frag); 

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                if(mca_btl_openib_component.use_srq) { 
                    /* repost receive descriptors */
                    OPAL_THREAD_ADD32((int32_t*) &openib_btl->srd_posted_lp, -1); 
                    MCA_BTL_OPENIB_POST_SRR_LOW(openib_btl, 0); 
                } else { 
#endif
                    /* repost receive descriptors */
                    OPAL_THREAD_ADD32((int32_t*) &endpoint->rd_posted_lp, -1); 
                    MCA_BTL_OPENIB_ENDPOINT_POST_RR_LOW(endpoint, 0); 

                    /* check to see if we need to progress any pending desciptors */
                    if( OPAL_THREAD_ADD32(&endpoint->sd_tokens_lp, credits) > 0) {

                        while(!opal_list_is_empty(&endpoint->pending_frags_lp) &&
                              endpoint->sd_wqe_lp > 0 && endpoint->sd_tokens_lp > 0) {
                            opal_list_item_t *frag_item;
                            OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
                            frag_item = opal_list_remove_first(&(endpoint->pending_frags_lp));
                            OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
                            if(NULL == (frag = (mca_btl_openib_frag_t *) frag_item))
                                break;
                            MCA_BTL_IB_FRAG_PROGRESS(frag);
                        }
                    }

                    /* check to see if we need to return credits */
                    if( endpoint->rd_credits_lp >= mca_btl_openib_component.rd_win &&
                        OPAL_THREAD_ADD32(&endpoint->sd_credits_lp, 1) == 1) {
                        mca_btl_openib_endpoint_send_credits_lp(endpoint);
                    }

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                }
#endif 
                count++; 
                break; 

            default:
                BTL_ERROR(("Unhandled work completion opcode is %d", wc.opcode));
                break;
            }
        }
        
    }
    return count;
}
