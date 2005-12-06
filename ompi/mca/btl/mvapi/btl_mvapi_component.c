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

#include "ompi/include/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"

#include "mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "mca/mpool/base/base.h" 
#include "btl_mvapi.h"
#include "btl_mvapi_frag.h"
#include "btl_mvapi_endpoint.h" 
#include "mca/btl/base/base.h" 
#include <vapi.h> 
#include <vapi_common.h> 
#include "datatype/convertor.h" 
#include "mca/mpool/mvapi/mpool_mvapi.h" 
#include "btl_mvapi_endpoint.h"
#include "mca/pml/base/pml_base_module_exchange.h"


mca_btl_mvapi_component_t mca_btl_mvapi_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        
        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_0,

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
    mca_btl_mvapi_param_register_int ("free_list_num", "intial size of free lists", 
                                       8, &mca_btl_mvapi_component.ib_free_list_num);
    mca_btl_mvapi_param_register_int ("free_list_max", "maximum size of free lists",
                                       -1, &mca_btl_mvapi_component.ib_free_list_max);
    mca_btl_mvapi_param_register_int ("free_list_inc", "increment size of free lists", 
                                       32, &mca_btl_mvapi_component.ib_free_list_inc);
    mca_btl_mvapi_param_register_string("mpool", "name of the memory pool to be used", 
                                         "mvapi", &mca_btl_mvapi_component.ib_mpool_name); 
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
    mca_btl_mvapi_param_register_int("ib_mtu", "IB MTU", 
                                     MTU1024, (int*) &mca_btl_mvapi_component.ib_mtu); 
       
    mca_btl_mvapi_param_register_int("ib_min_rnr_timer", "IB min rnr timer", 
                                      5, (int*) &mca_btl_mvapi_component.ib_min_rnr_timer);
    mca_btl_mvapi_param_register_int("ib_timeout", "IB transmit timeout", 
                                      10, (int*) &mca_btl_mvapi_component.ib_timeout); 
    mca_btl_mvapi_param_register_int("ib_retry_count", "IB transmit retry count",
                                      7, (int*) &mca_btl_mvapi_component.ib_retry_count); 
    mca_btl_mvapi_param_register_int("ib_rnr_retry", "IB rnr retry", 
                                     7, (int*) &mca_btl_mvapi_component.ib_rnr_retry); 
    mca_btl_mvapi_param_register_int("ib_max_rdma_dst_ops", "IB max rdma destination operations", 
                                      16, (int*) &mca_btl_mvapi_component.ib_max_rdma_dst_ops); 
    mca_btl_mvapi_param_register_int("ib_service_level", "IB service level", 
                                      0, (int*) &mca_btl_mvapi_component.ib_service_level); 
    mca_btl_mvapi_param_register_int("ib_static_rate", "IB static rate", 
                                      0, (int*) &mca_btl_mvapi_component.ib_static_rate); 
    mca_btl_mvapi_param_register_int("ib_src_path_bits", "IB source path bits", 
                                      0, (int*) &mca_btl_mvapi_component.ib_src_path_bits); 
        
    mca_btl_mvapi_param_register_int("rd_num", "number of receive descriptors to post to a QP", 
                                      16, (int*) &mca_btl_mvapi_component.rd_num);  
    mca_btl_mvapi_param_register_int("rd_low", "low water mark before reposting occurs", 
                                      12,  (int*) &mca_btl_mvapi_component.rd_low); 
    mca_btl_mvapi_param_register_int("rd_win", "window size at which generate explicity credit message", 
                                      8,  (int*) &mca_btl_mvapi_component.rd_win); 
    mca_btl_mvapi_component.rd_rsv = ((mca_btl_mvapi_component.rd_num<<1)-1) / mca_btl_mvapi_component.rd_win;

    mca_btl_mvapi_param_register_int("srq_rd_max", "Maximum number of receive descriptors posted per SRQ.\n",
                                      1000, (int*) &mca_btl_mvapi_component.srq_rd_max); 
    mca_btl_mvapi_param_register_int("srq_rd_per_peer", "receive descriptors posted per peer, SRQ mode only", 
                                      16, (int*) &mca_btl_mvapi_component.srq_rd_per_peer); 
    mca_btl_mvapi_param_register_int("srq_sd_max", "Maximum number of send descriptors posted per process", 
                                      8,  &mca_btl_mvapi_component.srq_sd_max); 

    mca_btl_mvapi_param_register_int ("exclusivity", "BTL exclusivity", 
                                      MCA_BTL_EXCLUSIVITY_DEFAULT, (int*) &mca_btl_mvapi_module.super.btl_exclusivity);
    mca_btl_mvapi_param_register_int ("eager_limit", "eager send limit", 
                                      (32*1024),(int*) &mca_btl_mvapi_module.super.btl_eager_limit);  
    mca_btl_mvapi_module.super.btl_eager_limit -= sizeof(mca_btl_mvapi_header_t);
    mca_btl_mvapi_param_register_int ("min_send_size", "minimum send size", 
                                      (32*1024),(int*)  &mca_btl_mvapi_module.super.btl_min_send_size);
    mca_btl_mvapi_module.super.btl_min_send_size -= sizeof(mca_btl_mvapi_header_t);
    mca_btl_mvapi_param_register_int ("max_send_size", "maximum send size", 
                                      (64*1024), (int*) &mca_btl_mvapi_module.super.btl_max_send_size); 
    mca_btl_mvapi_module.super.btl_max_send_size -= sizeof(mca_btl_mvapi_header_t);
    mca_btl_mvapi_param_register_int("min_rdma_size", "minimum rdma size", 
                                     1024*1024, (int*) &mca_btl_mvapi_module.super.btl_min_rdma_size);
    mca_btl_mvapi_param_register_int("max_rdma_size", "maximium rdma size", 
                                     1024*1024, (int*) &mca_btl_mvapi_module.super.btl_max_rdma_size); 
    mca_btl_mvapi_param_register_int("flags", "BTL flags, SEND=0, PUT=1, GET=2", 
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
    if(frag->size == mca_btl_mvapi_component.eager_limit) {
        OPAL_THREAD_ADD32(&endpoint->rd_credits_hp, -1);
    } else {
        OPAL_THREAD_ADD32(&endpoint->rd_credits_lp, -1);
    }
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

#if 0
    /* ugly HACK!! */ 
    mallopt(M_TRIM_THRESHOLD, -1); 
    mallopt(M_MMAP_MAX, 0);
#endif

    /* initialization */
    *num_btl_modules = 0;

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
                         
        hca_pd.hca = mvapi_btl->nic; 
        hca_pd.pd_tag = mvapi_btl->ptag; 
        
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
            mvapi_btl->super.btl_eager_limit+ 
            2*MCA_BTL_IB_FRAG_ALIGN; 
        
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
        
        
        /* Initialize the rr_desc_post array for posting of rr*/ 
        mvapi_btl->rr_desc_post = (VAPI_rr_desc_t*) malloc((mvapi_btl->rd_num * sizeof(VAPI_rr_desc_t))); 
        btls[i] = &mvapi_btl->super;
    
    }

    /* Post OOB receive to support dynamic connection setup */
    mca_btl_mvapi_post_recv();
    
    mca_btl_mvapi_modex_send(); 
    *num_btl_modules = mca_btl_mvapi_component.ib_num_btls;
    free(hca_ids);
    return btls;
}

/*
 *  IB component progress.
 */


int mca_btl_mvapi_component_progress( void )
{
    uint32_t i;
    int count = 0;
    int32_t credits;

    mca_btl_mvapi_frag_t* frag;
    mca_btl_mvapi_endpoint_t* endpoint; 
    /* Poll for completions */
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
                BTL_ERROR(("Got error : %s, Vendor code : %d Frag : %p", 
                          VAPI_wc_status_sym(comp.status), 
                          comp.vendor_err_syndrome, comp.id));  
                return OMPI_ERROR; 
            }
            
            /* Handle work completions */
            switch(comp.opcode) {
            case VAPI_CQE_RQ_RDMA_WITH_IMM: 
                BTL_ERROR(("Got an RDMA with Immediate data!, not supported!")); 
                return OMPI_ERROR; 
           
            case VAPI_CQE_SQ_SEND_DATA :

                /* Process a completed send */
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id; 
                endpoint = (mca_btl_mvapi_endpoint_t*) frag->endpoint; 
                frag->base.des_cbfunc(&mvapi_btl->super, endpoint, &frag->base, OMPI_SUCCESS); 

                /* check and see if we need to progress pending sends */ 
                if( mca_btl_mvapi_component.use_srq && 
                    OPAL_THREAD_ADD32(&mvapi_btl->sd_tokens_hp, 1)  > 0 
                    && !opal_list_is_empty(&mvapi_btl->pending_frags_hp)) {
                    opal_list_item_t *frag_item;
                    frag_item = opal_list_remove_first(&mvapi_btl->pending_frags_hp);
                    frag = (mca_btl_mvapi_frag_t *) frag_item;
                    if(OMPI_SUCCESS !=  mca_btl_mvapi_endpoint_send(endpoint, frag)) { 
                        BTL_ERROR(("error in posting pending send\n"));
                    }
                } 
                count++;
                break;
                
            case VAPI_CQE_RQ_SEND_DATA:
                
                /* process a RECV */ 
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id;
                endpoint = (mca_btl_mvapi_endpoint_t*) frag->endpoint; 
                credits = frag->hdr->credits;

                /* advance the segment address past the header and subtract from the length..*/ 
                frag->segment.seg_len =  comp.byte_len-((unsigned char*) frag->segment.seg_addr.pval  - (unsigned char*) frag->hdr); 
                /* call registered callback */
                mvapi_btl->ib_reg[frag->hdr->tag].cbfunc(&mvapi_btl->super, frag->hdr->tag, &frag->base, mvapi_btl->ib_reg[frag->hdr->tag].cbdata);         
                OMPI_FREE_LIST_RETURN(&(mvapi_btl->recv_free_eager), (opal_list_item_t*) frag); 
                
                /* repost receive descriptors */
#ifdef VAPI_FEATURE_SRQ
                if(mca_btl_mvapi_component.use_srq) { 
                    OPAL_THREAD_ADD32(&mvapi_btl->srd_posted_hp, -1); 
                    MCA_BTL_MVAPI_POST_SRR_HIGH(mvapi_btl, 0); 
                } else 
#endif
                { 
                    OPAL_THREAD_ADD32(&endpoint->rd_posted_hp, -1); 
                    MCA_BTL_MVAPI_ENDPOINT_POST_RR_HIGH(endpoint, 0); 
                }

                /* check to see if we need to progress any pending desciptors */
                if( !mca_btl_mvapi_component.use_srq && 
                    OPAL_THREAD_ADD32(&endpoint->sd_tokens_hp, credits) > 0
                    && !opal_list_is_empty(&(endpoint->pending_frags_hp))) { 
              
                    do {
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
                    } while(endpoint->sd_tokens_hp > 0);
                }

                /* check to see if we need to return credits */
                if( !mca_btl_mvapi_component.use_srq &&
                    endpoint->rd_credits_hp >= mca_btl_mvapi_component.rd_win) {
                    mca_btl_mvapi_endpoint_send_credits(
                        endpoint, 
                        endpoint->lcl_qp_hndl_hp, 
                        endpoint->rem_info.rem_qp_num_hp, 
                        &endpoint->rd_credits_hp);
                }
                count++; 
                break;
                
            case VAPI_CQE_SQ_RDMA_READ:
            case VAPI_CQE_SQ_RDMA_WRITE:
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
                
                /* Process a completed send - receiver must return tokens */
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id; 
                frag->base.des_cbfunc(&mvapi_btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);
                count++;

                /* if we have tokens, process pending sends */
                if(mca_btl_mvapi_component.use_srq && 
                   OPAL_THREAD_ADD32(&mvapi_btl->sd_tokens_lp, 1)  > 0 
                   && !opal_list_is_empty(&mvapi_btl->pending_frags_lp)) {
                    opal_list_item_t *frag_item;
                    frag_item = opal_list_remove_first(&mvapi_btl->pending_frags_lp);
                    frag = (mca_btl_mvapi_frag_t *) frag_item;
                    MCA_BTL_IB_FRAG_PROGRESS(frag);
                }
                break;

            case VAPI_CQE_SQ_RDMA_READ:

                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id; 
                OPAL_THREAD_ADD32(&frag->endpoint->get_tokens, 1);
                /* fall through */

            case VAPI_CQE_SQ_RDMA_WRITE:

                /* Process a completed write - returns tokens immediately */
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id; 
                endpoint = frag->endpoint;
                frag->base.des_cbfunc(&mvapi_btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS); 

                if(mca_btl_mvapi_component.use_srq && 
                   OPAL_THREAD_ADD32(&mvapi_btl->sd_tokens_lp, 1)  > 0 
                   && !opal_list_is_empty(&mvapi_btl->pending_frags_lp)) {
                    opal_list_item_t *frag_item;
                    frag_item = opal_list_remove_first(&mvapi_btl->pending_frags_lp);
                    frag = (mca_btl_mvapi_frag_t *) frag_item;
                    MCA_BTL_IB_FRAG_PROGRESS(frag);
                }
                if(!mca_btl_mvapi_component.use_srq && 
                   OPAL_THREAD_ADD32(&endpoint->sd_tokens_lp, 1) > 0 && 
                   !opal_list_is_empty(&(endpoint->pending_frags_lp))) { 
                    opal_list_item_t *frag_item;
                    OPAL_THREAD_LOCK(&frag->endpoint->endpoint_lock); 
                    frag_item = opal_list_remove_first(&(frag->endpoint->pending_frags_lp));
                    OPAL_THREAD_UNLOCK(&frag->endpoint->endpoint_lock); 
                    frag = (mca_btl_mvapi_frag_t *) frag_item;
                    MCA_BTL_IB_FRAG_PROGRESS(frag);
                }
                count++;
                break;
                
            case VAPI_CQE_RQ_SEND_DATA:
                
                frag = (mca_btl_mvapi_frag_t*) (unsigned long) comp.id;
                endpoint = (mca_btl_mvapi_endpoint_t*) frag->endpoint; 
                credits = frag->hdr->credits;

                /* process received frag */
                frag->rc=OMPI_SUCCESS; 
                frag->segment.seg_len =  comp.byte_len-((unsigned char*) frag->segment.seg_addr.pval  - (unsigned char*) frag->hdr); 
                /* call registered callback */
                mvapi_btl->ib_reg[frag->hdr->tag].cbfunc(&mvapi_btl->super, frag->hdr->tag, &frag->base, mvapi_btl->ib_reg[frag->hdr->tag].cbdata);         
                OMPI_FREE_LIST_RETURN(&(mvapi_btl->recv_free_max), (opal_list_item_t*) frag); 

                /* post descriptors */
#ifdef VAPI_FEATURE_SRQ
                if(mca_btl_mvapi_component.use_srq) { 
                    OPAL_THREAD_ADD32(&mvapi_btl->srd_posted_lp, -1); 
                    MCA_BTL_MVAPI_POST_SRR_LOW(mvapi_btl, 0); 
                } else 
#endif
                {
                    OPAL_THREAD_ADD32(&endpoint->rd_posted_lp, -1); 
                    MCA_BTL_MVAPI_ENDPOINT_POST_RR_LOW(endpoint, 0); 
                }

                /* check to see if we need to progress pending descriptors */ 
                if(!mca_btl_mvapi_component.use_srq && 
                   OPAL_THREAD_ADD32(&endpoint->sd_tokens_lp, credits) > 0 && 
                   !opal_list_is_empty(&(endpoint->pending_frags_lp))) { 
                    do {
                        opal_list_item_t *frag_item;
                        OPAL_THREAD_LOCK(&endpoint->endpoint_lock); 
                        frag_item = opal_list_remove_first(&(endpoint->pending_frags_lp));
                        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock); 
                        if(NULL == (frag = (mca_btl_mvapi_frag_t *) frag_item)) 
                            break;
                        MCA_BTL_IB_FRAG_PROGRESS(frag);
                    } while(endpoint->sd_tokens_lp > 0);
                }

                /* check to see if we need to return credits */
                if( !mca_btl_mvapi_component.use_srq &&
                    endpoint->rd_credits_lp >= mca_btl_mvapi_component.rd_win) {
                    mca_btl_mvapi_endpoint_send_credits(
                        endpoint, 
                        endpoint->lcl_qp_hndl_lp, 
                        endpoint->rem_info.rem_qp_num_lp, 
                       &endpoint->rd_credits_lp);
                }
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
