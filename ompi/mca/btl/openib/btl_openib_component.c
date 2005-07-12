/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "include/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"

#include "mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "mca/common/vapi/vapi_mem_reg.h"
#include "mca/mpool/base/base.h" 
#include "btl_openib.h"
#include "btl_openib_frag.h"
#include "btl_openib_endpoint.h" 
#include "mca/btl/base/base.h"


#include "datatype/convertor.h" 
#include "mca/mpool/mvapi/mpool_mvapi.h" 
#include <sysfs/libsysfs.h>  
#include <infiniband/verbs.h> 


mca_btl_openib_component_t mca_btl_openib_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_0,

            "ib", /* MCA component name */
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

static inline char* mca_btl_openib_param_register_string(
                                                     const char* param_name, 
                                                     const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","ib",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_openib_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("btl","ib",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_openib_component_open(void)
{

    int param, value; 
    
    /* initialize state */
    mca_btl_openib_component.ib_num_btls=0;
    mca_btl_openib_component.mvapi_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_procs, opal_list_t);
    /* OBJ_CONSTRUCT (&mca_btl_openib_component.ib_recv_frags, ompi_free_list_t); */

    /* register IB component parameters */
    mca_btl_openib_component.ib_free_list_num =
        mca_btl_openib_param_register_int ("free_list_num", 8);
    mca_btl_openib_component.ib_free_list_max =
        mca_btl_openib_param_register_int ("free_list_max", 1024);
    mca_btl_openib_component.ib_free_list_inc =
        mca_btl_openib_param_register_int ("free_list_inc", 32);
    mca_btl_openib_component.ib_mem_registry_hints_log_size = 
        mca_btl_openib_param_register_int ("hints_log_size", 8);
    mca_btl_openib_component.ib_mpool_name = 
        mca_btl_openib_param_register_string("mpool", "ib"); 
    mca_btl_openib_component.ib_rr_buf_max = 
        mca_btl_openib_param_register_int("rr_buf_max", 16); 
    mca_btl_openib_component.ib_rr_buf_min = 
        mca_btl_openib_param_register_int("rr_buf_min", 8); 
    mca_btl_openib_component.reg_mru_len = 
        mca_btl_openib_param_register_int("reg_mru_len",  16); 
    
    mca_btl_openib_module.super.btl_exclusivity =
        mca_btl_openib_param_register_int ("exclusivity", 0);
    mca_btl_openib_module.super.btl_eager_limit = 
        mca_btl_openib_param_register_int ("eager_limit", (64*1024)) 
        - sizeof(mca_btl_openib_header_t); 

    mca_btl_openib_module.super.btl_min_send_size =
        mca_btl_openib_param_register_int ("min_send_size", (64*1024))
        - sizeof(mca_btl_openib_header_t);

    mca_btl_openib_module.super.btl_max_send_size =
        mca_btl_openib_param_register_int ("max_send_size", (128*1024)) 
        - sizeof(mca_btl_openib_header_t);

    mca_btl_openib_module.ib_pin_min = 
        mca_btl_openib_param_register_int("ib_pin_min", 128*1024);                                    
    mca_btl_openib_module.ib_cq_size = 
        mca_btl_openib_param_register_int("ib_cq_size", 
                                      40000); 
    mca_btl_openib_module.ib_wq_size = 
        mca_btl_openib_param_register_int("ib_wq_size", 
                                      10000); 
    mca_btl_openib_module.ib_sg_list_size = 
        mca_btl_openib_param_register_int("ib_sg_list_size", 
                                      1); 
    mca_btl_openib_module.ib_pkey_ix = 
        mca_btl_openib_param_register_int("ib_pkey_ix", 
                                      0); 
    mca_btl_openib_module.ib_psn = 
        mca_btl_openib_param_register_int("ib_psn", 
                                      0); 
    mca_btl_openib_module.ib_qp_ous_rd_atom = 
        mca_btl_openib_param_register_int("ib_qp_ous_rd_atom", 
                                      1); 
    mca_btl_openib_module.ib_mtu = 
        mca_btl_openib_param_register_int("ib_mtu", 
                                      MTU1024); 
    mca_btl_openib_module.ib_min_rnr_timer = 
        mca_btl_openib_param_register_int("ib_min_rnr_timer", 
                                      5);
    mca_btl_openib_module.ib_timeout = 
        mca_btl_openib_param_register_int("ib_timeout", 
                                      10); 
    mca_btl_openib_module.ib_retry_count = 
        mca_btl_openib_param_register_int("ib_retry_count", 
                                      7); 
    mca_btl_openib_module.ib_rnr_retry = 
        mca_btl_openib_param_register_int("ib_rnr_retry", 
                                      7); 
    mca_btl_openib_module.ib_max_rdma_dst_ops = 
        mca_btl_openib_param_register_int("ib_max_rdma_dst_ops", 
                                      16); 

    mca_btl_openib_module.ib_service_level = 
        mca_btl_openib_param_register_int("ib_service_level", 
                                      0); 
    mca_btl_openib_module.ib_static_rate = 
        mca_btl_openib_param_register_int("ib_static_rate", 
                                      0); 
    mca_btl_openib_module.ib_src_path_bits = 
        mca_btl_openib_param_register_int("ib_src_path_bits", 
                                      0); 
    mca_btl_openib_module.super.btl_min_rdma_size = 
        mca_btl_openib_param_register_int("min_rdma_size", 
                                      1024*1024); 
    mca_btl_openib_module.super.btl_max_rdma_size = 
        mca_btl_openib_param_register_int("max_rdma_size", 
                                      1024*1024); 
    mca_btl_openib_module.super.btl_flags  = 
        mca_btl_openib_param_register_int("flags", 
                                      MCA_BTL_FLAGS_RDMA); 
    
    
    param = mca_base_param_find("mpi", NULL, "leave_pinned"); 
    mca_base_param_lookup_int(param, &value); 
    mca_btl_openib_component.leave_pinned = value; 


    
    
    
    
    
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
    mca_btl_openib_module_t * mvapi_btl; 
    mca_btl_base_selected_module_t* ib_selected; 
    opal_list_item_t* item; 
    /* initialization */
    *num_btl_modules = 0;
    num_devs = 0; 

    struct dlist *dev_list; 
    struct ibv_device* ib_dev; 
    
    /* Determine the number of hca's available on the host */
    dev_list = ibv_get_devices(); 
    dlist_start(dev_list); 

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        num_devs++; 
    
    if(0 == num_devs) { 
        opal_output(0, "No hca's found on this host! \n"); 
        return NULL; 
    }
        
    
    /* Allocate space for the ib devices */ 
    ib_devs = (struct ibv_dev**) malloc(num_devs * sizeof(struct ibv_dev*));
        if(NULL == ib_devs) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    dlist_start(dev_list); 

    int i = 0; 
    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        ib_devs[i++] =  ib_dev; 
    
            
    /** We must loop through all the hca id's, get there handles and 
        for each hca we query the number of ports on the hca and set up 
        a distinct btl module for each hca port */ 

    OBJ_CONSTRUCT(&btl_list, opal_list_t); 
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_lock, opal_mutex_t);


    for(i = 0; i < num_devs; i++){  
        struct ibv_device_attr* ib_attr; 
        struct ibv_context* ib_dev_context; 
        ib_dev = ib_devs[i]; 
        
        ib_dev_context = ibv_open_device(ib_dev); 
        if(!ib_dev_context) { 
            opal_output(0, "%s: error obtaining device context for %s\n", __func__, ibv_get_device_name(ib_dev)); 
            return NULL; 
        } 
        
        if(ibv_query_device(context, &ib_attr)){ 
            opal_output(0, "%s: error obtaining device attributes for %s\n", __func__, ibv_get_device_name(ib_dev)); 
            return NULL; 
        } 
        
        
        /* Note ports are 1 based hence j = 1 */
        
        for(j = 1; j <= ib_dev_attr.phys_port_cnt; j++){ 
            struct ibv_port_attr* ib_port_attr; 
            if(ibv_query_port(ib_dev_context, (uint8_t) j, &ib_port_attr)){ 
                opal_output(0, "%s: error getting port attributes for device %s port number %d", 
                            __func__, ibv_get_device_name(ib_dev), j); 
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
                 
                 opal_list_append(&btl_list, (opal_list_item_t*) ib_selected);
                 mca_btl_openib_component.ib_num_btls ++; 
                 
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
        
        /* Initialize module state */

        OBJ_CONSTRUCT(&openib_btl->ib_lock, opal_mutex_t); 
        OBJ_CONSTRUCT(&openib_btl->send_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->send_free_max, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->send_free_frag, ompi_free_list_t);
        
        OBJ_CONSTRUCT(&openib_btl->recv_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&openib_btl->recv_free_max, ompi_free_list_t);
        

        OBJ_CONSTRUCT(&openib_btl->repost, opal_list_t);
        OBJ_CONSTRUCT(&openib_btl->reg_mru_list, opal_list_t); 
        
      

        if(mca_btl_openib_module_init(openib_btl) != OMPI_SUCCESS) {
            free(ib_devs);
            return NULL;
        }
  
        mpool_resources.ib_pd = openib_btl->ib_pd; 
                
        /* initialize the memory pool using the hca */ 
        openib_btl->ib_pool = 
            mca_mpool_base_module_create(mca_btl_openib_component.ib_mpool_name,
                                         &openib_btl->super, 
                                         &mpool_resources); 
        
        if(NULL == openib_btl->ib_pool) { 
            opal_output(0, "%s: error creating vapi memory pool! aborting ib btl initialization", __func__); 
            return NULL; 
        }

        /* Initialize pool of send fragments */ 
        length = sizeof(mca_btl_openib_frag_t) + 
            sizeof(mca_btl_openib_header_t) + 
            openib_btl->super.btl_eager_limit+ 
            2*MCA_BTL_IB_FRAG_ALIGN; 
        
        ompi_free_list_init(&openib_btl->send_free_eager,
                            length, 
                            OBJ_CLASS(mca_btl_openib_send_frag_eager_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->ib_pool);
        
        ompi_free_list_init(&openib_btl->recv_free_eager,
                            length, 
                            OBJ_CLASS(mca_btl_openib_recv_frag_eager_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->ib_pool);
        
        
        
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
                            openib_btl->ib_pool);
                
        /* Initialize pool of receive fragments */
        ompi_free_list_init (&openib_btl->recv_free_max, 
                             length, 
                             OBJ_CLASS (mca_btl_openib_recv_frag_max_t),
                             mca_btl_openib_component.ib_free_list_num,
                             mca_btl_openib_component.ib_free_list_max,
                             mca_btl_openib_component.ib_free_list_inc, openib_btl->ib_pool);

        
        
        length = sizeof(mca_btl_openib_frag_t) + 
            sizeof(mca_btl_openib_header_t)+ 
            2*MCA_BTL_IB_FRAG_ALIGN; 

        
        
        
        ompi_free_list_init(&openib_btl->send_free_frag,
                            length, 
                            OBJ_CLASS(mca_btl_openib_send_frag_frag_t),
                            mca_btl_openib_component.ib_free_list_num,
                            mca_btl_openib_component.ib_free_list_max,
                            mca_btl_openib_component.ib_free_list_inc,
                            openib_btl->ib_pool);
        
        
        /* Initialize the rr_desc_post array for posting of rr*/ 
        openib_btl->rr_desc_post = (struct ibv_recv_wr **) 
            malloc((mca_btl_openib_component.ib_rr_buf_max * sizeof(struct ibv_recv_wr*))); 
        
        btls[i] = &openib_btl->super;
    }

    /* Post OOB receive to support dynamic connection setup */
    mca_btl_openib_post_recv();

    *num_btl_modules = mca_btl_openib_component.ib_num_btls;
    free(hca_ids);
    return btls;
}

/*
 *  IB component progress.
 */


int mca_btl_openib_component_progress()
{
    uint32_t i;
    int count = 0;
    mca_btl_openib_frag_t* frag; 
    /* Poll for completions */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        VAPI_ret_t ret; 
        VAPI_wc_desc_t comp; 
        mca_btl_openib_module_t* openib_btl = &mca_btl_openib_component.openib_btls[i];
        
        do{ 
        ret = VAPI_poll_cq(openib_btl->nic, openib_btl->cq_hndl_high, &comp); 
        if(VAPI_OK == ret) { 
            if(comp.status != VAPI_SUCCESS) { 
                opal_output(0, "Got error : %s, Vendor code : %d Frag : %p", 
                            VAPI_wc_status_sym(comp.status), 
                            comp.vendor_err_syndrome, comp.id);  
                return OMPI_ERROR; 
            }
            
            /* Handle n/w completions */
            switch(comp.opcode) {
            case VAPI_CQE_RQ_RDMA_WITH_IMM: 
                if(comp.imm_data_valid){ 
                    opal_output(0, "Got an RQ_RDMA_WITH_IMM!\n"); 
                    
                }
                break; 
            case VAPI_CQE_SQ_RDMA_WRITE:
            case VAPI_CQE_SQ_SEND_DATA :
                
                /* Process a completed send */
                frag = (mca_btl_openib_frag_t*) comp.id; 
                frag->rc = OMPI_SUCCESS; 
                frag->base.des_cbfunc(&openib_btl->super, frag->endpoint, &frag->base, frag->rc); 
                count++;
                break;
                
            case VAPI_CQE_RQ_SEND_DATA:
                
                DEBUG_OUT(0, "%s:%d ib recv under redesign\n", __FILE__, __LINE__); 
                frag = (mca_btl_openib_frag_t*) comp.id;
                frag->rc=OMPI_SUCCESS; 
                frag->segment.seg_len =  comp.byte_len-((unsigned char*) frag->segment.seg_addr.pval  - (unsigned char*) frag->hdr); 
                /* advance the segment address past the header and subtract from the length..*/ 
                openib_btl->ib_reg[frag->hdr->tag].cbfunc(&openib_btl->super, frag->hdr->tag, &frag->base, openib_btl->ib_reg[frag->hdr->tag].cbdata);         
                
                OMPI_FREE_LIST_RETURN(&(openib_btl->recv_free_eager), (opal_list_item_t*) frag); 
                OPAL_THREAD_ADD32(&openib_btl->rr_posted_high, -1); 
                
                mca_btl_openib_endpoint_post_rr(((mca_btl_openib_frag_t*)comp.id)->endpoint, 0); 
                
                count++; 
                break;
                
            default:
                opal_output(0, "Errorneous network completion");
                break;
            }
        }
        }
        while(VAPI_OK == ret); 

        ret = VAPI_poll_cq(openib_btl->nic, openib_btl->cq_hndl_low, &comp); 
        if(VAPI_OK == ret) { 
            if(comp.status != VAPI_SUCCESS) { 
                opal_output(0, "Got error : %s, Vendor code : %d Frag : %p", 
                            VAPI_wc_status_sym(comp.status), 
                            comp.vendor_err_syndrome, comp.id);  
                return OMPI_ERROR; 
            }
            
            /* Handle n/w completions */
            switch(comp.opcode) {
            case VAPI_CQE_SQ_RDMA_WRITE:
            case VAPI_CQE_SQ_SEND_DATA :
                
                /* Process a completed send */
                frag = (mca_btl_openib_frag_t*) comp.id; 
                frag->rc = OMPI_SUCCESS; 
                frag->base.des_cbfunc(&openib_btl->super, frag->endpoint, &frag->base, frag->rc); 
                count++;
                break;
                
            case VAPI_CQE_RQ_SEND_DATA:
                
                DEBUG_OUT(0, "%s:%d ib recv under redesign\n", __FILE__, __LINE__); 
                frag = (mca_btl_openib_frag_t*) comp.id;
                frag->rc=OMPI_SUCCESS; 
                frag->segment.seg_len =  comp.byte_len-((unsigned char*) frag->segment.seg_addr.pval  - (unsigned char*) frag->hdr); 
                /* advance the segment address past the header and subtract from the length..*/ 
                openib_btl->ib_reg[frag->hdr->tag].cbfunc(&openib_btl->super, frag->hdr->tag, &frag->base, openib_btl->ib_reg[frag->hdr->tag].cbdata);         
                
                OMPI_FREE_LIST_RETURN(&(openib_btl->recv_free_max), (opal_list_item_t*) frag); 
                OPAL_THREAD_ADD32(&openib_btl->rr_posted_low, -1); 
                

                mca_btl_openib_endpoint_post_rr(((mca_btl_openib_frag_t*)comp.id)->endpoint, 0); 
                
                count++; 
                break;
                
            default:
                opal_output(0, "Errorneous network completion");
                break;
            }
        }
        
    }
    return count;
}
