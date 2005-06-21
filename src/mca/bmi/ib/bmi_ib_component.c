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
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"

#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "mca/errmgr/errmgr.h"
#include "mca/common/vapi/vapi_mem_reg.h"
#include "mca/mpool/base/base.h" 
#include "bmi_ib.h"
#include "bmi_ib_frag.h"
#include "bmi_ib_endpoint.h" 
#include "mca/bmi/base/base.h" 
#include <vapi.h> 
#include <vapi_common.h> 
#include "datatype/convertor.h" 
mca_bmi_ib_component_t mca_bmi_ib_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BMI_BASE_VERSION_1_0_0,

            "ib", /* MCA component name */
            1,  /* MCA component major version */
            0,  /* MCA component minor version */
            0,  /* MCA component release version */
            mca_bmi_ib_component_open,  /* component open */
            mca_bmi_ib_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_bmi_ib_component_init,  
        mca_bmi_ib_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_bmi_ib_param_register_string(
                                                     const char* param_name, 
                                                     const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("bmi","ib",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_bmi_ib_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("bmi","ib",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_bmi_ib_component_open(void)
{

    
    /* initialize state */
    mca_bmi_ib_component.ib_num_bmis=0;
    mca_bmi_ib_component.ib_bmis=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_bmi_ib_component.ib_procs, ompi_list_t);
    /* OBJ_CONSTRUCT (&mca_bmi_ib_component.ib_recv_frags, ompi_free_list_t); */

    /* register IB component parameters */
    mca_bmi_ib_component.ib_free_list_num =
        mca_bmi_ib_param_register_int ("free_list_num", 8);
    mca_bmi_ib_component.ib_free_list_max =
        mca_bmi_ib_param_register_int ("free_list_max", 1024);
    mca_bmi_ib_component.ib_free_list_inc =
        mca_bmi_ib_param_register_int ("free_list_inc", 32);
    mca_bmi_ib_component.ib_mem_registry_hints_log_size = 
        mca_bmi_ib_param_register_int ("hints_log_size", 8);
    mca_bmi_ib_component.ib_mpool_name = 
        mca_bmi_ib_param_register_string("mpool", "ib"); 
    mca_bmi_ib_component.ib_rr_buf_max = 
        mca_bmi_ib_param_register_int("rr_buf_max", 16); 
    mca_bmi_ib_component.ib_rr_buf_min = 
        mca_bmi_ib_param_register_int("rr_buf_min", 8); 
    mca_bmi_ib_module.super.bmi_exclusivity =
        mca_bmi_ib_param_register_int ("exclusivity", 0);
    mca_bmi_ib_module.super.bmi_eager_limit = 
        mca_bmi_ib_param_register_int ("eager_limit",
                                       (64*1024
                                        - sizeof(mca_bmi_ib_header_t)));
    mca_bmi_ib_module.super.bmi_min_send_size =
        mca_bmi_ib_param_register_int ("min_send_size",
                                       (64*1024 
                                        - sizeof(mca_bmi_ib_header_t)));
    mca_bmi_ib_module.super.bmi_max_send_size =
        mca_bmi_ib_param_register_int ("max_send_size", 128*1024 -
                                       sizeof(mca_bmi_ib_header_t));
    mca_bmi_ib_module.ib_pin_min = 
        mca_bmi_ib_param_register_int("ib_pin_min", 128*1024);                                    
    mca_bmi_ib_module.ib_cq_size = 
        mca_bmi_ib_param_register_int("ib_cq_size", 
                                      40000); 
    mca_bmi_ib_module.ib_wq_size = 
        mca_bmi_ib_param_register_int("ib_wq_size", 
                                      10000); 
    mca_bmi_ib_module.ib_sg_list_size = 
        mca_bmi_ib_param_register_int("ib_sg_list_size", 
                                      1); 
    mca_bmi_ib_module.ib_pkey_ix = 
        mca_bmi_ib_param_register_int("ib_pkey_ix", 
                                      0); 
    mca_bmi_ib_module.ib_psn = 
        mca_bmi_ib_param_register_int("ib_psn", 
                                      0); 
    mca_bmi_ib_module.ib_qp_ous_rd_atom = 
        mca_bmi_ib_param_register_int("ib_qp_ous_rd_atom", 
                                      1); 
    mca_bmi_ib_module.ib_mtu = 
        mca_bmi_ib_param_register_int("ib_mtu", 
                                      MTU1024); 
    mca_bmi_ib_module.ib_min_rnr_timer = 
        mca_bmi_ib_param_register_int("ib_min_rnr_timer", 
                                      5);
    mca_bmi_ib_module.ib_timeout = 
        mca_bmi_ib_param_register_int("ib_timeout", 
                                      10); 
    mca_bmi_ib_module.ib_retry_count = 
        mca_bmi_ib_param_register_int("ib_retry_count", 
                                      7); 
    mca_bmi_ib_module.ib_rnr_retry = 
        mca_bmi_ib_param_register_int("ib_rnr_retry", 
                                      7); 
    mca_bmi_ib_module.ib_max_rdma_dst_ops = 
        mca_bmi_ib_param_register_int("ib_max_rdma_dst_ops", 
                                      16); 

    mca_bmi_ib_module.ib_service_level = 
        mca_bmi_ib_param_register_int("ib_service_level", 
                                      0); 
    mca_bmi_ib_module.ib_static_rate = 
        mca_bmi_ib_param_register_int("ib_static_rate", 
                                      0); 
    mca_bmi_ib_module.ib_src_path_bits = 
        mca_bmi_ib_param_register_int("ib_src_path_bits", 
                                      0); 
    mca_bmi_ib_module.super.bmi_min_rdma_size = 
        mca_bmi_ib_param_register_int("min_rdma_size", 
                                      1024*1024); 
    mca_bmi_ib_module.super.bmi_max_rdma_size = 
        mca_bmi_ib_param_register_int("max_rdma_size", 
                                      1024*1024); 
    mca_bmi_ib_module.super.bmi_flags  = 
        mca_bmi_ib_param_register_int("flags", 
                                      MCA_BMI_FLAGS_RDMA); 

    mca_bmi_ib_component.leave_pinned = 
        mca_bmi_ib_param_register_int("leave_pinned", 
                                      0); 


    
    
    
    
    
    mca_bmi_ib_component.max_send_size = mca_bmi_ib_module.super.bmi_max_send_size; 
    mca_bmi_ib_component.eager_limit = mca_bmi_ib_module.super.bmi_eager_limit; 
    
    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_bmi_ib_component_close(void)
{
    return OMPI_SUCCESS;
}

/*
 *  IB component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BMI instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) register BMI parameters with the MCA
 */

mca_bmi_base_module_t** mca_bmi_ib_component_init(int *num_bmi_modules, 
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
    mca_bmi_base_module_t** bmis;
    uint32_t i,j, length;
    mca_common_vapi_hca_pd_t hca_pd; 
    ompi_list_t bmi_list; 
    mca_bmi_ib_module_t * ib_bmi; 
    mca_bmi_base_selected_module_t* ib_selected; 
    ompi_list_item_t* item; 
    /* initialization */
    *num_bmi_modules = 0;

    /* Determine the number of hca's available on the host */
    vapi_ret=EVAPI_list_hcas(0, &num_hcas, NULL);
    if( VAPI_EAGAIN != vapi_ret || 0 == num_hcas ) {
        ompi_output(0,"No hca's found on this host \n"); 
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
        a distinct bmi module for each hca port */ 

    OBJ_CONSTRUCT(&bmi_list, ompi_list_t); 
    
    for(i = 0; i < num_hcas; i++){  
        vapi_ret = EVAPI_get_hca_hndl(hca_ids[i], &hca_hndl); 
        if(VAPI_OK != vapi_ret) { 
            ompi_output(0, "%s:error getting hca handle\n", __func__); 
            return NULL; 
        } 
        

        vapi_ret = VAPI_query_hca_cap(hca_hndl, &hca_vendor, &hca_cap); 
         if(VAPI_OK != vapi_ret) { 
            ompi_output(0, "%s:error getting hca properties\n", __func__); 
            return NULL; 
        } 
         
         
         /* Note ports are 1 based hence j = 1 */
         for(j = 1; j <= hca_cap.phys_port_num; j++){ 
             vapi_ret = VAPI_query_hca_port_prop(hca_hndl, (IB_port_t) j, &hca_port);  
             if(VAPI_OK != vapi_ret) { 
                 ompi_output(0, "%s:error getting hca port properties\n", __func__); 
                 return NULL; 
             } 
             
             if( PORT_ACTIVE == hca_port.state ){ 
                 
                 ib_bmi = (mca_bmi_ib_module_t*) malloc(sizeof(mca_bmi_ib_module_t)); 
                 memcpy(ib_bmi, &mca_bmi_ib_module, sizeof(mca_bmi_ib_module));
                 
                 ib_selected = OBJ_NEW(mca_bmi_base_selected_module_t); 
                 ib_selected->bmi_module = (mca_bmi_base_module_t*) ib_bmi; 
                 memcpy(ib_bmi->hca_id,   hca_ids[i], sizeof(VAPI_hca_id_t)); 
                 ib_bmi->nic = hca_hndl; 
                 ib_bmi->port_id = (IB_port_t) j; 
                 ib_bmi->port = hca_port; 
                 ompi_list_append(&bmi_list, (ompi_list_item_t*) ib_selected);
                 mca_bmi_ib_component.ib_num_bmis ++; 
                 
            } 
        }
 
    }
    
    
    /* Allocate space for bmi modules */
    mca_bmi_ib_component.ib_bmis = (mca_bmi_ib_module_t*) malloc(sizeof(mca_bmi_ib_module_t) * 
                                                                 mca_bmi_ib_component.ib_num_bmis);
    
    if(NULL == mca_bmi_ib_component.ib_bmis) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    bmis = (struct mca_bmi_base_module_t**) 
        malloc(mca_bmi_ib_component.ib_num_bmis * sizeof(struct mca_bmi_ib_module_t*));
    if(NULL == bmis) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    
    
    for(i = 0; i < mca_bmi_ib_component.ib_num_bmis; i++){
        item = ompi_list_remove_first(&bmi_list); 
        ib_selected = (mca_bmi_base_selected_module_t*)item; 
        ib_bmi = (mca_bmi_ib_module_t*) ib_selected->bmi_module; 
        memcpy(&(mca_bmi_ib_component.ib_bmis[i]), ib_bmi , sizeof(mca_bmi_ib_module_t)); 
        free(ib_selected); 
        free(ib_bmi); 

        ib_bmi = &mca_bmi_ib_component.ib_bmis[i];
        
        /* Initialize the modules function pointers */
        

        /* Initialize module state */
        OBJ_CONSTRUCT(&ib_bmi->send_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&ib_bmi->send_free_max, ompi_free_list_t);
        OBJ_CONSTRUCT(&ib_bmi->send_free_frag, ompi_free_list_t);
        
        OBJ_CONSTRUCT(&ib_bmi->recv_free_eager, ompi_free_list_t);
        OBJ_CONSTRUCT(&ib_bmi->recv_free_max, ompi_free_list_t);
        

        OBJ_CONSTRUCT(&ib_bmi->repost, ompi_list_t);

        
      

        if(mca_bmi_ib_module_init(ib_bmi) != OMPI_SUCCESS) {
            free(hca_ids);
            return NULL;
        }
  
        hca_pd.hca = ib_bmi->nic; 
        hca_pd.pd_tag = ib_bmi->ptag; 
        
        /* initialize the memory pool using the hca */ 
        ib_bmi->ib_pool = 
            mca_mpool_base_module_create(mca_bmi_ib_component.ib_mpool_name, &hca_pd); 
        
        if(NULL == ib_bmi->ib_pool) { 
            ompi_output(0, "%s: error creating vapi memory pool! aborting ib bmi initialization", __func__); 
            return NULL; 
        }
        /* Initialize pool of send fragments */ 
        
        length = sizeof(mca_bmi_ib_frag_t) + 
            sizeof(mca_bmi_ib_header_t) + 
            ib_bmi->super.bmi_eager_limit+ 
            2*MCA_BMI_IB_FRAG_ALIGN; 
        
        ompi_free_list_init(&ib_bmi->send_free_eager,
                            length, 
                            OBJ_CLASS(mca_bmi_ib_send_frag_eager_t),
                            mca_bmi_ib_component.ib_free_list_num,
                            mca_bmi_ib_component.ib_free_list_max,
                            mca_bmi_ib_component.ib_free_list_inc,
                            ib_bmi->ib_pool);
        
        ompi_free_list_init(&ib_bmi->recv_free_eager,
                            length, 
                            OBJ_CLASS(mca_bmi_ib_recv_frag_eager_t),
                            mca_bmi_ib_component.ib_free_list_num,
                            mca_bmi_ib_component.ib_free_list_max,
                            mca_bmi_ib_component.ib_free_list_inc,
                            ib_bmi->ib_pool);
        
        
        
        length = sizeof(mca_bmi_ib_frag_t) + 
            sizeof(mca_bmi_ib_header_t) + 
            ib_bmi->super.bmi_max_send_size+
            2*MCA_BMI_IB_FRAG_ALIGN; 
        
        
        ompi_free_list_init(&ib_bmi->send_free_max,
                            length, 
                            OBJ_CLASS(mca_bmi_ib_send_frag_max_t),
                            mca_bmi_ib_component.ib_free_list_num,
                            mca_bmi_ib_component.ib_free_list_max,
                            mca_bmi_ib_component.ib_free_list_inc,
                            ib_bmi->ib_pool);
        
        
        
        /* Initialize pool of receive fragments */
        ompi_free_list_init (&ib_bmi->recv_free_max, 
                             length, 
                             OBJ_CLASS (mca_bmi_ib_recv_frag_max_t),
                             mca_bmi_ib_component.ib_free_list_num,
                             mca_bmi_ib_component.ib_free_list_max,
                             mca_bmi_ib_component.ib_free_list_inc, ib_bmi->ib_pool);

        
        
        length = sizeof(mca_bmi_ib_frag_t) + 
            sizeof(mca_bmi_ib_header_t)+ 
            2*MCA_BMI_IB_FRAG_ALIGN; 

        
        
        
        ompi_free_list_init(&ib_bmi->send_free_frag,
                            length, 
                            OBJ_CLASS(mca_bmi_ib_send_frag_frag_t),
                            mca_bmi_ib_component.ib_free_list_num,
                            mca_bmi_ib_component.ib_free_list_max,
                            mca_bmi_ib_component.ib_free_list_inc,
                            ib_bmi->ib_pool);
        
        
        /* Initialize the rr_desc_post array for posting of rr*/ 
        ib_bmi->rr_desc_post = (VAPI_rr_desc_t*) malloc((mca_bmi_ib_component.ib_rr_buf_max * sizeof(VAPI_rr_desc_t))); 
        
        /* This is now done by the memory pool passed to free_list_init.. Initialize the send descriptors */
        /* if(mca_bmi_ib_send_frag_register(ib_bmi) != OMPI_SUCCESS) { */
        /*             free(hca_ids); */
        /*             return NULL; */
        /*         } */
        bmis[i] = &ib_bmi->super;
    }

    /* Post OOB receive to support dynamic connection setup */
    mca_bmi_ib_post_recv();

    *num_bmi_modules = mca_bmi_ib_component.ib_num_bmis;
    free(hca_ids);
    return bmis;
}

/*
 *  IB component progress.
 */


int mca_bmi_ib_component_progress()
{
    uint32_t i;
    int count = 0;
    mca_bmi_ib_frag_t* frag; 
    /* Poll for completions */
    for(i = 0; i < mca_bmi_ib_component.ib_num_bmis; i++) {
        VAPI_ret_t ret; 
        VAPI_wc_desc_t comp; 
        mca_bmi_ib_module_t* ib_bmi = &mca_bmi_ib_component.ib_bmis[i];
        
        do{ 
        ret = VAPI_poll_cq(ib_bmi->nic, ib_bmi->cq_hndl_high, &comp); 
        if(VAPI_OK == ret) { 
            if(comp.status != VAPI_SUCCESS) { 
                ompi_output(0, "Got error : %s, Vendor code : %d Frag : %p", 
                            VAPI_wc_status_sym(comp.status), 
                            comp.vendor_err_syndrome, comp.id);  
                return OMPI_ERROR; 
            }
            
            /* Handle n/w completions */
            switch(comp.opcode) {
            case VAPI_CQE_RQ_RDMA_WITH_IMM: 
                if(comp.imm_data_valid){ 
                    ompi_output(0, "Got an RQ_RDMA_WITH_IMM!\n"); 
                    
                }
                break; 
            case VAPI_CQE_SQ_RDMA_WRITE:
            case VAPI_CQE_SQ_SEND_DATA :
                
                /* Process a completed send */
                frag = (mca_bmi_ib_frag_t*) comp.id; 
                frag->rc = OMPI_SUCCESS; 
                frag->base.des_cbfunc(&ib_bmi->super, frag->endpoint, &frag->base, frag->rc); 
                count++;
                break;
                
            case VAPI_CQE_RQ_SEND_DATA:
                
                DEBUG_OUT(0, "%s:%d ib recv under redesign\n", __FILE__, __LINE__); 
                frag = (mca_bmi_ib_frag_t*) comp.id;
                frag->rc=OMPI_SUCCESS; 
                frag->segment.seg_len =  comp.byte_len-((unsigned char*) frag->segment.seg_addr.pval  - (unsigned char*) frag->hdr); 
                /* advance the segment address past the header and subtract from the length..*/ 
                ib_bmi->ib_reg[frag->hdr->tag].cbfunc(&ib_bmi->super, frag->hdr->tag, &frag->base, ib_bmi->ib_reg[frag->hdr->tag].cbdata);         
                
                OMPI_FREE_LIST_RETURN(&(ib_bmi->recv_free_eager), (ompi_list_item_t*) frag); 
                OMPI_THREAD_ADD32(&ib_bmi->rr_posted_high, -1); 
                
                mca_bmi_ib_endpoint_post_rr(((mca_bmi_ib_frag_t*)comp.id)->endpoint, 0); 
                
                count++; 
                break;
                
            default:
                ompi_output(0, "Errorneous network completion");
                break;
            }
        }
        }
        while(VAPI_OK == ret); 

        ret = VAPI_poll_cq(ib_bmi->nic, ib_bmi->cq_hndl_low, &comp); 
        if(VAPI_OK == ret) { 
            if(comp.status != VAPI_SUCCESS) { 
                ompi_output(0, "Got error : %s, Vendor code : %d Frag : %p", 
                            VAPI_wc_status_sym(comp.status), 
                            comp.vendor_err_syndrome, comp.id);  
                return OMPI_ERROR; 
            }
            
            /* Handle n/w completions */
            switch(comp.opcode) {
            case VAPI_CQE_SQ_RDMA_WRITE:
            case VAPI_CQE_SQ_SEND_DATA :
                
                /* Process a completed send */
                frag = (mca_bmi_ib_frag_t*) comp.id; 
                frag->rc = OMPI_SUCCESS; 
                frag->base.des_cbfunc(&ib_bmi->super, frag->endpoint, &frag->base, frag->rc); 
                count++;
                break;
                
            case VAPI_CQE_RQ_SEND_DATA:
                
                DEBUG_OUT(0, "%s:%d ib recv under redesign\n", __FILE__, __LINE__); 
                frag = (mca_bmi_ib_frag_t*) comp.id;
                frag->rc=OMPI_SUCCESS; 
                frag->segment.seg_len =  comp.byte_len-((unsigned char*) frag->segment.seg_addr.pval  - (unsigned char*) frag->hdr); 
                /* advance the segment address past the header and subtract from the length..*/ 
                ib_bmi->ib_reg[frag->hdr->tag].cbfunc(&ib_bmi->super, frag->hdr->tag, &frag->base, ib_bmi->ib_reg[frag->hdr->tag].cbdata);         
                
                OMPI_FREE_LIST_RETURN(&(ib_bmi->recv_free_max), (ompi_list_item_t*) frag); 
                OMPI_THREAD_ADD32(&ib_bmi->rr_posted_low, -1); 
                

                mca_bmi_ib_endpoint_post_rr(((mca_bmi_ib_frag_t*)comp.id)->endpoint, 0); 
                
                count++; 
                break;
                
            default:
                ompi_output(0, "Errorneous network completion");
                break;
            }
        }
        
    }
    return count;
}
