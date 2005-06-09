/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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

/* #include <hh_common.h> */

/* Open MPI includes */
#include "ompi_config.h"
#include "include/constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "mca/errmgr/errmgr.h"

/* IB ptl includes */
#include "ptl_ib.h"


mca_ptl_ib_component_t mca_ptl_ib_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_PTL_BASE_VERSION_1_0_0,

            "ib", /* MCA component name */
            1,  /* MCA component major version */
            0,  /* MCA component minor version */
            0,  /* MCA component release version */
            mca_ptl_ib_component_open,  /* component open */
            mca_ptl_ib_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_ptl_ib_component_init,  
        mca_ptl_ib_component_control,
        mca_ptl_ib_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_ptl_ib_param_register_string(
        const char* param_name, 
        const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","ib",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_ptl_ib_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("ptl","ib",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_ptl_ib_component_open(void)
{
    /* register component parameters */
    mca_ptl_ib_module.super.ptl_exclusivity =
        mca_ptl_ib_param_register_int ("exclusivity", 0);

    mca_ptl_ib_module.super.ptl_first_frag_size =
        mca_ptl_ib_param_register_int ("first_frag_size",
                (MCA_PTL_IB_FIRST_FRAG_SIZE
                 - sizeof(mca_ptl_base_header_t)));

    mca_ptl_ib_module.super.ptl_min_frag_size =
        mca_ptl_ib_param_register_int ("min_frag_size",
                (MCA_PTL_IB_FIRST_FRAG_SIZE 
                 - sizeof(mca_ptl_base_header_t)));

    mca_ptl_ib_module.super.ptl_max_frag_size =
        mca_ptl_ib_param_register_int ("max_frag_size", 2<<30);

    /* register IB component parameters */
    mca_ptl_ib_component.ib_free_list_num =
        mca_ptl_ib_param_register_int ("free_list_num", 8);
    mca_ptl_ib_component.ib_free_list_max =
        mca_ptl_ib_param_register_int ("free_list_max", 1024);
    mca_ptl_ib_component.ib_free_list_inc =
        mca_ptl_ib_param_register_int ("free_list_inc", 32);
    mca_ptl_ib_component.ib_mem_registry_hints_log_size = 
        mca_ptl_ib_param_register_int ("hints_log_size", 8);

    /* initialize global state */
    mca_ptl_ib_component.ib_num_ptls=0;
    mca_ptl_ib_component.ib_ptls=NULL;
    OBJ_CONSTRUCT(&mca_ptl_ib_component.ib_procs, ompi_list_t);
    OBJ_CONSTRUCT (&mca_ptl_ib_component.ib_recv_frags, ompi_free_list_t);

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_ptl_ib_component_close(void)
{
    D_PRINT("");
    /* Stub */
    return OMPI_SUCCESS;
}

/*
 *  IB component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a PTL instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) register PTL parameters with the MCA
 */
mca_ptl_base_module_t** mca_ptl_ib_component_init(int *num_ptl_modules, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    VAPI_ret_t vapi_ret;
    VAPI_hca_id_t* hca_ids;
    mca_ptl_base_module_t** ptls;
    int i, ret;

    /* initialization */
    *num_ptl_modules = 0;

    /* query the list of available hcas */
    vapi_ret=EVAPI_list_hcas(0, &(mca_ptl_ib_component.ib_num_ptls), NULL);
    if( VAPI_EAGAIN != vapi_ret || 0 == mca_ptl_ib_component.ib_num_ptls ) {
        ompi_output(0,"Warning: no IB HCAs found\n");
        return NULL;
    }

    hca_ids = (VAPI_hca_id_t*) malloc(mca_ptl_ib_component.ib_num_ptls * sizeof(VAPI_hca_id_t));
    if(NULL == hca_ids) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    vapi_ret=EVAPI_list_hcas(mca_ptl_ib_component.ib_num_ptls, &mca_ptl_ib_component.ib_num_ptls, hca_ids);
    if( VAPI_OK != vapi_ret ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
                                                                                                                      
    /* Allocate space for ptl modules */
    mca_ptl_ib_component.ib_ptls = (mca_ptl_ib_module_t*) malloc(sizeof(mca_ptl_ib_module_t) * 
            mca_ptl_ib_component.ib_num_ptls);
    if(NULL == mca_ptl_ib_component.ib_ptls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    ptls = (struct mca_ptl_base_module_t**) 
        malloc(mca_ptl_ib_component.ib_num_ptls * sizeof(struct mca_ptl_ib_module_t*));
    if(NULL == ptls) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }

    /* Initialize pool of receive fragments */
    ompi_free_list_init (&(mca_ptl_ib_component.ib_recv_frags),
            sizeof (mca_ptl_ib_recv_frag_t),
            OBJ_CLASS (mca_ptl_ib_recv_frag_t),
            mca_ptl_ib_component.ib_free_list_num,
            mca_ptl_ib_component.ib_free_list_max,
            mca_ptl_ib_component.ib_free_list_inc, NULL);

    /* Initialize each module */
    for(i = 0; i < mca_ptl_ib_component.ib_num_ptls; i++) {
        mca_ptl_ib_module_t* ib_ptl = &mca_ptl_ib_component.ib_ptls[i];

        /* Initialize the modules function pointers */
        memcpy(ib_ptl, &mca_ptl_ib_module, sizeof(mca_ptl_ib_module));

        /* Initialize module state */
        OBJ_CONSTRUCT(&ib_ptl->send_free, ompi_free_list_t);
        OBJ_CONSTRUCT(&ib_ptl->repost, ompi_list_t);

        ompi_free_list_init(&ib_ptl->send_free,
                sizeof(mca_ptl_ib_send_frag_t),
                OBJ_CLASS(mca_ptl_ib_send_frag_t),
                mca_ptl_ib_component.ib_free_list_num,
                mca_ptl_ib_component.ib_free_list_max,
                mca_ptl_ib_component.ib_free_list_inc,
                NULL);

      
        memcpy(ib_ptl->hca_id, hca_ids[i], sizeof(ib_ptl->hca_id));
        if(mca_ptl_ib_module_init(ib_ptl) != OMPI_SUCCESS) {
            free(hca_ids);
            return NULL;
        }

        /* Initialize the send descriptors */
        if(mca_ptl_ib_send_frag_register(ib_ptl) != OMPI_SUCCESS) {
            free(hca_ids);
            return NULL;
        }
        ptls[i] = &ib_ptl->super;
    }

    /* Post OOB receive to support dynamic connection setup */
    mca_ptl_ib_post_recv();

    *num_ptl_modules = mca_ptl_ib_component.ib_num_ptls;
    free(hca_ids);
    return ptls;
}

/*
 *  IB component control
 */

int mca_ptl_ib_component_control(int param, void* value, size_t size)
{
    return OMPI_SUCCESS;
}


/*
 *  IB component progress.
 */

#define MCA_PTL_IB_DRAIN_NETWORK(nic, cq_hndl, comp_type, comp_addr) \
{ \
    VAPI_ret_t ret; \
    VAPI_wc_desc_t comp; \
 \
    ret = VAPI_poll_cq(nic, cq_hndl, &comp); \
    if(VAPI_OK == ret) { \
        if(comp.status != VAPI_SUCCESS) { \
            ompi_output(0, "Got error : %s, Vendor code : %d Frag : %p", \
                    VAPI_wc_status_sym(comp.status), \
                    comp.vendor_err_syndrome, comp.id);  \
            *comp_type = IB_COMP_ERROR; \
            *comp_addr = NULL; \
        } else { \
            if(VAPI_CQE_SQ_SEND_DATA == comp.opcode) { \
                *comp_type = IB_COMP_SEND; \
                *comp_addr = (void*) (unsigned long) comp.id; \
            } else if(VAPI_CQE_RQ_SEND_DATA == comp.opcode) { \
                *comp_type = IB_COMP_RECV; \
                *comp_addr = (void*) (unsigned long) comp.id; \
            } else if(VAPI_CQE_SQ_RDMA_WRITE == comp.opcode) { \
                *comp_type = IB_COMP_RDMA_W; \
                *comp_addr = (void*) (unsigned long) comp.id; \
            } else { \
                ompi_output(0, "VAPI_poll_cq: returned unknown opcode : %d\n", \
                        comp.opcode); \
                *comp_type = IB_COMP_ERROR; \
                *comp_addr = NULL; \
            } \
        } \
    } else { \
        /* No completions from the network */ \
        *comp_type = IB_COMP_NOTHING; \
        *comp_addr = NULL; \
    } \
}


int mca_ptl_ib_component_progress(mca_ptl_tstamp_t tstamp)
{
    int i;
    int count = 0;

    /* Poll for completions */
    for(i = 0; i < mca_ptl_ib_component.ib_num_ptls; i++) {
        mca_ptl_ib_module_t* ib_ptl = &mca_ptl_ib_component.ib_ptls[i];
        int comp_type = IB_COMP_NOTHING;
        void* comp_addr;
        
        MCA_PTL_IB_DRAIN_NETWORK(ib_ptl->nic, ib_ptl->cq_hndl, &comp_type, &comp_addr);

        /* Handle n/w completions */
        switch(comp_type) {
            case IB_COMP_SEND :

                /* Process a completed send */
                mca_ptl_ib_send_frag_send_complete(ib_ptl, (mca_ptl_ib_send_frag_t*)comp_addr);
                count++;
                break;

            case IB_COMP_RECV :

                /* Process incoming receives */
                mca_ptl_ib_process_recv(ib_ptl, comp_addr);
#if 0
                /* Re post recv buffers */
                if(ompi_list_get_size(&ib_ptl->repost) <= 1) {
                    ompi_list_append(&ib_ptl->repost, (ompi_list_item_t*)comp_addr);
                } else {
                    ompi_list_item_t* item;
                    while(NULL != (item = ompi_list_remove_first(&ib_ptl->repost))) {
                         mca_ptl_ib_buffer_repost(ib_ptl->nic, item);
                    }
                    mca_ptl_ib_buffer_repost(ib_ptl->nic, comp_addr);
                }
#else
                    mca_ptl_ib_buffer_repost(ib_ptl->nic, comp_addr);
#endif
                count++;
                break;

            case IB_COMP_RDMA_W :

                ompi_output(0, "%s:%d RDMA not implemented\n", __FILE__,__LINE__);
                count++;
                break;

            case IB_COMP_NOTHING:
                break;
            default:
                ompi_output(0, "Errorneous network completion");
                break;
        }
    }
    return count;
}

