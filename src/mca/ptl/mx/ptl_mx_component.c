/*
 * $HEADER$
 */
#include "ompi_config.h"
#include "include/constants.h"
#include "util/output.h"
#include "ptl_mx.h"
#include "ptl_mx_recvfrag.h"
#include "ptl_mx_sendfrag.h"
#include "ptl_mx_peer.h"


/*
 * The MX component
 */

mca_ptl_mx_component_t mca_ptl_mx_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a pml v1.0.0 module (which also
           implies a specific MCA version) */

        MCA_PTL_BASE_VERSION_1_0_0,

        "mx", /* MCA module name */
        1,  /* MCA module major version */
        0,  /* MCA module minor version */
        0,  /* MCA module release version */
        mca_ptl_mx_component_open,  /* module open */
        mca_ptl_mx_component_close  /* module close */
      },
      
      /* Next the MCA v1.0.0 module meta data */
      
      {
        /* Whether the module is checkpointable or not */
        
        false
      },
      
      mca_ptl_mx_component_init,  
      mca_ptl_mx_component_control,
      mca_ptl_mx_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_ptl_mx_param_register_string(
    const char* param_name, 
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","mx",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                            
static inline int mca_ptl_mx_param_register_int(
    const char* param_name, 
    int default_value)
{
    int id = mca_base_param_register_int("ptl","mx",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int mca_ptl_mx_component_open(void)
{
    /* initialize state */
    mca_ptl_mx_component.mx_ptl_modules = NULL;
    mca_ptl_mx_component.mx_num_ptl_modules = 0;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_ptl_mx_component.mx_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_mx_component.mx_send_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_mx_component.mx_recv_frags, ompi_free_list_t);

    /* register MX module parameters */
    mca_ptl_mx_component.mx_free_list_num =
        mca_ptl_mx_param_register_int("free_list_num", 256);
    mca_ptl_mx_component.mx_free_list_max =
        mca_ptl_mx_param_register_int("free_list_max", -1);
    mca_ptl_mx_component.mx_free_list_inc =
        mca_ptl_mx_param_register_int("free_list_inc", 256);
    mca_ptl_mx_module.super.ptl_exclusivity =
        mca_ptl_mx_param_register_int("exclusivity", 0);
    mca_ptl_mx_module.super.ptl_first_frag_size =
        mca_ptl_mx_param_register_int("first_frag_size", 64*1024);
    mca_ptl_mx_module.super.ptl_min_frag_size = 
        mca_ptl_mx_param_register_int("min_frag_size", 64*1024);
    mca_ptl_mx_module.super.ptl_max_frag_size =
        mca_ptl_mx_param_register_int("max_frag_size", -1);
    return OMPI_SUCCESS;
}

/*
 * module cleanup - sanity checking of queue lengths
 */

int mca_ptl_mx_component_close(void)
{
    if (mca_ptl_mx_component.mx_send_frags.fl_num_allocated != 
        mca_ptl_mx_component.mx_send_frags.super.ompi_list_length) {
        ompi_output(0, "mx send frags: %d allocated %d returned\n",
            mca_ptl_mx_component.mx_send_frags.fl_num_allocated, 
            mca_ptl_mx_component.mx_send_frags.super.ompi_list_length);
    }
    if (mca_ptl_mx_component.mx_recv_frags.fl_num_allocated != 
        mca_ptl_mx_component.mx_recv_frags.super.ompi_list_length) {
        ompi_output(0, "mx recv frags: %d allocated %d returned\n",
            mca_ptl_mx_component.mx_recv_frags.fl_num_allocated, 
            mca_ptl_mx_component.mx_recv_frags.super.ompi_list_length);
    }

    /* release resources */
    OBJ_DESTRUCT(&mca_ptl_mx_component.mx_send_frags);
    OBJ_DESTRUCT(&mca_ptl_mx_component.mx_recv_frags);
    OBJ_DESTRUCT(&mca_ptl_mx_component.mx_lock);
    return OMPI_SUCCESS;
}


/*
 * Open NIC and initialize resources.
 */

static int ptl_mx_open_endpoints(void)
{
    return OMPI_SUCCESS;
}


/*
 *  MX module initialization.
 */
mca_ptl_base_module_t** mca_ptl_mx_component_init(int *num_ptl_modules, 
                                                bool *allow_multi_user_threads,
                                                bool *have_hidden_threads)
{
    mca_ptl_base_module_t** ptls;
    *num_ptl_modules = 0;
    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    ompi_free_list_init(&mca_ptl_mx_component.mx_send_frags, 
        sizeof(mca_ptl_mx_send_frag_t),
        OBJ_CLASS(mca_ptl_mx_send_frag_t),
        mca_ptl_mx_component.mx_free_list_num,
        mca_ptl_mx_component.mx_free_list_max,
        mca_ptl_mx_component.mx_free_list_inc,
        NULL); /* use default allocator */

    ompi_free_list_init(&mca_ptl_mx_component.mx_recv_frags, 
        sizeof(mca_ptl_mx_recv_frag_t),
        OBJ_CLASS(mca_ptl_mx_recv_frag_t),
        mca_ptl_mx_component.mx_free_list_num,
        mca_ptl_mx_component.mx_free_list_max,
        mca_ptl_mx_component.mx_free_list_inc,
        NULL); /* use default allocator */

    ptls = malloc(mca_ptl_mx_component.mx_num_ptl_modules * 
                  sizeof(mca_ptl_base_module_t*));
    if(NULL == ptls)
        return NULL;

    if(OMPI_SUCCESS != ptl_mx_open_endpoints())
        return NULL;

    memcpy(ptls, 
        mca_ptl_mx_component.mx_ptl_modules, 
        mca_ptl_mx_component.mx_num_ptl_modules*sizeof(mca_ptl_mx_module_t*));
    *num_ptl_modules = mca_ptl_mx_component.mx_num_ptl_modules;
    return ptls;
}

/*
 *  MX module control
 */

int mca_ptl_mx_component_control(int param, void* value, size_t size)
{
    switch(param) {
        case MCA_PTL_ENABLE:
            if(*(int*)value)
                ;  /* enable forwarding */
            else
                ;  /* disable forwarding */
            break;
        default:
            break;
    }
    return OMPI_SUCCESS;
}


/*
 *  MX module progress.
 */

int mca_ptl_mx_component_progress(mca_ptl_tstamp_t tstamp)
{
    return OMPI_SUCCESS;
}

