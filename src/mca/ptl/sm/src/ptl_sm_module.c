/*
 * $HEADER$
 */
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>

#include "constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_sm.h"
#include "ptl_sm_sendreq.h"
#include "ptl_sm_sendfrag.h"
#include "ptl_sm_recvfrag.h"



/*
 * Local utility functions.
 */

static int mca_ptl_sm_module_exchange(void);


/*
 * Shared Memory (SM) module instance. 
 */

mca_ptl_sm_module_1_0_0_t mca_ptl_sm_module = {
    {
    /* First, the mca_base_module_t struct containing meta information
       about the module itself */
                                                                                                                            
    {
    /* Indicate that we are a pml v1.0.0 module (which also implies a
       specific MCA version) */
                                                                                                                            
    MCA_PTL_BASE_VERSION_1_0_0,
                                                                                                                            
    "sm", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_ptl_sm_module_open,  /* module open */
    mca_ptl_sm_module_close  /* module close */
    },
                                                                                                                            
    /* Next the MCA v1.0.0 module meta data */
                                                                                                                            
    {
    /* Whether the module is checkpointable or not */
                                                                                                                            
    false
    },

    mca_ptl_sm_module_init,  
    mca_ptl_sm_module_control,
    mca_ptl_sm_module_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_ptl_sm_param_register_string(
    const char* param_name, 
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","sm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                            
static inline int mca_ptl_sm_param_register_int(
    const char* param_name, 
    int default_value)
{
    int id = mca_base_param_register_int("ptl","sm",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                                            
/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int mca_ptl_sm_module_open(void)
{
    /* register SM module parameters */
    mca_ptl_sm_module.sm_min_alloc =
        mca_ptl_sm_param_register_int("min_alloc", 64*1024*1024);
    mca_ptl_sm_module.sm_max_alloc =
        mca_ptl_sm_param_register_int("max_alloc", 512*1024*1024);
    mca_ptl_sm_module.sm_base_addr = (void*)
        mca_ptl_sm_param_register_int("base_addr", 0x7fffffff);
    mca_ptl_sm_module.sm_free_list_num =
        mca_ptl_sm_param_register_int("free_list_num", 256);
    mca_ptl_sm_module.sm_free_list_max =
        mca_ptl_sm_param_register_int("free_list_max", -1);
    mca_ptl_sm_module.sm_free_list_inc =
        mca_ptl_sm_param_register_int("free_list_inc", 256);

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_ptl_sm_module.sm_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_module.sm_allocator, ompi_allocator_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_module.sm_send_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_module.sm_send_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_module.sm_recv_frags, ompi_free_list_t);
   
    mca_ptl_sm_module.sm_allocator.alc_alloc_fn = mca_ptl_sm_mmap_alloc;
    mca_ptl_sm_module.sm_allocator.alc_free_fn = mca_ptl_sm_mmap_free;

    /* initialize state */
    mca_ptl_sm_module.sm_mmap = NULL;
    return OMPI_SUCCESS;
}


/*
 * module cleanup - sanity checking of queue lengths
 */

int mca_ptl_sm_module_close(void)
{
    OBJ_DESTRUCT(&mca_ptl_sm_module.sm_lock);
    OBJ_DESTRUCT(&mca_ptl_sm_module.sm_send_requests);
    OBJ_DESTRUCT(&mca_ptl_sm_module.sm_send_frags);
    OBJ_DESTRUCT(&mca_ptl_sm_module.sm_recv_frags);
    return OMPI_SUCCESS;
}


/*
 *  SM module initialization
 */
mca_ptl_t** mca_ptl_sm_module_init(
    int *num_ptls, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads)
{
    mca_ptl_t **ptls = NULL;

    *num_ptls = 0;
    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    /* allocate a block of shared memory */
    mca_ptl_sm_module.sm_mmap = mca_ptl_sm_mmap_init(mca_ptl_sm_module.sm_min_alloc);
    if(NULL == mca_ptl_sm_module.sm_mmap)
         return NULL;

    /* initialize free lists */
    ompi_free_list_init(&mca_ptl_sm_module.sm_send_requests, 
        sizeof(mca_ptl_sm_send_request_t),
        OBJ_CLASS(mca_ptl_sm_send_request_t),
        mca_ptl_sm_module.sm_free_list_num,
        mca_ptl_sm_module.sm_free_list_max,
        mca_ptl_sm_module.sm_free_list_inc,
        &mca_ptl_sm_module.sm_allocator); /* use shared-memory allocator */

    ompi_free_list_init(&mca_ptl_sm_module.sm_recv_frags, 
        sizeof(mca_ptl_sm_recv_frag_t),
        OBJ_CLASS(mca_ptl_sm_recv_frag_t),
        mca_ptl_sm_module.sm_free_list_num,
        mca_ptl_sm_module.sm_free_list_max,
        mca_ptl_sm_module.sm_free_list_inc,
        &mca_ptl_sm_module.sm_allocator); /* use default allocator */

    /* publish shared memory parameters with the MCA framework */
    if(mca_ptl_sm_module_exchange() != OMPI_SUCCESS)
        return 0;

    ptls = malloc(sizeof(mca_ptl_t*));
    if(NULL == ptls)
        return NULL;

    *ptls = &mca_ptl_sm.super;
    *num_ptls = 1;
    return ptls;
}

/*
 *  SM module control
 */

int mca_ptl_sm_module_control(int param, void* value, size_t size)
{
    switch(param) {
        case MCA_PTL_ENABLE:
            break;
        default:
            break;
    }
    return OMPI_SUCCESS;
}


/*
 *  SM module progress.
 */

int mca_ptl_sm_module_progress(mca_ptl_tstamp_t tstamp)
{
    return OMPI_SUCCESS;
}


/*
 *
 */

static int mca_ptl_sm_module_exchange()
{
    return OMPI_SUCCESS;
}


