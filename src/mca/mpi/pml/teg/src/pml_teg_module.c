/*
 * $HEADER$
 */

#include "lam/event/event.h"
#include "mpi.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/lam/base/mca_base_param.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"



mca_pml_base_module_1_0_0_t mca_pml_teg_module = {
    /* First, the mca_base_module_t struct containing meta information
       about the module itself */
                                                                                                                            
    {
    /* Indicate that we are a pml v1.0.0 module (which also implies a
       specific MCA version) */
                                                                                                                            
    MCA_PML_BASE_VERSION_1_0_0,
                                                                                                                            
    "teg", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_pml_teg_module_open,  /* module open */
    mca_pml_teg_module_close  /* module close */
    },
                                                                                                                            
    /* Next the MCA v1.0.0 module meta data */
                                                                                                                            
    {
    /* Whether the module is checkpointable or not */
                                                                                                                            
    false
    },

    mca_pml_teg_module_init,  /* module init */
    mca_pml_teg_module_fini   /* module finalize */
};



static inline int mca_pml_teg_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("pml","teg",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                                        

int mca_pml_teg_module_open(void)
{
    OBJ_CONSTRUCT(&mca_pml_teg.teg_lock, lam_mutex_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_recv_requests, lam_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_procs, lam_list_t);

    mca_pml_teg.teg_free_list_num =
        mca_pml_teg_param_register_int("free_list_num", 256);
    mca_pml_teg.teg_free_list_max =
        mca_pml_teg_param_register_int("free_list_max", -1);
    mca_pml_teg.teg_free_list_inc =
        mca_pml_teg_param_register_int("free_list_inc", 256);
    mca_pml_teg.teg_poll_iterations =
        mca_pml_teg_param_register_int("poll_iterations", 10000);
    return LAM_SUCCESS;
}


int mca_pml_teg_module_close(void)
{
    if(NULL != mca_pml_teg.teg_ptl_modules)
        free(mca_pml_teg.teg_ptl_modules);
    if(NULL != mca_pml_teg.teg_ptls)
        free(mca_pml_teg.teg_ptls);
    OBJ_DESTRUCT(&mca_pml_teg.teg_recv_requests);
    OBJ_DESTRUCT(&mca_pml_teg.teg_procs);
    OBJ_DESTRUCT(&mca_pml_teg.teg_lock);
    return LAM_SUCCESS;
}


static void* mca_pml_teg_thread(lam_object_t* thread)
{
    lam_event_dispatch();
    return NULL;
}


mca_pml_t* mca_pml_teg_module_init(int* priority, 
                                   bool *allow_multi_user_threads,
                                   bool *have_hidden_threads)
{
    *priority = 0;
    *allow_multi_user_threads = true;
    *have_hidden_threads = true;

    OBJ_CONSTRUCT(&mca_pml_teg.teg_lock, lam_mutex_t);
    mca_pml_teg.teg_ptl_modules = NULL;
    mca_pml_teg.teg_num_ptl_modules = 0;
    mca_pml_teg.teg_ptls = NULL;
    mca_pml_teg.teg_num_ptls = 0;

    /* recv requests */
    lam_free_list_init(
        &mca_pml_teg.teg_recv_requests,
        sizeof(mca_ptl_base_recv_request_t),
        OBJ_CLASS(mca_ptl_base_recv_request_t), 
        mca_pml_teg.teg_free_list_num,
        mca_pml_teg.teg_free_list_max,
        mca_pml_teg.teg_free_list_inc,
        NULL);
        
    /* recv sequence */
    mca_pml_teg.teg_recv_sequence = 0;

    /* request completion */
    OBJ_CONSTRUCT(&mca_pml_teg.teg_request_lock, lam_mutex_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_request_cond, lam_condition_t);
    mca_pml_teg.teg_request_waiting = 0;

    /* event dispatch thread */
    OBJ_CONSTRUCT(&mca_pml_teg.teg_thread, lam_thread_t);
    mca_pml_teg.teg_thread.t_run = mca_pml_teg_thread;
    if(lam_thread_start(&mca_pml_teg.teg_thread) != LAM_SUCCESS)
        return NULL;

    return &mca_pml_teg.super;
}

