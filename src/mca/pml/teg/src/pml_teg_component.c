/*
 * $HEADER$
 */

#include "event/event.h"
#include "mpi.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/base/mca_base_param.h"
#include "mca/pml/base/pml_base_bsend.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"


mca_pml_base_component_1_0_0_t mca_pml_teg_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      /* Indicate that we are a pml v1.0.0 component (which also implies
         a specific MCA version) */

      MCA_PML_BASE_VERSION_1_0_0,
    
      "teg", /* MCA component name */
      1,  /* MCA component major version */
      0,  /* MCA component minor version */
      0,  /* MCA component release version */
      mca_pml_teg_component_open,  /* component open */
      mca_pml_teg_component_close  /* component close */
    },

    /* Next the MCA v1.0.0 component meta data */

    {
      /* Whether the component is checkpointable or not */
      false
    },

    mca_pml_teg_component_init,  /* component init */
    mca_pml_teg_component_fini   /* component finalize */
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
                                                                                                                        

int mca_pml_teg_component_open(void)
{
    mca_pml_base_request_t* teg_null = &mca_pml_teg.teg_request_null;
    OBJ_CONSTRUCT(&mca_pml_teg.teg_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_send_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_recv_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_procs, ompi_list_t);

    OBJ_CONSTRUCT(teg_null, mca_pml_base_request_t);
    teg_null->req_type = MCA_PML_REQUEST_NULL;
    teg_null->req_status.MPI_SOURCE = OMPI_PROC_NULL;
    teg_null->req_status.MPI_TAG = OMPI_ANY_TAG;
    teg_null->req_status.MPI_ERROR = OMPI_SUCCESS;
    teg_null->req_status._count = 0;

#if MCA_PML_TEG_STATISTICS
    mca_pml_teg.teg_waits = 0;
    mca_pml_teg.teg_sends = 0;
    mca_pml_teg.teg_recvs = 0;
    mca_pml_teg.teg_isends = 0;
    mca_pml_teg.teg_irecvs = 0;
    mca_pml_teg.teg_condition_waits = 0;
    mca_pml_teg.teg_condition_broadcasts = 0;
#endif

    mca_pml_teg.teg_free_list_num =
        mca_pml_teg_param_register_int("free_list_num", 256);
    mca_pml_teg.teg_free_list_max =
        mca_pml_teg_param_register_int("free_list_max", -1);
    mca_pml_teg.teg_free_list_inc =
        mca_pml_teg_param_register_int("free_list_inc", 256);
    mca_pml_teg.teg_poll_iterations =
        mca_pml_teg_param_register_int("poll_iterations", 100000);
    return OMPI_SUCCESS;
}


int mca_pml_teg_component_close(void)
{
#if MCA_PML_TEG_STATISTICS && OMPI_ENABLE_DEBUG
    ompi_output(0, "mca_pml_teg.teg_waits = %d\n", 
        mca_pml_teg.teg_waits);
    ompi_output(0, "mca_pml_teg.teg_sends = %d\n", 
        mca_pml_teg.teg_sends);
    ompi_output(0, "mca_pml_teg.teg_recvs = %d\n", 
        mca_pml_teg.teg_recvs);
    ompi_output(0, "mca_pml_teg.teg_isends = %d\n", 
        mca_pml_teg.teg_isends);
    ompi_output(0, "mca_pml_teg.teg_irecvs = %d\n", 
        mca_pml_teg.teg_irecvs);
    ompi_output(0, "mca_pml_teg.teg_condition_waits = %d\n", 
        mca_pml_teg.teg_condition_waits);
    ompi_output(0, "mca_pml_teg.teg_condition_broadcast = %d\n", 
        mca_pml_teg.teg_condition_broadcasts);
#endif

    if (mca_pml_teg.teg_recv_requests.fl_num_allocated !=
        mca_pml_teg.teg_recv_requests.super.ompi_list_length) {
        ompi_output(0, "teg recv requests: %d allocated %d returned\n",
            mca_pml_teg.teg_recv_requests.fl_num_allocated,
            mca_pml_teg.teg_recv_requests.super.ompi_list_length);
    }

    if(NULL != mca_pml_teg.teg_ptl_components) {
        free(mca_pml_teg.teg_ptl_components);
    }
    if(NULL != mca_pml_teg.teg_ptl_components) {
        free(mca_pml_teg.teg_ptl_components);
    }
    OBJ_DESTRUCT(&mca_pml_teg.teg_send_requests);
    OBJ_DESTRUCT(&mca_pml_teg.teg_recv_requests);
    OBJ_DESTRUCT(&mca_pml_teg.teg_procs);
    OBJ_DESTRUCT(&mca_pml_teg.teg_lock);
    return OMPI_SUCCESS;
}


mca_pml_base_module_t* mca_pml_teg_component_init(int* priority, 
                                                  bool *allow_multi_user_threads,
                                                  bool *have_hidden_threads)
{
    *priority = 0;
    *have_hidden_threads = false;

    OBJ_CONSTRUCT(&mca_pml_teg.teg_lock, ompi_mutex_t);
    mca_pml_teg.teg_ptl_components = NULL;
    mca_pml_teg.teg_num_ptl_components = 0;
    mca_pml_teg.teg_ptl_components = NULL;
    mca_pml_teg.teg_num_ptl_components = 0;

    /* recv requests */
    ompi_free_list_init(
        &mca_pml_teg.teg_recv_requests,
        sizeof(mca_pml_base_recv_request_t),
        OBJ_CLASS(mca_pml_base_recv_request_t), 
        mca_pml_teg.teg_free_list_num,
        mca_pml_teg.teg_free_list_max,
        mca_pml_teg.teg_free_list_inc,
        NULL);

    /* request completion */
    OBJ_CONSTRUCT(&mca_pml_teg.teg_request_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_pml_teg.teg_request_cond, ompi_condition_t);
    mca_pml_teg.teg_request_waiting = 0;

    /* buffered send */
    if(mca_pml_base_bsend_init(allow_multi_user_threads) != OMPI_SUCCESS) {
        ompi_output(0, "mca_pml_teg_component_init: mca_pml_bsend_init failed\n");
        return NULL;
    }
    *allow_multi_user_threads &= true;
    return &mca_pml_teg.super;
}

