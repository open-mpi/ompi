/*
 * $HEADER$
 */

#include "mpi.h"
#include "lam/mem/malloc.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"

#define mca_pml_teg_param_register_int(n,v) \
    mca_base_param_lookup_int( \
        mca_base_param_register_int("pml","teg",n,0,v))


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

    mca_pml_teg_module_init  /* module init */
};
                                                                                                                            

int mca_pml_teg_module_open(void)
{
    return LAM_SUCCESS;
}


int mca_pml_teg_module_close(void)
{
    return LAM_SUCCESS;
}


mca_pml_t* mca_pml_teg_module_init(int* priority, int* min_thread, int* max_thread)
{
    *priority = 0;
    *min_thread = MPI_THREAD_SINGLE;
    *max_thread = MPI_THREAD_MULTIPLE;

    mca_pml_teg.teg_ptl_modules = 0;
    mca_pml_teg.teg_num_ptl_modules = 0;
    mca_pml_teg.teg_ptls = 0;
    mca_pml_teg.teg_num_ptls = 0;

    lam_list_init(&mca_pml_teg.teg_incomplete_sends);
    lam_mutex_init(&mca_pml_teg.teg_lock);
    return &mca_pml_teg.super;
}

