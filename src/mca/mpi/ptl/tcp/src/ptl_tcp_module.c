#include "lam/util/malloc.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "ptl_tcp.h"

#define mca_ptl_tcp_param_register_int(n,v) \
    mca_base_param_lookup_int( \
        mca_base_param_register_int("ptl","tcp",n,0,v))


mca_ptl_tcp_module_1_0_0_t mca_ptl_tcp_module = {
    {
    /* First, the mca_base_module_t struct containing meta information
       about the module itself */
                                                                                                                            
    {
    /* Indicate that we are a pml v1.0.0 module (which also implies a
       specific MCA version) */
                                                                                                                            
    MCA_PML_BASE_VERSION_1_0_0,
                                                                                                                            
    "tcp", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_ptl_tcp_module_open,  /* module open */
    mca_ptl_tcp_module_close  /* module close */
    },
                                                                                                                            
    /* Next the MCA v1.0.0 module meta data */
                                                                                                                            
    {
    /* Whether the module is checkpointable or not */
                                                                                                                            
    false
    },

    mca_ptl_tcp_module_init,    /* module init */
    mca_ptl_tcp_module_progress /* module progress */
    }
};
                                                                                                                            
/**
 *  some comment
 *
 *  @param foo description
 *  @return 
 *
 *  long description
 */

int mca_ptl_tcp_module_open(void)
{
    return LAM_SUCCESS;
}

int mca_ptl_tcp_module_close(void)
{
    return LAM_SUCCESS;
}

mca_ptl_t** mca_ptl_tcp_module_init(int* num_tcps, int* thread_min, int* thread_max)
{
    lam_reactor_init(&mca_ptl_tcp_module.tcp_reactor);
    return NULL;
}

void mca_ptl_tcp_module_progress(mca_ptl_base_tstamp_t tstamp)
{
    lam_reactor_poll(&mca_ptl_tcp_module.tcp_reactor);
}

