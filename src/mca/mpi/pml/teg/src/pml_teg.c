#include "mca/mpi/pml/base/pml_base_sendreq.h"
#include "mca/mpi/pml/base/pml_base_recvreq.h"
#include "pml_teg.h"

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
    mca_pml_teg_open,  /* module open */
    mca_pml_teg_close  /* module close */
    },
                                                                                                                            
    /* Next the MCA v1.0.0 module meta data */
                                                                                                                            
    {
    /* Whether the module is checkpointable or not */
                                                                                                                            
    false
    },

    mca_pml_teg_init  /* module init */
};
                                                                                                                            

mca_pml_teg_t mca_pml_teg = {
    {
    mca_pml_teg_add_procs,
    mca_pml_teg_add_ptls,
    mca_pml_teg_fini,
    mca_pml_teg_irecv_init,
    mca_pml_teg_irecv,
    mca_pml_teg_isend_init,
    mca_pml_teg_isend,
    mca_pml_teg_progress,
    mca_pml_teg_start,
    mca_pml_teg_test,
    mca_pml_teg_wait,
    }
};


int mca_pml_teg_open(lam_cmd_line_t* cmd_line)
{
    return LAM_SUCCESS;
}


mca_pml_1_0_0_t* mca_pml_teg_init(int* priority, int* min_thread, int* max_thread)
{
    *priority = 0;
    *min_thread = 0;
    *max_thread = 0;
    return &mca_pml_teg.super;
}

