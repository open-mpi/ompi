#include "mca/mpi/pml/pml.h"
#include "mca/mpi/pml/teg/teg.h"


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

    mca_pml_teg_query,  /* module query */
    mca_pml_teg_init  /* module init */
};


mca_pml_teg_1_0_0_t mca_pml_teg = {
    {
    mca_pml_teg_addprocs,
    mca_pml_teg_isend,
    mca_pml_teg_progress
    },
};



mca_pml_1_0_0_t* mca_pml_teg_init(
    struct lam_proc_t **procs,
    int nprocs,
    int *max_tag,
    int *max_cid)
{
    lam_frl_init(&mca_pml_teg.teg_send_requests);
    lam_frl_init(&mca_pml_teg.teg_recv_requests);
    return &mca_pml_teg.super;
}
