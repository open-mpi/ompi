#include "mca/mpi/pml/teg/teg.h"


mca_pml_module_1_0_0_t mca_pml_teg_module_1_0_0_0 = {
    {
    1,  /* MCA major version */
    0,  /* MCA minor version */
    0,  /* MCA release version */
    "pml", /* MCA type name */
    1,  /* MCA type major version */
    0,  /* MCA type minor version */
    0,  /* MCA type release version */
    "teg", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_pml_teg_open,  /* module open */
    mca_pml_teg_close,  /* module close */
    false,
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
                                                                                                 
                                                                                                 
