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
    mca_pml_teg_close  /* module close */
    },
    mca_pml_teg_query,  /* module query */
    mca_pml_teg_init    /* module init */
    true,               /* checkpoint/restart */
};

