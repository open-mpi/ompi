#include "qsnet/fence.h"
#include "ptl_elan.h"
#include "ptl_elan_priv.h"

extern mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module;

int test_rdma(mca_ptl_elan_module_1_0_0_t *emp);

int main (int argc, char ** argv)
{
    /* Initialization test */
    ompi_mca_ptl_elan_init (&mca_ptl_elan_module);

    /* Please add a barrier at the beginning */
#if 0
    test_rdma(&mca_ptl_elan_module);
#endif

    /* Please add a barrier at the end */

    /* Finalize the device */
    ompi_mca_ptl_elan_finalize (&mca_ptl_elan_module);

    /* Tell alive */
    fprintf(stdout, "I am still alive\n");
    fflush(stdout);
    return 0;
}

int test_rdma(mca_ptl_elan_module_1_0_0_t *emp, int reps)
{
    /* To test rdma (put/get) function */
    return 0;
}
