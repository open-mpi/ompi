#include <stdio.h>
#include "qsnet/fence.h"
#include "ptl_elan.h"
#include "ptl_elan_priv.h"
#include "test_util.h"

extern mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module;

int main (int argc, char ** argv)
{
    bool  allow_threads;
    bool  have_hidden_threads;
    int   input;
    int   num;

    /* Get some environmental variables set for Open MPI, OOB */
    env_init_for_elan();

    /* Initialization test */
    mca_ptl_elan_module_open();
    mca_ptl_elan_module_init(&num, &allow_threads, &have_hidden_threads);
    mca_ptl_elan_module_control(1, &input, 4);
    mca_ptl_elan_module_close();

    /* Tell alive */
    fprintf(stdout, "I am still alive\n");
    fflush(stdout);
    return 0;
}
