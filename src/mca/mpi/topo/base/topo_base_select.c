/*
 * $HEADER$
 */

#include "lam_config.h"
#include "mca/mca.h"
#include "mca/mpi/topo/topo.h"
#include "mca/mpi/topo/base/base.h"

int mca_topo_base_select (lam_list_t *selected, 
                          bool *allow_multi_user_thread,
                          bool *have_hidden_threads) {

    /*
     * Nothing to be done as of now
     */
    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    return LAM_SUCCESS;
}
    
