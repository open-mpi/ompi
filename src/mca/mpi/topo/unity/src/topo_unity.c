/*
 * $HEADER$
 */
#include "lam_config.h"
#include "mca/mpi/topo/topo.h"
#include "mca/mpi/topo/base/base.h"


/*
 * Init on the communicator. This function is called once the
 * module has been selected for a particular communicator. As
 * of now, do nothing
 */
int mca_topo_unity_init (lam_communicator_t *comm, 
                         const mca_topo_t **new_topo) {
    /* 
     * Nothing to init on the communicator
     */
    return LAM_SUCCESS;
}
