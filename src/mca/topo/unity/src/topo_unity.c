/*
 * $HEADER$
 */
#include "mca/topo/unity/src/topo_unity.h"

/*
 * Init on the communicator. This function is called once the
 * module has been selected for a particular communicator. As
 * of now, do nothing
 */
int mca_topo_unity_init (MPI_Comm comm, 
                         const mca_topo_t **new_topo) {
    /* 
     * Nothing to init on the communicator
     */
    return OMPI_SUCCESS;
}
