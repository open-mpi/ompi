/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"

/*
 * function - retrieves Cartesian topology information associated with a
 *            communicator
 *
 * @param comm communicator with cartesian structure (handle)
 * @param maxdims length of vectors  'dims', 'periods', and 'coords'
 *                 in the calling program (integer)
 * @param dims number of processes for each cartesian dimension (array of integer)
 * @param periods periodicity (true/false) for each cartesian dimension
 *                (array of logical)
 * @param coords coordinates of calling process in cartesian structure
 *               (array of integer)
 *
 * @retval MPI_SUCCESS
 */
int topo_base_cart_get (lam_communicator_t *comm,
                        int maxdims,
                        int *dims,
                        int *periods,
                        int *coords){
    int i;
    int *d;
    int *c;

    d = comm->c_topo_dims;
    c = comm->c_topo_coords;
    for (i = 0; (i < comm->c_topo_ndims) && (i < maxdims); ++i) {
        if (*d > 0) {
            *dims++ = *d++;
            *periods++ = 0;
        } else {
           *dims++ = -(*d++);
           *periods++ = 1;
        }
        *coords++ = *c++;
    }

    return MPI_SUCCESS;
}
