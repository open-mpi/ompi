/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"

/*
 * function - partitions a communicator into subgroups which
 *            form lower-dimensional cartesian subgrids
 *            
 * @param comm communicator with cartesian structure (handle)
 * @param remain_dims the 'i'th entry of 'remain_dims' specifies whether 
 *                the 'i'th dimension is kept in the subgrid (true) 
 *                or is dropped (false) (logical vector)
 * @param new_comm communicator containing the subgrid that includes the
 *                 calling process (handle)
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_TOPOLOGY
 * @retval MPI_ERR_COMM
 */                
int topo_base_cart_sub (lam_communicator_t *comm,
                        int *remain_dims,
                        lam_communicator_t **new_comm){
     lam_communicator_t *newcomm;
     int errcode;
     int colour;
     int key;
     int colfactor;
     int keyfactor;
     int rank;
     int ndim;
     int dim;
     int allfalse;
     int i;
     int *d;
     int *c;
     int *r;
     int *p;

    /*
     * Compute colour and key used in splitting the communicator.
     */
     colour = key = 0;
     colfactor = keyfactor = 1;
     ndim = 0;
     allfalse = 0;

     i = comm->c_topo_ndims - 1;
     d = comm->c_topo_dims + i;
     c = comm->c_topo_coords + i;
     r = remain_dims + i;

     for (; i >= 0; --i, --d, --c, --r) {
        dim = (*d > 0) ? *d : -(*d);
        if (*r == 0) {
           colour += colfactor * (*c);
           colfactor *= dim;
        } else {
          ++ndim;
          key += keyfactor * (*c);
          keyfactor *= dim;
        }
     }
    /*
     * Special case: if all of remain_dims were false, we need to make
     * a cartesian communicator with just ourselves in it (you can't
     * have a communicator unless you're in it).
     */
     if (ndim == 0) {
        lam_comm_rank (comm, &colour);
        ndim = 1;
        allfalse = 1;
     }
    /*
     * Split the communicator.
     */
     errcode = lam_comm_split (comm, colour, key, new_comm);
     if (errcode != MPI_SUCCESS) {
        return errcode;
     }
    /*
     * Fill the communicator with topology information.
     */
     newcomm = *new_comm;
     if (newcomm != MPI_COMM_NULL) {
        newcomm->c_topo_type = MPI_CART;
        newcomm->c_topo_nprocs = keyfactor;
        newcomm->c_topo_ndims = ndim;
        newcomm->c_topo_dims = (int *)
        malloc((unsigned) 2 * ndim * sizeof(int));
        if (newcomm->c_topo_dims == 0) {
            return MPI_ERR_OTHER;
        }
        newcomm->c_topo_coords = newcomm->c_topo_dims + ndim;
        if (!allfalse) {
           p = newcomm->c_topo_dims;
           d = comm->c_topo_dims;
           r = remain_dims;
           for (i = 0; i < comm->c_topo_ndims; ++i, ++d, ++r) {
             if (*r) {
                 *p++ = *d;
              }
           }
           } else {
             newcomm->c_topo_dims[0] = 1;
           }
          /*
           * Compute the caller's coordinates.
           */
          errcode = lam_comm_rank (newcomm, &rank);
          if (errcode != MPI_SUCCESS) {
             return errcode;
          }
          errcode = lam_cart_coords (newcomm, rank,
                                     ndim, newcomm->c_topo_coords);
          if (errcode != MPI_SUCCESS) {
             return errcode;
          }
      }

   return MPI_SUCCESS;
}
