/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"

/*
 * function - makes a new communicator to which topology information
 *            has been attached
 *
 * @param comm input communicator (handle)
 * @param ndims number of dimensions of cartesian grid (integer)
 * @param dims integer array of size ndims specifying the number of processes in
 *             each dimension
 * @param periods logical array of size ndims specifying whether the grid is
 *                periodic (true) or not (false) in each dimension
 * @param reorder ranking may be reordered (true) or not (false) (logical)
 * @param comm_cart communicator with new cartesian topology (handle)
 *
 * LAM/MPI currently ignores the 'reorder' flag.
 *
 * @retval MPI_SUCCESS
 */                       

int topo_base_cart_create (lam_communicator_t *old_comm,
                           int ndims,
                           int *dims,
                           int *periods,
                           int reorder,
                           lam_communicator_t *comm_cart){
   lam_groupt_t *newcomm;
   lam_group_t *newgroup;
   int size;
   int nprocs;
   int rank;
   int err;
   int range[1][3];
   int i;
   int *p;

  /*
   * Compute the # of processes in the grid.
   */
   nprocs = 1;
   for (i = 0, p = dims; i < ndims; ++i, ++p) {
      if (*p <= 0) {
         return MPI_ERR_DIMS;
       }
       nprocs *= *p;
   }
  /*
   * Create the group for the new communicator.
   */
   err = lam_comm_size (comm, &size);
   if (err != MPI_SUCCESS) {
       return err;
   }

  if (nprocs > size) {
      return MPI_ERR_DIMS;
   }

   if (nprocs == size) {
      err = lam_comm_group (comm, &newgroup);
   } else {
      range[0][0] = 0;
      range[0][1] = nprocs - 1;
      range[0][2] = 1;
      err = lam_group_range_incl (comm->c_group, 1, range, &newgroup);
   }

   if (err != MPI_SUCCESS) {
       return err;
   }
  /*
   * Create the new communicator.
   */
   err = lam_comm_create (comm, newgroup, comm_cart);
   if (err != MPI_SUCCESS) {
       lam_group_free (&newgroup);
       return err;
   }
   /*
    * Fill the communicator with topology information.
    */
   newcomm = *comm_cart;
   if (newcomm != MPI_COMM_NULL) {
      newcomm->c_topo_type = MPI_CART;
      newcomm->c_topo_nprocs = nprocs;
      newcomm->c_topo_ndims = ndims;
      newcomm->c_topo_dims = (int *)
                             malloc((unsigned) 2 * ndims * sizeof(int));
      if (newcomm->c_topo_dims == 0) {
         return MPI_ERR_OTHER;
      }
      newcomm->c_topo_coords = newcomm->c_topo_dims + ndims;
      for (i = 0, p = newcomm->c_topo_dims; i < ndims; ++i, ++p) {
          *p = (*periods) ? -(*dims) : *dims;
          ++dims;
          ++periods;
      }
      /*
       * Compute the caller's coordinates.
       */
       err = lam_comm_rank (newcomm, &rank);
       if (err != MPI_SUCCESS) {
           return err;
       }

       err = lam_cart_coors (newcomm, rank,
                             ndims, newcomm->c_topo_coords);
       if (err != MPI_SUCCESS) {
           return err;
       }
   }

   err = lam_group_free (&newgroup);
   if (err != MPI_SUCCESS) {
      return err;
   }

   return MPI_SUCCESS;
}
