/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"
#include "communicator/communicator.h"
#include "mca/topo/topo.h"

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

int topo_base_cart_create (MPI_Comm old_comm,
                           int ndims,
                           int *dims,
                           int *periods,
                           int reorder,
                           MPI_Comm *comm_cart){
   MPI_Comm newcomm;
#if 0
   MPI_Group newgroup;
#endif
   int rank;
   int size;
   int nprocs;
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
#if 0
   err = lam_comm_size (comm, &size);
#endif
   if (err != MPI_SUCCESS) {
       return err;
   }

  if (nprocs > size) {
      return MPI_ERR_DIMS;
   }

   if (nprocs == size) {
#if 0
      err = lam_comm_group (comm, &newgroup);
#endif
   } else {
      range[0][0] = 0;
      range[0][1] = nprocs - 1;
      range[0][2] = 1;
#if 0
      err = lam_group_range_incl (comm->c_group, 1, range, &newgroup);
#endif
   }

   if (err != MPI_SUCCESS) {
       return err;
   }
  /*
   * Create the new communicator.
   */
#if 0
   err = lam_comm_create (comm, newgroup, comm_cart);
#endif
   if (err != MPI_SUCCESS) {
#if 0
       lam_group_free (&newgroup);
#endif
       return err;
   }
   /*
    * Fill the communicator with topology information.
    */
   newcomm = *comm_cart;
   if (newcomm != MPI_COMM_NULL) {
      newcomm->c_flags |= LAM_COMM_CART;
      newcomm->c_topo_comm->mtc_type = MPI_CART;
      newcomm->c_topo_comm->mtc_nprocs = nprocs;
      newcomm->c_topo_comm->mtc_ndims = ndims;
      newcomm->c_topo_comm->mtc_dims = (int *)
                             malloc((unsigned) 2 * ndims * sizeof(int));
      if (newcomm->c_topo_comm->mtc_dims == 0) {
         return MPI_ERR_OTHER;
      }
      newcomm->c_topo_comm->mtc_coords = newcomm->c_topo_comm->mtc_dims + ndims;
      for (i = 0, p = newcomm->c_topo_comm->mtc_dims; i < ndims; ++i, ++p) {
          *p = (*periods) ? -(*dims) : *dims;
          ++dims;
          ++periods;
      }
      /*
       * Compute the caller's coordinates.
       */
#if 0
       err = lam_comm_rank (newcomm, &rank);
#endif
       if (err != MPI_SUCCESS) {
           return err;
       }

       err = newcomm->c_topo.topo_cart_coords (newcomm, rank,
                             ndims, newcomm->c_topo_comm->mtc_coords);
       if (err != MPI_SUCCESS) {
           return err;
       }
   }

#if 0
   err = lam_group_free (&newgroup);
#endif
   if (err != MPI_SUCCESS) {
      return err;
   }

   return MPI_SUCCESS;
}
