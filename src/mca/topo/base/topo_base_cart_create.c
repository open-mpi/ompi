/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"
#include "communicator/communicator.h"
#include "group/group.h"
#include "mca/topo/topo.h"
#include "mpi.h"

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
 * OMPI/MPI currently ignores the 'reorder' flag.
 *
 * @retval MPI_SUCCESS
 */                       

int mca_topo_base_cart_create (mca_topo_comm_t *topo_data,
                           int *proc_count,
                           ompi_proc_t **proc_pointers,
                           int *new_rank,
                           int ndims,
                           int *dims,
                           int *periods,
                           bool reorder) {

   int nprocs;
   int dim;
   int i;
   int *p;
   int *coords = topo_data->mtc_coords;

   nprocs = 1;
   p = topo_data->mtc_dims_or_index;

   /* Calculate the number of processes in this grid */
   for (i = 0; i < topo_data->mtc_ndims_or_nnodes; ++i, ++p) {
      if(*p <= 0) {
          return OMPI_ERROR;
       }
       nprocs *= *p;
   }

   /* check for the error condition */

   if (*proc_count < nprocs) {
       return MPI_ERR_DIMS;
   }

   /* check if we have to trim the list of processes */
   if (nprocs < *proc_count) {
       *proc_count = nprocs;
   }
   
   if (*new_rank > (nprocs-1)) {
       /* sorry, but in our scheme this process is cut off */
       *new_rank = MPI_UNDEFINED;
       return MPI_SUCCESS;
   }

   for (i = 0, p = topo_data->mtc_dims_or_index; i < ndims; ++i, ++p) {
       *p = (*periods) ? -(*dims) : *dims;
       ++dims;
       ++periods;
   }

   /* Have to replace this with the actual function body itself */
   p = topo_data->mtc_dims_or_index;
   coords =  topo_data->mtc_coords;

   for (i=0; 
        (i < topo_data->mtc_ndims_or_nnodes); 
        ++i, ++p) {
        dim = (*p > 0) ? *p : -(*p);
        nprocs /= dim;
        *coords++ = *new_rank / nprocs;
        *new_rank %= nprocs;
    }

   /* end here */
   return MPI_SUCCESS;
}
