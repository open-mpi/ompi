/*
 * $HEADER$
 */

#include "mca/topo/base/base.h"
#include "communicator/communicator.h"
#include "mca/topo/topo.h"

/*
 *
 * function - makes a new communicator to which topology information
 *            has been attached
 *
 * @param comm_old input communicator without topology (handle)
 * @param nnodes number of nodes in graph (integer)
 * @param index array of integers describing node degrees (see below)
 * @param edges array of integers describing graph edges (see below)
 * @param reorder ranking may be reordered (true) or not (false) (logical)
 * @param comm_graph communicator with graph topology added (handle)
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_OUT_OF_RESOURCE
 */

int topo_base_graph_create (MPI_Comm comm_old,
                            int nnodes,
                            int *index,
                            int *edges,
                            int reorder,
                            MPI_Comm *comm_graph) {
#if 0
    MPI_Group newgroup;
#endif
    int  nedges;
    int  size;
    int  err;
    int  range[1][3];
    int  i;
    int  *topo;
    int  *p;

    /*
     * Create and error check the topology information.
     */
    nedges = index[nnodes - 1];
    topo = (int *) malloc((unsigned) (nnodes + nedges) * sizeof(int));
    if (topo == 0) {
        printf ("Out of resources\n");
        return MPI_ERR_SYSRESOURCE;
    }

    for (i = 0, p = topo; i < nnodes; ++i, ++p) {
        *p = *index++;
    }

    for (i = 0; i < nedges; ++i, ++p) {
         *p = *edges++;
         if (*p < 0 || *p >= nnodes) {
            free((char *) topo);
            return MPI_ERR_TOPOLOGY;
         }
    }
    /*
     * Create the group for the new communicator.
     */
#if 0
    err = ompi_comm_size (comm_old, &size);
#endif
    if (err != MPI_SUCCESS) {
        free((char *) topo);
        return err;
    }

    if (nnodes > size) {
        free((char *) topo);
        return MPI_ERR_ARG;
    }

    if (nnodes == size) {
#if 0
        err = ompi_comm_group (comm_old, &newgroup);
#endif
    } else {
        range[0][0] = 0;
        range[0][1] = nnodes - 1;
        range[0][2] = 1;
#if 0
        err = ompi_group_range_incl(comm_old->c_group, 1, range, &newgroup);
#endif
    }
    if (err != MPI_SUCCESS) {
        free((char *) topo);
        return err;
    }
    /*
     * Create the new communicator.
     */
#if 0
    err = ompi_comm_create (comm_old, newgroup, comm_graph);
#endif
    if (err != MPI_SUCCESS) {
         free((char *) topo);
#if 0
         ompi_group_free (&newgroup);
#endif
         return err;
    }
    /*
     * Set the communicator topology information.
     */
    if (*comm_graph != MPI_COMM_NULL) {
        (*comm_graph)->c_flags |= OMPI_COMM_GRAPH;
        (*comm_graph)->c_topo_comm->mtc_type = MPI_GRAPH;
        (*comm_graph)->c_topo_comm->mtc_nprocs = nnodes;
        (*comm_graph)->c_topo_comm->mtc_nedges = nedges;
        (*comm_graph)->c_topo_comm->mtc_index = topo;
        (*comm_graph)->c_topo_comm->mtc_edges = topo + nnodes;
     }

#if 0
    err = ompi_group_free (&newgroup);
#endif
    if (err != MPI_SUCCESS) {
        return err;
    }

    return(MPI_SUCCESS);
}
