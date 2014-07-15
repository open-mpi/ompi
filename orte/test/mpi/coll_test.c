/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include "opal/mca/hwloc/hwloc.h"
#include "mpi.h"

#include "ompi/mca/rte/rte.h"

#include "orte/util/proc_info.h"

#define COLL_TEST_MAX  100

int main(int argc, char* argv[])
{
    int rank, size, rc;
    hwloc_cpuset_t cpus;
    char *bindings;
    int i, ret;
    ompi_rte_collective_t *coll;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    cpus = hwloc_bitmap_alloc();
    rc = hwloc_get_cpubind(opal_hwloc_topology, cpus, HWLOC_CPUBIND_PROCESS);
    hwloc_bitmap_list_asprintf(&bindings, cpus);

    printf("Hello, World, I am %d of %d [%d local peers]: get_cpubind: %d bitmap %s\n",
           rank, size, orte_process_info.num_local_peers, rc,
           (NULL == bindings) ? "NULL" : bindings);

    for (i=0; i < COLL_TEST_MAX; i++) {
        fprintf(stderr, "%d executing barrier %d\n", rank, i);
        coll = OBJ_NEW(ompi_rte_collective_t);
        coll->id = ompi_rte_get_collective_id(MPI_COMM_WORLD);
        coll->active = true;
        if (OMPI_SUCCESS != (ret = ompi_rte_barrier(coll))) {
            OMPI_ERROR_LOG(ret);
            return ret;
        }
        OMPI_LAZY_WAIT_FOR_COMPLETION(coll->active);
        OBJ_RELEASE(coll);
    }

    for (i=0; i < COLL_TEST_MAX; i++) {
        fprintf(stderr, "%d executing modex %d\n", rank, i);
        coll = OBJ_NEW(ompi_rte_collective_t);
        coll->id = ompi_rte_get_collective_id(MPI_COMM_WORLD);
        coll->active = true;
        if (OMPI_SUCCESS != (ret = ompi_rte_modex(coll))) {
            OMPI_ERROR_LOG(ret);
            return ret;
        }
        OMPI_LAZY_WAIT_FOR_COMPLETION(coll->active);
        OBJ_RELEASE(coll);
    }

    MPI_Finalize();
    return 0;
}
