/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include "opal/mca/hwloc/hwloc.h"
#include "mpi.h"

#include "orte/util/proc_info.h"

int main(int argc, char* argv[])
{
    int rank, size, rc;
    hwloc_cpuset_t cpus;
    char *bindings;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    cpus = hwloc_bitmap_alloc();
    rc = hwloc_get_cpubind(opal_hwloc_topology, cpus, HWLOC_CPUBIND_PROCESS);
    hwloc_bitmap_list_asprintf(&bindings, cpus);

    printf("Hello, World, I am %d of %d [%d local peers]: get_cpubind: %d bitmap %s\n",
           rank, size, orte_process_info.num_local_peers, rc,
           (NULL == bindings) ? "NULL" : bindings);

    MPI_Finalize();
    return 0;
}
