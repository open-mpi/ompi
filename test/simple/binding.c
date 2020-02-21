/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sched.h>
#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal.h"
#include "mpi.h"

#include "orte/util/proc_info.h"

int main(int argc, char* argv[])
{
    int rank, size, rc;
    hwloc_cpuset_t cpus;
    char *bindings = NULL;
    cpu_set_t *mask;
    int nrcpus, c;
    size_t csize;
    const char *hostname;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    hostname = opal_gethostname();

    if (OPAL_SUCCESS == opal_hwloc_base_get_topology()) {
        cpus = hwloc_bitmap_alloc();
        rc = hwloc_get_cpubind(opal_hwloc_topology, cpus, HWLOC_CPUBIND_PROCESS);
        hwloc_bitmap_list_asprintf(&bindings, cpus);
    }

    printf("[%s;%d] Hello, World, I am %d of %d [%d local peers]: get_cpubind: %d bitmap %s\n",
           hostname, (int)getpid(), rank, size, orte_process_info.num_local_peers, rc,
           (NULL == bindings) ? "NULL" : bindings);

    nrcpus = sysconf(_SC_NPROCESSORS_ONLN);
    mask = CPU_ALLOC(nrcpus);
    csize = CPU_ALLOC_SIZE(nrcpus);
    CPU_ZERO_S(csize, mask);
    if ( sched_getaffinity(0, csize, mask) == -1 ) {
        perror("sched_getaffinity");
    } else {
        for ( c = 0; c < nrcpus; c++ ) {
                if ( CPU_ISSET_S(c, csize, mask) ) {
                        printf("[%s:%d] CPU %d is set\n", hostname, (int)getpid(), c);
                }
        }
    }

    CPU_FREE(mask);

    MPI_Finalize();
    return 0;
}
