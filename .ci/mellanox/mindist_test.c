#include "mpi.h"

#define _GNU_SOURCE
#include <sched.h>
#include <numa.h>

#include <errno.h>
#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int get_cores_number(void)
{
    int cores_number;
    char *envstr;

    cores_number = -1;
    envstr = getenv("TEST_PHYS_ID_COUNT");
    if (NULL != envstr) {
        cores_number = (int)strtol(envstr, NULL, 10);
        envstr = getenv("TEST_CORE_ID_COUNT");
        if (NULL != envstr) {
            cores_number *= (int)strtol(envstr, NULL, 10);
        }
    }

    return cores_number;
}

int get_closest_numa(char *hca)
{
    int cnuma;
    char *numa;
    cnuma = -1;
    numa = getenv("TEST_CLOSEST_NUMA");
    if (NULL != numa) {
        cnuma = (int)strtol(numa, NULL, 10);
    }
    return cnuma;
}

int get_numa_cores_number(void)
{
    int cores_number;
    char *envstr;

    cores_number = -1;
    envstr = getenv("TEST_CORE_ID_COUNT");
    if (NULL != envstr) {
        cores_number = (int)strtol(envstr, NULL, 10);
    }

    return cores_number;
}

int main(int argc, char* argv[])
{
    char *dist_hca = NULL, *policy = NULL, *pch;
    cpu_set_t cpuset;

    int i, rc, my_rank,
	    numcpus, size;
    int numa = -1, next, numa_node;
    int num_numa_cores;

    numcpus = get_cores_number();
	if (numcpus < 0) {
		fprintf(stderr, "\nrank = %d: Bad CPUs number. Skip.\n", my_rank);
		fflush(stderr);
		return 1;
	}
    
    rc = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != rc) {
        printf ("\nrank - %d: Error starting MPI program. Skip.\n", my_rank);
        MPI_Abort(MPI_COMM_WORLD, rc);
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    
    num_numa_cores = get_numa_cores_number();
    if (size > num_numa_cores) {
		fprintf(stderr, "\nrank - %d: number of processes exceeds number of cores at a single numa node. Test won't get correct results in this case: num_procs = %d, num_cores = %d. Skip.\n", my_rank, size, num_numa_cores);
		fflush(stderr);
        MPI_Finalize();
        return 1;
    }

    policy = getenv("OMPI_MCA_rmaps_base_mapping_policy");
    dist_hca = getenv("OMPI_MCA_rmaps_dist_device");
    if (NULL != dist_hca) {
        dist_hca = strdup(dist_hca);
        if (NULL != (pch = strchr(dist_hca, ':'))) {
            *pch = '\0';
        }
    } else if (NULL != policy) {
        dist_hca = strstr(policy, "dist:");
        dist_hca += strlen("dist:");
        dist_hca = strdup(dist_hca);
        if (NULL != (pch = strchr(dist_hca, ','))) {
            *pch = '\0';
        }
    }

    if (NULL == policy || NULL == dist_hca) {
		fprintf(stderr, "\nrank - %d: the \"dist\" mapping policy was not specified. Skip.\n", my_rank);
		fflush(stderr);
        MPI_Finalize();
		return 1;
	}
    
    numa_node = get_closest_numa(dist_hca);
    if (-1 == numa_node) {
        fprintf(stderr, "\nrank - %d: info about locality to %s isn't provided by the BIOS. Skip.\n", my_rank, dist_hca);
        fflush(stderr);
        MPI_Finalize();
        free(dist_hca);
		return 1;
    }
    free(dist_hca);
    
    CPU_ZERO(&cpuset);
    if (sched_getaffinity(0, sizeof(cpuset), &cpuset) < 0) {
		fprintf(stderr, "\nrank - %d: sched_getaffinity failed, errno says %s. Skip.\n", my_rank, strerror(errno));
		fflush(stderr);
        MPI_Finalize();
		return 1;
	}

    for (i = 0; i < numcpus; ++i) {
	    if (CPU_ISSET(i, &cpuset)) {
            next = numa_node_of_cpu(i);
            if (-1 != numa && next != numa) {
                fprintf(stderr, "\nError rank - %d: scheduled on more than one numa node.\n", my_rank);
                fflush(stderr);
                MPI_Finalize();
                return 1;
            }
            numa = next;
	    }
    }

    if (numa_node != numa) {
        fprintf(stderr, "\nError rank - %d: scheduled on wrong NUMA node - %d, should be %d\n", my_rank, numa, numa_node);
        fflush(stderr);
        MPI_Finalize();
		return 1;
    }

    fprintf(stderr, "\nSuccess rank - %d: only one NUMA is scheduled.\n", my_rank);
    fflush(stderr);

    MPI_Finalize();

    return 0;
}
