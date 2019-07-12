/*
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_report_bindings_full.h"

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
//#include "opal/mca/dl/base/base.h" -- was going to use opal_dl_open etc
#include "opal/mca/hwloc/base/base.h"

typedef void (*VoidFuncPtr)(void); // a function pointer to a function that takes no arguments and returns void.
static void ompi_report_bindings();

void ompi_hook_report_bindings_full_mpi_init_bottom(int argc, char **argv, int requested, int *provided)
{
    if( hook_report_bindings_full_enable_mpi_init ) {
        ompi_report_bindings();
    }
}

// ----------------------------------------------------------------------------

static void
ompi_report_bindings()
{
    int myrank, nranks;
    int ret, i;
    char binding_string[1024];
    int len;
    int *lens, *disps;
    MPI_Comm active_comm = MPI_COMM_WORLD;
    char **all_binding_strings = NULL;

// early return in the case of spawn
    if (ompi_mpi_comm_parent != MPI_COMM_NULL) { return; }

// pick a comm, probably COMM_WORLD, only shrink it if it's quite large
    myrank = ompi_comm_rank(active_comm);
    nranks = ompi_comm_size(active_comm);
    if (nranks > 16*1024) {
        ret = ompi_comm_split(MPI_COMM_WORLD, (myrank<=16*1024)?1:MPI_UNDEFINED, 0, &active_comm, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return;
        }
        if (active_comm == MPI_COMM_NULL) { return; }
        myrank = ompi_comm_rank(active_comm);
        nranks = ompi_comm_size(active_comm);
    }

// produce binding string for the current rank
    hwloc_topology_t whole_system = NULL;
    hwloc_cpuset_t mycpus;
    hwloc_topology_init(&whole_system);
    hwloc_topology_set_flags(whole_system, HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM);
    hwloc_topology_load(whole_system);
    mycpus = hwloc_bitmap_alloc();
    hwloc_get_cpubind(whole_system, mycpus, HWLOC_CPUBIND_PROCESS);
    opal_hwloc_base_cset2mapstr_with_numa(binding_string, sizeof(binding_string), whole_system, mycpus);
    hwloc_bitmap_free(mycpus);
    hwloc_topology_destroy(whole_system);

// Collecting the data at rank 0 to print it in an ordered manner isn't totally
// necessary, but makes the output a little nicer.
    len = strlen(binding_string) + 1;
    lens = malloc(nranks * sizeof(int));
    disps = malloc(nranks * sizeof(int));

    active_comm->c_coll->coll_gather(
        &len, 1, MPI_INT,
        lens, 1, MPI_INT,
        0, active_comm, active_comm->c_coll->coll_gather_module);
    if (myrank == 0) {
        int tlen = 0;
        char *p;
        for (i=0; i<nranks; ++i) {
            disps[i] = tlen;
            tlen += lens[i];
        }
        all_binding_strings = malloc(nranks * sizeof(char*)  +  tlen);
        p = (char*) (all_binding_strings + nranks);
        for (i=0; i<nranks; ++i) {
            all_binding_strings[i] = p;
            p += lens[i];
        }
        active_comm->c_coll->coll_gatherv(
            binding_string, strlen(binding_string) + 1, MPI_CHAR,
            &all_binding_strings[0][0], lens, disps, MPI_CHAR,
            0, active_comm, active_comm->c_coll->coll_gatherv_module);
    } else {
        // matching above call from rank 0, just &all_binding_strings[0][0]
        // isn't legal here, and those args aren't used at non-root anyway
        active_comm->c_coll->coll_gatherv(
            binding_string, strlen(binding_string) + 1, MPI_CHAR,
            NULL, NULL, NULL, MPI_CHAR,
            0, active_comm, active_comm->c_coll->coll_gatherv_module);
    }

// print them from rank 0
    if (myrank == 0) {
        for (i=0; i<nranks; ++i) {
            printf("MCW %d: %s\n", i, all_binding_strings[i]);
        }
    }

    if (active_comm != MPI_COMM_WORLD) { ompi_comm_free(&active_comm); }
    free(lens);
    free(disps);
    if (myrank == 0) { free(all_binding_strings); }
}
