/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/util/output.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_object.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/threads/mutex.h"
#include "opal/sys/atomic.h"

#include "ompi/op/op.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/mca/coll/ml/coll_ml_inlines.h"
#include "ompi/patterns/comm/coll_ops.h"

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"

#include "ompi/mca/bcol/base/base.h"
#include "coll_ml_custom_utils.h"

/*
 * Local types
 */

struct avail_coll_t {
    opal_list_item_t super;
    int ac_priority;
    mca_coll_base_module_2_0_0_t *ac_module;
};
typedef struct avail_coll_t avail_coll_t;

/*
 * Stuff for the OBJ interface
 * If topo_index == COLL_ML_TOPO_MAX it looks over all possilbe topologies, otherwhise it looks
 * in the topology that was specified.
 */

int mca_coll_ml_check_if_bcol_is_used(const char *bcol_name, const mca_coll_ml_module_t *ml_module,
        int topo_index)
{
    int i, rc, hier, *ranks_in_comm,
        is_used = 0,
        comm_size = ompi_comm_size(ml_module->comm);
    int n_hier, tp , max_tp;
    const mca_coll_ml_topology_t *topo_info;

    ranks_in_comm = (int *) malloc(comm_size * sizeof(int));
    if (OPAL_UNLIKELY(NULL == ranks_in_comm)) {
        ML_ERROR(("Memory allocation failed."));
        ompi_mpi_abort(&ompi_mpi_comm_world.comm, MPI_ERR_NO_MEM, true);
        /* not reached but causes a clang warning to not return here */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < comm_size; ++i) {
        ranks_in_comm[i] = i;
    }

    if (COLL_ML_TOPO_MAX == topo_index) {
        tp = 0;
        max_tp = COLL_ML_TOPO_MAX;
    } else {
        tp = topo_index;
        max_tp = topo_index + 1;
    }

    for (; tp < max_tp; tp++) {
        topo_info = &ml_module->topo_list[tp];
        n_hier = topo_info->n_levels;
        for (hier = 0; hier < n_hier; ++hier) {
            hierarchy_pairs *pair = &topo_info->component_pairs[hier];
            mca_bcol_base_component_t *b_cm = pair->bcol_component;
            if(0 == strcmp(bcol_name,
                        b_cm->bcol_version.mca_component_name)) {
                is_used = 1;
                break;
            }
        }
    }

    rc = comm_allreduce_pml(&is_used, &is_used, 1, MPI_INT,
                  ompi_comm_rank(ml_module->comm), MPI_MAX,
                  comm_size, ranks_in_comm, ml_module->comm);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ML_ERROR(("comm_allreduce_pml failed."));
        ompi_mpi_abort(&ompi_mpi_comm_world.comm, MPI_ERR_OP, true);
    }

    free(ranks_in_comm);

    return is_used;
}

/* The function is very different from the above function */
int mca_coll_ml_check_if_bcol_is_requested(const char *component_name)
{
    mca_base_component_list_item_t *bcol_comp;

    ML_VERBOSE(10, ("Loop over bcol components"));
    OPAL_LIST_FOREACH(bcol_comp, &mca_bcol_base_components_in_use, mca_base_component_list_item_t) {
        if(0 == strcmp(component_name,
                    ((mca_bcol_base_component_2_0_0_t *)
                     bcol_comp->cli_component)->bcol_version.mca_component_name)) {
            return true;
        }
    }

    /* the component was not resquested */
    return false;
}
