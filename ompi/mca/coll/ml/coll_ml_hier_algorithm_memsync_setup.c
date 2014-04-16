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

#include "ompi_config.h"

#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/mca/coll/ml/coll_ml_inlines.h"
#include "ompi/include/ompi/constants.h"
#include "ompi/mca/coll/ml/coll_ml_functions.h"

static int mca_coll_ml_build_memsync_schedule(
                                    mca_coll_ml_topology_t *topo_info,
                                    mca_coll_ml_collective_operation_description_t **coll_desc)
{
    int i_hier, rc, i_fn, n_fcns, i,
        n_hiers = topo_info->n_levels;

    bool call_for_top_func;
    mca_bcol_base_module_t *bcol_module;

    mca_coll_ml_compound_functions_t *comp_fn;
    mca_coll_ml_collective_operation_description_t  *schedule;

    *coll_desc = (mca_coll_ml_collective_operation_description_t *)
      calloc(1, sizeof(mca_coll_ml_collective_operation_description_t));

    schedule = *coll_desc;
    if (OPAL_UNLIKELY(NULL == schedule)) {
        ML_ERROR(("Can't allocate memory."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (topo_info->global_highest_hier_group_index ==
          topo_info->component_pairs[n_hiers - 1].bcol_index) {
        /* The process that is member of highest level subgroup
           should call for top algorithms in addition to fan-in/out steps */
        call_for_top_func = true;
        n_fcns = 2 * n_hiers - 1; /* Up + Top + Down */
    } else {
        /* The process is not member of highest level subgroup,
           as result it does not call for top algorithm,
           but it calls for all fan-in/out steps */
        call_for_top_func = false;
        n_fcns = 2 * n_hiers;
    }

    /* Set dependencies equal to number of hierarchies */
    schedule->n_fns = n_fcns;
    schedule->topo_info = topo_info;

    /* Allocated the component function */
    schedule->component_functions = (struct mca_coll_ml_compound_functions_t *)
                                     calloc(n_fcns, sizeof(struct mca_coll_ml_compound_functions_t));

    if (OPAL_UNLIKELY(NULL == schedule->component_functions)) {
        ML_ERROR(("Can't allocate memory."));
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto Barrier_Setup_Error;
    }

    for (i_fn = 0; i_fn < n_fcns; ++i_fn) {
        i_hier = (i_fn < n_hiers ? i_fn : n_fcns - i_fn - 1);
        comp_fn = &schedule->component_functions[i_fn];

        /* The hierarchial level */
        comp_fn->h_level = i_hier;
        bcol_module = GET_BCOL(topo_info, i_hier);

        /* The UP direction */
        if (1 + i_fn < n_hiers || (1 + i_fn == n_hiers && !call_for_top_func)) {
            /* Pasha: We do not have memory syncronization FANIN function, instead I use barrier.
             * In future we have to replace it with memsync fan-in function
             * comp_fn->bcol_function =
             *       bcol_module->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING][BCOL_FANIN][1][0][0];
             */
            comp_fn->bcol_function = GET_BCOL_SYNC_FN(bcol_module);


            assert(NULL != comp_fn->bcol_function);

            /* Each function call with index K is depended of all K-1 previous indices -
               in simple words we will do sequential Fan-In calls */
            comp_fn->num_dependencies = i_fn;
            comp_fn->num_dependent_tasks = n_fcns - i_fn - 1;

            /* Init component function */
            strcpy(comp_fn->fn_name, "MEMSYNC-FANIN");

        /* On the highest level */
        } else if ((1 + i_fn == n_hiers && call_for_top_func)) {
            comp_fn->bcol_function = GET_BCOL_SYNC_FN(bcol_module);

            /* Each function call with index K is depended of all K-1 previous indices -
               in simple words we do sequential calls */
            comp_fn->num_dependencies = n_hiers - 1; /* All Fan-Ins */
            comp_fn->num_dependent_tasks = n_fcns - n_hiers; /* All Fan-Outs */

            /* Init component function */
            strcpy(comp_fn->fn_name, "MEMSYNC-BARRIER");

            assert(NULL != comp_fn->bcol_function);
            ML_VERBOSE(10, ("func indx %d set to BARRIER %p", i_fn, comp_fn->bcol_function));

        /* The DOWN direction */
        } else {
            /* Pasha: We do not have memory syncronization FANOUT function, instead I use barrier.
             * In future we have to replace it with memsync fan-out function
             * comp_fn->bcol_function =
             *       bcol_module->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING][BCOL_FANOUT][1][0][0];
             */
            comp_fn->bcol_function = GET_BCOL_SYNC_FN(bcol_module);

            /* Each function call with index K is depended of all UP and TOP algths */
            comp_fn->num_dependencies = n_hiers;
            comp_fn->num_dependent_tasks = 0;

            /* Init component function */
            strcpy(comp_fn->fn_name, "MEMSYNC-FANOUT");
        }

        assert(NULL != comp_fn->bcol_function);
        ML_VERBOSE(10, ("func indx %d set to %p", i_fn, comp_fn->bcol_function));

        if (comp_fn->num_dependent_tasks > 0) {
            comp_fn->dependent_task_indices = (int *) calloc(comp_fn->num_dependent_tasks, sizeof(int));
            if (OPAL_UNLIKELY(NULL == comp_fn->dependent_task_indices)) {
                ML_ERROR(("Can't allocate memory."));
                rc = OMPI_ERR_OUT_OF_RESOURCE;
                goto Barrier_Setup_Error;
            }

            /* All indexes follow after this one */
            for (i = 0; i < comp_fn->num_dependent_tasks; ++i) {
                comp_fn->dependent_task_indices[i] = i_fn + i + 1;
            }
        }

        /* No need completion func for Barrier */
        comp_fn->task_comp_fn = NULL;

        ML_VERBOSE(10, ("Setting collective [Barrier] fn_idx %d, n_of_this_type_in_a_row %d, "
                        "index_in_consecutive_same_bcol_calls %d.",
                         i_fn, comp_fn->constant_group_data.n_of_this_type_in_a_row,
                         comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls));
    }

    rc = ml_coll_barrier_constant_group_data_setup(topo_info, schedule);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ML_ERROR(("Failed to init const group data."));
        goto Barrier_Setup_Error;
    }

    schedule->progress_type = 0;

    return OMPI_SUCCESS;

Barrier_Setup_Error:
    if (NULL != schedule->component_functions) {
        free(schedule->component_functions);
        schedule->component_functions = NULL;
    }

    free (schedule);
    *coll_desc = NULL;

    return rc;
}

int ml_coll_memsync_setup(mca_coll_ml_module_t *ml_module)
{
    int ret;
    /* For barrier syncronization we use barrier topology */
    mca_coll_ml_topology_t *topo_info =
           &ml_module->topo_list[ml_module->collectives_topology_map[ML_BARRIER][ML_SMALL_MSG]];

    ret = mca_coll_ml_build_memsync_schedule(topo_info,
                            &ml_module->coll_ml_memsync_function);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ML_VERBOSE(10, ("Failed to setup static bcast"));
        return ret;
    }

    return OMPI_SUCCESS;
}
