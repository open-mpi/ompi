/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/include/ompi/constants.h"
#include "ompi/mca/coll/ml/coll_ml_functions.h"

#define ALLREDUCE_SMALL 1
#define ALLREDUCE_LARGE 5
#define SMALL_MSG_RANGE 1
#define LARGE_MSG_RANGE 5

static int mca_coll_ml_build_allreduce_schedule(
        mca_coll_ml_topology_t *topo_info,
        mca_coll_ml_collective_operation_description_t **coll_desc, int bcol_func_index)
{

    bool call_for_top_function, prev_is_zero;
    int n_hiers = topo_info->n_levels;
    int i_hier, j_hier;
    int cnt, value_to_set = 0;
    int ret; /* exit code in case of error */
    int nfn=0;
    int *scratch_indx = NULL,
        *scratch_num = NULL;
     int global_high_hierarchy_index =
             topo_info->global_highest_hier_group_index;

    mca_coll_ml_collective_operation_description_t  *schedule;
    mca_coll_ml_compound_functions_t *comp_fn;
    mca_bcol_base_module_t *prev_bcol,
                           *bcol_module;
    int num_up_levels,nbcol_functions,i;

    if (global_high_hierarchy_index ==
          topo_info->component_pairs[n_hiers - 1].bcol_index) {
        /* The process that is member of highest level subgroup
           should call for top algorithms in addition to fan-in/out steps*/
        call_for_top_function = true;
        /* hier level run only top algorithm, so we deduct 1 */
        num_up_levels = n_hiers - 1;
        /* Top algorithm is called only once, so we deduct 1 */
        nbcol_functions = 2 * n_hiers - 1;
    } else {
        /* The process is not member of highest level subgroup,
           as result it does not call for top algorithm,
           but it calls for all fan-in/out steps */
        call_for_top_function = false;
        num_up_levels = n_hiers;
        nbcol_functions = 2 * n_hiers;
    }

    *coll_desc = (mca_coll_ml_collective_operation_description_t *)
        calloc(1, sizeof(mca_coll_ml_collective_operation_description_t));
    schedule = *coll_desc;
    if (NULL == schedule) {
        ML_ERROR(("Can't allocate memory."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    scratch_indx = (int *) calloc(n_hiers * 2, sizeof (int));
    if (NULL == scratch_indx) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Allreduce_Setup_Error;
    }

    scratch_num = (int *) malloc(sizeof(int) * (n_hiers * 2));
    if (NULL == scratch_num) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Allreduce_Setup_Error;
    }

    prev_bcol = NULL;

    for (i = 0, cnt = 0; i < num_up_levels; ++i, ++cnt) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            prev_bcol = GET_BCOL(topo_info, i);
        }
    }

    /* top  - only if the proc arrive to highest_level_is_global_highest_level */
    if (call_for_top_function) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, n_hiers - 1))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            prev_bcol = GET_BCOL(topo_info, n_hiers - 1);
        }

        ++cnt;
    }

    /* going down */
    for (i = num_up_levels - 1; i >= 0; --i, ++cnt) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            prev_bcol = GET_BCOL(topo_info, i);
        }
    }

    i = cnt - 1;
    prev_is_zero = true;

    do {
        if (prev_is_zero) {
            value_to_set = scratch_indx[i] + 1;
            prev_is_zero = false;
        }

        if (0 == scratch_indx[i]) {
            prev_is_zero = true;
        }

        scratch_num[i] = value_to_set;
        --i;
    } while(i >= 0);

    /* Set dependencies equal to number of hierarchies */
    schedule->n_fns = nbcol_functions;
    schedule->topo_info = topo_info;
    schedule->progress_type = 0;

    /* Allocated the component function */
    schedule->component_functions = (struct mca_coll_ml_compound_functions_t *)
            calloc(nbcol_functions, sizeof(struct mca_coll_ml_compound_functions_t));

    if (NULL == schedule->component_functions) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Allreduce_Setup_Error;
    }

    for (i = 0; i < num_up_levels; i++) {
        comp_fn = &schedule->component_functions[i];
        comp_fn->h_level = i; /* hierarchy level */
        bcol_module = GET_BCOL(topo_info, i);

        /* strcpy (comp_fn->fn_name, "ALLREDUCE_SMALL_DATA"); */

        comp_fn->num_dependent_tasks     = 0;
        comp_fn->num_dependencies        = 0;

        comp_fn->bcol_function =
            bcol_module->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING][BCOL_REDUCE][bcol_func_index][0][0];
        if (NULL == comp_fn->bcol_function) {
            /* if there isn't a bcol function for this then we can't continue */
            ret = OMPI_ERR_NOT_SUPPORTED;
            goto Allreduce_Setup_Error;
        }

        comp_fn->task_comp_fn = NULL;

        comp_fn->constant_group_data.bcol_module = bcol_module;
        comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls = scratch_indx[i];
        comp_fn->constant_group_data.n_of_this_type_in_a_row = scratch_num[i];
        comp_fn->constant_group_data.n_of_this_type_in_collective = 0;
        comp_fn->constant_group_data.index_of_this_type_in_collective = 0;
    }

    nfn = i;
    if (call_for_top_function) {
        comp_fn = &schedule->component_functions[nfn];
        comp_fn->h_level = nfn; /* hierarchy level */
        bcol_module = GET_BCOL(topo_info, nfn);

        assert (NULL != bcol_module);

        /* strcpy (comp_fn->fn_name, "ALLREDUCE_SMALL_DATA"); */

        /* The allreduce should depend on the reduce */
        comp_fn->num_dependent_tasks     = 0;
        comp_fn->num_dependencies        = 0;
        comp_fn->bcol_function =
            bcol_module->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING][BCOL_ALLREDUCE][bcol_func_index][0][0];
        if (NULL == comp_fn->bcol_function) {
            /* if there isn't a bcol function for this then we can't continue */
            ret = OMPI_ERR_NOT_SUPPORTED;
            goto Allreduce_Setup_Error;
        }

        comp_fn->task_comp_fn = NULL;

        comp_fn->constant_group_data.bcol_module = bcol_module;
        comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls = scratch_indx[nfn];
        comp_fn->constant_group_data.n_of_this_type_in_a_row = scratch_num[nfn];
        comp_fn->constant_group_data.n_of_this_type_in_collective = 0;
        comp_fn->constant_group_data.index_of_this_type_in_collective = 0;

        ++nfn;
    }

    for (i = num_up_levels - 1; i >= 0; i--) {
        comp_fn = &schedule->component_functions[nfn];
        comp_fn->h_level = i; /* hierarchy level */
        bcol_module = GET_BCOL(topo_info, i);

        assert (NULL != bcol_module);

    /*    strcpy (comp_fn->fn_name, "ALLREDUCE_SMALL_DATA"); */

        comp_fn->num_dependent_tasks     = 0;
        comp_fn->num_dependencies        = 0;

        comp_fn->bcol_function =
            bcol_module->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING][BCOL_BCAST][bcol_func_index][0][0];
        if (NULL == comp_fn->bcol_function) {
            /* if there isn't a bcol function for this then we can't continue */
            ret = OMPI_ERR_NOT_SUPPORTED;
            goto Allreduce_Setup_Error;
        }

        comp_fn->task_comp_fn = NULL;

        comp_fn->constant_group_data.bcol_module = bcol_module;
        comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls = scratch_indx[nfn];
        comp_fn->constant_group_data.n_of_this_type_in_a_row = scratch_num[nfn];
        comp_fn->constant_group_data.n_of_this_type_in_collective = 0;
        comp_fn->constant_group_data.index_of_this_type_in_collective = 0;

        ++nfn;
    }

    /* Fill the rest of constant data */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        mca_bcol_base_module_t *current_bcol =
            schedule->component_functions[i_hier].
            constant_group_data.bcol_module;
        cnt = 0;
        for (j_hier = 0; j_hier < n_hiers; j_hier++) {
            if (current_bcol ==
                    schedule->component_functions[j_hier].
                    constant_group_data.bcol_module) {
                schedule->component_functions[j_hier].
                    constant_group_data.index_of_this_type_in_collective = cnt;
                cnt++;
            }
        }

        schedule->component_functions[i_hier].
            constant_group_data.n_of_this_type_in_collective = cnt;
    }

    MCA_COLL_ML_SET_SCHEDULE_ORDER_INFO(schedule);

    free(scratch_num);
    free(scratch_indx);

    return OMPI_SUCCESS;

Allreduce_Setup_Error:

    if (NULL != scratch_indx) {
        free(scratch_indx);
    }

    if (NULL != scratch_num) {
        free(scratch_num);
    }

    if (NULL != schedule->component_functions) {
        free(schedule->component_functions);
    }
    *coll_desc = NULL;
    free (schedule);

    return ret;
}

int ml_coll_hier_allreduce_setup_new(mca_coll_ml_module_t *ml_module)
{
    /* Hierarchy Setup */
    int ret;
    int topo_index;
    int alg;
    mca_coll_ml_topology_t *topo_info = ml_module->topo_list;

    alg = mca_coll_ml_component.coll_config[ML_ALLREDUCE][ML_SMALL_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_ALLREDUCE][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_ALLREDUCE] = NULL;
        return OMPI_ERROR;
    }

    ret = mca_coll_ml_build_allreduce_schedule(
                    &ml_module->topo_list[topo_index],
                    &ml_module->coll_ml_allreduce_functions[alg],
                    SMALL_MSG_RANGE);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ML_VERBOSE(10, ("Failed to setup Small Message Allreduce"));
         return ret;
       }

    alg = mca_coll_ml_component.coll_config[ML_ALLREDUCE][ML_LARGE_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_ALLREDUCE][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_ALLREDUCE] = NULL;
        return OMPI_ERROR;
    }

    ret = mca_coll_ml_build_allreduce_schedule(
                    &ml_module->topo_list[topo_index],
                    &ml_module->coll_ml_allreduce_functions[alg],
                    LARGE_MSG_RANGE);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ML_VERBOSE(10, ("Failed to setup Large Message Allreduce"));
         return ret;
       }

    if (true == mca_coll_ml_component.need_allreduce_support) {
        topo_index = ml_module->collectives_topology_map[ML_ALLREDUCE][ML_SMALL_DATA_EXTRA_TOPO_ALLREDUCE];
        if (ML_UNDEFINED == topo_index) {
            ML_ERROR(("No topology index was defined"));
            topo_info->hierarchical_algorithms[ML_ALLREDUCE] = NULL;
            return OMPI_ERROR;
        }

        ret = mca_coll_ml_build_allreduce_schedule(
                        &ml_module->topo_list[topo_index],
                        &ml_module->coll_ml_allreduce_functions[ML_SMALL_DATA_EXTRA_TOPO_ALLREDUCE],
                        SMALL_MSG_RANGE);

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            ML_VERBOSE(10, ("Failed to setup Extra Small Message Allreduce"));
            return ret;
        }

        topo_index = ml_module->collectives_topology_map[ML_ALLREDUCE][ML_LARGE_DATA_EXTRA_TOPO_ALLREDUCE];
        if (ML_UNDEFINED == topo_index) {
            ML_ERROR(("No topology index was defined"));
            topo_info->hierarchical_algorithms[ML_ALLREDUCE] = NULL;
            return OMPI_ERROR;
        }

        ret = mca_coll_ml_build_allreduce_schedule(
                        &ml_module->topo_list[topo_index],
                        &ml_module->coll_ml_allreduce_functions[ML_LARGE_DATA_EXTRA_TOPO_ALLREDUCE],
                        LARGE_MSG_RANGE);

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            ML_VERBOSE(10, ("Failed to setup Extra Large Message Allreduce"));
            return ret;
        }
    }

    return OMPI_SUCCESS;
}

void ml_coll_hier_allreduce_cleanup_new(mca_coll_ml_module_t *ml_module)
{
    /* Hierarchy Setup */
    int topo_index;
    int alg;
    mca_coll_ml_topology_t *topo_info = ml_module->topo_list;

    alg = mca_coll_ml_component.coll_config[ML_ALLREDUCE][ML_SMALL_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_ALLREDUCE][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_ALLREDUCE] = NULL;
        return;
    }

    if (NULL == ml_module->coll_ml_allreduce_functions[alg]) {
        return;
    }

    free(ml_module->coll_ml_allreduce_functions[alg]->component_functions);
    ml_module->coll_ml_allreduce_functions[alg]->component_functions = NULL;
    free(ml_module->coll_ml_allreduce_functions[alg]);
    ml_module->coll_ml_allreduce_functions[alg] = NULL;

    alg = mca_coll_ml_component.coll_config[ML_ALLREDUCE][ML_LARGE_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_ALLREDUCE][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_ALLREDUCE] = NULL;
        return;
    }

    free(ml_module->coll_ml_allreduce_functions[alg]->component_functions);
    ml_module->coll_ml_allreduce_functions[alg]->component_functions = NULL;
    free(ml_module->coll_ml_allreduce_functions[alg]);
    ml_module->coll_ml_allreduce_functions[alg] = NULL;

    if (true == mca_coll_ml_component.need_allreduce_support) {
        topo_index = ml_module->collectives_topology_map[ML_ALLREDUCE][ML_SMALL_DATA_EXTRA_TOPO_ALLREDUCE];
        if (ML_UNDEFINED == topo_index) {
            ML_ERROR(("No topology index was defined"));
            topo_info->hierarchical_algorithms[ML_ALLREDUCE] = NULL;
            return;
        }

        alg = ML_SMALL_DATA_EXTRA_TOPO_ALLREDUCE;
        free(ml_module->coll_ml_allreduce_functions[alg]->component_functions);
        ml_module->coll_ml_allreduce_functions[alg]->component_functions = NULL;
        free(ml_module->coll_ml_allreduce_functions[alg]);
        ml_module->coll_ml_allreduce_functions[alg] = NULL;

        topo_index = ml_module->collectives_topology_map[ML_ALLREDUCE][ML_LARGE_DATA_EXTRA_TOPO_ALLREDUCE];
        if (ML_UNDEFINED == topo_index) {
            ML_ERROR(("No topology index was defined"));
            topo_info->hierarchical_algorithms[ML_ALLREDUCE] = NULL;
            return;
        }

        alg = ML_LARGE_DATA_EXTRA_TOPO_ALLREDUCE;
        free(ml_module->coll_ml_allreduce_functions[alg]->component_functions);
        ml_module->coll_ml_allreduce_functions[alg]->component_functions = NULL;
        free(ml_module->coll_ml_allreduce_functions[alg]);
        ml_module->coll_ml_allreduce_functions[alg] = NULL;
    }
}
