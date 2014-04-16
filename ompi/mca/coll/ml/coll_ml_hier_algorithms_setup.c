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

int ml_coll_up_and_down_hier_setup(mca_coll_ml_module_t *ml_module,
                                   mca_coll_ml_topology_t *topo_info,
                                   int up_function_idx,
                                   int top_function_idx,
                                   int down_function_idx,
                                   int collective)
{
    /* local variables */
    int i, j, cnt, value_to_set = -1;
    int ret = OMPI_SUCCESS, num_up_levels;

    int num_hierarchies = topo_info->n_levels;
    int global_high_hierarchy_index = topo_info->global_highest_hier_group_index;

    bool call_for_top_function, prev_is_zero;

    int *scratch_indx = NULL, *scratch_num = NULL;

    coll_ml_collective_description_t *collective_alg = NULL;
    mca_bcol_base_module_t *bcol_module = NULL,
                           *prev_bcol = NULL;

    /* RLG:  one blocking barrier collective algorithm - this is really a hack,
     * we need to figure out how to do this in a bit more extensible
     * manner.
     */
     collective_alg = (coll_ml_collective_description_t *)
         malloc(sizeof(coll_ml_collective_description_t));
     if (NULL == collective_alg) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
     }

    /* am I a member of the highest level subgroup ? */
    if (global_high_hierarchy_index ==
          topo_info->component_pairs[num_hierarchies - 1].bcol_index) {
        /* The process that is member of highest level subgroup
           should call for top algorithms in addition to fan-in/out steps*/
        call_for_top_function = true;
        /* hier level run only top algorithm, so we deduct 1 */
        num_up_levels = num_hierarchies - 1;
        /* Top algorithm is called only once, so we deduct 1 */
        collective_alg->n_functions = 2 * num_hierarchies - 1;
    } else {
        /* The process is not member of highest level subgroup,
           as result it does not call for top algorithm,
           but it calls for all fan-in/out steps */
        call_for_top_function = false;
        num_up_levels = num_hierarchies;
        collective_alg->n_functions = 2 * num_hierarchies;
    }

    ML_VERBOSE(10, ("high_index %d == bcol_index %d: Call top %d, num_up_levels %d, collective_alg->n_functions %d",
                global_high_hierarchy_index,
                topo_info->component_pairs[num_hierarchies - 1].bcol_index,
                call_for_top_function,
                num_up_levels,
                collective_alg->n_functions ));

    /* allocate space for the functions */
    collective_alg->functions = (mca_bcol_base_function_t *)
        calloc(collective_alg->n_functions, sizeof(mca_bcol_base_function_t));
    if( NULL == collective_alg->functions) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }

    /* Algorithm Description:
     * =====================
     * The algorithm used here for an N level system
     *  - up to level N-2, inclusive : up algorithm (fan in in barrier, reduce in Allreduce)
     *  - level N-1: top algorithm (barrier or allreduce)
     *  - level N-2, to level 0: down algorithm (fanout)
     */


    /* Starting scratch_num and scratch_index calculations */
    /* =================================================== */

    /* Figure out how many of the same bcols are called in a row.
     * The index of the bcol in row we store in scratch_indx and
     * the total number of bcols in the row we store in scratch_num */
    scratch_indx = (int *) calloc (2 * num_hierarchies, sizeof (int));
    if(NULL == scratch_indx) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }

    scratch_num = (int *) malloc(sizeof(int) * (2 * num_hierarchies));
    if(NULL == scratch_num) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }

    /* We go through all stages of algorithm (up, top, down)
     * and calculate bcol index. If previous bcol is the same type as current
     * one the counter index is increased, other way the index is zero */
    prev_bcol = NULL;
    /* going up */
    for (i = 0, cnt = 0; i < num_up_levels; ++i, ++cnt) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            scratch_indx[cnt] = 0;
            prev_bcol = GET_BCOL(topo_info, i);
        }
    }

    /* top  - only if the proc arrive to highest_level_is_global_highest_level */
    if (call_for_top_function) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, num_hierarchies - 1))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            scratch_indx[cnt] = 0;
            prev_bcol = GET_BCOL(topo_info, num_hierarchies - 1);
        }

        ++cnt;
    }

    /* going down */
    for (i = num_up_levels - 1; i >= 0; --i, ++cnt) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            scratch_indx[cnt] = 0;
            prev_bcol = GET_BCOL(topo_info, i);
        }
    }

    /*
     * Calculate the number of the same bcols in row.
     * We parse the index array, if index is zero
     * it means that the row is done and we start
     * to calculate next bcols row. The maximum number
     * for the row is equal to maximal bcol index in the row + 1
     */
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

    /* =========================================================== */
    /* We are done with scratch_num and scratch_index calculations */

    /* Setup function call for each algorithm step */
    cnt = 0;
    /* up phase */
    for (i = 0; i < num_up_levels; i++) {
        bcol_module = GET_BCOL(topo_info, i);
        collective_alg->functions[cnt].fn_idx = up_function_idx;
        collective_alg->functions[cnt].bcol_module = bcol_module;
        collective_alg->functions[cnt].index_in_consecutive_same_bcol_calls = scratch_indx[cnt];
        collective_alg->functions[cnt].n_of_this_type_in_a_row = scratch_num[cnt];
        ML_VERBOSE(10, ("Setting collective [collective code %d][count %d], fn_idx %d, index_in_consecutive_same_bcol_calls %d, n_of_this_type_in_a_row %d",
                    collective, cnt, collective_alg->functions[cnt].fn_idx,
                    collective_alg->functions[cnt].index_in_consecutive_same_bcol_calls,
                    collective_alg->functions[cnt].n_of_this_type_in_a_row));
        ++cnt;
    }

    /* top function */
    if (call_for_top_function) {
        bcol_module = GET_BCOL(topo_info, num_hierarchies - 1);
        collective_alg->functions[cnt].fn_idx = top_function_idx;
        collective_alg->functions[cnt].bcol_module = bcol_module;
        collective_alg->functions[cnt].index_in_consecutive_same_bcol_calls = scratch_indx[cnt];
        collective_alg->functions[cnt].n_of_this_type_in_a_row = scratch_num[cnt];
        ML_VERBOSE(10, ("Setting collective [collective code %d][count %d], fn_idx %d, index_in_consecutive_same_bcol_calls %d, n_of_this_type_in_a_row %d",
                    collective, cnt, collective_alg->functions[cnt].fn_idx,
                    collective_alg->functions[cnt].index_in_consecutive_same_bcol_calls,
                    collective_alg->functions[cnt].n_of_this_type_in_a_row));
        ++cnt;
    }

    /* down phase*/
    for (i = num_up_levels - 1; i >= 0; i--) {
        bcol_module = GET_BCOL(topo_info, i);
        collective_alg->functions[cnt].fn_idx = down_function_idx;
        collective_alg->functions[cnt].bcol_module = bcol_module;
        collective_alg->functions[cnt].index_in_consecutive_same_bcol_calls = scratch_indx[cnt];
        collective_alg->functions[cnt].n_of_this_type_in_a_row = scratch_num[cnt];
        ML_VERBOSE(10, ("Setting collective [collective code %d][count %d], fn_idx %d, index_in_consecutive_same_bcol_calls %d, n_of_this_type_in_a_row %d",
                    collective, cnt, collective_alg->functions[cnt].fn_idx,
                    collective_alg->functions[cnt].index_in_consecutive_same_bcol_calls,
                    collective_alg->functions[cnt].n_of_this_type_in_a_row));
        ++cnt;
    }

    /* figure out how many times this bcol is used in this collective call */
    for (i = 0; i < collective_alg->n_functions; i++) {
        mca_bcol_base_module_t *current_bcol=
            collective_alg->functions[i].bcol_module;

        cnt = 0;
        for (j = 0; j < collective_alg->n_functions; ++j) {
            if (current_bcol ==
                    collective_alg->functions[j].bcol_module) {
                collective_alg->functions[j].index_of_this_type_in_collective = cnt;
                ML_VERBOSE(10, ("Pasha: Setting collective [collective code %d][count %d], fn_idx %d, collective_alg->functions[i].index_of_this_type_in_collective %d",
                            collective, cnt, i,
                            collective_alg->functions[j].index_of_this_type_in_collective));
                cnt++;
            }
        }

        collective_alg->functions[i].n_of_this_type_in_collective=cnt;
        ML_VERBOSE(10, ("Pasha: Setting collective [collective code %d][count %d], fn_idx %d, collective_alg->functions[i].n_of_this_type_in_collective %d",
                    collective, cnt, i,
                    collective_alg->functions[i].n_of_this_type_in_collective));
    }

    /* set Barrier algorithm */
    topo_info->hierarchical_algorithms[collective] = collective_alg;
    /* Setup maximum number function calls, it is used for resource allocation */
    ml_module->max_fn_calls = (collective_alg->n_functions > ml_module->max_fn_calls) ?
                                    collective_alg->n_functions : ml_module->max_fn_calls;
    /* Ishai: What is this n_buffers? I did not find where it is being used*/
    topo_info->hierarchical_algorithms[collective]->n_buffers = 1;

    /* Release temporary memories */
    if (NULL != scratch_indx) {
        free(scratch_indx);
    }

    if (NULL != scratch_num) {
       free(scratch_num);
    }

    return OMPI_SUCCESS;

Error:
    if (NULL != collective_alg->functions) {
       free(collective_alg->functions);
    }

    if (NULL != collective_alg) {
       free(collective_alg);
    }

    if (NULL != scratch_indx) {
        free(scratch_indx);
    }

    if (NULL != scratch_num) {
        free(scratch_num);
    }

    return ret;
}

int ml_coll_hier_allreduce_setup(mca_coll_ml_module_t *ml_module)
{
    int topo_index = 
        ml_module->collectives_topology_map[ML_ALLREDUCE][ML_SMALL_DATA_ALLREDUCE];
    int ret = ml_coll_up_and_down_hier_setup(ml_module,
                                             &ml_module->topo_list[topo_index],
                                             BCOL_REDUCE,
                                             BCOL_ALLREDUCE,
                                             BCOL_BCAST,
                                             BCOL_ALLREDUCE);

    if (OMPI_SUCCESS == ret) {
        return ret;
    }

    /* Make sure to reset the allreduce pointer to NULL */
    ml_module->topo_list[topo_index].hierarchical_algorithms[BCOL_ALLREDUCE] = NULL;
    return ret;
}

#if 0
/*
 * Manju: New setup function in coll_ml_hier_algorithms_reduce_setup.c
 */
/* Ishai: Reduce is not an hier algorithm (it is rooted) - it needs a different ML algorithm */
/* Need to rewrite */
int ml_coll_hier_reduce_setup(mca_coll_ml_module_t *ml_module)
{
    int topo_index = ml_module->collectives_topology_map[ML_ALLREDUCE][ML_SMALL_DATA_GATHER];
    /* Hierarchy Setup */
    int ret = ml_coll_up_and_down_hier_setup(ml_module,
                                             &ml_module->topo_list[topo_index],
                                             BCOL_REDUCE, /*NULL,*/
                                             BCOL_REDUCE,
                                             BCOL_REDUCE, /*NULL,*/
                                             BCOL_REDUCE);
    if (OMPI_SUCCESS == ret) {
        return ret;
    }
    /* Make sure to reset the bcast pointer to NULL */
    ml_module->topo_list[topo_index].hierarchical_algorithms[BCOL_BCAST] = NULL;
    return ret;
}
#endif

int ml_coll_barrier_constant_group_data_setup(
                mca_coll_ml_topology_t *topo_info,
                mca_coll_ml_collective_operation_description_t  *schedule)
{
    /* local variables */
    int i, j, cnt, value_to_set = -1, ret = OMPI_SUCCESS, num_up_levels,
        num_hierarchies = topo_info->n_levels, n_functions = schedule->n_fns,
        global_high_hierarchy_index = topo_info->global_highest_hier_group_index;

    bool call_for_top_function, prev_is_zero;
    mca_coll_ml_utility_data_t *constant_group_data = NULL;

    int *scratch_indx = NULL, *scratch_num = NULL;

    mca_bcol_base_module_t *prev_bcol = NULL,
                           *bcol_module = NULL;

    /* Am I a member of the highest level subgroup ? */
    if (global_high_hierarchy_index ==
          topo_info->component_pairs[num_hierarchies - 1].bcol_index) {
        /* The process that is member of highest level subgroup
           should call for top algorithms in addition to fan-in/out steps*/
        call_for_top_function = true;
        /* hier level run only top algorithm, so we deduct 1 */
        num_up_levels = num_hierarchies - 1;
    } else {
        /* The process is not member of highest level subgroup,
           as result it does not call for top algorithm,
           but it calls for all fan-in/out steps */
        call_for_top_function = false;
        num_up_levels = num_hierarchies;
    }

    /* Algorithm Description:
     * =====================
     * The algorithm used here for an N level system
     *  - up to level N-2, inclusive : up algorithm (Fan-In in Barrier)
     *  - level N-1: top algorithm (Barrier algth)
     *  - level N-2, to level 0: down algorithm (Fan-out)
     */


    /* Starting scratch_num and scratch_index calculations */
    /* =================================================== */

    /* Figure out how many of the same bcols are called in a row.
     * The index of the bcol in row we store in scratch_indx and
     * the total number of bcols in the row we store in scratch_num */
    scratch_indx = (int *) calloc (2 * num_hierarchies, sizeof (int));
    if(NULL == scratch_indx) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Const_Data_Setup_Error;
    }

    scratch_num = (int *) malloc(sizeof(int) * (2 * num_hierarchies));
    if(NULL == scratch_num) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Const_Data_Setup_Error;
    }

    /* We go through all stages of algorithm (up, top, down)
     * and calculate bcol index. If previous bcol is the same type as current
     * one the counter index is increased, other way the index is zero */
    prev_bcol = NULL;

    /* Going up */
    for (i = 0, cnt = 0; i < num_up_levels; ++i, ++cnt) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            scratch_indx[cnt] = 0;
            prev_bcol = GET_BCOL(topo_info, i);
        }
    }

    /* Top  - only if the proc arrive to highest_level_is_global_highest_level */
    if (call_for_top_function) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, num_hierarchies - 1))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            scratch_indx[cnt] = 0;
            prev_bcol = GET_BCOL(topo_info, num_hierarchies - 1);
        }

        ++cnt;
    }

    /* Going down */
    for (i = num_up_levels - 1; i >= 0; --i, ++cnt) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            scratch_indx[cnt] = 0;
            prev_bcol = GET_BCOL(topo_info, i);
        }
    }

    /*
     * Calculate the number of the same bcols in row.
     * We parse the index array, if index is zero
     * it means that the row is done and we start
     * to calculate next bcols row. The maximum number
     * for the row is equal to maximal bcol index in the row + 1
     */
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

    /* =========================================================== */
    /* We are done with scratch_num and scratch_index calculations */

    /* Setup function call for each algorithm step */
    cnt = 0;

    /* Up phase */
    for (i = 0; i < num_up_levels; ++i) {
        bcol_module = GET_BCOL(topo_info, i);
        constant_group_data = &schedule->component_functions[cnt].constant_group_data;

        constant_group_data->bcol_module = bcol_module;
        constant_group_data->index_in_consecutive_same_bcol_calls = scratch_indx[cnt];
        constant_group_data->n_of_this_type_in_a_row = scratch_num[cnt];

        ++cnt;
    }

    /* Top function */
    if (call_for_top_function) {
        bcol_module = GET_BCOL(topo_info, num_hierarchies - 1);
        constant_group_data = &schedule->component_functions[cnt].constant_group_data;

        constant_group_data->bcol_module = bcol_module;
        constant_group_data->index_in_consecutive_same_bcol_calls = scratch_indx[cnt];
        constant_group_data->n_of_this_type_in_a_row = scratch_num[cnt];

        ++cnt;
    }

    /* Down phase */
    for (i = num_up_levels - 1; i >= 0; --i) {
        bcol_module = GET_BCOL(topo_info, i);
        constant_group_data = &schedule->component_functions[cnt].constant_group_data;

        constant_group_data->bcol_module = bcol_module;

        /* All Fan-Outs will be done in parallel */
        constant_group_data->index_in_consecutive_same_bcol_calls = 0;
        constant_group_data->n_of_this_type_in_a_row = 1;

        ++cnt;
    }

    /* Figure out how many times this bcol is used in this collective call */
    for (i = 0; i < n_functions; ++i) {
        struct mca_coll_ml_compound_functions_t *component_functions =
                                 schedule->component_functions;
        mca_bcol_base_module_t *current_bcol =
                                 component_functions[i].constant_group_data.bcol_module;

        /* silence clang warning about possible NULL dereference of component_functions.
         * this case is a developer error if it occurs */
        assert (NULL != component_functions && NULL != constant_group_data);

        cnt = 0;
        for (j = 0; j < n_functions; ++j) {
            if (current_bcol ==
                    component_functions[j].constant_group_data.bcol_module) {
                constant_group_data->index_of_this_type_in_collective = cnt;

                ++cnt;
            }
        }

        component_functions[i].constant_group_data.n_of_this_type_in_collective = cnt;
    }

    MCA_COLL_ML_SET_SCHEDULE_ORDER_INFO(schedule);

    /* Release temporary memories */
    free(scratch_num);
    free(scratch_indx);

    return OMPI_SUCCESS;

Const_Data_Setup_Error:
    if (NULL != scratch_indx) {
        free(scratch_indx);
    }

    if (NULL != scratch_num) {
        free(scratch_num);
    }

    return ret;
}
