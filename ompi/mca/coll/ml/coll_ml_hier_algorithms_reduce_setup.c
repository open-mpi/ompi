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
static int mca_coll_ml_task_comp_static_reduce
    (struct mca_coll_ml_task_status_t *task) {

    task->ml_coll_operation->variable_fn_params.root_flag = true;

    return OMPI_SUCCESS;
}

static void mca_coll_ml_static_reduce_non_root(mca_coll_ml_task_status_t *task_status, int index,
        mca_coll_ml_compound_functions_t *func)
{
    /* I am not a root rank, but someone in my group is a root*/
    if (task_status->ml_coll_operation->variable_fn_params.root_route->level == index) {
        task_status->rt_num_dependencies = func->num_dependencies;
        task_status->rt_num_dependent_tasks = 0;
        task_status->rt_dependent_task_indices = NULL;
        task_status->ml_coll_operation->variable_fn_params.root =
                                task_status->ml_coll_operation->variable_fn_params.root_route->rank;
    } else {
        task_status->rt_num_dependencies = 0;
        task_status->rt_num_dependent_tasks = 1;
        task_status->rt_dependent_task_indices = &task_status->ml_coll_operation->variable_fn_params.root_route->level;
    }

}

static void mca_coll_ml_static_reduce_root(mca_coll_ml_task_status_t *task_status, int index,
        mca_coll_ml_compound_functions_t *func)
{
        task_status->rt_num_dependencies = func->num_dependencies;
        task_status->rt_num_dependent_tasks = 0;
        task_status->rt_dependent_task_indices = NULL;
}

/*
 * Fill up the collective descriptor
 *
 */
static int mca_coll_ml_build_static_reduce_schedule(
                                    mca_coll_ml_topology_t *topo_info,
                                    mca_coll_ml_collective_operation_description_t **coll_desc)
{
    int i_hier, j_hier,  n_fcns,
        n_hiers = topo_info->n_levels;
    int *scratch_indx = NULL,
        *scratch_num = NULL;
    int cnt, value_to_set = 0;
    int ret = OMPI_SUCCESS;
    bool prev_is_zero;
    mca_coll_ml_compound_functions_t *comp_fns_temp;
    mca_bcol_base_module_t *prev_bcol,
                           *bcol_module;
    mca_coll_ml_compound_functions_t *comp_fn;
    mca_coll_ml_collective_operation_description_t  *schedule = NULL;

    *coll_desc = (mca_coll_ml_collective_operation_description_t *)
        calloc(1, sizeof(mca_coll_ml_collective_operation_description_t));

    schedule = *coll_desc;
    if (OPAL_UNLIKELY(NULL == schedule)) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }

    scratch_indx = (int *) calloc (n_hiers, sizeof (int));
    if (NULL == scratch_indx) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }

    scratch_num = (int *) malloc(sizeof(int) * (n_hiers));
    if (NULL == scratch_num) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }

    prev_bcol = NULL;

    /* Calculate scratch numbers */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i_hier))) {
            scratch_indx[i_hier] = scratch_indx[i_hier - 1] + 1;
        } else {
            scratch_indx[i_hier] = 0;
            prev_bcol = GET_BCOL(topo_info, i_hier);
        }
    }

    --i_hier;
    prev_is_zero = true;

    do {
        if (prev_is_zero) {
            value_to_set = scratch_indx[i_hier] + 1;
            prev_is_zero = false;
        }

        if (0 == scratch_indx[i_hier]) {
            prev_is_zero = true;
        }

        scratch_num[i_hier] = value_to_set;
        --i_hier;
    } while(i_hier >= 0);

    /* All hierarchies call one function, unlike other collectives */
    n_fcns = n_hiers;

    /* Set dependencies equal to number of hierarchies */
    schedule->n_fns = n_fcns;
    schedule->topo_info = topo_info;
    schedule->progress_type = 0;
    /* Allocated the component function */
    schedule->component_functions = (struct mca_coll_ml_compound_functions_t *)
                                     calloc(n_fcns, sizeof(struct mca_coll_ml_compound_functions_t));

    if (OPAL_UNLIKELY(NULL == schedule->component_functions)) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }


    for (i_hier = 0; i_hier < n_hiers; ++i_hier) {
        comp_fn = &schedule->component_functions[i_hier];

        /* The hierarchial level */
        comp_fn->h_level = i_hier;
        bcol_module = GET_BCOL(topo_info, i_hier);

        comp_fn->bcol_function =
                bcol_module->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING][BCOL_REDUCE][1][0][0];

        strcpy(comp_fn->fn_name, "REDUCE");
        ML_VERBOSE(10, ("func indx %d set to %p", i_hier, comp_fn->bcol_function));


        ML_VERBOSE(1,("In ML_REDUCE_SETUP  .. looks fine here"));
        /* No need completion func for Barrier */
        comp_fn->task_comp_fn = mca_coll_ml_task_comp_static_reduce;

        /* Constants */
        comp_fn->constant_group_data.bcol_module = bcol_module;
        comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls = scratch_indx[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_a_row = scratch_num[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_collective = 0;
        comp_fn->constant_group_data.index_of_this_type_in_collective = 0;

        ML_VERBOSE(10, ("Setting collective [reduce] fn_idx %d, n_of_this_type_in_a_row %d, "
                        "index_in_consecutive_same_bcol_calls %d.",
                         i_hier, comp_fn->constant_group_data.n_of_this_type_in_a_row,
                         comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls));
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

    /* Manju: Reduction should always use the fixed schedule.
     * The subgroups that this process is leader should be executed first, then
     * it should execute the subgroups where this process is not a leader, and
     * then execute the subgroup that includes the root.
     */

    /* Allocate the schedule list */
    schedule->comp_fn_arr = (struct mca_coll_ml_compound_functions_t **)
        calloc(n_hiers,sizeof(struct mca_coll_ml_compound_functions_t *));
    if (NULL == schedule->comp_fn_arr) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }

    /* Now that the functions have been set-up properly, we can simple permute the ordering a bit */

    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        /* first one is trivial */
        int leader_hierarchy = 0;
        int non_leader_hierarchy = 0;
        int func_index;

        comp_fns_temp = (struct mca_coll_ml_compound_functions_t *)
            calloc(n_hiers, sizeof(struct mca_coll_ml_compound_functions_t));

        leader_hierarchy = 0;
        non_leader_hierarchy = n_hiers - 2;

        for(j_hier = 0; j_hier < n_hiers - 1 ; j_hier++) {

            func_index = j_hier < i_hier ? j_hier : j_hier + 1;
            /* I'm a leader for this group */
            if (0 == topo_info->component_pairs->subgroup_module->my_index) {
                comp_fns_temp[leader_hierarchy++] =
                    schedule->component_functions[func_index];
            }
            else {
                comp_fns_temp[non_leader_hierarchy--] =
                    schedule->component_functions[func_index];
            }
        }

        comp_fns_temp[j_hier] = schedule->component_functions[i_hier];
        /* now let's attach this list to our array of lists */
        schedule->comp_fn_arr[i_hier] = comp_fns_temp;
    }

    /* Manju: Do we need this ? */

    /* I'm going to just loop over each schedule and
     * set up the scratch indices, scratch numbers
     * and other constant data
     */
    /*
    for( i_hier = 1; i_hier < n_hiers; i_hier++) {
        ret = mca_coll_ml_setup_scratch_vals(schedule->comp_fn_arr[i_hier], scratch_indx,
                scratch_num, n_hiers);
        if( OMPI_SUCCESS != ret ) {
            ret = OMPI_ERROR;
            goto Error;
        }

    }
    */

    /* Do I need this ? */
    schedule->task_setup_fn[COLL_ML_ROOT_TASK_FN] = mca_coll_ml_static_reduce_root;
    schedule->task_setup_fn[COLL_ML_GENERAL_TASK_FN] = mca_coll_ml_static_reduce_non_root;

    MCA_COLL_ML_SET_SCHEDULE_ORDER_INFO(schedule);

    /* reduce does not use the component functions so we no longer need this. see
     *  coll_ml_reduce.c:442 */
    free (schedule->component_functions);
    schedule->component_functions = NULL;

    free(scratch_num);
    free(scratch_indx);

    return OMPI_SUCCESS;

Error:
    if (NULL != scratch_num) {
        free (scratch_num);
    }

    if (NULL != scratch_indx) {
        free (scratch_indx);
    }

    if (NULL != schedule) {
        if (NULL != schedule->component_functions) {
            free(schedule->component_functions);
            schedule->component_functions = NULL;
        }
        free (schedule);
        *coll_desc = NULL;
    }

    return ret;
}


int ml_coll_hier_reduce_setup(mca_coll_ml_module_t *ml_module)
{
    int alg, ret, topo_index=0;
    mca_coll_ml_topology_t *topo_info =
           &ml_module->topo_list[ml_module->collectives_topology_map[ML_REDUCE][ML_SMALL_MSG]];

    if ( ml_module->max_fn_calls < topo_info->n_levels ) {
        ml_module->max_fn_calls = topo_info->n_levels;
    }


    alg = mca_coll_ml_component.coll_config[ML_REDUCE][ML_SMALL_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_REDUCE][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_REDUCE] = NULL;
        return OMPI_ERROR;
    }

    ret = mca_coll_ml_build_static_reduce_schedule(&ml_module->topo_list[topo_index],
            &ml_module->coll_ml_reduce_functions[alg]);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ML_VERBOSE(10, ("Failed to setup static reduce"));
        return ret;
    }


    return OMPI_SUCCESS;
}

void ml_coll_hier_reduce_cleanup(mca_coll_ml_module_t *ml_module)
{
    int alg, i, topo_index=0;
    mca_coll_ml_topology_t *topo_info =
           &ml_module->topo_list[ml_module->collectives_topology_map[ML_REDUCE][ML_SMALL_MSG]];

    if ( ml_module->max_fn_calls < topo_info->n_levels ) {
        ml_module->max_fn_calls = topo_info->n_levels;
    }


    alg = mca_coll_ml_component.coll_config[ML_REDUCE][ML_SMALL_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_REDUCE][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_REDUCE] = NULL;
        return;
    }

    if (NULL == ml_module->coll_ml_reduce_functions[alg]) {
        return;
    }

    if (ml_module->coll_ml_reduce_functions[alg]->comp_fn_arr) {
        for (i=0; i<ml_module->topo_list[topo_index].n_levels; i++) {
            if (ml_module->coll_ml_reduce_functions[alg]->comp_fn_arr[i]) {
                free(ml_module->coll_ml_reduce_functions[alg]->comp_fn_arr[i]);
                ml_module->coll_ml_reduce_functions[alg]->comp_fn_arr[i] = NULL;
            }
        }

        free(ml_module->coll_ml_reduce_functions[alg]->comp_fn_arr);
        ml_module->coll_ml_reduce_functions[alg]->comp_fn_arr = NULL;
    }

    ml_module->coll_ml_reduce_functions[alg]->component_functions = NULL;

    free(ml_module->coll_ml_reduce_functions[alg]);
    ml_module->coll_ml_reduce_functions[alg] = NULL;
}
