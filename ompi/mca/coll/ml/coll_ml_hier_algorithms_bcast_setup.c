/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

static int mca_coll_ml_task_comp_dynamic_root_small_message
    (struct mca_coll_ml_task_status_t *task) {

        task->ml_coll_operation->variable_fn_params.root_flag = true;

        return OMPI_SUCCESS;
}


int mca_coll_ml_setup_scratch_vals(mca_coll_ml_compound_functions_t *func_list,
        int *scratch_indx, int *scratch_num, int n_hiers)
{
    int i_hier, j_hier;
    int cnt, value_to_set = 0;
    bool prev_is_zero;
    mca_coll_ml_compound_functions_t *comp_fn;
    mca_bcol_base_module_t *prev_bcol = NULL,
                           *bcol_module;

    /* Calculate scratch numbers */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        bcol_module = func_list[i_hier].constant_group_data.bcol_module;
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, bcol_module)) {
            scratch_indx[i_hier] = scratch_indx[i_hier - 1] + 1;
        } else {
            scratch_indx[i_hier] = 0;
            prev_bcol = bcol_module;
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


    /* Each hierarchy has one function to be implemented */
    /* this is the basic setup required of the bcol function */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        /* We want to be generic, but on this stage we support only single
         * bcol per hierarchy level
         */
        comp_fn = &func_list[i_hier];
        comp_fn->h_level = i_hier; /* hierarchy level */

        /* we can change this */
        comp_fn->task_comp_fn = mca_coll_ml_task_comp_dynamic_root_small_message;
        /* assert(NULL != comp_fn->bcol_function); */
        /* Constants */
        comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls = scratch_indx[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_a_row = scratch_num[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_collective = 0;
        comp_fn->constant_group_data.index_of_this_type_in_collective = 0;

        ML_VERBOSE(10, ("Setting collective [bcast] fn_idx %d, index_in_consecutive_same_bcol_calls %d, n_of_this_type_in_a_row %d",
                    i_hier,
                    comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls,
                    comp_fn->constant_group_data.n_of_this_type_in_a_row));
    }

    /* Fill the rest of constant data */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        mca_bcol_base_module_t *current_bcol =
            func_list[i_hier].
            constant_group_data.bcol_module;
        cnt = 0;
        for (j_hier = 0; j_hier < n_hiers; j_hier++) {
            if (current_bcol ==
                    func_list[j_hier].
                    constant_group_data.bcol_module) {
                func_list[j_hier].constant_group_data.
                    index_of_this_type_in_collective = cnt;
                
                cnt++;
            }
        }
        func_list[i_hier].constant_group_data.n_of_this_type_in_collective = cnt;
    }

    return OMPI_SUCCESS;

}

static void mca_coll_ml_zero_dep_bcast(mca_coll_ml_task_status_t *task_status, int index, mca_coll_ml_compound_functions_t *func)
{
    /* no real dependency, set everything to zero */
    task_status->rt_num_dependencies = 0;
    task_status->rt_num_dependent_tasks = 0;
    task_status->rt_dependent_task_indices = NULL;
}

/*
 * Build schedule without runtime attributes
 */
static int mca_coll_ml_build_bcast_dynamic_schedule_no_attributes(
        mca_coll_ml_topology_t *topo_info,
        mca_coll_ml_collective_operation_description_t **coll_desc, int bcol_func_index)
{

    int n_hiers = topo_info->n_levels;
    int i_hier, j_hier;
    int cnt, value_to_set = 0;
    int ret; /* exit code in case of error */
    bool prev_is_zero;
    int *scratch_indx = NULL,
        *scratch_num = NULL;

    mca_coll_ml_collective_operation_description_t  *schedule;
    mca_coll_ml_compound_functions_t *comp_fn;
    mca_bcol_base_module_t *prev_bcol,
                           *bcol_module;

    *coll_desc = (mca_coll_ml_collective_operation_description_t *)
        calloc(1, sizeof(mca_coll_ml_collective_operation_description_t));
    schedule = *coll_desc;
    if (NULL == schedule) {
        ML_ERROR(("Can't allocate memory."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    scratch_indx = (int *) calloc(n_hiers, sizeof (int));
    if (NULL == scratch_indx) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
    }

    scratch_num = (int *) malloc(sizeof(int) * (n_hiers));
    if (NULL == scratch_num) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
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

    /* Set dependencies equal to number of hierarchies */
    schedule->n_fns = n_hiers;
    schedule->topo_info = topo_info;
    schedule->progress_type = 0; /* Pasha: Not really defined, puting zero */

    /* Allocated the component function */
    schedule->component_functions = (struct mca_coll_ml_compound_functions_t *)
            calloc(n_hiers, sizeof(struct mca_coll_ml_compound_functions_t));
    if (NULL == schedule->component_functions) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
    }

    /* Each hierarchy has one function to be implemented */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        /* We want to be generic, but on this stage we support only single
         * bcol per hierarchy level
         */
        comp_fn = &schedule->component_functions[i_hier];
        comp_fn->h_level = i_hier; /* hierarchy level */
        bcol_module = GET_BCOL(topo_info, i_hier);
        /* Init component function */
        strcpy (comp_fn->fn_name, "BCAST_TEST_SMALL_DYNAMIC");
        comp_fn->num_dependent_tasks = 0;
        comp_fn->num_dependencies = 0;
        comp_fn->dependent_task_indices = NULL;
        comp_fn->bcol_function =
            bcol_module->filtered_fns_table[DATA_SRC_UNKNOWN][NON_BLOCKING][BCOL_BCAST][bcol_func_index][0][0];
        comp_fn->task_comp_fn = mca_coll_ml_task_comp_dynamic_root_small_message;
        assert(NULL != comp_fn->bcol_function);
        /*
        comp_fn->bcol_function->progress_fn =
            bcol_module->filtered_fns_table[BCOL_BCAST][1][0][0];
         */
        /* Constants */
        comp_fn->constant_group_data.bcol_module = bcol_module;
        comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls = scratch_indx[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_a_row = scratch_num[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_collective = 0;
        comp_fn->constant_group_data.index_of_this_type_in_collective = 0;

        ML_VERBOSE(10, ("Setting collective [bcast] fn_idx %d, index_in_consecutive_same_bcol_calls %d, n_of_this_type_in_a_row %d",
                    i_hier,
                    comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls,
                    comp_fn->constant_group_data.n_of_this_type_in_a_row));
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
                ML_VERBOSE(10, ("Pasha: Setting collective [bcast small][count %d], fn_idx %d, collective_alg->functions[i].index_of_this_type_in_collective %d",
                           cnt, i_hier,
                           schedule->component_functions[j_hier].
                                constant_group_data.index_of_this_type_in_collective));
                cnt++;
            }
        }

        schedule->component_functions[i_hier].
            constant_group_data.n_of_this_type_in_collective = cnt;
    }

    schedule->task_setup_fn[COLL_ML_ROOT_TASK_FN] = mca_coll_ml_zero_dep_bcast;
    schedule->task_setup_fn[COLL_ML_GENERAL_TASK_FN] = mca_coll_ml_zero_dep_bcast;

    MCA_COLL_ML_SET_SCHEDULE_ORDER_INFO(schedule);

    free(scratch_num);
    free(scratch_indx);

    return OMPI_SUCCESS;

Bcast_Setup_Error:

    if (NULL != scratch_indx) {
        free(scratch_indx);
    }

    if (NULL != scratch_num) {
        free(scratch_num);
    }

    if (NULL != schedule->component_functions) {
        free(schedule->component_functions);
    }

    return ret;
}

static int mca_coll_ml_build_bcast_sequential_schedule_no_attributes(
        mca_coll_ml_topology_t *topo_info,
        mca_coll_ml_collective_operation_description_t **coll_desc, int bcol_func_index)
{

    int n_hiers = topo_info->n_levels;
    int i_hier, j_hier;
    int cnt, value_to_set = 0;
    int ret; /* exit code in case of error */
    bool prev_is_zero;
    int *scratch_indx = NULL,
        *scratch_num = NULL;

    mca_coll_ml_collective_operation_description_t  *schedule;
    mca_coll_ml_compound_functions_t *comp_fn;
    mca_coll_ml_compound_functions_t *comp_fns_temp;
    mca_bcol_base_module_t *prev_bcol,
                           *bcol_module;

    *coll_desc = (mca_coll_ml_collective_operation_description_t *)
        calloc(1, sizeof(mca_coll_ml_collective_operation_description_t));
    schedule = *coll_desc;
    if (NULL == schedule) {
        ML_ERROR(("Can't allocate memory."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    scratch_indx = (int *) calloc(n_hiers, sizeof (int));
    if (NULL == scratch_indx) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
    }

    scratch_num = (int *) malloc(sizeof(int) * (n_hiers));
    if (NULL == scratch_num) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
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

    /* Set dependencies equal to number of hierarchies */
    schedule->n_fns = n_hiers;
    schedule->topo_info = topo_info;
    schedule->progress_type = 0; /* Pasha: Not really defined, puting zero 
                                  * Josh: would be nice to define it as "sequential"
                                  * or "concurrent"
                                  */

    /* Allocated the component function */
    schedule->component_functions = (struct mca_coll_ml_compound_functions_t *)
            calloc(n_hiers, sizeof(struct mca_coll_ml_compound_functions_t));
    if (NULL == schedule->component_functions) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
    }
    /* Allocate the schedule list */
    schedule->comp_fn_arr = (struct mca_coll_ml_compound_functions_t **)
        calloc(n_hiers,sizeof(struct mca_coll_ml_compound_functions_t *));
    if (NULL == schedule->comp_fn_arr) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
    }
    /* Each hierarchy has one function to be implemented */
    /* this is the basic setup required of the bcol function */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        /* We want to be generic, but on this stage we support only single
         * bcol per hierarchy level
         */
        comp_fn = &schedule->component_functions[i_hier];
        comp_fn->h_level = i_hier; /* hierarchy level */
        bcol_module = GET_BCOL(topo_info, i_hier);
        /* Init component function */
        strcpy (comp_fn->fn_name, "BCAST_TEST_SMALL_SEQUENTIAL");

        /* should be very simple, shouldn't require any kind of fancy dependencies set*/

        comp_fn->bcol_function =
            bcol_module->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING][BCOL_BCAST][bcol_func_index][0][0];

        /* initialize the coll_fn_started flag to false */
        /*comp_fn->coll_fn_started = false;*/
        /* debug print */
        
        /*
        if(comp_fn->coll_fn_started){
            fprintf(stderr,"this statement is true\n");
        } else {
            fprintf(stderr,"done setting to false \n");
        }
        */
       
        comp_fn->task_comp_fn = mca_coll_ml_task_comp_dynamic_root_small_message;
        /* assert(NULL != comp_fn->bcol_function); */
        /* Constants */
        comp_fn->constant_group_data.bcol_module = bcol_module;
        comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls = scratch_indx[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_a_row = scratch_num[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_collective = 0;
        comp_fn->constant_group_data.index_of_this_type_in_collective = 0;

        ML_VERBOSE(10, ("Setting collective [bcast] fn_idx %d, index_in_consecutive_same_bcol_calls %d, n_of_this_type_in_a_row %d",
                    i_hier,
                    comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls,
                    comp_fn->constant_group_data.n_of_this_type_in_a_row));
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
                ML_VERBOSE(10, ("Pasha: Setting collective [bcast small][count %d], fn_idx %d, collective_alg->functions[i].index_of_this_type_in_collective %d",
                           cnt, i_hier,
                           schedule->component_functions[j_hier].
                                constant_group_data.index_of_this_type_in_collective));
                cnt++;
            }
        }
        schedule->component_functions[i_hier].
            constant_group_data.n_of_this_type_in_collective = cnt;
    }
    /* Now that the functions have been set-up properly, we can simple permute the ordering a bit */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        /* first one is trivial */
        comp_fns_temp = (struct mca_coll_ml_compound_functions_t *)
            calloc(n_hiers, sizeof(struct mca_coll_ml_compound_functions_t));
        /* else we need to build the schedule */
       
        for(j_hier = 0; j_hier < n_hiers; j_hier++) {
            /* put the i_hier-th function first in the list */
            if( 0 == j_hier ) {
                comp_fns_temp[j_hier] = schedule->component_functions[i_hier];
            } else if( j_hier  <= i_hier ) {
                comp_fns_temp[j_hier] = schedule->component_functions[j_hier-1];
            } else {
                comp_fns_temp[j_hier] = schedule->component_functions[j_hier];
            }
        }
        /* now let's attach this list to our array of lists */
        schedule->comp_fn_arr[i_hier] = comp_fns_temp;

    }


#if 1 
    /* I'm going to just loop over each schedule and 
     * set up the scratch indices, scratch numbers 
     * and other constant data
     */
    for( i_hier = 1; i_hier < n_hiers; i_hier++) {
        /* calculate the scratch indices and associated numbers */
        ret = mca_coll_ml_setup_scratch_vals(schedule->comp_fn_arr[i_hier], scratch_indx,
                scratch_num, n_hiers);
        if( OMPI_SUCCESS != ret ) {
            ret = OMPI_ERROR;
            goto Bcast_Setup_Error;
        }

    }
#endif  

    MCA_COLL_ML_SET_SCHEDULE_ORDER_INFO(schedule);

    free(scratch_num);
    free(scratch_indx);

    return OMPI_SUCCESS;

Bcast_Setup_Error:

    if (NULL != scratch_indx) {
        free(scratch_indx);
    }

    if (NULL != scratch_num) {
        free(scratch_num);
    }

    if (NULL != schedule->component_functions) {
        free(schedule->component_functions);
    }

    if (NULL != schedule->comp_fn_arr) {
        free(schedule->comp_fn_arr);
    }
    free (schedule);
    *coll_desc = NULL;

    return ret;
}

static void mca_coll_ml_static_bcast_root(mca_coll_ml_task_status_t *task_status, int index,
        mca_coll_ml_compound_functions_t *func)
{
    task_status->rt_num_dependencies = 0;
    task_status->rt_num_dependent_tasks = 0;
    task_status->rt_dependent_task_indices = 0;
}

static void mca_coll_ml_static_bcast_non_root(mca_coll_ml_task_status_t *task_status, int index,
        mca_coll_ml_compound_functions_t *func)
{
    /* Make active only the first level of hierarchy the gets the data, all the rest of levels
       will be activated by dependency list */
    if (task_status->ml_coll_operation->variable_fn_params.root_route->level == index) {
        task_status->rt_num_dependencies = 0;
        task_status->rt_num_dependent_tasks = func->num_dependent_tasks;
        task_status->rt_dependent_task_indices = func->dependent_task_indices;
        task_status->ml_coll_operation->variable_fn_params.root =
            task_status->ml_coll_operation->variable_fn_params.root_route->rank;
    } else {
        task_status->rt_num_dependencies = 1;           /* wait for root */
        task_status->rt_num_dependent_tasks = 0;        /* no depended task */
        task_status->rt_dependent_task_indices = NULL; /* NULL */
    }
}

static int mca_coll_ml_build_bcast_known_schedule_no_attributes(
        mca_coll_ml_topology_t *topo_info,
        mca_coll_ml_collective_operation_description_t **coll_desc, int bcol_func_index)
{

    int n_hiers = topo_info->n_levels;
    int i_hier, j_hier;
    int cnt, value_to_set = 0;
    int ret; /* exit code in case of error */
    bool prev_is_zero;
    int *scratch_indx = NULL,
        *scratch_num = NULL;

    mca_coll_ml_collective_operation_description_t  *schedule;
    mca_coll_ml_compound_functions_t *comp_fn;
    mca_bcol_base_module_t *prev_bcol,
                           *bcol_module;

    *coll_desc = (mca_coll_ml_collective_operation_description_t *)
        calloc(1, sizeof(mca_coll_ml_collective_operation_description_t));
    schedule = *coll_desc;
    if (NULL == schedule) {
        ML_ERROR(("Can't allocate memory."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    scratch_indx = (int *) calloc(n_hiers, sizeof (int));
    if (NULL == scratch_indx) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
    }

    scratch_num = (int *) malloc(sizeof(int) * (n_hiers));
    if (NULL == scratch_num) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
    }

    prev_bcol = NULL;

    /* Calculate scratch numbers */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i_hier))) {
            scratch_indx[i_hier] = scratch_indx[i_hier - 1] + 1;
        } else {
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

    /* Set dependencies equal to number of hierarchies */
    schedule->n_fns = n_hiers;
    schedule->topo_info = topo_info;
    schedule->progress_type = 0; /* Pasha: Not really defined, puting zero */

    /* Allocated the component function */
    schedule->component_functions = (struct mca_coll_ml_compound_functions_t *)
            calloc(n_hiers, sizeof(struct mca_coll_ml_compound_functions_t));
    if (NULL == schedule->component_functions) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Bcast_Setup_Error;
    }

    /* Each hierarchy has one function to be implemented */
    for (i_hier = 0; i_hier < n_hiers; i_hier++) {
        int j;
        /* We want to be generic, but on this stage we support only single
         * bcol per hierarchy level
         */
        comp_fn = &schedule->component_functions[i_hier];
        comp_fn->h_level = i_hier; /* hierarchy level */
        bcol_module = GET_BCOL(topo_info, i_hier);
        /* Init component function */
        strcpy (comp_fn->fn_name, "BCAST_TEST_SMALL_STATIC");
        /* Hack for single layer of hierarchy */
        if (1 == n_hiers) {
            comp_fn->num_dependent_tasks     = n_hiers - 1;
            comp_fn->num_dependencies        = 0;
        } else {
            comp_fn->num_dependent_tasks     = n_hiers;    /* root will have n_hier - 1 depended tasks, non root zero*/
            comp_fn->num_dependencies        = 0;              /* root will have zero dependencies */
        }

        if (0 != comp_fn->num_dependent_tasks) {
            comp_fn->dependent_task_indices = (int *)calloc(n_hiers, sizeof(int));
            for (j = 0; j < n_hiers; j++) {
                comp_fn->dependent_task_indices[j] = j; /* only root will use this one */
            }
        }

        comp_fn->bcol_function =
            bcol_module->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING][BCOL_BCAST][bcol_func_index][0][0];

        comp_fn->task_comp_fn = mca_coll_ml_task_comp_dynamic_root_small_message;
        /* assert(NULL != comp_fn->bcol_function); */
        /* Constants */
        comp_fn->constant_group_data.bcol_module = bcol_module;
        comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls = scratch_indx[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_a_row = scratch_num[i_hier];
        comp_fn->constant_group_data.n_of_this_type_in_collective = 0;
        comp_fn->constant_group_data.index_of_this_type_in_collective = 0;

        ML_VERBOSE(10, ("Setting collective [bcast] fn_idx %d, index_in_consecutive_same_bcol_calls %d, n_of_this_type_in_a_row %d",
                    i_hier,
                    comp_fn->constant_group_data.index_in_consecutive_same_bcol_calls,
                    comp_fn->constant_group_data.n_of_this_type_in_a_row));
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
                ML_VERBOSE(10, ("Pasha: Setting collective [bcast small][count %d], fn_idx %d, collective_alg->functions[i].index_of_this_type_in_collective %d",
                           cnt, i_hier,
                           schedule->component_functions[j_hier].
                                constant_group_data.index_of_this_type_in_collective));
                cnt++;
            }
        }
        schedule->component_functions[i_hier].
            constant_group_data.n_of_this_type_in_collective = cnt;
    }

    schedule->task_setup_fn[COLL_ML_ROOT_TASK_FN] = mca_coll_ml_static_bcast_root;
    schedule->task_setup_fn[COLL_ML_GENERAL_TASK_FN] = mca_coll_ml_static_bcast_non_root;

    MCA_COLL_ML_SET_SCHEDULE_ORDER_INFO(schedule);

    free(scratch_num);
    free(scratch_indx);

    return OMPI_SUCCESS;

Bcast_Setup_Error:

    if (NULL != scratch_indx) {
        free(scratch_indx);
    }

    if (NULL != scratch_num) {
        free(scratch_num);
    }

    if (NULL != schedule->component_functions) {
        free(schedule->component_functions);
    }
    free (schedule);
    *coll_desc = NULL;

    return ret;
}



#define BCAST_SMALL 1
#define BCAST_LARGE 5

int ml_coll_hier_bcast_setup(mca_coll_ml_module_t *ml_module)
{
    /* Hierarchy Setup */
    int ret, i , size_code, alg;
    int topo_index = 0;
    mca_coll_ml_topology_t *topo_info = ml_module->topo_list;

    for (i = 0; i < ML_NUM_MSG; i++) {

        switch (i) {
            case ML_SMALL_MSG:
                size_code = BCAST_SMALL;
                break;
            case ML_LARGE_MSG:
                size_code = BCAST_LARGE;
                break;
            default:
                topo_info->hierarchical_algorithms[ML_BCAST] = NULL;
                return OMPI_ERROR;
        }

        alg = mca_coll_ml_component.coll_config[ML_BCAST][i].algorithm_id;
        topo_index = ml_module->collectives_topology_map[ML_BCAST][alg];
        if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
            ML_ERROR(("No topology index or algorithm was defined"));
            topo_info->hierarchical_algorithms[ML_BCAST] = NULL;
            return OMPI_ERROR;
        }

        switch (alg) {
            case ML_BCAST_SMALL_DATA_KNOWN:
            case ML_BCAST_LARGE_DATA_KNOWN:
                ret = mca_coll_ml_build_bcast_known_schedule_no_attributes(&topo_info[topo_index],
                        &ml_module->coll_ml_bcast_functions[alg], size_code);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    ML_VERBOSE(10, ("Failed to setup static bcast"));
                    topo_info->hierarchical_algorithms[ML_BCAST] = NULL;
                    return ret;
                }
                break;
            case ML_BCAST_SMALL_DATA_UNKNOWN:
            case ML_BCAST_LARGE_DATA_UNKNOWN:
                ret = mca_coll_ml_build_bcast_dynamic_schedule_no_attributes(&topo_info[topo_index],
                        &ml_module->coll_ml_bcast_functions[alg], size_code);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    ML_VERBOSE(10, ("Failed to setup dynamic bcast"));
                    topo_info->hierarchical_algorithms[ML_BCAST] = NULL;
                    return ret;
                }
                break;
            case ML_BCAST_SMALL_DATA_SEQUENTIAL:
            case ML_BCAST_LARGE_DATA_SEQUENTIAL:
                ret = mca_coll_ml_build_bcast_sequential_schedule_no_attributes(&topo_info[topo_index],
                        &ml_module->coll_ml_bcast_functions[alg], size_code);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    ML_VERBOSE(10, ("Failed to setup static bcast"));
                    topo_info->hierarchical_algorithms[ML_BCAST] = NULL;
                    return ret;
                }
                break;
            default:
                topo_info->hierarchical_algorithms[ML_BCAST] = NULL;
                return OMPI_ERROR;
        }
        assert(NULL != ml_module->coll_ml_bcast_functions[alg] &&
                NULL != ml_module->coll_ml_bcast_functions[alg]);
    }

    topo_info->hierarchical_algorithms[BCOL_BCAST] = NULL;
    return ret;
}

void ml_coll_hier_bcast_cleanup(mca_coll_ml_module_t *ml_module)
{
    /* Hierarchy Setup */
    int i, alg;
    int topo_index = 0;
    mca_coll_ml_topology_t *topo_info = ml_module->topo_list;

    assert (NULL != ml_module);

    for (i = 0; i < ML_NUM_MSG; i++) {

        switch (i) {
            case ML_SMALL_MSG:
            case ML_LARGE_MSG:
                break;
            default:
                topo_info->hierarchical_algorithms[ML_BCAST] = NULL;
                return;
        }

        alg = mca_coll_ml_component.coll_config[ML_BCAST][i].algorithm_id;
        topo_index = ml_module->collectives_topology_map[ML_BCAST][alg];
        if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
            ML_ERROR(("No topology index or algorithm was defined"));
            topo_info->hierarchical_algorithms[ML_BCAST] = NULL;
            return;
        }

        if (NULL != ml_module->coll_ml_bcast_functions[alg]) {
            if (ML_BCAST_SMALL_DATA_KNOWN <= alg && ML_BCAST_LARGE_DATA_SEQUENTIAL >= alg) {
                if (ml_module->coll_ml_bcast_functions[alg]->component_functions) {
                    free(ml_module->coll_ml_bcast_functions[alg]->component_functions);
                    ml_module->coll_ml_bcast_functions[alg]->component_functions = NULL;
                }

                free(ml_module->coll_ml_bcast_functions[alg]);
                ml_module->coll_ml_bcast_functions[alg] = NULL;
            } else {
                topo_info->hierarchical_algorithms[ML_BCAST] = NULL;
            }
        }
    }
}
