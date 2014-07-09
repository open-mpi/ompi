/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/mca/coll/ml/coll_ml_inlines.h"
#include "ompi/mca/coll/ml/coll_ml_hier_algorithms_common_setup.h"

int mca_coll_ml_schedule_init_scratch(mca_coll_ml_topology_t *topo_info,
        mca_coll_ml_schedule_hier_info_t *h_info,
        int **out_scratch_indx, int **out_scratch_num) 
{
    bool prev_is_zero;
    int i, cnt;
    int n_hiers = h_info->n_hiers;
    int value_to_set = 0;
    mca_bcol_base_module_t *prev_bcol = NULL;
    int *scratch_indx, *scratch_num;

    scratch_indx = *out_scratch_indx = 
        (int *) calloc(n_hiers * 2, sizeof(int));
    if (NULL == *out_scratch_indx) {
        ML_ERROR(("Can't allocate memory."));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    scratch_num = *out_scratch_num = 
        (int *) calloc(n_hiers * 2, sizeof(int));
    if (NULL == *out_scratch_num) {
        ML_ERROR(("Can't allocate memory."));
        free(out_scratch_indx);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0, cnt = 0; i < h_info->num_up_levels; ++i, ++cnt) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            scratch_indx[cnt] = 0;
            prev_bcol = GET_BCOL(topo_info, i);
        }
    }

    /* top  - only if the proc arrive to highest_level_is_global_highest_level */
    if (h_info->call_for_top_function) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, n_hiers - 1))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            scratch_indx[cnt] = 0;
            prev_bcol = GET_BCOL(topo_info, n_hiers - 1);
        }
        ++cnt;
    }

    /* going down */
    for (i = h_info->num_up_levels - 1; i >= 0; --i, ++cnt) {
        if (IS_BCOL_TYPE_IDENTICAL(prev_bcol, GET_BCOL(topo_info, i))) {
            scratch_indx[cnt] = scratch_indx[cnt - 1] + 1;
        } else {
            scratch_indx[cnt] = 0;
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

    return OMPI_SUCCESS;
}

mca_coll_ml_collective_operation_description_t * 
        mca_coll_ml_schedule_alloc(mca_coll_ml_schedule_hier_info_t *h_info)
{
    mca_coll_ml_collective_operation_description_t  *schedule = NULL;

    schedule = (mca_coll_ml_collective_operation_description_t *)
        malloc(sizeof(mca_coll_ml_collective_operation_description_t));
    if (NULL == schedule) {
        ML_ERROR(("Can't allocate memory."));
        return NULL;
    }

    /* Set dependencies equal to number of hierarchies */
    schedule->n_fns = h_info->nbcol_functions;
    schedule->progress_type = 0;
    /* Allocated the component function */
    schedule->component_functions = (struct mca_coll_ml_compound_functions_t *)
        calloc(h_info->nbcol_functions, sizeof(struct mca_coll_ml_compound_functions_t));
    if (NULL == schedule->component_functions) {
        ML_ERROR(("Can't allocate memory."));
        free(schedule);
        return NULL;
    }
    return schedule;
}

void mca_coll_ml_call_types(mca_coll_ml_schedule_hier_info_t *h_info,
        mca_coll_ml_collective_operation_description_t *schedule)
{
    int i_hier, j_hier, cnt;
    mca_bcol_base_module_t *current_bcol = NULL;

    for (i_hier = 0; i_hier < h_info->n_hiers; i_hier++) {
        current_bcol =
            schedule->component_functions[i_hier].
            constant_group_data.bcol_module;
        cnt = 0;
        for (j_hier = 0; j_hier < h_info->n_hiers; j_hier++) {
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
}
