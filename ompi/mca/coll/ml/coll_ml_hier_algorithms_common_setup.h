/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_ML_COMMON_SETUP_H
#define MCA_COLL_ML_COMMON_SETUP_H

#include "ompi_config.h"

#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/coll/ml/coll_ml.h"

struct mca_coll_ml_schedule_hier_info_t {
    int n_hiers;
    int num_up_levels;
    int nbcol_functions;
    bool call_for_top_function;
};
typedef struct mca_coll_ml_schedule_hier_info_t 
               mca_coll_ml_schedule_hier_info_t;

#define MCA_COLL_ML_INIT_HIER_INFO(info, n_hr, g_hr, ml_module) \
do { \
    info.n_hiers = n_hr; \
    if (g_hr == \
            ml_module->component_pairs[n_hr - 1].bcol_index) { \
        /* The process that is member of highest level subgroup  \
           should call for top algorithms in addition to fan-in/out steps*/ \
        ML_VERBOSE(9, ("Setting top %d %d", n_hr, ml_module->component_pairs[g_hr - 1].bcol_index)); \
        info.call_for_top_function = true; \
        /* hier level run only top algorithm, so we deduct 1 */ \
        info.num_up_levels = n_hr - 1; \
        /* Top algorithm is called only once, so we deduct 1 */ \
        info.nbcol_functions = 2 * n_hr - 1; \
    } else { \
        ML_VERBOSE(9, ("not setting top %d %d", n_hr, ml_module->component_pairs[g_hr - 1].bcol_index)); \
        /* The process is not member of highest level subgroup, \
           as result it does not call for top algorithm, \
           but it calls for all fan-in/out steps */ \
        info.call_for_top_function = false; \
        info.num_up_levels = n_hr; \
        info.nbcol_functions = 2 * n_hr; \
    } \
} while (0);

#define MCA_COLL_ML_SET_COMP_FN(fn, level, module, s_level,                         \
                                               scratch_indx, scratch_num, qc, name) \
do {                                                                                \
    fn->h_level = level; /* hierarchy level */                                      \
    strcpy (fn->fn_name, "name");                                                   \
    fn->num_dependent_tasks     = 0;                                                \
    fn->num_dependencies        = 0;                                                \
    fn->task_comp_fn = NULL;                                                        \
    fn->constant_group_data.bcol_module = GET_BCOL(module, level);                  \
    fn->constant_group_data.index_in_consecutive_same_bcol_calls =                  \
                                                              scratch_indx[s_level];\
    fn->constant_group_data.n_of_this_type_in_a_row = scratch_num[s_level];         \
    fn->constant_group_data.n_of_this_type_in_collective = 0;                       \
    fn->constant_group_data.index_of_this_type_in_collective = 0;                   \
    fn->bcol_function = fn->constant_group_data.bcol_module->                       \
                                            filtered_fns_table[qc[0]]               \
                                                              [qc[1]]               \
                                                              [qc[2]]               \
                                                              [qc[3]]               \
                                                              [qc[4]]               \
                                                              [qc[5]];              \
} while (0);

#define MCA_COLL_ML_QUERY_SIZE 6

#define MCA_COLL_ML_SET_QUERY(query, src_type, blocking, coll_type, index, other0, other1) \
do { \
    query[0] = src_type; \
    query[1] = blocking; \
    query[2] = coll_type; \
    query[3] = index; \
    query[4] = other0; \
    query[5] = other1; \
} while (0);

int mca_coll_ml_schedule_init_scratch(mca_coll_ml_topology_t *topo_info,
        mca_coll_ml_schedule_hier_info_t *h_info,
        int **out_scratch_indx, int **out_scratch_num);

mca_coll_ml_collective_operation_description_t* 
mca_coll_ml_schedule_alloc(mca_coll_ml_schedule_hier_info_t *h_info);

void mca_coll_ml_call_types(mca_coll_ml_schedule_hier_info_t *h_info,
        mca_coll_ml_collective_operation_description_t *schedule);
#endif
