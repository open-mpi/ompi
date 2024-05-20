/*
 * Copyright (c) 2021-2023 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"

#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

#include "opal/mca/shmem/base/base.h"
#include "opal/util/show_help.h"

#include "coll_xhc.h"

typedef int (*csv_parse_conv_fn_t)(char *str, void *dst);
typedef void (*csv_parse_destruct_fn_t)(void *data);

static int xhc_register(void);

const char *mca_coll_xhc_component_version_string =
    "Open MPI xhc collective MCA component version " OMPI_VERSION;

static const char *hwloc_topo_str[] = {
    "node", "flat",
    "socket",
    "numa",
    "l3", "l3cache",
    "l2", "l2cache",
    "l1", "l1cache",
    "core",
    "hwthread", "thread"
};

static const xhc_loc_t hwloc_topo_val[] = {
    OPAL_PROC_ON_NODE, OPAL_PROC_ON_NODE,
    OPAL_PROC_ON_SOCKET,
    OPAL_PROC_ON_NUMA,
    OPAL_PROC_ON_L3CACHE, OPAL_PROC_ON_L3CACHE,
    OPAL_PROC_ON_L2CACHE, OPAL_PROC_ON_L2CACHE,
    OPAL_PROC_ON_L1CACHE, OPAL_PROC_ON_L1CACHE,
    OPAL_PROC_ON_CORE,
    OPAL_PROC_ON_HWTHREAD, OPAL_PROC_ON_HWTHREAD
};

mca_coll_xhc_component_t mca_coll_xhc_component = {
    .super = {
        .collm_version = {
            MCA_COLL_BASE_VERSION_3_0_0,

            .mca_component_name = "xhc",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION,
                OMPI_MINOR_VERSION, OMPI_RELEASE_VERSION),

            .mca_register_component_params = xhc_register,
        },

        .collm_data = {
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .collm_init_query = mca_coll_xhc_component_init_query,
        .collm_comm_query = mca_coll_xhc_module_comm_query,
    },

    .priority = 0,
    .print_info = false,

    .shmem_backing = NULL,

    .dynamic_leader = false,

    .barrier_root = 0,

    .dynamic_reduce = OMPI_XHC_DYNAMIC_REDUCE_NON_FLOAT,
    .lb_reduce_leader_assist =
        (OMPI_XHC_LB_RLA_TOP_LEVEL | OMPI_XHC_LB_RLA_FIRST_CHUNK),

    .force_reduce = false,

    .cico_max = 1024,

    .uniform_chunks = true,
    .uniform_chunks_min = 1024,

    /* These are the parameters that will need
     * processing, and their default values. */
    .hierarchy_mca = "numa,socket",
    .chunk_size_mca = "16K"
};

/* Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support. */
int mca_coll_xhc_component_init_query(bool enable_progress_threads,
        bool enable_mpi_threads) {

    return OMPI_SUCCESS;
}

static mca_base_var_enum_value_t dynamic_reduce_options[] = {
    {OMPI_XHC_DYNAMIC_REDUCE_DISABLED, "disabled"},
    {OMPI_XHC_DYNAMIC_REDUCE_NON_FLOAT, "non-float"},
    {OMPI_XHC_DYNAMIC_REDUCE_ALL, "all"},
    {0, NULL}
};

static mca_base_var_enum_value_flag_t lb_reduce_leader_assist_options[] = {
    {OMPI_XHC_LB_RLA_TOP_LEVEL, "top", OMPI_XHC_LB_RLA_ALL},
    {OMPI_XHC_LB_RLA_FIRST_CHUNK, "first", OMPI_XHC_LB_RLA_ALL},
    {OMPI_XHC_LB_RLA_ALL, "all",
        (OMPI_XHC_LB_RLA_TOP_LEVEL | OMPI_XHC_LB_RLA_FIRST_CHUNK)},
    {0, NULL, 0}
};

static int xhc_register(void) {
    mca_base_var_enum_t *var_enum;
    mca_base_var_enum_flag_t *var_enum_flag;
    char *tmp, *desc;
    int ret;

    /* Priority */

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "priority", "Priority of the xhc component",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_2,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.priority);

    /* Info */

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "print_info", "Print information during initialization",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.print_info);

    /* SHM Backing dir */

    mca_coll_xhc_component.shmem_backing = (access("/dev/shm", W_OK) == 0 ?
        "/dev/shm" : opal_process_info.job_session_dir);

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "shmem_backing", "Directory to place backing files for shared-memory"
        " control-data communication", MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
        OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_xhc_component.shmem_backing);

    /* Dynamic leader */

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "dynamic_leader", "Enable dynamic operation-wise group-leader selection",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.dynamic_leader);

    /* Dynamic reduce */

    ret = mca_base_var_enum_create("coll_xhc_dynamic_reduce_options",
        dynamic_reduce_options, &var_enum);
    if(ret != OPAL_SUCCESS) {
        return ret;
    }

    /* Barrier root */

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "barrier_root", "Internal root for the barrier operation (rank ID)",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.barrier_root);

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "dynamic_reduce", "Dynamic/out-of-order intra-group reduction",
        MCA_BASE_VAR_TYPE_INT, var_enum, 0, 0, OPAL_INFO_LVL_6,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.dynamic_reduce);

    OBJ_RELEASE(var_enum);

    /* Load balancing: Reduce leader assistance */

    ret = mca_base_var_enum_create_flag("coll_xhc_lb_reduce_leader_assist",
        lb_reduce_leader_assist_options, &var_enum_flag);
    if(ret != OPAL_SUCCESS) {
        return ret;
    }

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "lb_reduce_leader_assist", "Reduction leader assistance modes for load balancing",
        MCA_BASE_VAR_TYPE_INT, &var_enum_flag->super, 0, 0, OPAL_INFO_LVL_6,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.lb_reduce_leader_assist);

    OBJ_RELEASE(var_enum_flag);

    /* Force enable "hacky" reduce */

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "force_reduce", "Force enable the \"special\" Reduce for all calls",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.force_reduce);

    /* Hierarchy features */

    desc = NULL;

    for(size_t i = 0; i < sizeof(hwloc_topo_str)/sizeof(char *); i++) {
        ret = opal_asprintf(&tmp, "%s%s%s", (i > 0 ? desc : ""),
            (i > 0 ? ", " : ""), hwloc_topo_str[i]);
        free(desc); desc = tmp;
        if(ret < 0) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    ret = opal_asprintf(&tmp, "Comma-separated list of topology features to "
        "consider for the hierarchy (%s)", desc);
    free(desc); desc = tmp;
    if(ret < 0) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "hierarchy", desc, MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_4,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.hierarchy_mca);

    free(desc);

    /* Chunk size(s) */

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "chunk_size", "The chunk size(s) to be used for the pipeline "
        "(single value, or comma separated list for different hierarchy levels "
        "(bottom to top))",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.chunk_size_mca);

    /* Allreduce uniform chunks */

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "uniform_chunks", "Automatically optimize chunk size in reduction "
        "collectives according to message size, for load balancing",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.uniform_chunks);

    /* Allreduce uniform chunks min size */

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "uniform_chunks_min", "Minimum chunk size for reduction collectives, "
        "when \"uniform chunks\" are enabled", MCA_BASE_VAR_TYPE_SIZE_T,
        NULL, 0, 0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_xhc_component.uniform_chunks_min);

    /* CICO threshold (inclusive) */

    (void) mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "cico_max", "Maximum message size up to which to use CICO",
        MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.cico_max);

    return OMPI_SUCCESS;
}

static int parse_csv(const char *csv_orig, char sep, char ignore_start,
        char ignore_end, void **vals_dst, int *len_dst, size_t type_size,
        csv_parse_conv_fn_t conv_fn, csv_parse_destruct_fn_t destructor_fn,
        char *err_help_header) {

    if(csv_orig == NULL || strlen(csv_orig) == 0) {
        *vals_dst = NULL;
        *len_dst = 0;
        return OMPI_SUCCESS;
    }

    char *csv = NULL;
    void *vals = NULL;

    int vals_size = 0;
    int ntokens = 0;

    int return_code = OMPI_SUCCESS;

    if(!(csv = strdup(csv_orig))) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    if(!(vals = malloc((vals_size = 5) * type_size))) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    int ignore_cnt = 0;
    char *token = csv;

    int csv_len = strlen(csv);

    for(int i = 0; i < csv_len + 1; i++) {
        char *c = csv+i;

        if(ntokens == vals_size) {
            void *tmp = realloc(vals, (vals_size *= 2) * sizeof(type_size));
            if(!tmp) {
                RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
            }
            vals = tmp;
        }

        if(ignore_start != 0) {
            if(*c == ignore_start) {
                ignore_cnt++;
            } else if(*c == ignore_end) {
                ignore_cnt--;
            }

            if(ignore_cnt < 0) {
                RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
            }
        }

        if(ignore_cnt == 0 && (*c == sep || *c == '\0')) {
            char oldc = *c;
            *c = '\0';

            int status = conv_fn(token, (char *) vals + ntokens*type_size);

            if(status != OMPI_SUCCESS) {
                if(err_help_header) {
                    opal_show_help("help-coll-xhc.txt",
                        err_help_header, true, token, csv_orig);
                }

                RETURN_WITH_ERROR(return_code, status, end);
            }

            ntokens++;

            *c = oldc;
            token = c + 1;
        }
    }

    *vals_dst = vals;
    *len_dst = ntokens;

    end:

    free(csv);

    if(return_code != OMPI_SUCCESS) {
        if(vals && destructor_fn) {
            for(int i = 0; i < ntokens; i++) {
                destructor_fn((char *) vals + i*type_size);
            }
        }

        free(vals);
    }

    return return_code;
}

static int conv_xhc_loc_def_rank_list(char *str, void *result) {
    char *strs[2] = {str, NULL};
    int nums[2] = {-1, -1};

    char *range_op_pos = NULL;

    int return_code = OMPI_SUCCESS;

    if((range_op_pos = strstr(str, ".."))) {
        strs[1] = range_op_pos + 2;
        *range_op_pos = '\0';
    }

    for(int i = 0; i < 2 && strs[i]; i++) {
        char *endptr;

        nums[i] = strtol(strs[i], &endptr, 10);

        if(endptr[0] != '\0' || nums[i] < 0) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
        }
    }

    ((xhc_rank_range_t *) result)->start_rank = nums[0];
    ((xhc_rank_range_t *) result)->end_rank = (nums[1] != -1 ? nums[1] : nums[0]);

    end:

    if(range_op_pos) {
        *range_op_pos = '.';
    }

    return return_code;
}

static void mca_coll_xhc_loc_def_construct(xhc_loc_def_t *def) {
    def->named_loc = 0;
    def->rank_list = NULL;
    def->rank_list_len = 0;
    def->split = 0;
    def->max_ranks = 0;
    def->repeat = false;
}

static void mca_coll_xhc_loc_def_destruct(xhc_loc_def_t *def) {
    free(def->rank_list);
}

OBJ_CLASS_INSTANCE(xhc_loc_def_t, opal_list_item_t,
    mca_coll_xhc_loc_def_construct, mca_coll_xhc_loc_def_destruct);

static int conv_xhc_loc_def(char *str, void *result) {
    int return_code = OMPI_SUCCESS;

    char *s = strdup(str);
    xhc_loc_def_t *def = OBJ_NEW(xhc_loc_def_t);

    if(!s || !def) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);
    }

    /* Parse modifiers and remove them from string */

    if(s[strlen(s) - 1] == '*') {
        def->repeat = true;
        s[strlen(s) - 1] = '\0';
    }

    char *colon_pos = strrchr(s, ':');
    char *qmark_pos = strrchr(s, '?');

    if(colon_pos && qmark_pos) {
        RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
    } else if(colon_pos || qmark_pos) {
        char *numstr = (colon_pos ? colon_pos : qmark_pos);
        char *endptr;

        int num = strtol(numstr + 1, &endptr, 10);

        if(endptr[0] != '\0' || num <= 0) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
        }

        if(colon_pos) {
            def->split = num;
        } else {
            def->max_ranks = num;
        }

        *numstr = '\0';
    }

    /* Parse locality definition */

    if(s[0] == '[') {
        if(def->repeat) { // repeat only makes sense with named localities
            RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
        }

        s[strlen(s) - 1] = '\0';

        int status = parse_csv(s+1, ',', 0, 0, (void **) &def->rank_list,
            &def->rank_list_len, sizeof(xhc_rank_range_t),
            conv_xhc_loc_def_rank_list, NULL, NULL);

        if(status != OMPI_SUCCESS) {
            RETURN_WITH_ERROR(return_code, status, end);
        }
    } else {
        bool found = false;

        for(size_t i = 0; i < sizeof(hwloc_topo_str)/sizeof(char *); i++) {
            if(strcasecmp(s, hwloc_topo_str[i]) == 0) {
                def->named_loc = hwloc_topo_val[i];
                found = true;
                break;
            }
        }

        if(!found) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
        }
    }

    * (xhc_loc_def_t **) result = def;

    end:

    free(s);

    if(return_code != OMPI_SUCCESS) {
        OBJ_RELEASE_IF_NOT_NULL(def);
    }

    return return_code;
}

static void destruct_xhc_loc_def(void *data) {
    OBJ_RELEASE(* (xhc_loc_def_t **) data);
}

static int conv_xhc_loc_def_combination(char *str, void *result) {
    xhc_loc_def_t **defs;
    int ndefs;

    int status = parse_csv(str, '+', 0, 0, (void **) &defs,
        &ndefs, sizeof(xhc_loc_def_t *), conv_xhc_loc_def,
        destruct_xhc_loc_def, NULL);
    if(status != OMPI_SUCCESS) {
        return status;
    }

    opal_list_t *def_list = (opal_list_t *) result;
    OBJ_CONSTRUCT(def_list, opal_list_t);

    for(int i = 0; i < ndefs; i++) {
        opal_list_append(def_list, (opal_list_item_t *) defs[i]);
    }

    free(defs);

    return OMPI_SUCCESS;
}

static void destruct_xhc_loc_def_combination(void *data) {
    OPAL_LIST_DESTRUCT((opal_list_t *) data);
}

int mca_coll_xhc_component_parse_hierarchy(const char *val_str,
        opal_list_t **level_defs_dst, int *nlevel_defs_dst) {

    /* The hierarchy is in a comma-separated list format. Each item in the
     * list specifies how to group ranks, and each different item entails
     * a grouping step.
     *
     * Each item in this list is a '+'-separated list. Of course, this can
     * be just one item, without any delimiter, specifying the locality to
     * follow for the grouping (e.g. numa, socket, etc).
     *
     * But, it can also be more complex (multiple '+'-separated items), used
     * to describe virtual hierarchies. This allows to group different ranks
     * in different ways, e.g. some ranks according to numa, then others by
     * something else, etc.
     *
     * Each item in this '+'-separated list, can be of the following types:
     * 1. A "named locality", e.g. hwloc's localities (only ones currently
     *    available), see hwloc_topo_str[].
     * 2. A list of ranks that should be grouped together. This is a comma-
     *    separated list of integers, enclosed in [] (I know, list-ception!).
     *    It may also contain range operators (..), to select multiple ranks
     *    at once (e.g. 0..3 expands to 0,1,2,3). Example: [0..15,20,22].
     *    The order of the ranks does not matter.
     *
     * Finally, each such item may be suffixed by a special modifier:
     * 1. The split modifier (:<n>) specifies to group according to the
     *    locality it refers to, but to split each such group into multiple
     *    parts. E.g. the locality 'numa:2' will group ranks into half-numas
     *    group, such that for each NUMA node, half the ranks are in one
     *    group, and the rest are in another.
     * 2. The max-ranks modifier (?<n>) works similarly to the split modifier,
     *    only that it specifies that at most _n_ ranks should be placed in
     *    each group. If more than _n_ ranks share the locality the modifier
     *    refers to, multiple groups will be created for these ranks, each one
     *    not more than _n_ ranks in size.
     * 3. The repeat modifier (*), which can be specified along with the two
     *    previous modifiers, allows manual control over the repetition of
     *    named localities. See below, under 'repetition'.
     *
     * Repetition:
     *   Named localities are repeated for all distinct rank clusters. For
     *   example, "numa", even though it is a single key, means to group
     *   all ranks that are in the same NUMA together, which will lead to
     *   multiple groups if multiple NUMA nodes are present. This is in
     *   contract to rank lists, which only create a single group, containing
     *   the ranks specified in it. The different items in the '+'-separated
     *   list are consumed in-order left-to-right, and any named localities
     *   are automatically repeated to apply all ranks that are not included
     *   in other items. When multiple named localities are present one after
     *   the other, the last one is repeated, unless another repetition was
     *   explicitly requested via the repeat modifier.
     *
     * Examples:
     *   "numa": Group according to numa locality
     *   "numa,socket": Group according to numa and then socket locality
     *   "node"/"flat": Group according to node locality -> all ranks in
     *     same node -> flat hierarchy i.e. none at all
     *
     *   "numa:2,socket": Group according to numa locality but with two
     *     groups per NUMA, and then according to socket.
     *   "numa:2,numa,socket": Similar to the previous one, but this case
     *     will result in one of the two half-numa-leaders further becoming
     *     the leader of the NUMA node.
     *   "numa?10,socket": Group according to numa, but no more than 10 ranks
     *     per NUMA; create multiple groups if necessary. Then group according
     *     to socket.
     *
     *   "[0..9]+[10..24]": Create 2 groups: one for the first 10 ranks,
     *     and another for the next 15 ones.
     *   "[0..39]+numa,socket": Group the first 40 ranks, and the rest
     *     according to numa locality. Then group according to socket.
     *
     *   "socket+socket:2": Create at least two groups: one for all ranks
     *     in the first socket, and all the other ranks group them according
     *     to socket locality, but with two groups for each socket.
     *   "socket*+socket:2": Similar to the previous one, but only the last
     *     socket is split into two groups, all the other ranks are grouped
     *     according to socket locality.
     *
     * If the top-most locality specified does not cover all ranks, one such
     * locality will automatically be added (in the hierarchy sort method).
     *
     * (Oh god what have I done! -Frankenstein, probably) */

    int status = parse_csv(val_str, ',', '[', ']', (void **) level_defs_dst,
        nlevel_defs_dst, sizeof(opal_list_t), conv_xhc_loc_def_combination,
        destruct_xhc_loc_def_combination, "bad-hierarchy-item");

    return status;
}

static int conv_chunk_size(char *str, void *result) {
    size_t last_idx = strlen(str) - 1;
    char saved_char = str[last_idx];

    size_t mult = 1;

    switch(str[last_idx]) {
        case 'g': case 'G':
            mult *= 1024;
        case 'm': case 'M':
            mult *= 1024;
        case 'k': case 'K':
            mult *= 1024;

        str[last_idx] = '\0';
    }

    bool legal = (str[0] != '\0');

    for(char *c = str; *c; c++) {
        if((*c < '0' || *c > '9') && *c != '-') {
            legal = false;
            break;
        }
    }

    if(legal) {
        long long num = atoll(str) * mult;
        * (size_t *) result = (size_t) (num > 0 ? num : -1);
    }

    str[last_idx] = saved_char;

    return (legal ? OMPI_SUCCESS : OMPI_ERR_BAD_PARAM);
}

int mca_coll_xhc_component_parse_chunk_sizes(const char *val_str,
        size_t **chunks_dst, int *len_dst) {

    if(val_str == NULL) {
        *chunks_dst = malloc(sizeof(size_t));
        if(*chunks_dst == NULL) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        (*chunks_dst)[0] = (size_t) -1;
        *len_dst = 1;

        return OMPI_SUCCESS;
    }

    int status = parse_csv(val_str, ',', 0, 0, (void **) chunks_dst, len_dst,
        sizeof(size_t), conv_chunk_size, NULL, "bad-chunk-size-item");

    return status;
}
