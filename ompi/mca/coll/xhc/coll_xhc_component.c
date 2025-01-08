/*
 * Copyright (c) 2021-2024 Computer Architecture and VLSI Systems (CARV)
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
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_base_util.h"

#include "opal/include/opal/align.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/util/show_help.h"

#include "coll_xhc.h"

// -----------------------------

typedef int (*csv_parse_conv_fn_t)(char *str, void *dst);
typedef void (*csv_parse_destruct_fn_t)(void *data);

static int xhc_register(void);
static int xhc_var_check_exclusive(const char *param_a, const char *param_b);

// -----------------------------

static const char *xhc_topo_str[] = {
    "node", "flat",
    "socket",
    "numa",
    "l3", "l3cache",
    "l2", "l2cache",
    "l1", "l1cache",
    "core",
    "hwthread", "thread"
};

static const xhc_loc_t xhc_topo_val[] = {
    OPAL_PROC_ON_NODE, OPAL_PROC_ON_NODE,
    OPAL_PROC_ON_SOCKET,
    OPAL_PROC_ON_NUMA,
    OPAL_PROC_ON_L3CACHE, OPAL_PROC_ON_L3CACHE,
    OPAL_PROC_ON_L2CACHE, OPAL_PROC_ON_L2CACHE,
    OPAL_PROC_ON_L1CACHE, OPAL_PROC_ON_L1CACHE,
    OPAL_PROC_ON_CORE,
    OPAL_PROC_ON_HWTHREAD, OPAL_PROC_ON_HWTHREAD
};

static const COLLTYPE_T xhc_colltype_to_universal_map[XHC_COLLCOUNT] = {
    [XHC_BCAST] = BCAST,
    [XHC_BARRIER] = BARRIER,
    [XHC_REDUCE] = REDUCE,
    [XHC_ALLREDUCE] = ALLREDUCE
};

static const char *xhc_config_source_to_str_map[XHC_CONFIG_SOURCE_COUNT] = {
    [XHC_CONFIG_SOURCE_INFO_GLOBAL] = "info/global",
    [XHC_CONFIG_SOURCE_INFO_OP] = "info/op",
    [XHC_CONFIG_SOURCE_MCA_GLOBAL] = "mca/global",
    [XHC_CONFIG_SOURCE_MCA_OP] = "mca/op"
};

const char *mca_coll_xhc_component_version_string =
    "Open MPI xhc collective MCA component version " OMPI_VERSION;

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
    .print_info = 0,

    .shmem_backing = NULL,

    .memcpy_chunk_size = 256 << 10,

    .dynamic_leader = false,

    .barrier_root = 0,
    .allreduce_root = 0,

    .dynamic_reduce = XHC_DYNAMIC_REDUCE_NON_FLOAT,
    .reduce_load_balance = (XHC_REDUCE_LB_LEADER_ASSIST_TOP_LEVEL
        | XHC_REDUCE_LB_LEADER_ASSIST_FIRST_CHUNK),

    .uniform_chunks = true,
    .uniform_chunks_min = 4096,

    .op_mca = {{0}},
    .op_mca_global = {0}
};

/* Rather than having the defaults directly inside the component, we keep
 * them in a separate structure and copy them over (in xhc_register()). The
 * structs in the component are used as storage for the MCA variables, and
 * the MCA system will nullify the storage of string variables when it is
 * teared down during Finalize. This is a problem if we have multiple MPI
 * Sessions, as we'll have lost our defaults the next time we attempt to
 * initialize our MCA variables at the second Init. */
static xhc_op_mca_t op_mca_default[XHC_COLLCOUNT] = {
     [XHC_BCAST] = {
        .hierarchy = "numa,socket",
        .chunk_size = "16K",
        .cico_max = 256
    },

    [XHC_BARRIER] = {
        .hierarchy = "numa,socket",
        .chunk_size = "1",
        .cico_max = 0
    },

    [XHC_REDUCE] = {
        .hierarchy = "l3,numa,socket",
        .chunk_size = "16K",
        .cico_max = 4096
    },

    [XHC_ALLREDUCE] = {
        .hierarchy = "l3,numa,socket",
        .chunk_size = "16K",
        .cico_max = 4096
    }
};
static xhc_op_mca_t op_mca_global_default = {0};

// -----------------------------

/* Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support. */
int mca_coll_xhc_component_init_query(bool enable_progress_threads,
        bool enable_mpi_threads) {
    return OMPI_SUCCESS;
}

COLLTYPE_T mca_coll_xhc_colltype_to_universal(XHC_COLLTYPE_T xhc_colltype) {
    if(xhc_colltype < 0 || xhc_colltype >= XHC_COLLCOUNT) {
        return -1;
    }

    return xhc_colltype_to_universal_map[xhc_colltype];
}

const char *mca_coll_xhc_colltype_to_str(XHC_COLLTYPE_T colltype) {
    return mca_coll_base_colltype_to_str(xhc_colltype_to_universal(colltype));
}

const char *mca_coll_xhc_config_source_to_str(xhc_config_source_t source) {
    return (source >= 0 && source < XHC_CONFIG_SOURCE_COUNT ?
        xhc_config_source_to_str_map[source] : NULL);
}

static int xhc_register(void) {
    mca_base_var_enum_t *var_enum;
    mca_base_var_enum_flag_t *var_enum_flag;
    const mca_base_var_t *var = NULL;
    int vari;

    char *tmp, *name, *desc;
    int err;

    /* Priority */
    // -----------

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "priority", "Priority of the xhc component.",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_2,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.priority);

    /* SHM Backing dir */
    // ------------------

    mca_coll_xhc_component.shmem_backing = (0 == access("/dev/shm", W_OK) ?
        "/dev/shm" : opal_process_info.job_session_dir);

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "shmem_backing", "Directory to place backing files for shared-memory"
        " control-data communication.", MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
        OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_xhc_component.shmem_backing);

    /* Memcpy limit (see smsc_xpmem_memcpy_chunk_size) */
    // --------------------------------------------------

    int var_index = mca_base_var_find("opal", "smsc", "xpmem", "memcpy_chunk_size");

    if(var_index >= 0) {
        size_t *var_value_ptr;

        err = mca_base_var_get_value(var_index, &var_value_ptr, NULL, NULL);

        if(OPAL_SUCCESS == err) {
            mca_coll_xhc_component.memcpy_chunk_size = *var_value_ptr;
        } else {
            opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
                ompi_coll_base_framework.framework_output, "coll:xhc:component: "
                "Can't get smsc_xpmem_memcpy_chunk_size MCA param value "
                "from var index %d (%d); using xhc default", var_index, err);
        }
    } else {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
            ompi_coll_base_framework.framework_output, "coll:xhc:component: "
            "Can't find smsc_xpmem_memcpy_chunk_size MCA param (%d); "
            "using xhc default", var_index);
    }

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "memcpy_chunk_size", "Maximum size to copy with a single call to memcpy, "
        "following smsc/xpmem's paradigm. A smaller/larger value may provide "
        "better performance on some systems.", MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0,
        MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_xhc_component.memcpy_chunk_size);

    /* Print Info */
    // -------------

    mca_base_var_enum_value_flag_t print_info_options[XHC_COLLCOUNT + 4] = {
        [XHC_COLLCOUNT + 0] = {XHC_PRINT_INFO_HIER_DOT, "dot", 0},
        [XHC_COLLCOUNT + 1] = {XHC_PRINT_INFO_CONFIG, "config", 0},
        [XHC_COLLCOUNT + 2] = {XHC_PRINT_INFO_ALL, "all", 0},
        [XHC_COLLCOUNT + 3] = {0, NULL, 0}
    };

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        print_info_options[t] = (mca_base_var_enum_value_flag_t) {
            1 << t, xhc_colltype_to_str(t), 0};
    }

    err = mca_base_var_enum_create_flag("coll_xhc_print_info",
        print_info_options, &var_enum_flag);
    if(OPAL_SUCCESS != err) {return err;}

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "print_info", "Print information during initialization.",
        MCA_BASE_VAR_TYPE_UNSIGNED_INT, &var_enum_flag->super, 0, 0, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.print_info);

    if(mca_coll_xhc_component.print_info & XHC_PRINT_INFO_ALL) {
        for(mca_base_var_enum_value_flag_t *flag = print_info_options;
                flag->string; flag++) {
            mca_coll_xhc_component.print_info |= flag->flag;
        }
    }

    OBJ_RELEASE(var_enum_flag);

    /* Root ranks for unrooted collectives */
    // --------------------------------------

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "barrier_root", "Internal root for the barrier operation (rank ID).",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.barrier_root);

    // Currently, only rank 0 is supported in allreduce.
    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "allreduce_root", "Internal root for the allreduce operation (rank ID).",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_DEFAULT_ONLY, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.allreduce_root);

    /* Dynamic leader */
    // -----------------

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "dynamic_leader", "Enable dynamic operation-wise group-leader selection.",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.dynamic_leader);

    /* Dynamic reduce */
    // -----------------

    static mca_base_var_enum_value_t dynamic_reduce_options[] = {
        {XHC_DYNAMIC_REDUCE_DISABLED, "disabled"},
        {XHC_DYNAMIC_REDUCE_NON_FLOAT, "non-float"},
        {XHC_DYNAMIC_REDUCE_ALL, "all"},
        {0, NULL}
    };

    err = mca_base_var_enum_create("coll_xhc_dynamic_reduce_options",
        dynamic_reduce_options, &var_enum);
    if(OPAL_SUCCESS != err) {return err;}

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "dynamic_reduce", "Dynamic/out-of-order intra-group reduction.",
        MCA_BASE_VAR_TYPE_INT, var_enum, 0, 0, OPAL_INFO_LVL_6,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.dynamic_reduce);

    OBJ_RELEASE(var_enum);

    /* Load balancing: Reduction leader assistance */
    // ----------------------------------------------

    mca_base_var_enum_value_flag_t reduce_load_balance_options[] = {
        {XHC_REDUCE_LB_LEADER_ASSIST_TOP_LEVEL, "top", XHC_REDUCE_LB_LEADER_ASSIST_ALL},
        {XHC_REDUCE_LB_LEADER_ASSIST_FIRST_CHUNK, "first", XHC_REDUCE_LB_LEADER_ASSIST_ALL},
        {XHC_REDUCE_LB_LEADER_ASSIST_ALL, "all", (XHC_REDUCE_LB_LEADER_ASSIST_TOP_LEVEL |
            XHC_REDUCE_LB_LEADER_ASSIST_FIRST_CHUNK)},
        {0, NULL, 0}
    };

    err = mca_base_var_enum_create_flag("coll_xhc_reduce_load_balance",
        reduce_load_balance_options, &var_enum_flag);
    if(OPAL_SUCCESS != err) {return err;}

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "reduce_load_balance", "Reduction leader assistance modes for load balancing.",
        MCA_BASE_VAR_TYPE_INT, &var_enum_flag->super, 0, 0, OPAL_INFO_LVL_6,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.reduce_load_balance);

    OBJ_RELEASE(var_enum_flag);

    /* (All)reduce uniform chunks */
    // -----------------------------

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "uniform_chunks", "Automatically optimize chunk size in reduction "
        "collectives according to message size, for load balancing.",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.uniform_chunks);

    mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "uniform_chunks_min", "Minimum chunk size for reduction collectives, "
        "when \"uniform chunks\" are enabled.", MCA_BASE_VAR_TYPE_SIZE_T,
        NULL, 0, 0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_xhc_component.uniform_chunks_min);

    /* Apply the op mca defaults. Gotta do it here rather than in-line in
     * the registration loops below, as some iterations are skipped, for the
     * variables that are not applicable (e.g. chunk size in Barrier). */

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        mca_coll_xhc_component.op_mca[t] = op_mca_default[t];
    }
    mca_coll_xhc_component.op_mca_global = op_mca_global_default;

    /* Hierarchy */
    // ------------

    char *topo_list = NULL;

    for(size_t i = 0; i < sizeof(xhc_topo_str)/sizeof(char *); i++) {
        err = opal_asprintf(&tmp, "%s%s%s", (i > 0 ? topo_list : ""),
            (i > 0 ? ", " : ""), xhc_topo_str[i]);
        free(topo_list); topo_list = tmp;
        if(err < 0) {return OPAL_ERR_OUT_OF_RESOURCE;}
    }

    err = opal_asprintf(&desc, "Comma-separated list of topology features to "
        "consider for the hierarchy (%s), for all collectives. Mutually "
        "exclusive with respective op-specific params.", topo_list);
    if(err < 0) {free(topo_list); return OMPI_ERR_OUT_OF_RESOURCE;}

    vari = mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "hierarchy", desc, MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_DEF_UNSET,
        OPAL_INFO_LVL_4, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_xhc_component.op_mca_global.hierarchy);

    free(desc);

    mca_base_var_get(vari, &var);

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        err = opal_asprintf(&name, "%s_hierarchy", xhc_colltype_to_str(t));
        if(err < 0) {free(topo_list); return OPAL_ERR_OUT_OF_RESOURCE;}

        err = opal_asprintf(&desc, "Comma-separated list of topology features to "
            "consider for the hierarchy (%s), for %s.", topo_list, xhc_colltype_to_str(t));
        if(err < 0) {free(topo_list); free(name); return OMPI_ERR_OUT_OF_RESOURCE;}

        mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
            name, desc, MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_7,
            MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.op_mca[t].hierarchy);

        /* The op-specific MCA vars are mutually exclusive with
         * the global one. Otherwise things get complicated. */
        err = xhc_var_check_exclusive("hierarchy", name);

        free(name);
        free(desc);

        if(OPAL_SUCCESS != err) {
            return err;
        }

        /* If the global MCA var was set (because the user set it, it's unset
         * by default), it overrides the op-specific ones. (and both of them
         * can't be set at the same time, as they are mutually exclusive. */
        if(var && MCA_BASE_VAR_SOURCE_DEFAULT != var->mbv_source) {
            mca_coll_xhc_component.op_mca[t].hierarchy =
                strdup(mca_coll_xhc_component.op_mca_global.hierarchy);
        }
    }

    free(topo_list);

    /* Chunk size */
    // ---------------

    vari = mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "chunk_size", "Chunk size(s) for the pipeline (single value, or comma-separated "
        "list for different hierarchy levels (bottom to top)), for all collectives. "
        "Mutually exclusive with respective op-specific params.",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_DEF_UNSET, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.op_mca_global.chunk_size);

    mca_base_var_get(vari, &var);

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        if(XHC_BARRIER == t) {
            continue;
        }

        err = opal_asprintf(&name, "%s_chunk_size", xhc_colltype_to_str(t));
        if(err < 0) {return OPAL_ERR_OUT_OF_RESOURCE;}

        err = opal_asprintf(&desc, "Chunk size(s) for the pipeline (single "
            "value, or comma-separated list for different hierarchy levels "
            "(bottom to top)), for %s.", xhc_colltype_to_str(t));
        if(err < 0) {free(name); return OMPI_ERR_OUT_OF_RESOURCE;}

        mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
            name, desc, MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_8,
            MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.op_mca[t].chunk_size);

        err = xhc_var_check_exclusive("chunk_size", name);

        free(name);
        free(desc);

        if(OPAL_SUCCESS != err) {
            return err;
        }

        if(var && MCA_BASE_VAR_SOURCE_DEFAULT != var->mbv_source) {
            mca_coll_xhc_component.op_mca[t].chunk_size =
                strdup(mca_coll_xhc_component.op_mca_global.chunk_size);
        }
    }

    /* CICO threshold */
    // -----------------

    vari = mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
        "cico_max", "Maximum message size up to which to use CICO, for all collectives. "
        "Mutually exclusive with respective op-specific params.",
        MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, MCA_BASE_VAR_FLAG_DEF_UNSET, OPAL_INFO_LVL_5,
        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_xhc_component.op_mca_global.cico_max);

    mca_coll_xhc_component.op_mca_global.cico_max = OPAL_ALIGN(
        mca_coll_xhc_component.op_mca_global.cico_max, XHC_ALIGN, size_t);

    mca_base_var_get(vari, &var);

    for(int t = 0; t < XHC_COLLCOUNT; t++) {
        if(XHC_BARRIER == t) {
            continue;
        }

        err = opal_asprintf(&name, "%s_cico_max", xhc_colltype_to_str(t));
        if(err < 0) {return OPAL_ERR_OUT_OF_RESOURCE;}

        err = opal_asprintf(&desc, "Maximum message size up to which "
            "to use CICO, for %s.", xhc_colltype_to_str(t));
        if(err < 0) {free(name); return OMPI_ERR_OUT_OF_RESOURCE;}

        mca_base_component_var_register(&mca_coll_xhc_component.super.collm_version,
            name, desc, MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
            OPAL_INFO_LVL_8, MCA_BASE_VAR_SCOPE_READONLY,
            &mca_coll_xhc_component.op_mca[t].cico_max);

        mca_coll_xhc_component.op_mca[t].cico_max = OPAL_ALIGN(
            mca_coll_xhc_component.op_mca[t].cico_max, XHC_ALIGN, size_t);

        err = xhc_var_check_exclusive("cico_max", name);

        free(name);
        free(desc);

        if(OPAL_SUCCESS != err) {
            return err;
        }

        if(var && MCA_BASE_VAR_SOURCE_DEFAULT != var->mbv_source) {
            mca_coll_xhc_component.op_mca[t].cico_max =
                mca_coll_xhc_component.op_mca_global.cico_max;
        }
    }

    return OMPI_SUCCESS;
}

static int xhc_var_check_exclusive(const char *param_a, const char *param_b) {
    return mca_base_var_check_exclusive("ompi",
        mca_coll_xhc_component.super.collm_version.mca_type_name,
        mca_coll_xhc_component.super.collm_version.mca_component_name, param_a,
        mca_coll_xhc_component.super.collm_version.mca_type_name,
        mca_coll_xhc_component.super.collm_version.mca_component_name, param_b);
}

// -----------------------------

static int parse_csv(const char *csv_orig, char sep, char ignore_start,
        char ignore_end, void **vals_dst, int *len_dst, size_t type_size,
        csv_parse_conv_fn_t conv_fn, csv_parse_destruct_fn_t destructor_fn,
        char *err_help_header) {

    if(NULL == csv_orig || 0 == strlen(csv_orig)) {
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
            if(!tmp) {RETURN_WITH_ERROR(return_code, OMPI_ERR_OUT_OF_RESOURCE, end);}
            vals = tmp;
        }

        if(0 != ignore_start) {
            if(*c == ignore_start) {
                ignore_cnt++;
            } else if(*c == ignore_end) {
                ignore_cnt--;
            }

            if(ignore_cnt < 0) {
                RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
            }
        }

        if(0 == ignore_cnt && (*c == sep || '\0' == *c)) {
            char oldc = *c;
            *c = '\0';

            int status = conv_fn(token, (char *) vals + ntokens*type_size);

            if(OMPI_SUCCESS != status) {
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

    if(OMPI_SUCCESS != return_code) {
        if(vals && destructor_fn) {
            for(int i = 0; i < ntokens; i++) {
                destructor_fn((char *) vals + i*type_size);
            }
        }

        free(vals);
    }

    return return_code;
}

// -----------------------------

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

        if('\0' != endptr[0] || nums[i] < 0) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
        }
    }

    ((xhc_rank_range_t *) result)->start_rank = nums[0];
    ((xhc_rank_range_t *) result)->end_rank = (-1 != nums[1] ? nums[1] : nums[0]);

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

    if('*' == s[strlen(s) - 1]) {
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

        if('\0' != endptr[0] || num <= 0) {
            RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
        }

        if(colon_pos) {def->split = num;}
        else {def->max_ranks = num;}

        *numstr = '\0';
    }

    /* Parse locality definition */

    if('[' == s[0]) {
        if(def->repeat) { // repeat only makes sense with named localities
            RETURN_WITH_ERROR(return_code, OMPI_ERR_BAD_PARAM, end);
        }

        s[strlen(s) - 1] = '\0';

        int status = parse_csv(s+1, ',', 0, 0, (void **) &def->rank_list,
            &def->rank_list_len, sizeof(xhc_rank_range_t),
            conv_xhc_loc_def_rank_list, NULL, NULL);

        if(OMPI_SUCCESS != status) {
            RETURN_WITH_ERROR(return_code, status, end);
        }
    } else {
        bool found = false;

        for(size_t i = 0; i < sizeof(xhc_topo_str)/sizeof(char *); i++) {
            if(0 == strcasecmp(s, xhc_topo_str[i])) {
                def->named_loc = xhc_topo_val[i];
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

    if(OMPI_SUCCESS != return_code) {
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
    if(OMPI_SUCCESS != status) {return status;}

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
     *    available), see xhc_topo_str[].
     * 2. A list of ranks that should be grouped together. This is a comma-
     *    separated list of integers, enclosed in [] (I know, list-ception!).
     *    It may also contain range operators (..), to select multiple ranks
     *    at once (e.g. 0..3 expands to 0,1,2,3). Example: [0..15,20,22].
     *    The order of the ranks does not matter.
     *
     * Finally, each such item may be suffixed by a special modifier:
     * 1. The split modifier (:<n>) specifies to group according to the
     *    locality it refers to, but to split each such group into multiple
     *    parts. E.g. the locality 'numa:2' will group ranks into half-numa
     *    groups, such that for each NUMA node, half the ranks are in one
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
     *   contrast to rank lists, which only create a single group, containing
     *   the ranks specified in it. The different items in the '+'-separated
     *   list are consumed in-order left-to-right, and any named localities
     *   are automatically repeated to apply to all ranks that are not included
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

// -----------------------------

static int conv_num_size(char *str, void *result) {
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

    bool legal = ('\0' != str[0]);

    for(char *c = str; *c; c++) {
        if((*c < '0' || *c > '9') && '-' != *c) {
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

    if(NULL == val_str) {
        *chunks_dst = malloc(sizeof(size_t));
        if(NULL == *chunks_dst) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        (*chunks_dst)[0] = (size_t) -1;
        *len_dst = 1;

        return OMPI_SUCCESS;
    }

    int status = parse_csv(val_str, ',', 0, 0, (void **) chunks_dst, len_dst,
        sizeof(size_t), conv_num_size, NULL, "bad-chunk-size-item");

    return status;
}

int mca_coll_xhc_component_parse_cico_max(const char *val_str,
        size_t *cico_max_dst) {

    int status;

    if(val_str) {
        status = conv_num_size((char *) val_str, cico_max_dst);
    } else {
        status = OMPI_ERR_BAD_PARAM;
    }

    if(OMPI_ERR_BAD_PARAM == status) {
        opal_show_help("help-coll-xhc.txt", "bad_cico_max", true, val_str);
    }

    return status;
}
