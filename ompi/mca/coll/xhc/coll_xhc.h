/*
 * Copyright (c) 2021-2026 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef MCA_COLL_XHC_EXPORT_H
#define MCA_COLL_XHC_EXPORT_H

#include "ompi_config.h"

#include <stdint.h>
#include <limits.h>

#include "mpi.h"

#include "ompi/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_base_util.h"

#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"

#include "opal/class/opal_hash_table.h"
#include "opal/mca/shmem/shmem.h"
#include "opal/mca/smsc/smsc.h"
#include "opal/util/minmax.h"

#include "coll_xhc_intrinsic.h"

#define RETURN_WITH_ERROR(var, err, label) do {(var) = (err); goto label;} \
    while(0)

#define OBJ_RELEASE_IF_NOT_NULL(obj) do {if(NULL != (obj)) {OBJ_RELEASE(obj);}} while(0)

#define REALLOC(p, s, t) do {void *_tmp = realloc(p, (s)*sizeof(t)); \
    if(_tmp) {(p) = _tmp;}} while(0)

#define PEER_IS_LOCAL(peer_info, rank, loc) \
    (((peer_info)[(rank)].locality & (loc)) == (loc))

#define XHC_LOC_EXT_BITS (8*(sizeof(xhc_loc_t) - sizeof(opal_hwloc_locality_t)))
#define XHC_LOC_EXT_START (8*sizeof(opal_hwloc_locality_t))

#define XHC_CALL_FALLBACK(fns, colltype, op, ...) \
    ((mca_coll_base_module_ ## op ## _fn_t) (fns).coll_fn[colltype]) \
    (__VA_ARGS__, (mca_coll_base_module_t *) (fns).coll_module[colltype])

// Change the pointers so that the fallback is always called in the future
#define XHC_INSTALL_FALLBACK(module, comm, colltype, op) do { \
    (comm)->c_coll->coll_ ## op = (mca_coll_base_module_ ## op ## _fn_t) \
        (module)->prev_colls.coll_fn[colltype]; \
    (comm)->c_coll->coll_ ## op ## _module = (mca_coll_base_module_t *) \
        (module)->prev_colls.coll_module[colltype]; \
} while(0)

#define WARN_ONCE(...) do { \
    static bool warn_shown = false; \
    if(!warn_shown) { \
        opal_output_verbose(MCA_BASE_VERBOSE_WARN, \
            ompi_coll_base_framework.framework_output, __VA_ARGS__); \
        warn_shown = true; \
    } \
} while(0)

// ---

// Set according to CPU cache line (safe bet)
#define XHC_ALIGN 128

/* Chunk size can't be set lower than this.
 * Mustn't be lower that any IMM_SIZE below. */
#define XHC_MIN_CHUNK_SIZE 64

/* While we treat the cache line as 128 bytes (XHC_ALIGN), we calculate
 * IMM_SIZE according to a more conservative 64 bytes, to ensure the imm
 * data is always inside one cache line. Though, it might also be okay to
 * also occupy the next cache line, with the L2 adjacent line prefetcher in
 * mind. Our results were mixed, so for now we choose the simplest/safest
 * option. */
#define XHC_BCAST_IMM_SIZE (64 - (offsetof(xhc_comm_ctrl_t, \
    imm_data) - offsetof(xhc_comm_ctrl_t, seq)))
#define XHC_REDUCE_IMM_SIZE (64 - (offsetof(xhc_member_ctrl_t, \
    imm_data) - offsetof(xhc_member_ctrl_t, seq)))

// Call opal_progress every this many ticks when busy-waiting
#define XHC_OPAL_PROGRESS_CYCLE 10000

#define XHC_PRINT_INFO_HIER_DOT (1 << (sizeof(mca_coll_xhc_component.print_info) * 8 - 4))
#define XHC_PRINT_INFO_CONFIG (1 << (sizeof(mca_coll_xhc_component.print_info) * 8 - 3))
#define XHC_PRINT_INFO_ALL (1 << (sizeof(mca_coll_xhc_component.print_info) * 8 - 2))

// ---

BEGIN_C_DECLS

// ----------------------------------------

typedef uint32_t xhc_loc_t;
typedef void xhc_reg_t;
typedef void xhc_copy_data_t;

typedef struct mca_coll_xhc_component_t mca_coll_xhc_component_t;
typedef struct mca_coll_xhc_module_t mca_coll_xhc_module_t;
typedef struct mca_coll_xhc_module_t xhc_module_t;

typedef struct xhc_coll_fns_t xhc_coll_fns_t;
typedef struct xhc_peer_info_t xhc_peer_info_t;

typedef struct xhc_op_mca_t xhc_op_mca_t;
typedef struct xhc_op_config_t xhc_op_config_t;
typedef struct xhc_op_data_t xhc_op_data_t;

typedef struct xhc_comm_t xhc_comm_t;
typedef xhc_comm_t xhc_subcomm_t;

typedef struct xhc_comm_ctrl_t xhc_comm_ctrl_t;
typedef struct xhc_member_ctrl_t xhc_member_ctrl_t;
typedef struct xhc_member_info_t xhc_member_info_t;

typedef struct xhc_sh_slice_t xhc_sh_slice_t;
typedef struct xhc_reduce_queue_item_t xhc_rq_item_t;
typedef struct xhc_bcast_queue_item_t xhc_bq_item_t;
typedef struct xhc_reduce_workload_t xhc_reduce_workload_t;

typedef struct xhc_hierarchy_t xhc_hierarchy_t;
typedef struct xhc_hierarchy_level_t xhc_hierarchy_level_t;
typedef struct xhc_rank_range_t xhc_rank_range_t;
typedef struct xhc_loc_def_t xhc_loc_def_t;

OMPI_DECLSPEC extern mca_coll_xhc_component_t mca_coll_xhc_component;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_xhc_module_t);
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(xhc_rq_item_t);
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(xhc_bq_item_t);
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(xhc_loc_def_item_t);

// ----------------------------------------

typedef enum xhc_dynamic_reduce_enum_t {
    XHC_DYNAMIC_REDUCE_DISABLED = 0,
    XHC_DYNAMIC_REDUCE_NON_FLOAT,
    XHC_DYNAMIC_REDUCE_ALL
} xhc_dynamic_reduce_enum_t;

/* Reduce load balancing, AKA what should leaders do? Leaders propagate the
 * group's results, and perform reductions on upper level(s). Thus we generally
 * don't want them to not do reductions inside the group, so they can focus on
 * the upper ones. However, in certain instances, like at the start of the op
 * (when no chunks are ready to be reduced on the upper levels), or on the top
 * level, the leaders do not actually have any other work to do, so might as
 * well enlist the for the intra-group reductions.
 *
 * - ASSIST_TOP_LEVEL: The top level's leader performs reductions
 *   (on the top level), as if a common member.
 * - ASSIST_FIRST_CHUNK: Leaders reduce only a single chunk,
 *   on each level, at the beginning of the operation.
 * - ASSIST_ALL: All leaders performs reductions exactly as if common members
 *
 * (ASSIST_TOP_LEVEL and ASSIST_FIRST_CHUNK are combinable) */
typedef enum xhc_reduction_load_balance_enum_t {
    XHC_REDUCTION_LB_LEADER_ASSIST_TOP_LEVEL = 0x01,
    XHC_REDUCTION_LB_LEADER_ASSIST_FIRST_CHUNK = 0x02,
    XHC_REDUCTION_LB_LEADER_ASSIST_ALL = 0x80
} xhc_reduction_load_balance_enum_t;

/* Changes to XHC_COLLTYPE_T have to be reflected in:
 * 1. xhc_colltype_to_universal_map[]
 * 2. xhc_colltype_to_c_coll_fn_offset_map[]
 * 3. xhc_colltype_to_c_coll_module_offset_map[]
 * 4. xhc_colltype_to_coll_base_fn_offset_map[] */
typedef enum XHC_COLLTYPE_T {
    XHC_BCAST = 0,
    XHC_BARRIER,
    XHC_REDUCE,
    XHC_ALLREDUCE,

    XHC_COLLCOUNT
} XHC_COLLTYPE_T;

typedef enum xhc_config_source_t {
    XHC_CONFIG_SOURCE_INFO_GLOBAL = 0,
    XHC_CONFIG_SOURCE_INFO_OP,
    XHC_CONFIG_SOURCE_MCA_GLOBAL,
    XHC_CONFIG_SOURCE_MCA_OP,

    XHC_CONFIG_SOURCE_COUNT
} xhc_config_source_t;

typedef enum xhc_copy_method_t {
    /* 0 is reserved */
    XHC_COPY_IMM = 1,
    XHC_COPY_CICO,
    XHC_COPY_SMSC_NO_MAP,
    XHC_COPY_SMSC_MAP,
    XHC_COPY_SMSC, /* kind of a catch-all value for ops that don't
                    * make a distinction between map/no_map */
} xhc_copy_method_t;

struct mca_coll_xhc_component_t {
    mca_coll_base_component_t super;

    int priority;
    uint print_info;

    char *shmem_backing;

    size_t memcpy_chunk_size;

    bool dynamic_leader;

    uint barrier_root;
    uint allreduce_root;

    xhc_dynamic_reduce_enum_t dynamic_reduce;
    xhc_reduction_load_balance_enum_t reduction_load_balance;

    // Storage for the op-specific MCA params
    struct xhc_op_mca_t {
        char *hierarchy;
        char *chunk_size;
        size_t cico_max;
    } op_mca[XHC_COLLCOUNT];

    // Storage for the all-op MCA params
    xhc_op_mca_t op_mca_global;
};

/* The opal_shmem_ds_t is a bit on the large side, and we regularly pass
 * its kind around via allgather (during comm creation). We make an effort
 * to replace it with this struct, which omits the seg_name field. Instead,
 * with this approach, we simply share the various 'salts' used to create
 * the name of the shm backing file, and recreate it locally as needed. */
typedef struct xhc_shmem_ds_t {
    char opal_ds_short[offsetof(opal_shmem_ds_t, seg_name)];
    uid_t uid;
    opal_vpid_t vpid;
    uint32_t cid;
    char id[8]; // max 7, 1 null
    uint id_ext[4];
} xhc_shmem_ds_t;

struct mca_coll_xhc_module_t {
    mca_coll_base_module_t super;

    ompi_communicator_t *comm;
    int rank;
    int n_ranks;

    /* pointers to functions/modules of
     * previous coll components for fallback */
    struct xhc_coll_fns_t {
        void (*coll_fn[XHC_COLLCOUNT])(void);
        void *coll_module[XHC_COLLCOUNT];
    } prev_colls;

    // ---

    uint barrier_root;
    uint allreduce_root;

    bool zcopy_support;
    bool zcopy_map_support;

    ssize_t smsc_reg_size;

    // Cached topology information
    int n_cores_l3;
    int n_cores_numa;
    int n_cores_socket;
    size_t l3_cache_size;

    bool prefetchw_strong;

    // number of virtual localities created
    int n_virt_localities;

    // book-keeping for info on other ranks
    struct xhc_peer_info_t {
        xhc_loc_t locality;
        ompi_proc_t *proc;

        mca_smsc_endpoint_t *smsc_ep;
    } *peer_info;

    // temporary (private) internal buffer, for methods like Reduce
    void *rbuf;
    size_t rbuf_size;

    // ---

    // Cache to avoid multiple calls to hierarchy_make for the same spec
    opal_hash_table_t hierarchy_cache;

    // The configuration for each op, as generated by parsing the MCA params
    struct xhc_op_config_t {
        char *hierarchy_string;

        size_t chunk_min;
        size_t chunk_max;
        size_t cico_max;

        bool hierarchy_autotune;
        bool chunk_autotune;

        xhc_config_source_t hierarchy_source;
        xhc_config_source_t chunk_source;
        xhc_config_source_t cico_max_source;
    } op_config[XHC_COLLCOUNT];

    // State and misc data for each op
    xhc_op_data_t *op_data[XHC_COLLCOUNT];

    bool init;
    bool error;
};

// -----

struct xhc_op_data_t {
    // Pointers back to module & config, for convenience
    xhc_module_t *module;
    xhc_op_config_t *config;

    // Settings
    // --------

    XHC_COLLTYPE_T colltype;

    xhc_hierarchy_t *hierarchy;
    size_t chunk_min, chunk_max;
    size_t cico_max;

    // Data
    // ----

    xhc_comm_t *comms;
    int n_comms;

    /* A de-duplicated list of the various comm sizes that exist,
     * used for choosing a chunk size in Reduce and Allreduce. */
    int *comm_sizes;
    int n_comm_sizes;

    /* A directory recording for each rank in the
     * ompi comm its parent in the hierarchy. */
    int *parents;

    // CICO buffer for each rank
    struct {
        xhc_shmem_ds_t shmem_ds;
        void *buffer;
    } *cico;

    bool init;

    // States
    // ------

    xf_sig_t seq;

    /* Book-keeping for multi-sliced shared areas.
     * See MPI_Reduce implementation for details. */
    struct xhc_sh_slice_t {
        int ack_level; // up to which hierarchy level we've received an ack
        xhc_comm_t *ceil; // the 'ceil' comm in the op
        xf_sig_t seq; // the seq number of the op
        bool is_cico; // whether the op is CICO type
        size_t len; // the length of the message in the op (bytes)
    } *slices;
    int n_slices;// int n_slices;

    int slice_id; // designated slice ID for this op
};

struct xhc_comm_t {
    // Pointer back to op_data, for convenience
    xhc_op_data_t *data;

    xhc_loc_t locality;
    int level;

    int my_id;
    int size;

    size_t chunk_size;

    /* Be careful using this. This is the list of the ranks of this comm's
     * 'regular' members, yes. But there are cases where a rank will take
     * the place of another in a member slot (common case: root != 0). */
    int *rank_list;

    // ---

    /* Op state (any) */

    // Am I a leader in the current collective?
    bool is_leader;

    // Op-specific state tracking
    int op_state;

    // ---

    /* Reduce op state */

    int leader_id;

    struct xhc_reduce_workload_t {
        size_t chunk; // chunk size (fixed)
        ssize_t leftover; // leftover for last chunk (can be negative)
        int w_first; // worker count for the first chunk
        int w_main; // worker count for the other chunks
    } reduce_work;

    // Keep track of reduce progress for each peer
    opal_list_t *reduce_queue;

    // Help keep track in Allreduce's multi-root bcast
    opal_list_t *bcast_queue;

    /* These are cached copies of the respective fields in
     * my_ctrl, to minimize accesses to the shared variables. */
    size_t reduce_idx;
    size_t reduce_avail;

    struct xhc_member_info_t {
        xhc_reg_t *sbuf_reg, *rbuf_reg;
        void *sbuf, *rbuf;
        bool attach, join;

        size_t sbuf_avail, rbuf_avail; // elements
        size_t reduce_idx; // cached value
    } *member_info;

    xhc_member_info_t *my_info; // = &member_info[my_id]

    // ---

    /* Shared structs */

    xhc_comm_ctrl_t *comm_ctrl;

    xhc_member_ctrl_t *member_ctrl;
    xhc_member_ctrl_t *my_ctrl; // = &member_ctrl[my_id]

    /* Original addresses of the potentially multi-sliced comm/member
     * ctrl, since the pointers above are modified in per-op basis. */
    char *comm_ctrl_base;
    char *member_ctrl_base;

    xhc_shmem_ds_t shmem_ds;

    // ---

    xhc_subcomm_t **subcomms;

    xhc_comm_t *up, *down;
    xhc_comm_t *top, *bottom;
    bool is_top, is_bottom;
};

// -----

struct xhc_comm_ctrl_t {
    volatile xf_sig_t leader_seq;
    volatile xf_sig_t ack __attribute__((aligned(XHC_ALIGN)));

    /* below fields are all in the same cache line */
    volatile xf_sig_t seq __attribute__((aligned(XHC_ALIGN)));

    union {
        struct {
            volatile int rank;
            void* volatile buf_vaddr;
            volatile xf_size_t data_ready;
            volatile xf_size_t data_ready_aux;
        };

        /* sized 4 here, but will actually use all that's left
         * of the cache line. Keep it last in the struct. */
        volatile char imm_data[4];
    };

    /* imm_data can overwrite access_token. That's
     * okay, the two aren't used at the same time. */
    volatile char access_token[];
} __attribute__((aligned(XHC_ALIGN)));

struct xhc_member_ctrl_t {
    volatile xf_sig_t ack;

    /* below fields in same cache line */

    volatile xf_sig_t seq __attribute__((aligned(XHC_ALIGN)));

    volatile xf_size_t reduce_avail;
    volatile xf_size_t reduce_idx;

    union {
        struct {
            void* volatile sbuf_vaddr;
            void* volatile rbuf_vaddr;
            volatile int rank;
        };

        /* sized 4 here, but will actually use all that's left
         * of the cache line. Keep it last in the struct. */
        volatile char imm_data[4];
    };
} __attribute__((aligned(XHC_ALIGN)));

// -----

struct xhc_reduce_queue_item_t {
    opal_list_item_t super;
    int queue_rank; // to implement the preferred reduce order
    int member; // ID of member
    int sub_member; // the sub-member responsible for the chunk at count
    size_t count; // current reduction progress for member
};

struct xhc_bcast_queue_item_t {
    opal_list_item_t super;
    int root_id; // member ID
    size_t avail, count; // progress counters
};

struct xhc_hierarchy_t {
    struct xhc_hierarchy_level_t {
        xhc_loc_t loc;
        int max_members;
        int approx_members;
        int split;
    } *levels;
    int n_levels;
};

struct xhc_loc_def_t {
    opal_list_item_t super;

    opal_hwloc_locality_t named_loc;

    struct xhc_rank_range_t {
        int start_rank, end_rank;
    } *rank_list;

    int rank_list_len;

    int max_members;
    int approx_members;
    int split;

    bool repeat;
};

// ----------------------------------------

// coll_xhc_component.c
// --------------------

#define xhc_colltype_to_universal(...) \
    mca_coll_xhc_colltype_to_universal(__VA_ARGS__)
#define xhc_colltype_to_str(...) \
    mca_coll_xhc_colltype_to_str(__VA_ARGS__)
#define xhc_config_source_to_str(...) \
    mca_coll_xhc_config_source_to_str(__VA_ARGS__)

#define xhc_component_parse_hierarchy(...) \
    mca_coll_xhc_component_parse_hierarchy(__VA_ARGS__)
#define xhc_component_parse_chunk_size(...) \
    mca_coll_xhc_component_parse_chunk_size(__VA_ARGS__)
#define xhc_component_parse_size(...) \
    mca_coll_xhc_component_parse_size(__VA_ARGS__)

int mca_coll_xhc_component_init_query(bool enable_progress_threads,
    bool enable_mpi_threads);
COLLTYPE_T mca_coll_xhc_colltype_to_universal(XHC_COLLTYPE_T xhc_colltype);
const char *mca_coll_xhc_colltype_to_str(XHC_COLLTYPE_T colltype);
const char *mca_coll_xhc_config_source_to_str(xhc_config_source_t source);

int mca_coll_xhc_component_parse_hierarchy(const char *val_str,
    opal_list_t **level_defs_dst, int *nlevel_defs_dst,
    const char *err_param_name);
int mca_coll_xhc_component_parse_chunk_size(const char *val_str,
    size_t *chunk_min, size_t *chunk_max, const char *err_param_name);
int mca_coll_xhc_component_parse_size(const char *val_str, size_t *size_dst,
    const char *err_help_msg, const char *err_param_name);

// coll_xhc_module.c
// -----------------

#define xhc_module_set_coll_fns(...) \
    mca_coll_xhc_module_set_coll_fns(__VA_ARGS__)
#define xhc_module_prepare_hierarchy(...) \
    mca_coll_xhc_module_prepare_hierarchy(__VA_ARGS__)

mca_coll_base_module_t *mca_coll_xhc_module_comm_query(
    ompi_communicator_t *comm, int *priority);

int mca_coll_xhc_module_enable(mca_coll_base_module_t *module,
    ompi_communicator_t *comm);
int mca_coll_xhc_module_disable(mca_coll_base_module_t *module,
    ompi_communicator_t *comm);

void mca_coll_xhc_module_set_coll_fns(ompi_communicator_t *comm,
    xhc_coll_fns_t *new_fns, xhc_coll_fns_t *save_fns);

// coll_xhc.c
// ----------

#define xhc_fini(...) mca_coll_xhc_fini(__VA_ARGS__)
#define xhc_get_op_data(...) mca_coll_xhc_get_op_data(__VA_ARGS__)

#define xhc_read_op_config(...) mca_coll_xhc_read_op_config(__VA_ARGS__)
#define xhc_op_config_source_param(...) mca_coll_xhc_op_config_source_param(__VA_ARGS__)

#define xhc_shmem_create(...) mca_coll_xhc_shmem_create(__VA_ARGS__)
#define xhc_shmem_attach(...) mca_coll_xhc_shmem_attach(__VA_ARGS__)
#define xhc_shmem_detach(...) mca_coll_xhc_shmem_detach(__VA_ARGS__)
#define xhc_shmem_unlink(...) mca_coll_xhc_shmem_unlink(__VA_ARGS__)

#define xhc_get_cico(...) mca_coll_xhc_get_cico(__VA_ARGS__)

#define xhc_copy_expose_region(...) mca_coll_xhc_copy_expose_region(__VA_ARGS__)
#define xhc_copy_region_post(...) mca_coll_xhc_copy_region_post(__VA_ARGS__)
#define xhc_copy_from(...) mca_coll_xhc_copy_from(__VA_ARGS__)
#define xhc_copy_close_region(...) mca_coll_xhc_copy_close_region(__VA_ARGS__)

#define xhc_get_registration(...) mca_coll_xhc_get_registration(__VA_ARGS__)
#define xhc_return_registration(...) mca_coll_xhc_return_registration(__VA_ARGS__)

void mca_coll_xhc_fini(mca_coll_xhc_module_t *module);

xhc_op_data_t *mca_coll_xhc_get_op_data(xhc_module_t *module,
    XHC_COLLTYPE_T colltype, size_t size);

int mca_coll_xhc_read_op_config(xhc_module_t *module, XHC_COLLTYPE_T colltype);
char *mca_coll_xhc_op_config_source_param(const char *var_name,
    XHC_COLLTYPE_T colltype, xhc_config_source_t source);

void *mca_coll_xhc_shmem_create(xhc_shmem_ds_t *xhc_ds, size_t size,
    ompi_communicator_t *comm, const char *id, uint id_ext[4]);
void *mca_coll_xhc_shmem_attach(xhc_shmem_ds_t *xhc_ds);
int mca_coll_xhc_shmem_detach(xhc_shmem_ds_t *xhc_ds);
int mca_coll_xhc_shmem_unlink(xhc_shmem_ds_t *xhc_ds);

void *mca_coll_xhc_get_cico(xhc_op_data_t *data, int rank, int slice_id);

int mca_coll_xhc_copy_expose_region(void *base, size_t len,
    xhc_copy_data_t **region_data);
void mca_coll_xhc_copy_region_post(void *dst,
    xhc_copy_data_t *region_data, size_t length);

int mca_coll_xhc_copy_from(xhc_op_data_t *data, int rank,
    void *dst, void *src, size_t size, void *access_token);
void mca_coll_xhc_copy_close_region(xhc_copy_data_t *region_data);

void *mca_coll_xhc_get_registration(xhc_op_data_t *data, int rank,
    void *peer_vaddr, size_t size, xhc_reg_t **reg);
void mca_coll_xhc_return_registration(xhc_reg_t *reg);

// coll_xhc_comm.c
// --------------------

#define xhc_comms_make(...) mca_coll_xhc_comms_make(__VA_ARGS__)
#define xhc_comms_fini(...) mca_coll_xhc_comms_fini(__VA_ARGS__)

int mca_coll_xhc_comms_make(xhc_module_t *module, xhc_op_data_t *data);
void mca_coll_xhc_comms_fini(xhc_op_data_t *data);

// coll_xhc_hierarchy.c
// --------------------

#define xhc_hierarchy_make(...) mca_coll_xhc_hierarchy_make(__VA_ARGS__)

int mca_coll_xhc_hierarchy_make(xhc_module_t *module, XHC_COLLTYPE_T colltype,
    const char *hierarchy_string, xhc_hierarchy_t **hierarchy_dst);

// Primitives (respective file)
// ----------------------------

int mca_coll_xhc_bcast(void *buf, size_t count, ompi_datatype_t *datatype,
    int root, ompi_communicator_t *comm, mca_coll_base_module_t *module);

int mca_coll_xhc_barrier(ompi_communicator_t *ompi_comm,
    mca_coll_base_module_t *module);

int mca_coll_xhc_reduce(const void *sbuf, void *rbuf,
    size_t count, ompi_datatype_t *datatype, ompi_op_t *op, int root,
    ompi_communicator_t *comm, mca_coll_base_module_t *module);

int mca_coll_xhc_allreduce(const void *sbuf, void *rbuf,
    size_t count, ompi_datatype_t *datatype, ompi_op_t *op,
    ompi_communicator_t *comm, mca_coll_base_module_t *module);

// coll_xhc_allreduce.c
// --------------------

#define xhc_allreduce_internal(...) mca_coll_xhc_allreduce_internal(__VA_ARGS__)

int mca_coll_xhc_allreduce_internal(const void *sbuf, void *rbuf,
    size_t count, ompi_datatype_t *datatype, ompi_op_t *op, int root,
    ompi_communicator_t *ompi_comm, mca_coll_base_module_t *module,
    XHC_COLLTYPE_T colltype);

// ----------------------------------------

/* Rollover-safe check that _flag_ has reached _thresh_,
 * without having exceeded it by more than _win_. */
static inline bool CHECK_FLAG(volatile xf_sig_t *flag,
        xf_sig_t thresh, xf_sig_t win) {

    // This is okay because xf_sig_t is unsigned. Take care.
    // The cast's necessity is dependent on the size of xf_sig_t
    return ((xf_sig_t) (*flag - thresh) <= win);
}

static inline void WAIT_FLAG(volatile xf_sig_t *flag,
        xf_sig_t thresh, xf_sig_t win) {
    bool ready = false;

    do {
        for(int i = 0; i < XHC_OPAL_PROGRESS_CYCLE; i++) {
            if(CHECK_FLAG(flag, thresh, win)) {
                ready = true;
                break;
            }

            /* xf_sig_t f = *flag;
            if(CHECK_FLAG(&f, thresh, win)) {
                ready = true;
                break;
            } else if(CHECK_FLAG(&f, thresh, 1000))
                printf("Debug: Flag check with window %d failed, "
                    "but succeeded with window 1000. flag = %d, "
                    "thresh = %d\n", win, f, thresh); */
        }

        if(!ready) {
            opal_progress();
        }
    } while(!ready);
}

/* Modeled after smsc/xpmem. We don't currently want to deviate from its
 * behaviour. But this adds a second parameter (additionally to smsc/xpmem's)
 * to set if one wants to adjust the limit. Ideally there would be a singular
 * opal-level MCA parameter for this, and/or an opal_memcpy that would do the
 * job that xhc_mempy and smsc/xpmem's respective function do. */
static inline void xhc_memcpy(void *dst, const void *src, size_t size) {
    for(size_t copied = 0; copied < size; ) {
        size_t chunk = opal_min(size - copied,
            mca_coll_xhc_component.memcpy_chunk_size);

        memcpy((char *) dst + copied, (char *) src + copied, chunk);
        copied += chunk;
    }
}

static inline void xhc_memcpy_offset(void *dst, const void *src,
        size_t offset, size_t size) {
    xhc_memcpy((char *) dst + offset, (char *) src + offset, size);
}

// ----------------------------------------

END_C_DECLS

#endif
