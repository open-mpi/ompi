/*
 * Copyright (c) 2021-2023 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
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
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"

#include "opal/mca/shmem/shmem.h"
#include "opal/mca/smsc/smsc.h"

#include "coll_xhc_atomic.h"

#define RETURN_WITH_ERROR(var, err, label) do {(var) = (err); goto label;} \
    while(0)

#define OBJ_RELEASE_IF_NOT_NULL(obj) do {if((obj) != NULL) OBJ_RELEASE(obj);} while(0)

#define REALLOC(p, s, t) do {void *_tmp = realloc(p, (s)*sizeof(t)); \
    if(_tmp) (p) = _tmp;} while(0)

#define PEER_IS_LOCAL(peer_info, rank, loc) \
    (((peer_info)[(rank)].locality & (loc)) == (loc))

#define OMPI_XHC_LOC_EXT_BITS (8*(sizeof(xhc_loc_t) - sizeof(opal_hwloc_locality_t)))
#define OMPI_XHC_LOC_EXT_START (8*sizeof(opal_hwloc_locality_t))

// ---

#define OMPI_XHC_ACK_WIN 0

// Align to CPU cache line (portable way to obtain it?)
#define OMPI_XHC_ALIGN 64

// Call opal_progress every this many ticks when busy-waiting
#define OMPI_XHC_OPAL_PROGRESS_CYCLE 10000

/* Reduction leader-member load balancing, AKA should leaders reduce data?
 * Normally, non-leaders reduce and leaders propagate. But there are instances
 * where leaders can/should also help with the group's reduction load.
 *
 * OMPI_XHC_LB_RLA_TOP_LEVEL: The top level's leader performs reductions
 *   on the top level as if a common member
 *
 * OMPI_XHC_LB_RLA_FIRST_CHUNK: Leaders reduce only a single chunk, on
 *   each level, at the beginning of the operation
 *
 * (OMPI_XHC_LB_RLA_TOP_LEVEL and OMPI_XHC_LB_RLA_FIRST_CHUNK are combinable)
 *
 * OMPI_XHC_LB_RLM_ALL: All leaders performs reductions exactly as if
 *   common members
 *
 * Generally, we might not want leaders reducing, as that may lead to load
 * imbalance, since they will also have to reduce the comm's result(s)
 * on upper levels. Unless a leader is also one on all levels! (e.g. the
 * top-level leader). This leader should probably be assisting in the
 * reduction; otherwise, the only thing he will be doing is checking
 * and updating synchronization flags.
 *
 * Regarding the load balancing problem, the leaders will actually not have
 * anything to do until the first chunk is reduced, so they might as well be
 * made to help the other members with this first chunk. Keep in mind though,
 * this might increase the memory load, and cause this first chunk to take
 * slightly more time to be produced. */
#define OMPI_XHC_LB_RLA_TOP_LEVEL 0x01
#define OMPI_XHC_LB_RLA_FIRST_CHUNK 0x02
#define OMPI_XHC_LB_RLA_ALL 0x80

enum {
    OMPI_XHC_DYNAMIC_REDUCE_DISABLED,
    OMPI_XHC_DYNAMIC_REDUCE_NON_FLOAT,
    OMPI_XHC_DYNAMIC_REDUCE_ALL
};

#define OMPI_XHC_CICO_MAX (mca_coll_xhc_component.cico_max)

/* For other configuration options and default
 * values check coll_xhc_component.c */

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

typedef struct xhc_data_t xhc_data_t;
typedef struct xhc_comm_t xhc_comm_t;

typedef struct xhc_comm_ctrl_t xhc_comm_ctrl_t;
typedef struct xhc_member_ctrl_t xhc_member_ctrl_t;
typedef struct xhc_member_info_t xhc_member_info_t;

typedef struct xhc_reduce_area_t xhc_reduce_area_t;
typedef struct xhc_reduce_queue_item_t xhc_rq_item_t;

typedef struct xhc_rank_range_t xhc_rank_range_t;
typedef struct xhc_loc_def_t xhc_loc_def_t;

OMPI_DECLSPEC extern mca_coll_xhc_component_t mca_coll_xhc_component;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_xhc_module_t);
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(xhc_rq_item_t);
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(xhc_loc_def_item_t);

// ----------------------------------------

struct xhc_coll_fns_t {
    mca_coll_base_module_allreduce_fn_t coll_allreduce;
    mca_coll_base_module_t *coll_allreduce_module;

    mca_coll_base_module_barrier_fn_t coll_barrier;
    mca_coll_base_module_t *coll_barrier_module;

    mca_coll_base_module_bcast_fn_t coll_bcast;
    mca_coll_base_module_t *coll_bcast_module;

    mca_coll_base_module_reduce_fn_t coll_reduce;
    mca_coll_base_module_t *coll_reduce_module;
};

struct mca_coll_xhc_component_t {
    mca_coll_base_component_t super;

    int priority;
    bool print_info;

    char *shmem_backing;

    bool dynamic_leader;

    int barrier_root;

    int dynamic_reduce;
    int lb_reduce_leader_assist;

    bool force_reduce;

    bool uniform_chunks;
    size_t uniform_chunks_min;

    size_t cico_max;

    char *hierarchy_mca;
    char *chunk_size_mca;
};

struct mca_coll_xhc_module_t {
    mca_coll_base_module_t super;

    /* pointers to functions/modules of
     * previous coll components for fallback */
    xhc_coll_fns_t prev_colls;

    // copied from comm
    int comm_size;
    int rank;

    // list of localities to consider during grouping
    char *hierarchy_string;
    xhc_loc_t *hierarchy;
    int hierarchy_len;

    // list of requested chunk sizes, to be applied to comms
    size_t *chunks;
    int chunks_len;

    // temporary (private) internal buffer, for methods like Reduce
    void *rbuf;
    size_t rbuf_size;

    // xhc-specific info for every other rank in the comm
    xhc_peer_info_t *peer_info;

    xhc_data_t *data;

    bool init;
};

struct xhc_peer_info_t {
    xhc_loc_t locality;

    ompi_proc_t *proc;
    mca_smsc_endpoint_t *smsc_ep;

    opal_shmem_ds_t cico_ds;
    void *cico_buffer;
};

struct xhc_data_t {
    xhc_comm_t *comms;
    int comm_count;

    xf_sig_t pvt_coll_seq;
};

struct xhc_comm_t {
    xhc_loc_t locality;
    size_t chunk_size;

    int size;
    int manager_rank;
    int member_id;

    // ---

    // Am I a leader in the current collective?
    bool is_coll_leader;

    // Have handshaked with all members in the current op? (useful to leader)
    bool all_joined;

    /* A reduce set defines a range/area of data to be reduced, and its
     * settings. We require multiple areas, because there might be different
     * circumstances:
     *
     * 1. Under certain load balancing policies, leaders perform reductions
     *    for the just one chunk, and then they don't. Thus, the worker count
     *    changes, and the settings have to recomputed for the next areas.
     *
     * 2. During the "middle" of the operation, all members continuously
     *    reduce data in maximum-sized pieces (according to the configured
     *    chunk size). But, towards the end of the operation, the remaining
     *    elements are less than ((workers * elem_chunk)), we have to
     *    recalculate `elem_chunk`, so that all workers will perform
     *    equal work. */
    struct xhc_reduce_area_t {
        size_t start; // where the area begins
        size_t len; // the size of the area
        int workers; // how many processes perform reductions in the area
        size_t stride; /* how much to advance inside the area after
                     * each reduction, unused for non-combo areas */

        // local process settings
        size_t work_begin; // where to begin the first reduction from
        size_t work_end; // up to where to reduce
        size_t work_chunk; // how much to reduce each time
        size_t work_leftover; /* assigned leftover elements to include as
                            * part of the last reduction in the area */
    } reduce_area[3];
    int n_reduce_areas;

    struct xhc_member_info_t {
        xhc_reg_t *sbuf_reg, *rbuf_reg;
        void *sbuf, *rbuf;
        bool init;
    } *member_info;

    // Queue to keep track of individual reduction progress for different peers
    opal_list_t *reduce_queue;

    // ---

    xhc_comm_ctrl_t *comm_ctrl;
    xhc_member_ctrl_t *member_ctrl;

    opal_shmem_ds_t ctrl_ds;

    // ---

    xhc_member_ctrl_t *my_member_ctrl; // = &member_ctrl[member_id]
    xhc_member_info_t *my_member_info; // = &member_info[member_id]
};

struct xhc_comm_ctrl_t {
    // We want leader_seq, coll_ack, coll_seq to all lie in their own cache lines

    volatile xf_sig_t leader_seq;

    volatile xf_sig_t coll_ack __attribute__((aligned(OMPI_XHC_ALIGN)));

    volatile xf_sig_t coll_seq __attribute__((aligned(OMPI_XHC_ALIGN)));

    /* - Reason *NOT* to keep below fields in the same cache line as coll_seq:
     *
     *   While members busy-wait on leader's coll_seq, initializing the rest of
     *   the fields will trigger cache-coherency-related "invalidate" and then
     *   "read miss" messages, for each store.
     *
     * - Reason to *DO* keep below fields in the same cache line as coll_seq:
     *
     *   Members load from coll_seq, and implicitly fetch the entire cache
     *   line, which also contains the values of the other fields, that will
     *   also need to be loaded soon.
     *
     * (not 100% sure of my description here)
     *
     * Bcast seemed to perform better with the second option, so I went with
     * that one. The best option might also be influenced by the ranks' order
     * of entering in the operation.
     */

    // "Guarded" by members' coll_seq
    volatile int leader_id;
    volatile int leader_rank;
    volatile int cico_id;

    void* volatile data_vaddr;
    volatile xf_size_t bytes_ready;

    char access_token[];
} __attribute__((aligned(OMPI_XHC_ALIGN)));

struct xhc_member_ctrl_t {
    volatile xf_sig_t member_ack; // written by member

    // written by member, at beginning of operation
    volatile xf_sig_t member_seq __attribute__((aligned(OMPI_XHC_ALIGN)));
    volatile int rank;

    void* volatile sbuf_vaddr;
    void* volatile rbuf_vaddr;
    volatile int cico_id;

    // reduction progress counters, written by member
    volatile xf_size_t reduce_ready;
    volatile xf_size_t reduce_done;
} __attribute__((aligned(OMPI_XHC_ALIGN)));

struct xhc_reduce_queue_item_t {
    opal_list_item_t super;
    int member; // ID of member
    size_t count; // current reduction progress for member
    int area_id; // current reduce area
};

// ----------------------------------------

struct xhc_rank_range_t {
    int start_rank, end_rank;
};

struct xhc_loc_def_t {
    opal_list_item_t super;

    opal_hwloc_locality_t named_loc;

    xhc_rank_range_t *rank_list;
    int rank_list_len;

    int split;
    int max_ranks;

    bool repeat;
};

// ----------------------------------------

// coll_xhc_component.c
// --------------------

#define xhc_component_parse_hierarchy(...) mca_coll_xhc_component_parse_hierarchy(__VA_ARGS__)
#define xhc_component_parse_chunk_sizes(...) mca_coll_xhc_component_parse_chunk_sizes(__VA_ARGS__)

int mca_coll_xhc_component_init_query(bool enable_progress_threads,
    bool enable_mpi_threads);

int mca_coll_xhc_component_parse_hierarchy(const char *val_str,
    opal_list_t **level_defs_dst, int *nlevel_defs_dst);
int mca_coll_xhc_component_parse_chunk_sizes(const char *val_str,
    size_t **vals_dst, int *len_dst);

// coll_xhc_module.c
// -----------------

#define xhc_module_install_fns(...) mca_coll_xhc_module_install_fns(__VA_ARGS__)
#define xhc_module_install_fallback_fns(...) mca_coll_xhc_module_install_fallback_fns(__VA_ARGS__)

#define xhc_module_prepare_hierarchy(...) mca_coll_xhc_module_prepare_hierarchy(__VA_ARGS__)

mca_coll_base_module_t *mca_coll_xhc_module_comm_query(
    ompi_communicator_t *comm, int *priority);

int mca_coll_xhc_module_enable(mca_coll_base_module_t *module,
    ompi_communicator_t *comm);
int mca_coll_xhc_module_disable(mca_coll_base_module_t *module,
    ompi_communicator_t *comm);

void mca_coll_xhc_module_install_fallback_fns(xhc_module_t *module,
	ompi_communicator_t *comm, xhc_coll_fns_t *prev_fns_dst);
void mca_coll_xhc_module_install_fns(xhc_module_t *module,
	ompi_communicator_t *comm, xhc_coll_fns_t fns);

int mca_coll_xhc_module_prepare_hierarchy(mca_coll_xhc_module_t *module,
    ompi_communicator_t *comm);

// coll_xhc.c
// ----------

#define xhc_lazy_init(...) mca_coll_xhc_lazy_init(__VA_ARGS__)
#define xhc_fini(...) mca_coll_xhc_fini(__VA_ARGS__)

#define xhc_get_cico(...) mca_coll_xhc_get_cico(__VA_ARGS__)

#define xhc_copy_expose_region(...) mca_coll_xhc_copy_expose_region(__VA_ARGS__)
#define xhc_copy_region_post(...) mca_coll_xhc_copy_region_post(__VA_ARGS__)
#define xhc_copy_from(...) mca_coll_xhc_copy_from(__VA_ARGS__)
#define xhc_copy_close_region(...) mca_coll_xhc_copy_close_region(__VA_ARGS__)

#define xhc_get_registration(...) mca_coll_xhc_get_registration(__VA_ARGS__)
#define xhc_return_registration(...) mca_coll_xhc_return_registration(__VA_ARGS__)

int mca_coll_xhc_lazy_init(mca_coll_xhc_module_t *module, ompi_communicator_t *comm);
void mca_coll_xhc_fini(mca_coll_xhc_module_t *module);

void *mca_coll_xhc_get_cico(xhc_peer_info_t *peer_info, int rank);

int mca_coll_xhc_copy_expose_region(void *base, size_t len, xhc_copy_data_t **region_data);
void mca_coll_xhc_copy_region_post(void *dst, xhc_copy_data_t *region_data);
int mca_coll_xhc_copy_from(xhc_peer_info_t *peer_info, void *dst,
    void *src, size_t size, void *access_token);
void mca_coll_xhc_copy_close_region(xhc_copy_data_t *region_data);

void *mca_coll_xhc_get_registration(xhc_peer_info_t *peer_info,
    void *peer_vaddr, size_t size, xhc_reg_t **reg);
void mca_coll_xhc_return_registration(xhc_reg_t *reg);

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

// Miscellaneous
// -------------

#define xhc_allreduce_internal(...) mca_coll_xhc_allreduce_internal(__VA_ARGS__)

int mca_coll_xhc_allreduce_internal(const void *sbuf, void *rbuf, size_t count,
    ompi_datatype_t *datatype, ompi_op_t *op, ompi_communicator_t *ompi_comm,
    mca_coll_base_module_t *module, bool require_bcast);

// ----------------------------------------

// Rollover-safe check that flag has reached/exceeded thresh, with max deviation
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
        for(int i = 0; i < OMPI_XHC_OPAL_PROGRESS_CYCLE; i++) {
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

// ----------------------------------------

END_C_DECLS

#endif
