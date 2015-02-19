/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OSC_PORTALS4_PORTALS4_H
#define OSC_PORTALS4_PORTALS4_H

#include <portals4.h>
#include "ompi/group/group.h"
#include "ompi/communicator/communicator.h"

#define OSC_PORTALS4_MB_DATA    0x0000000000000000ULL
#define OSC_PORTALS4_MB_CONTROL 0x1000000000000000ULL

/* Component structure.  There is one of these per process, per process lifetime.
 *
 * Currently, the Portals 4 one-sided implementation only uses a
 * matching interface for all communication.  There are plans for
 * using a non-matching interface for a few windows (they each need
 * their own PTE, which is a precious resource).  In anticipation of
 * that, we initialize the network interfaces and keep them in the
 * component structure (for win create), but then also keep a handle
 * copy in the window module, so that we can use the right structures
 * once we add the non-matching support.
 *
 * The sizes are kept in the component structure because we can only
 * find them during PtlNIInit, and it would be poor to do that for
 * every window creation.  Again, the window module has a copy of the
 * max sizes, but tweaked to match the window configuration (ie,
 * there's one atomic size, instead of an ordered and unordered size,
 * since we know the ordering constraints during window creation).
 */
struct ompi_osc_portals4_component_t {
    ompi_osc_base_component_t super;

    ptl_handle_ni_t matching_ni_h;
    ptl_handle_eq_t matching_eq_h;
    ptl_pt_index_t matching_pt_idx;
    ptl_size_t matching_atomic_max;
    ptl_size_t matching_fetch_atomic_max;
    ptl_size_t matching_atomic_ordered_size;

    opal_free_list_t requests; /* request free list for the r* communication variants */
};
typedef struct ompi_osc_portals4_component_t ompi_osc_portals4_component_t;
OMPI_DECLSPEC extern ompi_osc_portals4_component_t mca_osc_portals4_component;

/* Data about me exposed to remote peers.  Used in generalized active
   target and passive target synchronization. */
struct ompi_osc_portals4_node_state_t {
    volatile int32_t post_count;
    volatile int32_t complete_count;
    volatile uint64_t lock;
};
typedef struct ompi_osc_portals4_node_state_t ompi_osc_portals4_node_state_t;

#define LOCK_ILLEGAL   (0x4000000000000000ULL)
#define LOCK_UNLOCKED  (0x0000000000000000ULL)
#define LOCK_EXCLUSIVE (0x0000000100000000ULL)

/* Module structure.  There is one of these per window */
struct ompi_osc_portals4_module_t {
    ompi_osc_base_module_t super;
    void *free_after; /* if non-null, this pointer should be free()ed when window destroyed */
    struct ompi_communicator_t *comm; /* communicator which backs this window (unique to this window) */
    int disp_unit; /* if -1, have to look at disp_units */
    int *disp_units; /* array (possibly NULL!) of displacement units, per peer */
    ptl_handle_ni_t ni_h; /* network interface used by this window */
    ptl_pt_index_t pt_idx; /* portal table index used by this window (this will be same across window) */
    ptl_handle_ct_t ct_h; /* Counting event handle used for completion in this window */
#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    ptl_handle_md_t *md_h; /* memory descriptor describing all of memory used by this window */
    ptl_handle_md_t *req_md_h; /* memory descriptor with event completion used by this window */
#else
    ptl_handle_md_t md_h[1]; /* memory descriptor describing all of memory used by this window */
    ptl_handle_md_t req_md_h[1]; /* memory descriptor with event completion used by this window */
#endif
    ptl_handle_me_t data_me_h; /* data match list entry (MB are CID | OSC_PORTALS4_MB_DATA) */
    ptl_handle_me_t control_me_h; /* match list entry for control data (node_state_t).  Match bits are (CID | OSC_PORTALS4_MB_CONTROL). */
    int64_t opcount;
    ptl_match_bits_t match_bits; /* match bits for module.  Same as cid for comm in most cases. */

    ptl_size_t atomic_max; /* max size of atomic messages.  Will guarantee ordering IF ordering requested */
    ptl_size_t fetch_atomic_max; /* max size of fetchatomic messages.  Will guarantee ordering IF ordering requested */

    /* variable containing specified value.  Needed for atomic
    increments so they can be non-blocking */
    int32_t zero;
    int32_t one;

    ompi_group_t *start_group;
    ompi_group_t *post_group;
    opal_list_t outstanding_locks;

    bool passive_target_access_epoch; /* True if the access epoch is a passive target access epoch */

    /* things that are remotely accessible */
    ompi_osc_portals4_node_state_t state;
};
typedef struct ompi_osc_portals4_module_t ompi_osc_portals4_module_t;


static inline size_t
get_displacement(ompi_osc_portals4_module_t *module,
                 int target)
{
    if (-1 == module->disp_unit) {
        return module->disp_units[target];
    } else {
        return module->disp_unit;
    }
}


/*
 * See note in ompi/mtl/portals4/mtl_portals4.h for how we deal with
 * platforms that don't allow us to crate an MD that covers all of
 * memory.
 */
static inline void
ompi_osc_portals4_get_md(const void *ptr, const ptl_handle_md_t *array,
                         ptl_handle_md_t *md_h, void **base_ptr)
{
#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    int mask = (1ULL << (OPAL_PORTALS4_MAX_VA_SIZE - OPAL_PORTALS4_MAX_MD_SIZE + 1)) - 1;
    int which = (((uintptr_t) ptr) >> (OPAL_PORTALS4_MAX_MD_SIZE - 1)) & mask;
    *md_h = array[which];
    *base_ptr = (void*) (which * (1ULL << (OPAL_PORTALS4_MAX_MD_SIZE - 1)));
#else
    *md_h = array[0];
    *base_ptr = 0;
#endif
}


static inline int
ompi_osc_portals4_get_num_mds(void)
{
#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    return (1 << (OPAL_PORTALS4_MAX_VA_SIZE - OPAL_PORTALS4_MAX_MD_SIZE + 1));
#else
    return 1;
#endif
}



int ompi_osc_portals4_attach(struct ompi_win_t *win, void *base, size_t len);
int ompi_osc_portals4_detach(struct ompi_win_t *win, void *base);

int ompi_osc_portals4_free(struct ompi_win_t *win);

int ompi_osc_portals4_put(void *origin_addr,
                          int origin_count,
                          struct ompi_datatype_t *origin_dt,
                          int target,
                          OPAL_PTRDIFF_TYPE target_disp,
                          int target_count,
                          struct ompi_datatype_t *target_dt,
                          struct ompi_win_t *win);

int ompi_osc_portals4_get(void *origin_addr,
                          int origin_count,
                          struct ompi_datatype_t *origin_dt,
                          int target,
                          OPAL_PTRDIFF_TYPE target_disp,
                          int target_count,
                          struct ompi_datatype_t *target_dt,
                          struct ompi_win_t *win);

int ompi_osc_portals4_accumulate(void *origin_addr,
                                 int origin_count,
                                 struct ompi_datatype_t *origin_dt,
                                 int target,
                                 OPAL_PTRDIFF_TYPE target_disp,
                                 int target_count,
                                 struct ompi_datatype_t *target_dt,
                                 struct ompi_op_t *op,
                                 struct ompi_win_t *win);

int ompi_osc_portals4_compare_and_swap(void *origin_addr,
                                       void *compare_addr,
                                       void *result_addr,
                                       struct ompi_datatype_t *dt,
                                       int target,
                                       OPAL_PTRDIFF_TYPE target_disp,
                                       struct ompi_win_t *win);

int ompi_osc_portals4_fetch_and_op(void *origin_addr,
                                   void *result_addr,
                                   struct ompi_datatype_t *dt,
                                   int target,
                                   OPAL_PTRDIFF_TYPE target_disp,
                                   struct ompi_op_t *op,
                                   struct ompi_win_t *win);

int ompi_osc_portals4_get_accumulate(void *origin_addr, 
                                     int origin_count, 
                                     struct ompi_datatype_t *origin_datatype,
                                     void *result_addr, 
                                     int result_count, 
                                     struct ompi_datatype_t *result_datatype,
                                     int target_rank, 
                                     MPI_Aint target_disp, 
                                     int target_count,
                                     struct ompi_datatype_t *target_datatype, 
                                     struct ompi_op_t *op, 
                                     struct ompi_win_t *win);

int ompi_osc_portals4_rput(void *origin_addr,
                           int origin_count,
                           struct ompi_datatype_t *origin_dt,
                           int target,
                           OPAL_PTRDIFF_TYPE target_disp,
                           int target_count,
                           struct ompi_datatype_t *target_dt,
                           struct ompi_win_t *win,
                           struct ompi_request_t **request);

int ompi_osc_portals4_rget(void *origin_addr,
                           int origin_count,
                           struct ompi_datatype_t *origin_dt,
                           int target,
                           OPAL_PTRDIFF_TYPE target_disp,
                           int target_count,
                           struct ompi_datatype_t *target_dt,
                           struct ompi_win_t *win,
                           struct ompi_request_t **request);

int ompi_osc_portals4_raccumulate(void *origin_addr,
                                  int origin_count,
                                  struct ompi_datatype_t *origin_dt,
                                  int target,
                                  OPAL_PTRDIFF_TYPE target_disp,
                                  int target_count,
                                  struct ompi_datatype_t *target_dt,
                                  struct ompi_op_t *op,
                                  struct ompi_win_t *win,
                                  struct ompi_request_t **request);

int ompi_osc_portals4_rget_accumulate(void *origin_addr, 
                                      int origin_count, 
                                      struct ompi_datatype_t *origin_datatype,
                                      void *result_addr, 
                                      int result_count, 
                                      struct ompi_datatype_t *result_datatype,
                                      int target_rank, 
                                      MPI_Aint target_disp, 
                                      int target_count,
                                      struct ompi_datatype_t *target_datatype, 
                                      struct ompi_op_t *op, 
                                      struct ompi_win_t *win,
                                      struct ompi_request_t **request);

int ompi_osc_portals4_fence(int assert, struct ompi_win_t *win);

int ompi_osc_portals4_start(struct ompi_group_t *group,
                            int assert,
                            struct ompi_win_t *win);

int ompi_osc_portals4_complete(struct ompi_win_t *win);

int ompi_osc_portals4_post(struct ompi_group_t *group,
                           int assert,
                           struct ompi_win_t *win);

int ompi_osc_portals4_wait(struct ompi_win_t *win);

int ompi_osc_portals4_test(struct ompi_win_t *win,
                           int *flag);

int ompi_osc_portals4_lock(int lock_type,
                           int target,
                           int assert,
                           struct ompi_win_t *win);

int ompi_osc_portals4_unlock(int target,
                             struct ompi_win_t *win);


int ompi_osc_portals4_lock_all(int assert,
                               struct ompi_win_t *win);

int ompi_osc_portals4_unlock_all(struct ompi_win_t *win);

int ompi_osc_portals4_sync(struct ompi_win_t *win);

int ompi_osc_portals4_flush(int target,
                            struct ompi_win_t *win);
int ompi_osc_portals4_flush_all(struct ompi_win_t *win);
int ompi_osc_portals4_flush_local(int target,
                                  struct ompi_win_t *win);
int ompi_osc_portals4_flush_local_all(struct ompi_win_t *win);

int ompi_osc_portals4_set_info(struct ompi_win_t *win, struct ompi_info_t *info);
int ompi_osc_portals4_get_info(struct ompi_win_t *win, struct ompi_info_t **info_used);

static inline int
ompi_osc_portals4_complete_all(ompi_osc_portals4_module_t *module)
{
    int ret;
    ptl_ct_event_t event;

    ret = PtlCTWait(module->ct_h, module->opcount, &event);
    if (PTL_OK != ret || 0 != event.failure) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: flush_all ct failure: ret=%d, failure=%d\n",
                            __FILE__, __LINE__, ret, (int) event.failure);
        event.success = event.failure = 0;
        PtlCTSet(module->ct_h, event);
        module->opcount = 0;
    }
    assert(event.success == (size_t) module->opcount);

    PtlAtomicSync();

    return ret;
}

static inline ptl_process_t
ompi_osc_portals4_get_peer(ompi_osc_portals4_module_t *module, int rank)
{
    ompi_proc_t *proc = ompi_comm_peer_lookup(module->comm, rank);
    return *((ptl_process_t*) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);
}

static inline ptl_process_t
ompi_osc_portals4_get_peer_group(struct ompi_group_t *group, int rank)
{
    ompi_proc_t *proc = ompi_group_get_proc_ptr(group, rank);
    return *((ptl_process_t*) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);
}

#endif
