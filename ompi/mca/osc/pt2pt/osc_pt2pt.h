/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_OSC_PT2PT_H
#define OMPI_OSC_PT2PT_H

#include "opal/class/opal_list.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_hash_table.h"

#include "ompi/mca/osc/osc.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/win/win.h"
#include "ompi/communicator/communicator.h"

struct ompi_osc_pt2pt_component_t {
    /** Extend the basic osc component interface */
    ompi_osc_base_component_t super;

    /** store the state of progress threads for this instance of OMPI */
    bool p2p_c_have_progress_threads;

    /** lock access to datastructures in the component structure */
    opal_mutex_t p2p_c_lock;

    /** List of ompi_osc_pt2pt_module_ts currently in existance.
        Needed so that received fragments can be dispatched to the
        correct module */
    opal_hash_table_t p2p_c_modules;

    /** free list of ompi_osc_pt2pt_sendreq_t structures */
    opal_free_list_t p2p_c_sendreqs;
    /** free list of ompi_osc_pt2pt_replyreq_t structures */
    opal_free_list_t p2p_c_replyreqs;
    /** free list of ompi_osc_pt2pt_longreq_t structures */
    opal_free_list_t p2p_c_longreqs;
};
typedef struct ompi_osc_pt2pt_component_t ompi_osc_pt2pt_component_t;


struct ompi_osc_pt2pt_module_t {
    /** Extend the basic osc module interface */
    ompi_osc_base_module_t super;

    /** lock access to data structures in the current module */
    opal_mutex_t p2p_lock;

    /** lock for "atomic" window updates from reductions */
    opal_mutex_t p2p_acc_lock;

    /** pointer back to window */
    ompi_win_t *p2p_win;

    /** communicator created with this window */
    ompi_communicator_t *p2p_comm;

    /** store weather user disabled locks for this window */
    bool p2p_want_locks;

    /** array of opal_list_ts, not a pointer to one of them.  Array is
        of size <num ranks in communicator>, although only the first
        <num ranks in group> are used for PWSC synchronization */
    opal_list_t *p2p_pending_out_sendreqs;

    /* For MPI_Fence synchronization, the number of messages to send
       in epoch.  For Start/Complete, the number of updates for this
       Complete.  For Post/Wait (poorly named), the number of Complete
       counters we're waiting for.*/
    volatile int32_t p2p_num_pending_out;
    /* For MPI_Fence synchronization, the number of expected incoming
       messages.  For Start/Complete, the number of expected Post
       messages.  For Post/Wait, the number of expected updates from
       complete. */
    volatile int32_t p2p_num_pending_in;

    /* cyclic counter for a unique tage for long messages.  Not
       protected by the p2p_lock - must use create_send_tag() to
       create a send tag */
    volatile int32_t p2p_tag_counter;

    /** list of outstanding long messages that must be processes
        (ompi_osc_pt2pt_request_long) */
    opal_list_t p2p_long_msgs;
    /** number of outstanding long messages */
    volatile int32_t p2p_num_long_msgs;

    struct ompi_group_t *pw_group;
    struct ompi_group_t *sc_group;
};
typedef struct ompi_osc_pt2pt_module_t ompi_osc_pt2pt_module_t;


extern ompi_osc_pt2pt_component_t mca_osc_pt2pt_component;


/*
 * Helper macro for grabbing the module structure from a window instance
 */
#if OMPI_ENABLE_DEBUG

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

static inline ompi_osc_pt2pt_module_t* P2P_MODULE(struct ompi_win_t* win) 
{
    ompi_osc_pt2pt_module_t *module = 
        (ompi_osc_pt2pt_module_t*) win->w_osc_module;

    assert(module->p2p_win == win);

    return module;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#else
#define P2P_MODULE(win) ((ompi_osc_pt2pt_module_t*) win->w_osc_module)
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/*
 * Component functions 
 */
int ompi_osc_pt2pt_component_open(void);

int ompi_osc_pt2pt_component_close(void);

int ompi_osc_pt2pt_component_init(bool enable_progress_threads,
                                 bool enable_mpi_threads);

int ompi_osc_pt2pt_component_finalize(void);

int ompi_osc_pt2pt_component_query(struct ompi_win_t *win,
                                  struct ompi_info_t *info,
                                  struct ompi_communicator_t *comm);

int ompi_osc_pt2pt_component_select(struct ompi_win_t *win,
                                   struct ompi_info_t *info,
                                   struct ompi_communicator_t *comm);


/*
 * Module interface function types 
 */
int ompi_osc_pt2pt_module_free(struct ompi_win_t *win);

int ompi_osc_pt2pt_module_put(void *origin_addr,
                             int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target,
                             int target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_win_t *win);

int ompi_osc_pt2pt_module_accumulate(void *origin_addr,
                                    int origin_count,
                                    struct ompi_datatype_t *origin_dt,
                                    int target,
                                    int target_disp,
                                    int target_count,
                                    struct ompi_datatype_t *target_dt,
                                    struct ompi_op_t *op,
                                    struct ompi_win_t *win);

int ompi_osc_pt2pt_module_get(void *origin_addr,
                             int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target,
                             int target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_win_t *win);

int ompi_osc_pt2pt_module_fence(int assert, struct ompi_win_t *win);

int ompi_osc_pt2pt_module_start(struct ompi_group_t *group,
                               int assert,
                               struct ompi_win_t *win);
int ompi_osc_pt2pt_module_complete(struct ompi_win_t *win);

int ompi_osc_pt2pt_module_post(struct ompi_group_t *group,
                              int assert,
                              struct ompi_win_t *win);

int ompi_osc_pt2pt_module_wait(struct ompi_win_t *win);

int ompi_osc_pt2pt_module_test(struct ompi_win_t *win,
                              int *flag);

int ompi_osc_pt2pt_module_lock(int lock_type,
                              int target,
                              int assert,
                              struct ompi_win_t *win);

int ompi_osc_pt2pt_module_unlock(int target,
                                struct ompi_win_t *win);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_OSC_PT2PT_H */
