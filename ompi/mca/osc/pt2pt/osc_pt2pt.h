/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_OSC_PT2PT_H
#define OMPI_OSC_PT2PT_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/threads/threads.h"
#include "opal/mca/btl/btl.h"

#include "ompi/win/win.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "opal/mca/btl/btl.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/memchecker.h"

#include "osc_pt2pt_header.h"

BEGIN_C_DECLS

struct ompi_osc_pt2pt_frag_t;

struct ompi_osc_pt2pt_component_t {
    /** Extend the basic osc component interface */
    ompi_osc_base_component_t super;

    /** lock access to modules */
    opal_mutex_t lock;

    /** cid -> module mapping */
    opal_hash_table_t modules;

    /** module count */
    int module_count;

    /** free list of ompi_osc_pt2pt_frag_t structures */
    opal_free_list_t frags;

    /** Free list of requests */
    opal_free_list_t requests;

    /** PT2PT component buffer size */
    unsigned int buffer_size;

    /** Lock for pending_operations */
    opal_mutex_t pending_operations_lock;

    /** List of operations that need to be processed */
    opal_list_t pending_operations;

    /** Is the progress function enabled? */
    bool progress_enable;
};
typedef struct ompi_osc_pt2pt_component_t ompi_osc_pt2pt_component_t;


struct ompi_osc_pt2pt_peer_t {
    /** make this an opal object */
    opal_object_t super;

    /** pointer to the current send fragment for each outgoing target */
    struct ompi_osc_pt2pt_frag_t *active_frag;

    /** lock for this peer */
    opal_mutex_t lock;

    /** fragments queued to this target */
    opal_list_t queued_frags;

    /** number of acks pending.  New requests can not be sent out if there are
     * acks pending (to fulfill the ordering constraints of accumulate) */
    uint32_t num_acks_pending;

    /** number of fragments incomming (negative - expected, positive - unsynchronized) */
    int32_t passive_incoming_frag_count;

    /** peer is in an access epoch */
    bool access_epoch;

    /** eager sends are active to this peer */
    bool eager_send_active;
};
typedef struct ompi_osc_pt2pt_peer_t ompi_osc_pt2pt_peer_t;

OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_peer_t);

#define SEQ_INVALID 0xFFFFFFFFFFFFFFFFULL

/** Module structure.  Exactly one of these is associated with each
    PT2PT window */
struct ompi_osc_pt2pt_module_t {
    /** Extend the basic osc module interface */
    ompi_osc_base_module_t super;

    /** window should have accumulate ordering... */
    bool accumulate_ordering;

    /** pointer to free on cleanup (may be NULL) */
    void *free_after;

    /** Base pointer for local window */
    void *baseptr;

    /** communicator created with this window.  This is the cid used
        in the component's modules mapping. */
    ompi_communicator_t *comm;

    /** Local displacement unit. */
    int disp_unit;

    /** Mutex lock protecting module data */
    opal_mutex_t lock;

    /** condition variable associated with lock */
    opal_condition_t cond;

    /** lock for atomic window updates from reductions */
    opal_mutex_t acc_lock;

    /** peer data */
    ompi_osc_pt2pt_peer_t *peers;

    /** Nmber of communication fragments started for this epoch, by
        peer.  Not in peer data to make fence more manageable. */
    uint32_t *epoch_outgoing_frag_count;

    /** cyclic counter for a unique tage for long messages. */
    unsigned int tag_counter;

    /* Number of outgoing fragments that have completed since the
       begining of time */
    uint32_t outgoing_frag_count;
    /* Next outgoing fragment count at which we want a signal on cond */
    uint32_t outgoing_frag_signal_count;

    /* Number of incoming fragments that have completed since the
       begining of time */
    uint32_t active_incoming_frag_count;
    /* Next incoming buffer count at which we want a signal on cond */
    uint32_t active_incoming_frag_signal_count;

    /* Number of flush ack requests send since beginning of time */
    uint64_t flush_ack_requested_count;
    /* Number of flush ack replies received since beginning of
       time. cond should be signalled on every flush reply
       received. */
    uint64_t flush_ack_received_count;

    /** True if the access epoch is a passive target access epoch */
    bool passive_target_access_epoch;

    /** start sending data eagerly */
    bool active_eager_send_active;

    /** Indicates the window is in an all access epoch (fence, lock_all) */
    bool all_access_epoch;

    /* ********************* PWSC data ************************ */
    struct ompi_group_t *pw_group;
    struct ompi_group_t *sc_group;

    /** Number of "ping" messages from the remote post group we've
        received */
    int32_t num_post_msgs;

    /** Number of "count" messages from the remote complete group
        we've received */
    int32_t num_complete_msgs;

    /* ********************* LOCK data ************************ */

    /** Status of the local window lock.  One of 0 (unlocked),
        MPI_LOCK_EXCLUSIVE, or MPI_LOCK_SHARED. */
    int32_t lock_status;

    /** lock for locks_pending list */
    opal_mutex_t locks_pending_lock;

    /** target side list of lock requests we couldn't satisfy yet */
    opal_list_t locks_pending;

    /** origin side list of locks currently outstanding */
    opal_list_t outstanding_locks;

    uint64_t lock_serial_number;

    unsigned char *incoming_buffer;
    ompi_request_t *frag_request;

    /* enforce accumulate semantics */
    opal_atomic_lock_t accumulate_lock;
    opal_list_t        pending_acc;

    /* enforce pscw matching */
    /** list of unmatched post messages */
    opal_list_t        pending_posts;

    /** Lock for garbage collection lists */
    opal_mutex_t gc_lock;

    /** List of requests that need to be freed */
    opal_list_t request_gc;

    /** List of buffers that need to be freed */
    opal_list_t buffer_gc;
};
typedef struct ompi_osc_pt2pt_module_t ompi_osc_pt2pt_module_t;
OMPI_MODULE_DECLSPEC extern ompi_osc_pt2pt_component_t mca_osc_pt2pt_component;

struct ompi_osc_pt2pt_pending_t {
    opal_list_item_t super;
    ompi_osc_pt2pt_module_t *module;
    int source;
    ompi_osc_pt2pt_header_t header;
};
typedef struct ompi_osc_pt2pt_pending_t ompi_osc_pt2pt_pending_t;
OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_pending_t);

#define GET_MODULE(win) ((ompi_osc_pt2pt_module_t*) win->w_osc_module)

extern bool ompi_osc_pt2pt_no_locks;

int ompi_osc_pt2pt_attach(struct ompi_win_t *win, void *base, size_t len);
int ompi_osc_pt2pt_detach(struct ompi_win_t *win, void *base);

int ompi_osc_pt2pt_free(struct ompi_win_t *win);

int ompi_osc_pt2pt_put(void *origin_addr,
                             int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target,
                             OPAL_PTRDIFF_TYPE target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_win_t *win);

int ompi_osc_pt2pt_accumulate(void *origin_addr,
                                    int origin_count,
                                    struct ompi_datatype_t *origin_dt,
                                    int target,
                                    OPAL_PTRDIFF_TYPE target_disp,
                                    int target_count,
                                    struct ompi_datatype_t *target_dt,
                                    struct ompi_op_t *op,
                                    struct ompi_win_t *win);

int ompi_osc_pt2pt_get(void *origin_addr,
                             int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target,
                             OPAL_PTRDIFF_TYPE target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_win_t *win);

int ompi_osc_pt2pt_compare_and_swap(void *origin_addr,
                                   void *compare_addr,
                                   void *result_addr,
                                   struct ompi_datatype_t *dt,
                                   int target,
                                   OPAL_PTRDIFF_TYPE target_disp,
                                   struct ompi_win_t *win);

int ompi_osc_pt2pt_fetch_and_op(void *origin_addr,
                               void *result_addr,
                               struct ompi_datatype_t *dt,
                               int target,
                               OPAL_PTRDIFF_TYPE target_disp,
                               struct ompi_op_t *op,
                               struct ompi_win_t *win);

int ompi_osc_pt2pt_get_accumulate(void *origin_addr,
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

int ompi_osc_pt2pt_rput(void *origin_addr,
                       int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       OPAL_PTRDIFF_TYPE target_disp,
                       int target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win,
                       struct ompi_request_t **request);

int ompi_osc_pt2pt_rget(void *origin_addr,
                       int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       OPAL_PTRDIFF_TYPE target_disp,
                       int target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win,
                       struct ompi_request_t **request);

int ompi_osc_pt2pt_raccumulate(void *origin_addr,
                              int origin_count,
                              struct ompi_datatype_t *origin_dt,
                              int target,
                              OPAL_PTRDIFF_TYPE target_disp,
                              int target_count,
                              struct ompi_datatype_t *target_dt,
                              struct ompi_op_t *op,
                              struct ompi_win_t *win,
                              struct ompi_request_t **request);

int ompi_osc_pt2pt_rget_accumulate(void *origin_addr,
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

int ompi_osc_pt2pt_fence(int assert, struct ompi_win_t *win);

/* received a post message */
int osc_pt2pt_incoming_post (ompi_osc_pt2pt_module_t *module, int source);

int ompi_osc_pt2pt_start(struct ompi_group_t *group,
                        int assert,
                        struct ompi_win_t *win);
int ompi_osc_pt2pt_complete(struct ompi_win_t *win);

int ompi_osc_pt2pt_post(struct ompi_group_t *group,
                              int assert,
                              struct ompi_win_t *win);

int ompi_osc_pt2pt_wait(struct ompi_win_t *win);

int ompi_osc_pt2pt_test(struct ompi_win_t *win,
                              int *flag);

int ompi_osc_pt2pt_lock(int lock_type,
                              int target,
                              int assert,
                              struct ompi_win_t *win);

int ompi_osc_pt2pt_unlock(int target,
                                struct ompi_win_t *win);

int ompi_osc_pt2pt_lock_all(int assert,
                           struct ompi_win_t *win);

int ompi_osc_pt2pt_unlock_all(struct ompi_win_t *win);

int ompi_osc_pt2pt_sync(struct ompi_win_t *win);

int ompi_osc_pt2pt_flush(int target,
                        struct ompi_win_t *win);
int ompi_osc_pt2pt_flush_all(struct ompi_win_t *win);
int ompi_osc_pt2pt_flush_local(int target,
                              struct ompi_win_t *win);
int ompi_osc_pt2pt_flush_local_all(struct ompi_win_t *win);

int ompi_osc_pt2pt_set_info(struct ompi_win_t *win, struct ompi_info_t *info);
int ompi_osc_pt2pt_get_info(struct ompi_win_t *win, struct ompi_info_t **info_used);

int ompi_osc_pt2pt_component_irecv(ompi_osc_pt2pt_module_t *module,
                                  void *buf,
                                  size_t count,
                                  struct ompi_datatype_t *datatype,
                                  int src,
                                  int tag,
                                  struct ompi_communicator_t *comm);

int ompi_osc_pt2pt_component_isend(ompi_osc_pt2pt_module_t *module,
                                  void *buf,
                                  size_t count,
                                  struct ompi_datatype_t *datatype,
                                  int dest,
                                  int tag,
                                  struct ompi_communicator_t *comm);

/**
 * ompi_osc_pt2pt_progress_pending_acc:
 *
 * @short Progress one pending accumulation or compare and swap operation.
 *
 * @param[in] module   - OSC PT2PT module
 *
 * @long If the accumulation lock can be aquired progress one pending
 *       accumulate or compare and swap operation.
 */
int ompi_osc_pt2pt_progress_pending_acc (ompi_osc_pt2pt_module_t *module);


/**
 * mark_incoming_completion:
 *
 * @short Increment incoming completeion count.
 *
 * @param[in] module - OSC PT2PT module
 * @param[in] source - Passive target source or MPI_PROC_NULL (active target)
 *
 * @long This function incremements either the passive or active incoming counts.
 *       If the count reaches the signal count we signal the module's condition.
 *       This function uses atomics if necessary so it is not necessary to hold
 *       the module lock before calling this function.
 */
static inline void mark_incoming_completion (ompi_osc_pt2pt_module_t *module, int source)
{
    if (MPI_PROC_NULL == source) {
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                             "mark_incoming_completion marking active incoming complete. count = %d. signal = %d",
                             (int) module->active_incoming_frag_count + 1, module->active_incoming_frag_signal_count));
        OPAL_THREAD_ADD32((int32_t *) &module->active_incoming_frag_count, 1);
        if (module->active_incoming_frag_count >= module->active_incoming_frag_signal_count) {
            opal_condition_broadcast(&module->cond);
        }
    } else {
        ompi_osc_pt2pt_peer_t *peer = module->peers + source;
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                             "mark_incoming_completion marking passive incoming complete. source = %d, count = %d",
                             source, (int) peer->passive_incoming_frag_count + 1));
        OPAL_THREAD_ADD32((int32_t *) &peer->passive_incoming_frag_count, 1);
        if (0 == peer->passive_incoming_frag_count) {
            opal_condition_broadcast(&module->cond);
        }
    }
}

/**
 * mark_outgoing_completion:
 *
 * @short Increment outgoing count.
 *
 * @param[in] module - OSC PT2PT module
 *
 * @long This function is used to signal that an outgoing send is complete. It
 *       incrememnts only the outgoing fragment count and signals the module
 *       condition the fragment count is >= the signal count. This function
 *       uses atomics if necessary so it is not necessary to hold the module
 *       lock before calling this function.
 */
static inline void mark_outgoing_completion (ompi_osc_pt2pt_module_t *module)
{
    OPAL_THREAD_ADD32((int32_t *) &module->outgoing_frag_count, 1);
    if (module->outgoing_frag_count >= module->outgoing_frag_signal_count) {
        opal_condition_broadcast(&module->cond);
    }
}

/**
 * ompi_osc_signal_outgoing:
 *
 * @short Increment outgoing signal counters.
 *
 * @param[in] module - OSC PT2PT module
 * @param[in] target - Passive target rank or MPI_PROC_NULL (active target)
 * @param[in] count  - Number of outgoing messages to signal.
 *
 * @long This function uses atomics if necessary so it is not necessary to hold
 *       the module lock before calling this function.
 */
static inline void ompi_osc_signal_outgoing (ompi_osc_pt2pt_module_t *module, int target, int count)
{
    OPAL_THREAD_ADD32((int32_t *) &module->outgoing_frag_signal_count, count);
    if (MPI_PROC_NULL != target) {
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                             "ompi_osc_signal_outgoing_passive: target = %d, count = %d, total = %d", target,
                             count, module->epoch_outgoing_frag_count[target] + count));
        OPAL_THREAD_ADD32((int32_t *) (module->epoch_outgoing_frag_count + target), count);
    }
}

/**
 * osc_pt2pt_copy_on_recv:
 *
 * @short Helper function. Copies data from source to target through the
 * convertor.
 *
 * @param[in] target     - destination for the data
 * @param[in] source     - packed data
 * @param[in] source_len - length of source buffer
 * @param[in] proc       - proc that packed the source data
 * @param[in] count      - count of datatype items
 * @param[in] datatype   - datatype used for unpacking
 *
 * @long This functions unpacks data from the source buffer into the target
 *       buffer. The copy is done with a convertor generated from proc,
 *       datatype, and count.
 */
static inline void osc_pt2pt_copy_on_recv (void *target, void *source, size_t source_len, ompi_proc_t *proc,
                                          int count, ompi_datatype_t *datatype)
{
    opal_convertor_t convertor;
    uint32_t iov_count = 1;
    struct iovec iov;
    size_t max_data;

    /* create convertor */
    OBJ_CONSTRUCT(&convertor, opal_convertor_t);

    /* initialize convertor */
    opal_convertor_copy_and_prepare_for_recv(proc->super.proc_convertor, &datatype->super, count, target,
                                             0, &convertor);

    iov.iov_len  = source_len;
    iov.iov_base = (IOVBASE_TYPE *) source;
    max_data     = iov.iov_len;
    MEMCHECKER(memchecker_convertor_call(&opal_memchecker_base_mem_defined, &convertor));

    opal_convertor_unpack (&convertor, &iov, &iov_count, &max_data);

    MEMCHECKER(memchecker_convertor_call(&opal_memchecker_base_mem_noaccess, &convertor));

    OBJ_DESTRUCT(&convertor);
}

/**
 * osc_pt2pt_copy_for_send:
 *
 * @short: Helper function. Copies data from source to target through the
 * convertor.
 *
 * @param[in] target     - destination for the packed data
 * @param[in] target_len - length of the target buffer
 * @param[in] source     - original data
 * @param[in] proc       - proc this data will be sent to
 * @param[in] count      - count of datatype items
 * @param[in] datatype   - datatype used for packing
 *
 * @long This functions packs data from the source buffer into the target
 *       buffer. The copy is done with a convertor generated from proc,
 *       datatype, and count.
 */
static inline void osc_pt2pt_copy_for_send (void *target, size_t target_len, void *source, ompi_proc_t *proc,
                                           int count, ompi_datatype_t *datatype)
{
    opal_convertor_t convertor;
    uint32_t iov_count = 1;
    struct iovec iov;
    size_t max_data;

    OBJ_CONSTRUCT(&convertor, opal_convertor_t);

    opal_convertor_copy_and_prepare_for_send(proc->super.proc_convertor, &datatype->super,
                                             count, source, 0, &convertor);

    iov.iov_len = target_len;
    iov.iov_base = (IOVBASE_TYPE *) target;
    opal_convertor_pack(&convertor, &iov, &iov_count, &max_data);

    OBJ_DESTRUCT(&convertor);
}

/**
 * osc_pt2pt_request_gc_clean:
 *
 * @short Release finished PML requests and accumulate buffers.
 *
 * @long This function exists because it is not possible to free a PML request
 *       or buffer from a request completion callback. We instead put requests
 *       and buffers on the module's garbage collection lists and release then
 *       at a later time.
 */
static inline void osc_pt2pt_gc_clean (ompi_osc_pt2pt_module_t *module)
{
    ompi_request_t *request;
    opal_list_item_t *item;

    OPAL_THREAD_LOCK(&module->gc_lock);

    while (NULL != (request = (ompi_request_t *) opal_list_remove_first (&module->request_gc))) {
        OPAL_THREAD_UNLOCK(&module->gc_lock);
        ompi_request_free (&request);
        OPAL_THREAD_LOCK(&module->gc_lock);
    }

    while (NULL != (item = opal_list_remove_first (&module->buffer_gc))) {
        OBJ_RELEASE(item);
    }

    OPAL_THREAD_UNLOCK(&module->gc_lock);
}

static inline void osc_pt2pt_gc_add_request (ompi_osc_pt2pt_module_t *module, ompi_request_t *request)
{
    OPAL_THREAD_SCOPED_LOCK(&module->gc_lock,
                            opal_list_append (&module->request_gc, (opal_list_item_t *) request));
}

static inline void osc_pt2pt_gc_add_buffer (ompi_osc_pt2pt_module_t *module, opal_list_item_t *buffer)
{
    OPAL_THREAD_SCOPED_LOCK(&module->gc_lock,
                            opal_list_append (&module->buffer_gc, buffer));
}

static inline void osc_pt2pt_add_pending (ompi_osc_pt2pt_pending_t *pending)
{
    OPAL_THREAD_SCOPED_LOCK(&mca_osc_pt2pt_component.pending_operations_lock,
                            opal_list_append (&mca_osc_pt2pt_component.pending_operations, &pending->super));
}

#define OSC_PT2PT_FRAG_TAG   0x10000
#define OSC_PT2PT_FRAG_MASK  0x0ffff

/**
 * get_tag:
 *
 * @short Get a send/recv tag for large memory operations.
 *
 * @param[in] module - OSC PT2PT module
 *
 * @long This function aquires a 16-bit tag for use with large memory operations. The
 *       tag will be odd or even depending on if this is in a passive target access
 *       or not.
 */
static inline int get_tag(ompi_osc_pt2pt_module_t *module)
{
    /* the LSB of the tag is used be the receiver to determine if the
       message is a passive or active target (ie, where to mark
       completion). */
    int tmp = module->tag_counter + !!(module->passive_target_access_epoch);

    module->tag_counter = (module->tag_counter + 2) & OSC_PT2PT_FRAG_MASK;

    return tmp;
}

/**
 * ompi_osc_pt2pt_accumulate_lock:
 *
 * @short Internal function that spins until the accumulation lock has
 *        been aquired.
 *
 * @param[in] module - OSC PT2PT module
 *
 * @returns 0
 *
 * @long This functions blocks until the accumulation lock has been aquired. This
 *       behavior is only acceptable from a user-level call as blocking in a
 *       callback may cause deadlock. If a callback needs the accumulate lock and
 *       it is not available it should be placed on the pending_acc list of the
 *       module. It will be released by ompi_osc_pt2pt_accumulate_unlock().
 */
static inline int ompi_osc_pt2pt_accumulate_lock (ompi_osc_pt2pt_module_t *module)
{
    while (opal_atomic_trylock (&module->accumulate_lock)) {
        opal_progress ();
    }

    return 0;
}

/**
 * ompi_osc_pt2pt_accumulate_trylock:
 *
 * @short Try to aquire the accumulation lock.
 *
 * @param[in] module - OSC PT2PT module
 *
 * @returns 0 if the accumulation lock was aquired
 * @returns 1 if the lock was not available
 *
 * @long This function will try to aquire the accumulation lock. This function
 *       is safe to call from a callback.
 */
static inline int ompi_osc_pt2pt_accumulate_trylock (ompi_osc_pt2pt_module_t *module)
{
    return opal_atomic_trylock (&module->accumulate_lock);
}

/**
 * ompi_osc_pt2pt_accumulate_unlock:
 *
 * @short Unlock the accumulation lock and release a pending accumulation operation.
 *
 * @param[in] module - OSC PT2PT module
 *
 * @long This function unlocks the accumulation lock and release a single pending
 *       accumulation operation if one exists. This function may be called recursively.
 */
static inline void ompi_osc_pt2pt_accumulate_unlock (ompi_osc_pt2pt_module_t *module)
{
    opal_atomic_unlock (&module->accumulate_lock);
    if (0 != opal_list_get_size (&module->pending_acc)) {
        ompi_osc_pt2pt_progress_pending_acc (module);
    }
}

static inline bool ompi_osc_pt2pt_check_access_epoch (ompi_osc_pt2pt_module_t *module, int rank)
{
   return module->all_access_epoch || module->peers[rank].access_epoch;
}

END_C_DECLS

#endif /* OMPI_OSC_PT2PT_H */
