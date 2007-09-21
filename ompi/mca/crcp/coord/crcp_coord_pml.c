/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
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

/*
 *
 * Case 1:
 * -------
 *  No Messages Sent yet
 *
 *  Rank N     | Rank M
 * ------------+------------
 *   --        |  --
 *  **** Checkpoint ****
 *   --        |  --
 * Action:
 *   Nothing to do. Just return once all bookmarks have been exchanged.
 *
 *
 * Case 2:
 * -------
 * All sent messages have been received.
 *
 *  Rank N     | Rank M
 * ------------+------------
 *   send(1)   |  recv(1)
 *   send(2)   |  recv(2)
 *  **** Checkpoint ****
 *   --        |  --
 * Action:
 *  Nothing to do. Just return once all bookmarks have been exchanged.
 *
 *
 * Case 3:
 * -------
 *  Checkpoint during a send, receive has *not* been posted.
 *
 *  Rank N     | Rank M
 * ------------+------------
 *   send(1)   |  recv(1)
 *   send(2)   |  --
 *  **** Checkpoint ****
 *   --        |  recv(2)
 * Action:
 *   Rank M posts a draining receive to match Rank N's send(2). 
 *   Rank N stalls until the send operation is complete.
 *   After checkpoint is complete, when Rank M attempts to post recv(2), it
 *   will match the message off the drained message list, and complete
 *   right away.
 *
 *
 * Case 4:
 * -------
 *  Checkpoint during a send, receive has been posted. Neither operation
 *  is completed yet.
 *
 *  Rank N     | Rank M
 * ------------+------------
 *   send(1)   |  recv(1)
 *   send(2)...|  recv(2)...
 *  **** Checkpoint ****
 *   --        |  --
 * Action:
 *   Rank N stalls until the send operation is complete.
 *   Rank M stalls until the receive operation is complete.
 *
 *
 * Case 5:
 * -------
 *  Checkpoint when the receive has been posted, but *not* the
 *  cooresponding send.
 *
 *  Rank N     | Rank M
 * ------------+------------
 *   send(1)   |  recv(1)
 *   --        |  recv(2)...
 *  **** Checkpoint ****
 *   send(2)   |  --
 * Action:
 *  Rank N need not do anything.
 *  Rank M will cancel recv(2) to clear its data from the PML stack.
 *  The checkpoint will continue. Once the checkpoint is complete
 *  Rank M will repost recv(2) and continue execution.
 *
 *  So the results looks like:
 *  Rank N     | Rank M
 * ------------+------------
 *   send(1)   |  recv(1)
 *   --        |  recv(2)...
 *  **** Start Checkpoint ****
 *   --        |  recv(2)... (cancel)
 *  **** End Checkpoint ****
 *  **** Start Continue/Restart ****
 *   --        |  recv(2)... (repost)
 *  **** End Continue/Restart ****
 *   send(2)   |  --
 *
 *
 * Case 5.1:
 * ---------
 *  Checkpoint when *multiple* receive has been posted, but *not* the
 *  cooresponding sends.
 *
 *  Rank N     | Rank M
 * ------------+------------
 *   send(1)   |  recv(1)
 *   --        |  irecv(2)...
 *   --        |  irecv(3)...
 *   --        |  irecv(4)...
 *  **** Checkpoint ****
 *   send(2)   |  --
 *   send(3)   |  --
 *   send(4)   |  --
 * Action:
 *  Rank N need not do anything.
 *  After the bookmark exchange all not 'marked' receive messages can be
 *  identified. This list will include irecv[2,3,4].
 *  Rank M will cancel all not marked receives (irecv[2,3,4]) to clear the
 *  data from the PML stack.
 *  The checkpoint will continue. Once the checkpoint is complete
 *  Rank M will repost the receives (irecv[2,3,4]) and continue execution.
 *
 *  So the results looks like:
 *  Rank N     | Rank M
 * ------------+------------
 *   send(1)   |  recv(1)
 *   --        |  irecv(2)...
 *   --        |  irecv(3)...
 *   --        |  irecv(4)...
 *  **** Start Checkpoint ****
 *   --        |  irecv(2)... (cancel)
 *   --        |  irecv(3)... (cancel)
 *   --        |  irecv(4)... (cancel)
 *  **** End Checkpoint ****
 *  **** Start Continue/Restart ****
 *   --        |  irecv(2)... (repost)
 *   --        |  irecv(3)... (repost)
 *   --        |  irecv(4)... (repost)
 *  **** End Continue/Restart ****
 *   send(2)   |  --
 *   send(3)   |  --
 *   send(4)   |  --
 */
#include "ompi_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNIST_H */

#include "opal/runtime/opal_cr.h"
#include "opal/event/event.h"
#include "opal/util/output.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "ompi/request/request.h"
#include "ompi/datatype/dt_arch.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"

#include "crcp_coord.h"
#include "crcp_coord_pml.h"

/************************************
 * Locally Global vars
 ************************************/
#define PROBE_ANY_SIZE  ((size_t) 0)
#define PROBE_ANY_COUNT ((size_t) 0)

/* Pointers to the 'real' PML */
static mca_pml_base_component_t  *wrapped_pml_component = NULL;
static mca_pml_base_module_t     *wrapped_pml_module    = NULL;

/* A unique ID for each message in the system */
static uint64_t message_seq_num = 1;

/* The current message being worked on */
static uint64_t current_msg_id = 0;
static ompi_crcp_coord_pml_message_type_t current_msg_type = 0;

/* If we need to stall the C/R coordination until the current
 * operation is complete */
static bool stall_for_completion;

/*
 * State of the ft_event
 */
static int ft_event_state = OPAL_CRS_RUNNING;

/*
 * List of known peers
 */
opal_list_t ompi_crcp_coord_pml_peer_refs;

opal_list_t unknown_recv_from_list;
opal_list_t unknown_persist_recv_list;

opal_list_t drained_msg_ack_list;
opal_list_t drained_msg_list;

/************************************
 * Local Funcation Decls.
 ************************************/

/*
 * Find a peer_ref in the list given the ORTE process name.
 * Returns NULL if not found.
 */
static ompi_crcp_coord_pml_peer_ref_t* find_peer(orte_process_name_t proc);

/*
 * Find a peer_ref in the list given the communicator in which it belongs
 */
static int find_peer_in_comm(struct ompi_communicator_t* comm, int proc_idx,
                             ompi_crcp_coord_pml_peer_ref_t **peer_ref);

/*
 * Find a drained message matching the specified characteristics
 * Return found_msg_ref = NULL if no matches found.
 */
static int find_drained_msg(size_t ddt_size,
                            size_t count, int tag, int peer,
                            ompi_crcp_coord_pml_message_ref_t ** found_msg_ref);

/*
 * Coordinate Peers
 *  - Quiet channels
 */
static int ft_event_coordinate_peers(void);

/*
 * Finalize the coordination of peers.
 *  - Mostly cleanup.
 */
static int ft_event_finalize_exchange(void);

/*
 * Exchange the bookmarks
 *  - Staggered All-to-All
 * LAM/MPI used a staggered all-to-all algoritm for bookmark exachange
 *    http://www.lam-mpi.org/papers/lacsi2003/
 */
static int ft_event_exchange_bookmarks(void);

/*
 * A basic barrier -- JJH Improve this
 */
static int coord_basic_barrier(void);

static int coord_basic_barrier_send(int idx);
static int coord_basic_barrier_recv(int idx);

/*
 * Send Bookmarks to peer
 */
static int send_bookmarks(int peer_idx);

/*
 * Recv Bookmarks from peer
 */
static int recv_bookmarks(int peer_idx);

/*
 * Callback to receive the bookmarks from a peer
 */
static void recv_bookmarks_cbfunc(int status,
                                  orte_process_name_t* sender,
                                  orte_buffer_t *buffer,
                                  orte_rml_tag_t tag,
                                  void* cbdata);
static int total_recv_bookmarks = 0;

/*
 * Now that we have all the bookmarks, check them to see if we need to 
 * drain any messages.
 */
static int ft_event_check_bookmarks(void);

/*
 * Send message details to peer
 * - matched with recv_msg_details()
 */
static int send_msg_details(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                            int total_sent, int total_matched);

/*
 * Send a single message reference to a peer.
 * found_match = true if peer found a message to drain.
 */
static int do_send_msg_detail(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                              ompi_crcp_coord_pml_message_ref_t*msg_ref,
                              bool *found_match,
                              bool *finished);
/*
 * Recv message details from peer
 * - matched with send_msg_details()
 */
static int recv_msg_details(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                            int total_recv, int total_matched);

/*
 * Receive a single message reference from a peer.
 */
static int do_recv_msg_detail(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                              int *rank, uint32_t *comm_id, int *tag,
                              size_t *count, size_t *datatype_size);

/*
 * Check the message reference to determine if:
 * - We have received this message already, or
 * - We need to post this message
 */
static int do_recv_msg_detail_check(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                                    int rank, uint32_t comm_id, int tag,
                                    size_t count, size_t datatype_size,
                                    bool *found_match);

/*
 * Determine if we have received this message or not.
 * If found return a pointer to the message, and the list it was received on.
 * Also set some booleans to represent the state of the message found.
 */
static int have_received_msg(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                             int rank, uint32_t comm_id, int tag,
                             size_t count, size_t datatype_size,
                             ompi_crcp_coord_pml_message_ref_t ** found_msg_ref,
                             opal_list_t **found_on_this_list,
                             bool *found, bool *complete, bool *already_posted);

/*
 * Given a list of messages, find the message with the provided characteristics.
 */
static int find_message_named(opal_list_t * search_list,
                              size_t count, int tag, int peer,       
                              size_t ddt_size,
                              ompi_crcp_coord_pml_message_ref_t ** found_msg_ref,
                              int matched, int done, int active, int already_posted);
/*
 * Respond to peer regarding a received message detail
 */
static int do_recv_msg_detail_resp(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                                   int resp);

/*
 * Post the Drain Message Acks
 *  - These are sent once the receiver has finished receiving
 *    all of the messages it needed to drain off the wire.
 */
static int ft_event_post_drain_acks(void);

/*
 * Callback to service drain message acks.
 */
static void drain_message_ack_cbfunc(int status,
                                     orte_process_name_t* sender,
                                     orte_buffer_t *buffer,
                                     orte_rml_tag_t tag,
                                     void* cbdata);

/*
 * Post the Drain Messages
 *  - These are irecvs to be completed in any order.
 */
static int ft_event_post_drained(void);

/*
 * Wait for all drained messages and acks to complete.
 *  - Once this this finished then all channels associated
 *    with this process have been drained.
 */
static int ft_event_wait_quiesce(void);

/*
 * Wait for all the posted drain messages to finish
 */
static int wait_quiesce_drained(void);

/*
 * An optimized local version of waitall.
 * - Remove some unnecessary logic
 * - Remove logic to 'free' the request
 */
static int coord_request_wait_all( size_t count,
                                   ompi_request_t ** requests,
                                   ompi_status_public_t ** statuses);

/*
 * An optimized local version of wait.
 * - Remove some unnecessary logic
 * - Remove logic to 'free' the request
 * - Allow it to return if we need to stop waiting
 */
static int coord_request_wait( ompi_request_t * request,
                               ompi_status_public_t * status);

/*
 * Wait for all the drain ACKs to be received
 */
static int wait_quiesce_drain_ack(void);

/************************************
 * A few timing structures
 ************************************/
#define CRCP_TIMER_CKPT         0
#define CRCP_TIMER_CKPT_EX_B    1
#define CRCP_TIMER_CKPT_CK_B    2
#define CRCP_TIMER_CKPT_POST    3
#define CRCP_TIMER_CKPT_WAIT    4
#define CRCP_TIMER_CKPT_BARR    5
#define CRCP_TIMER_CONT         6
#define CRCP_TIMER_CKPT_PEER_S  7
#define CRCP_TIMER_CKPT_PEER_R  8
#define CRCP_TIMER_CKPT_WAIT_B  9
#define CRCP_TIMER_MAX         10

static double get_time(void);
static void start_time(int idx);
static void end_time(int idx);
static void display_indv_timer(int idx, int var);
static void display_all_timers(int state);
static void clear_timers(void);

double timer_start[CRCP_TIMER_MAX];
double timer_end[CRCP_TIMER_MAX];
char * timer_label[CRCP_TIMER_MAX];

#define START_TIMER(idx)                 \
  {                                      \
    if(OPAL_UNLIKELY(timing_enabled)) {  \
      start_time(idx);                   \
    }                                    \
  }

#define END_TIMER(idx)                   \
  {                                      \
    if(OPAL_UNLIKELY(timing_enabled)) {  \
      end_time(idx);                     \
    }                                    \
  }

#define DISPLAY_INDV_TIMER(idx, var)     \
  {                                      \
    if(OPAL_UNLIKELY(timing_enabled)) {  \
      display_indv_timer(idx, var);      \
    }                                    \
  }

#define DISPLAY_ALL_TIMERS(var)          \
  {                                      \
    if(OPAL_UNLIKELY(timing_enabled)) {  \
      display_all_timers(var);           \
    }                                    \
  }

/************************************
 * Declare/Define Object Structures
 ************************************/
#define INVALID_INT -123456789

#define FIND_MSG_TRUE     0
#define FIND_MSG_FALSE    1
#define FIND_MSG_UNKNOWN  2

OBJ_CLASS_INSTANCE(ompi_crcp_coord_pml_message_ref_t,
                   opal_list_item_t,
                   ompi_crcp_coord_pml_message_ref_construct,
                   ompi_crcp_coord_pml_message_ref_destruct);

void ompi_crcp_coord_pml_message_ref_construct(ompi_crcp_coord_pml_message_ref_t *msg_ref) {
    msg_ref->msg_id     = 0;
    msg_ref->msg_type   = COORD_MSG_TYPE_UNKNOWN;

    msg_ref->buffer     = NULL;
    msg_ref->count      = 0;

    msg_ref->datatype   = NULL;
    msg_ref->ddt_size   = 0;

    msg_ref->tag        = 0;
    msg_ref->rank       = 0;
    msg_ref->comm       = NULL;
    msg_ref->request    = NULL;

    msg_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    msg_ref->proc_name.vpid   = ORTE_VPID_INVALID;

    msg_ref->matched        = false;
    msg_ref->done           = false;
    msg_ref->active         = false;
    msg_ref->already_posted = false;

    msg_ref->suggested_rank = INVALID_INT;
}

void ompi_crcp_coord_pml_message_ref_destruct( ompi_crcp_coord_pml_message_ref_t *msg_ref) {
    msg_ref->msg_id     = 0;
    msg_ref->msg_type   = COORD_MSG_TYPE_UNKNOWN;

    if( NULL != msg_ref->buffer ) {
        free(msg_ref->buffer);
        msg_ref->buffer     = NULL;
    }
    msg_ref->count      = 0;

    if( NULL != msg_ref->datatype ) {
        OBJ_RELEASE(msg_ref->datatype);
        msg_ref->datatype   = NULL;
    }
    msg_ref->ddt_size   = 0;

    msg_ref->tag        = 0;
    msg_ref->rank       = 0;
    msg_ref->comm       = NULL;
    if( NULL != msg_ref->request ) {
        OBJ_RELEASE(msg_ref->request);
        msg_ref->request    = NULL;
    }

    msg_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    msg_ref->proc_name.vpid   = ORTE_VPID_INVALID;

    msg_ref->matched        = false;
    msg_ref->done           = false;
    msg_ref->active         = false;
    msg_ref->already_posted = false;

    msg_ref->suggested_rank = INVALID_INT;
}

OBJ_CLASS_INSTANCE(ompi_crcp_coord_pml_peer_ref_t,
                   opal_list_item_t,
                   ompi_crcp_coord_pml_peer_ref_construct,
                   ompi_crcp_coord_pml_peer_ref_destruct);

void ompi_crcp_coord_pml_peer_ref_construct(ompi_crcp_coord_pml_peer_ref_t *peer_ref) {
    peer_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    peer_ref->proc_name.vpid   = ORTE_VPID_INVALID;

    OBJ_CONSTRUCT(&peer_ref->send_list,       opal_list_t);
    OBJ_CONSTRUCT(&peer_ref->isend_list,      opal_list_t);
    OBJ_CONSTRUCT(&peer_ref->send_init_list,  opal_list_t);

    OBJ_CONSTRUCT(&peer_ref->recv_list,       opal_list_t);
    OBJ_CONSTRUCT(&peer_ref->irecv_list,      opal_list_t);
    OBJ_CONSTRUCT(&peer_ref->recv_init_list,  opal_list_t);

    peer_ref->total_send_msgs           = 0;
    peer_ref->total_isend_msgs          = 0;
    peer_ref->total_send_init_msgs      = 0;
    peer_ref->matched_send_msgs         = 0;
    peer_ref->matched_isend_msgs        = 0;
    peer_ref->matched_send_init_msgs    = 0;

    peer_ref->total_recv_msgs           = 0;
    peer_ref->total_irecv_msgs          = 0;
    peer_ref->total_recv_init_msgs      = 0;
    peer_ref->matched_recv_msgs         = 0;
    peer_ref->matched_irecv_msgs        = 0;
    peer_ref->matched_recv_init_msgs    = 0;

    peer_ref->total_drained_msgs        = 0;
}

void ompi_crcp_coord_pml_peer_ref_destruct( ompi_crcp_coord_pml_peer_ref_t *peer_ref) {
    opal_list_item_t* item = NULL;

    peer_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    peer_ref->proc_name.vpid   = ORTE_VPID_INVALID;
    
    while( NULL != (item = opal_list_remove_first(&peer_ref->send_list)) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&peer_ref->send_list);
    while( NULL != (item = opal_list_remove_first(&peer_ref->isend_list)) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&peer_ref->isend_list);
    while( NULL != (item = opal_list_remove_first(&peer_ref->send_init_list)) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&peer_ref->send_init_list);

    while( NULL != (item = opal_list_remove_first(&peer_ref->recv_list)) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&peer_ref->recv_list);
    while( NULL != (item = opal_list_remove_first(&peer_ref->irecv_list)) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&peer_ref->irecv_list);
    while( NULL != (item = opal_list_remove_first(&peer_ref->recv_init_list)) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&peer_ref->recv_init_list);

    peer_ref->total_send_msgs           = 0;
    peer_ref->total_isend_msgs          = 0;
    peer_ref->total_send_init_msgs      = 0;
    peer_ref->matched_send_msgs         = 0;
    peer_ref->matched_isend_msgs        = 0;
    peer_ref->matched_send_init_msgs    = 0;

    peer_ref->total_recv_msgs           = 0;
    peer_ref->total_irecv_msgs          = 0;
    peer_ref->total_recv_init_msgs      = 0;
    peer_ref->matched_recv_msgs         = 0;
    peer_ref->matched_irecv_msgs        = 0;
    peer_ref->matched_recv_init_msgs    = 0;

    peer_ref->total_drained_msgs        = 0;
}

/*
 * List of pending ACKs to drained messages
 */
struct drain_msg_ack_ref_t {
    /** This is a list object */
    opal_list_item_t super;
    /** Complete flag */
    bool complete;
    /** Peer which we received from */
    orte_process_name_t peer;
};
typedef struct drain_msg_ack_ref_t drain_msg_ack_ref_t;
    
OBJ_CLASS_DECLARATION(drain_msg_ack_ref_t);
void drain_msg_ack_ref_construct(drain_msg_ack_ref_t *msg_ack_ref);
void drain_msg_ack_ref_destruct( drain_msg_ack_ref_t *msg_ack_ref);

OBJ_CLASS_INSTANCE(drain_msg_ack_ref_t,
                   opal_list_item_t,
                   drain_msg_ack_ref_construct,
                   drain_msg_ack_ref_destruct);

void drain_msg_ack_ref_construct(drain_msg_ack_ref_t *msg_ack_ref) {
    msg_ack_ref->complete    = false;

    msg_ack_ref->peer.jobid  = ORTE_JOBID_INVALID;
    msg_ack_ref->peer.vpid   = ORTE_VPID_INVALID;
}

void drain_msg_ack_ref_destruct( drain_msg_ack_ref_t *msg_ack_ref) {
    msg_ack_ref->complete   = false;

    msg_ack_ref->peer.jobid  = ORTE_JOBID_INVALID;
    msg_ack_ref->peer.vpid   = ORTE_VPID_INVALID;
}

OBJ_CLASS_INSTANCE(ompi_crcp_coord_pml_state_t,
                   ompi_crcp_base_pml_state_t,
                   NULL,
                   NULL
                   );

/************************************
 * Some Macro shortcuts
 ************************************/
/* JJH -- Improve this with a free list */
#define CRCP_COORD_STATE_ALLOC(coord_state, rc)               \
 {                                                            \
   coord_state = (ompi_crcp_coord_pml_state_t *)              \
                 malloc(sizeof(ompi_crcp_coord_pml_state_t)); \
 }

#define CRCP_COORD_STATE_RETURN(coord_state)      \
 {                                                \
   free(coord_state);                             \
 }

#define CREATE_COORD_STATE(coord_state, pml_state, v_peer_ref, v_msg_ref)         \
 {                                                                                \
   CRCP_COORD_STATE_ALLOC(coord_state, NULL);                                     \
                                                                                  \
   coord_state->prev_ptr           = pml_state;                                   \
   coord_state->p_super.super      = pml_state->super;                            \
   coord_state->p_super.state      = pml_state->state;                            \
   coord_state->p_super.error_code = pml_state->error_code;                       \
   coord_state->p_super.wrapped_pml_component = pml_state->wrapped_pml_component; \
   coord_state->p_super.wrapped_pml_module    = pml_state->wrapped_pml_module;    \
                                                                                  \
   coord_state->peer_ref         = v_peer_ref;                                    \
   coord_state->msg_ref          = v_msg_ref;                                     \
 }

#define EXTRACT_COORD_STATE(pml_state, v_coord_state, v_rtn_state, v_peer_ref, v_msg_ref) \
 {                                                           \
   v_coord_state = (ompi_crcp_coord_pml_state_t*)pml_state;  \
   v_rtn_state   = v_coord_state->prev_ptr;                  \
   v_peer_ref    = v_coord_state->peer_ref;                  \
   v_msg_ref     = v_coord_state->msg_ref;                   \
 }

#define CREATE_NEW_MSG(msg_ref, v_type, v_buffer, v_count, v_datatype, v_tag, v_rank, v_comm,  v_request, p_jobid, p_vpid) \
 {                                                       \
   msg_ref = OBJ_NEW(ompi_crcp_coord_pml_message_ref_t); \
   msg_ref->msg_id   = message_seq_num;                  \
   message_seq_num++;                                    \
   msg_ref->msg_type = v_type;                           \
                                                         \
   msg_ref->buffer   = v_buffer;                         \
   msg_ref->count    = v_count;                          \
                                                         \
   msg_ref->datatype = v_datatype;                       \
   if( NULL != msg_ref->datatype ) {                     \
      OBJ_RETAIN(msg_ref->datatype);                     \
      ompi_ddt_type_size(msg_ref->datatype,              \
                         &(msg_ref->ddt_size));          \
   } else {                                              \
      msg_ref->ddt_size = 0;                             \
   }                                                     \
                                                         \
   msg_ref->tag     = v_tag;                             \
   msg_ref->rank    = v_rank;                            \
   msg_ref->comm    = v_comm;                            \
   msg_ref->request = v_request;                         \
   if( NULL != msg_ref->request ) {                      \
      OBJ_RETAIN(msg_ref->request);                      \
   }                                                     \
                                                         \
   msg_ref->proc_name.jobid  = p_jobid;                  \
   msg_ref->proc_name.vpid   = p_vpid;                   \
 }

#define DUP_MSG(dup_msg_ref, msg_ref)                             \
 {                                                                \
    dup_msg_ref = OBJ_NEW(ompi_crcp_coord_pml_message_ref_t);     \
    dup_msg_ref->msg_id = message_seq_num;                        \
    message_seq_num++;                                            \
    dup_msg_ref->msg_type = msg_ref->msg_type;                    \
                                                                  \
    dup_msg_ref->buffer = NULL;                                   \
    dup_msg_ref->count    = msg_ref->count;                       \
                                                                  \
    dup_msg_ref->datatype = msg_ref->datatype;                    \
    if( NULL != msg_ref->datatype ) {                             \
       OBJ_RETAIN(msg_ref->datatype);                             \
       dup_msg_ref->ddt_size = msg_ref->ddt_size;                 \
    } else {                                                      \
       dup_msg_ref->ddt_size = 0;                                 \
    }                                                             \
                                                                  \
    dup_msg_ref->tag     = msg_ref->tag;                          \
    dup_msg_ref->rank    = msg_ref->rank;                         \
    dup_msg_ref->comm    = msg_ref->comm;                         \
    dup_msg_ref->request = msg_ref->request;                      \
    if( NULL != msg_ref->request ) {                              \
       OBJ_RETAIN(msg_ref->request);                              \
    }                                                             \
                                                                  \
    dup_msg_ref->proc_name.jobid  = msg_ref->proc_name.jobid;     \
    dup_msg_ref->proc_name.vpid   = msg_ref->proc_name.vpid;      \
 }

#define PACK_BUFFER(buffer, var, count, type, error_msg)                       \
 {                                                                             \
    if (OMPI_SUCCESS != (ret = orte_dss.pack(buffer, &(var), count, type)) ) { \
        opal_output(mca_crcp_coord_component.super.output_handle,              \
                    "%s (Return %d)", error_msg, ret);                         \
        exit_status = ret;                                                     \
        goto cleanup;                                                          \
    }                                                                          \
 }

#define UNPACK_BUFFER(buffer, var, count, type, error_msg)                     \
 {                                                                             \
    orte_std_cntr_t n = count;                                                 \
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &(var), &n, type)) ) {  \
        opal_output(mca_crcp_coord_component.super.output_handle,              \
                    "%s (Return %d)", error_msg, ret);                         \
        exit_status = ret;                                                     \
        goto cleanup;                                                          \
    }                                                                          \
 }

/****************
 * PML Wrapper Init/Finalize
 ****************/
int ompi_crcp_coord_pml_init(void) {
    message_seq_num = 1;
    current_msg_id  = 0;
    current_msg_type = COORD_MSG_TYPE_UNKNOWN;
    stall_for_completion = false;
    ft_event_state = OPAL_CRS_RUNNING;

    OBJ_CONSTRUCT(&ompi_crcp_coord_pml_peer_refs, opal_list_t);

    OBJ_CONSTRUCT(&unknown_recv_from_list, opal_list_t);
    OBJ_CONSTRUCT(&unknown_persist_recv_list, opal_list_t);

    OBJ_CONSTRUCT(&drained_msg_ack_list, opal_list_t);
    OBJ_CONSTRUCT(&drained_msg_list, opal_list_t);

    clear_timers();
    timer_label[CRCP_TIMER_CKPT]      = strdup("Ckpt");
    timer_label[CRCP_TIMER_CKPT_EX_B] = strdup("Ckpt Exchange");
    timer_label[CRCP_TIMER_CKPT_CK_B] = strdup("Ckpt Check");
    timer_label[CRCP_TIMER_CKPT_POST] = strdup("Ckpt Post");
    timer_label[CRCP_TIMER_CKPT_WAIT] = strdup("Ckpt Wait");
    timer_label[CRCP_TIMER_CKPT_BARR] = strdup("Ckpt Barrier");
    timer_label[CRCP_TIMER_CONT]      = strdup("Continue");
    timer_label[CRCP_TIMER_CKPT_PEER_S] = strdup("Peer Send");
    timer_label[CRCP_TIMER_CKPT_PEER_R] = strdup("Peer Recv");
    timer_label[CRCP_TIMER_CKPT_WAIT_B] = strdup("Bookmark Wait");

    return OMPI_SUCCESS;
}

int ompi_crcp_coord_pml_finalize(void) {
    current_msg_id = 0;
    current_msg_type = COORD_MSG_TYPE_UNKNOWN;
    stall_for_completion = false;
    ft_event_state = OPAL_CRS_RUNNING;

    OBJ_DESTRUCT(&ompi_crcp_coord_pml_peer_refs);

    OBJ_DESTRUCT(&unknown_recv_from_list);
    OBJ_DESTRUCT(&unknown_persist_recv_list);

    OBJ_DESTRUCT(&drained_msg_ack_list);
    OBJ_DESTRUCT(&drained_msg_list);

    return OMPI_SUCCESS;
}

/****************
 * PML Wrapper
 ****************/
/**************** Enable *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_enable(
                                  bool enable,
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_enable()");
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Progress *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_progress(
                                  ompi_crcp_base_pml_state_t* pml_state)
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_progress()");
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Probe *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_iprobe(
                                  int dst, int tag, 
                                  struct ompi_communicator_t* comm, 
                                  int *matched,
                                  ompi_status_public_t* status, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_message_ref_t *drain_msg_ref = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_iprobe(%d, %d)", dst, tag);

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise let the PML handle it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        if( OMPI_SUCCESS != (ret = find_drained_msg(PROBE_ANY_SIZE, PROBE_ANY_COUNT,
                                                    tag, dst,
                                                    &drain_msg_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_iprobe(): Failed trying to find a drained message."
                        " This should never happen. (%d)",
                        ret);
            exit_status = ret;
            goto DONE;
        }

        /*
         * If the message is a drained message
         *  - Copy of the status structure to pass back to the user
         *  - Mark the 'matched' flag as true
         */
        if( NULL != drain_msg_ref ) {
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_iprobe(): Matched a drained message...");
            
            /* Copy the status information */
            if( MPI_STATUS_IGNORE != status ) {
                memcpy(status, &drain_msg_ref->status, sizeof(ompi_status_public_t)); 
            }

            /* Mark as complete */
            *matched = 1;

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
        /*
         * Otherwise the message is not drained (common case), so let the PML deal with it
         */
        else {
            /* Mark as not complete */
            *matched = 0;
        }
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_probe(
                                  int dst, int tag, 
                                  struct ompi_communicator_t* comm, 
                                  ompi_status_public_t* status, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_message_ref_t *drain_msg_ref = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_probe(%d, %d)", dst, tag);

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise let the PML handle it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        if( OMPI_SUCCESS != (ret = find_drained_msg(PROBE_ANY_SIZE, PROBE_ANY_COUNT,
                                                    tag, dst,
                                                    &drain_msg_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_probe(): Failed trying to find a drained message."
                        " This should never happen. (%d)",
                        ret);
            exit_status = ret;
            goto DONE;
        }

        /*
         * If the message is a drained message
         *  - Copy of the status structure to pass back to the user
         */
        if( NULL != drain_msg_ref ) {
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_iprobe(): Matched a drained message...");
            
            /* Copy the status information */
            if( MPI_STATUS_IGNORE != status ) {
                memcpy(status, &drain_msg_ref->status, sizeof(ompi_status_public_t)); 
            }

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

/**************** Dump *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_dump(
                                  struct ompi_communicator_t* comm,
                                  int verbose, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_dump()");
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}


/**************** Communicator *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_add_comm(
                                  struct ompi_communicator_t* comm, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_add_comm()");
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_del_comm(
                                  struct ompi_communicator_t* comm, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_del_comm()");
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Processes *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_add_procs(
                                   struct ompi_proc_t **procs,
                                   size_t nprocs, 
                                   ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_peer_ref_t *new_peer_ref;
    size_t i;

    if( OMPI_CRCP_PML_PRE != pml_state->state ){
        goto DONE;
    }

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_add_procs()");

    /*
     * Save pointers to the wrapped PML
     */
    wrapped_pml_component = pml_state->wrapped_pml_component;
    wrapped_pml_module    = pml_state->wrapped_pml_module;

    /*
     * Create a peer_ref for each peer added
     */
    for( i = 0; i < nprocs; ++i) {
        new_peer_ref = OBJ_NEW(ompi_crcp_coord_pml_peer_ref_t);

        new_peer_ref->proc_name.jobid  = procs[i]->proc_name.jobid;
        new_peer_ref->proc_name.vpid   = procs[i]->proc_name.vpid;

        opal_list_append(&ompi_crcp_coord_pml_peer_refs, &(new_peer_ref->super));
    }

 DONE:
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_del_procs(
                                  struct ompi_proc_t **procs,
                                  size_t nprocs, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    opal_list_item_t *item = NULL;
    ompi_crcp_coord_pml_peer_ref_t *old_peer_ref;
    int exit_status = OMPI_SUCCESS;
    size_t i;

    if( OMPI_CRCP_PML_PRE != pml_state->state ){
        goto DONE;
    }

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_del_procs()");

    for( i = 0; i < nprocs; ++i) {
        item = (opal_list_item_t*)find_peer(procs[i]->proc_name);
        if(NULL == item) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: del_procs: Unable to find peer %s\n",
                        ORTE_NAME_PRINT(&(procs[i]->proc_name)));
            exit_status = OMPI_ERROR;
            goto DONE;
        }

        /* Remove the found peer from the list */
        opal_list_remove_item(&ompi_crcp_coord_pml_peer_refs, item);
        old_peer_ref = (ompi_crcp_coord_pml_peer_ref_t*)item;
        OBJ_RELEASE(old_peer_ref);
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

/**************** Send *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_isend_init(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int dst, int tag, 
                                  mca_pml_base_send_mode_t mode,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_peer_ref_t    *peer_ref    = NULL;
    ompi_crcp_coord_pml_message_ref_t *msg_ref     = NULL;
    ompi_crcp_coord_pml_state_t       *coord_state = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_isend_init()");

    /*
     * Before the PML gets the message:
     *  - Setup structure to track the message
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state ) {
        /*
         * Find the peer reference
         */
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, dst, &peer_ref) ) ){
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: isend: Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }
        if( NULL == peer_ref ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: isend: Failed to find peer_ref - peer_ref is NULL\n");
            exit_status = ret;
            goto DONE;
        }

        /*
         * Create a new Message Object
         */
        CREATE_NEW_MSG(msg_ref, COORD_MSG_TYPE_P_SEND,
                       buf,
                       count, datatype, tag, dst, comm,
                       NULL,
                       peer_ref->proc_name.jobid,
                       peer_ref->proc_name.vpid);

        msg_ref->matched        = false;
        msg_ref->done           = false;
        msg_ref->active         = false;
        msg_ref->already_posted = true;

        opal_list_append(&(peer_ref->send_init_list), &(msg_ref->super));

        /* Save the pointers */
        CREATE_COORD_STATE(coord_state, pml_state,
                           peer_ref, msg_ref);

        coord_state->p_super.error_code = OMPI_SUCCESS;
        return &coord_state->p_super;
    }
    /*
     * After PML is done, update message reference
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state ) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /*
         * Update Message
         * - Booleans already set...
         *   msg_ref->matched        = false;
         *   msg_ref->done           = false;
         *   msg_ref->active         = false;
         *   msg_ref->already_posted = true;
         */
        msg_ref->request = *request;
        OBJ_RETAIN(msg_ref->request);

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_isend(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int dst, int tag,
                                  mca_pml_base_send_mode_t mode,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_peer_ref_t    *peer_ref    = NULL;
    ompi_crcp_coord_pml_message_ref_t *msg_ref     = NULL;
    ompi_crcp_coord_pml_state_t       *coord_state = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_isend()");

    /*
     * Before the PML gets the message:
     *  - Setup structure to track the message
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state ) {
        /*
         * Find the peer reference
         */
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, dst, &peer_ref) ) ){
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: isend: Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }
        if( NULL == peer_ref ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: isend: Failed to find peer_ref - peer_ref is NULL\n");
            exit_status = ret;
            goto DONE;
        }

        /*
         * Create a new Message Object
         */
        CREATE_NEW_MSG(msg_ref, COORD_MSG_TYPE_I_SEND,
                       buf,
                       count, datatype, tag, dst, comm, NULL,
                       peer_ref->proc_name.jobid,
                       peer_ref->proc_name.vpid);

        msg_ref->matched        = false;
        msg_ref->done           = false;
        msg_ref->active         = true;
        msg_ref->already_posted = true;

        opal_list_append(&(peer_ref->isend_list), &(msg_ref->super));

        /*  Bookkeeping */
        peer_ref->total_isend_msgs += 1;

        /* Save the pointers */
        CREATE_COORD_STATE(coord_state, pml_state,
                           peer_ref, msg_ref);

        coord_state->p_super.error_code = OMPI_SUCCESS;
        return &coord_state->p_super;
    }
    /*
     * After PML is done, update message reference
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state ) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /*
         * Update Message
         * - Booleans already set...
         *   msg_ref->matched        = false;
         *   msg_ref->done           = false;
         *   msg_ref->active         = true;
         *   msg_ref->already_posted = true;
         */
        msg_ref->request = *request;
        OBJ_RETAIN(msg_ref->request);

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_send(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int dst, int tag,
                                  mca_pml_base_send_mode_t mode,
                                  struct ompi_communicator_t* comm, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_peer_ref_t    *peer_ref    = NULL;
    ompi_crcp_coord_pml_message_ref_t *msg_ref     = NULL;
    ompi_crcp_coord_pml_state_t       *coord_state = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_send()");

    /*
     * Before the PML gets the message:
     *  - Setup structure to track the message
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state ) {
        /*
         * Find the peer reference
         */
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, dst, &peer_ref) ) ){
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: send: Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }
        if( NULL == peer_ref ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: send: Failed to find peer_ref - peer_ref is NULL\n");
            exit_status = ret;
            goto DONE;
        }

        /*
         * Create a new Message Object
         */
        CREATE_NEW_MSG(msg_ref, COORD_MSG_TYPE_B_SEND,
                       buf,
                       count, datatype, tag, dst, comm, NULL,
                       peer_ref->proc_name.jobid,
                       peer_ref->proc_name.vpid);

        msg_ref->matched        = false;
        msg_ref->done           = false;
        msg_ref->active         = true;
        msg_ref->already_posted = true;

        opal_list_append(&(peer_ref->send_list), &(msg_ref->super));

        /*  Bookkeeping */
        peer_ref->total_send_msgs += 1;
        current_msg_id = msg_ref->msg_id;
        current_msg_type = COORD_MSG_TYPE_B_SEND;

        /* Save the pointers */
        CREATE_COORD_STATE(coord_state, pml_state,
                           peer_ref, msg_ref);
        coord_state->p_super.error_code = OMPI_SUCCESS;

        return &coord_state->p_super;
    }
    /*
     * After PML is done, update message reference
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state ) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /*
         * Update Message
         * - Booleans already set...
         *   msg_ref->matched        = false;
         *   msg_ref->already_posted = true;
         */
        msg_ref->done   = true;
        msg_ref->active = false;

        current_msg_id = 0;
        current_msg_type = COORD_MSG_TYPE_UNKNOWN;

        CRCP_COORD_STATE_RETURN(coord_state);
        rtn_state->error_code = OMPI_SUCCESS;

        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

/**************** Recv *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_irecv_init(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int src, int tag,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request, 
                                  ompi_crcp_base_pml_state_t* pml_state)
{
    ompi_crcp_coord_pml_peer_ref_t    *peer_ref      = NULL;
    ompi_crcp_coord_pml_message_ref_t *msg_ref       = NULL;
    ompi_crcp_coord_pml_state_t       *coord_state   = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_irecv_init()");

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise create a new reference to it so we can track it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * A message will never be on the drained list for this function since
         * it does not actually receive anything, just sets up the system.
         * The receive for these reqeusts are done in the start() and wait()
         * commands.
         */

        /*
         * Create a new Message Object
         */
        CREATE_NEW_MSG(msg_ref, COORD_MSG_TYPE_P_RECV,
                       buf,
                       count, datatype, tag, src, comm,
                       NULL, /* Leave this NULL for now, will pick up real value in POST */
                       ORTE_JOBID_INVALID,
                       ORTE_VPID_INVALID);
            
        msg_ref->matched        = false;
        msg_ref->done           = false;
        msg_ref->active         = false;
        msg_ref->already_posted = true;
            
        /*
         * Find the Peer
         */
        if( MPI_ANY_SOURCE == src || src < 0) {
            CREATE_COORD_STATE(coord_state, pml_state,
                               NULL, msg_ref);
            
            opal_list_append(&(unknown_persist_recv_list), &(msg_ref->super));
        }
        else {
            if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: recv: Failed to find peer_ref\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL == peer_ref ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: recv: Failed to find peer_ref - peer_ref is NULL\n");
                exit_status = ret;
                goto DONE;
            }
                
            msg_ref->proc_name.jobid  = peer_ref->proc_name.jobid;
            msg_ref->proc_name.vpid   = peer_ref->proc_name.vpid;
            
            opal_list_append(&(peer_ref->recv_init_list), &(msg_ref->super));
                
            CREATE_COORD_STATE(coord_state, pml_state,
                               peer_ref, msg_ref);
            
        }

        coord_state->p_super.error_code = OMPI_SUCCESS;
        return &coord_state->p_super;
    }
    /*
     * Post PML Call
     * - bookkeeping...
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /* 
         * Do the update
         * - some Booleans already set... 
         *   msg_ref->matched         = false;
         *   msg_ref->done            = false;
         *   msg_ref->active          = false;
         *   msg_ref->already_posted  = true;
         */
        msg_ref->request           = *request;
        OBJ_RETAIN(msg_ref->request);

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_irecv(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int src, int tag,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_peer_ref_t    *peer_ref      = NULL;
    ompi_crcp_coord_pml_message_ref_t *msg_ref       = NULL;
    ompi_crcp_coord_pml_message_ref_t *drain_msg_ref = NULL;
    ompi_crcp_coord_pml_state_t       *coord_state   = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_irecv()");

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise create a new reference to it so we can track it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        if( OMPI_SUCCESS != (ret = find_drained_msg(datatype->size,
                                                    count, tag, src,
                                                    &drain_msg_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_irecv(): Failed trying to find a drained message."
                        " This should never happen. (%d)",
                        ret);
            exit_status = ret;
            goto DONE;
        }

        /*
         * If the message is a drained message
         *  - Complete it right now
         *  - We do not need to increment any counters here since we already have
         *    when we originally drained the message.
         */
        if( NULL != drain_msg_ref ) {
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_irecv(): Matched a drained message. "
                                "[%3d, %3d] vs [%3d, %3d]",
                                (int)datatype->size, (int)count,
                                (int)drain_msg_ref->ddt_size, (int)drain_msg_ref->count);

            /* Copy the drained message */
            src = drain_msg_ref->rank;
            tag = drain_msg_ref->tag;

            if( 0 != ompi_ddt_copy_content_same_ddt(datatype, count,
                                                    buf, drain_msg_ref->buffer) ) {
                opal_output( mca_crcp_coord_component.super.output_handle,
                             "crcp:coord: pml_irecv(): Datatype copy failed (%d)",
                             ret);
            }
                
            *request = drain_msg_ref->request;
            OBJ_RETAIN(*request);

            /* Remove the message from the list */
            opal_list_remove_item(&drained_msg_list, &(drain_msg_ref->super));
            drain_msg_ref->request = NULL;
            OBJ_RELEASE(drain_msg_ref);

            /*
             * Find the peer for this source
             */
            if( NULL == peer_ref ) {
                if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv: Failed to find peer_ref\n");
                    exit_status = ret;
                    goto DONE;
                }
                if( NULL == peer_ref ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv: Failed to find peer_ref - peer_ref is NULL\n");
                    exit_status = ret;
                    goto DONE;
                }
            }

            peer_ref->total_drained_msgs -= 1;
            /* Do *not* increment:
             *    peer_ref->total_irecv_msgs += 1;
             * Because we accounted for this message during the last checkpoint.
             */

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
        /*
         * Otherwise the message is not drained (common case)
         */
        else {
            /*
             * Create a new Message Object
             */
            CREATE_NEW_MSG(msg_ref, COORD_MSG_TYPE_I_RECV,
                           buf,
                           count, datatype, tag, src, comm,
                           NULL, /* Leave this NULL for now, will pick up real value in POST */
                           ORTE_JOBID_INVALID,
                           ORTE_VPID_INVALID);
            
            msg_ref->matched        = false;
            msg_ref->done           = false;
            msg_ref->active         = true;
            msg_ref->already_posted = true;
            
            /*
             * Find the Peer
             */
            if( MPI_ANY_SOURCE == src || src < 0) {
                CREATE_COORD_STATE(coord_state, pml_state,
                                   NULL, msg_ref);
                
                opal_list_append(&(unknown_recv_from_list), &(msg_ref->super));
            }
            else {
                if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv: Failed to find peer_ref\n");
                    exit_status = ret;
                    goto DONE;
                }
                if( NULL == peer_ref ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv: Failed to find peer_ref - peer_ref is NULL\n");
                    exit_status = ret;
                    goto DONE;
                }
                
                msg_ref->proc_name.jobid  = peer_ref->proc_name.jobid;
                msg_ref->proc_name.vpid   = peer_ref->proc_name.vpid;
                
                opal_list_append(&(peer_ref->irecv_list), &(msg_ref->super));
                
                CREATE_COORD_STATE(coord_state, pml_state,
                                   peer_ref, msg_ref);

            }

            coord_state->p_super.error_code = OMPI_SUCCESS;
            return &coord_state->p_super;
        }
    }
    /*
     * Post PML Call
     * - bookkeeping...
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);


        /* 
         * Do the update
         * - some Booleans already set... 
         *   msg_ref->matched         = false;
         *   msg_ref->done            = false;
         *   msg_ref->active          = true;
         *   msg_ref->already_posted  = true;
         */
        msg_ref->request           = *request;
        OBJ_RETAIN(msg_ref->request);

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_recv(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int src, int tag,
                                  struct ompi_communicator_t* comm,
                                  ompi_status_public_t* status, 
                                  ompi_crcp_base_pml_state_t* pml_state)
{
    ompi_crcp_coord_pml_peer_ref_t    *peer_ref      = NULL;
    ompi_crcp_coord_pml_message_ref_t *msg_ref       = NULL;
    ompi_crcp_coord_pml_message_ref_t *drain_msg_ref = NULL;
    ompi_crcp_coord_pml_state_t       *coord_state   = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_recv()");

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise create a new reference to it so we can track it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        if( OMPI_SUCCESS != (ret = find_drained_msg(datatype->size,
                                                    count, tag, src,
                                                    &drain_msg_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_recv(): Failed trying to find a drained message."
                        " This should never happen. (%d)",
                        ret);
            exit_status = ret;
            goto DONE;
        }

        /*
         * If the message is a drained message
         *  - Complete it right now
         *  - We do not need to increment any counters here since we already have
         *    when we originally drained the message.
         */
        if( NULL != drain_msg_ref ) {
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_recv(): Matched a drained message...");

            /* Copy the drained message */
            src = drain_msg_ref->rank;
            tag = drain_msg_ref->tag;
            if( 0 != ompi_ddt_copy_content_same_ddt(datatype, count,
                                                    buf, drain_msg_ref->buffer) ) {
                opal_output( mca_crcp_coord_component.super.output_handle,
                             "crcp:coord: pml_recv(): Datatype copy failed (%d)",
                             ret);
            }

            if( MPI_STATUS_IGNORE != status ) {
                memcpy(status, &drain_msg_ref->status, sizeof(ompi_status_public_t)); 
            }

            /* Remove the message from the list */
            opal_list_remove_item(&drained_msg_list, &(drain_msg_ref->super));
            drain_msg_ref->request = NULL;
            OBJ_RELEASE(drain_msg_ref);

            /*
             * Find the peer for this source
             */
            if( NULL == peer_ref ) {
                if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv: Failed to find peer_ref\n");
                    exit_status = ret;
                    goto DONE;
                }
                if( NULL == peer_ref ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv: Failed to find peer_ref - peer_ref is NULL\n");
                    exit_status = ret;
                    goto DONE;
                }
            }

            peer_ref->total_drained_msgs -= 1;
            /* Do *not* increment:
             *    peer_ref->total_recv_msgs += 1;
             * Because we accounted for this message during the last checkpoint.
             */

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
        /*
         * Otherwise the message is not drained (common case)
         */
        else {
            /*
             * Create a new Message Object
             */
            CREATE_NEW_MSG(msg_ref, COORD_MSG_TYPE_B_RECV,
                           buf,
                           count, datatype, tag, src, comm, NULL,
                           ORTE_JOBID_INVALID,
                           ORTE_VPID_INVALID);
            
            msg_ref->matched        = false;
            msg_ref->done           = false;
            msg_ref->active         = true;
            msg_ref->already_posted = true;
            
            /*  Bookkeeping */
            current_msg_id = msg_ref->msg_id;
            current_msg_type = COORD_MSG_TYPE_B_RECV;

            /*
             * Find the Peer
             */
            if( MPI_ANY_SOURCE == src || src < 0) {
                CREATE_COORD_STATE(coord_state, pml_state,
                                   NULL, msg_ref);
                
                opal_list_append(&(unknown_recv_from_list), &(msg_ref->super));
            }
            else {
                if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv: Failed to find peer_ref\n");
                    exit_status = ret;
                    goto DONE;
                }
                if( NULL == peer_ref ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv: Failed to find peer_ref - peer_ref is NULL\n");
                    exit_status = ret;
                    goto DONE;
                }
                
                msg_ref->proc_name.jobid  = peer_ref->proc_name.jobid;
                msg_ref->proc_name.vpid   = peer_ref->proc_name.vpid;
                
                opal_list_append(&(peer_ref->recv_list), &(msg_ref->super));
                
                CREATE_COORD_STATE(coord_state, pml_state,
                                   peer_ref, msg_ref);

            }

            coord_state->p_super.error_code = OMPI_SUCCESS;
            return &coord_state->p_super;
        }
    }
    /*
     * Post PML Call
     * - bookkeeping...
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /*
         * If MPI_ANY_SOUCE, then move the message from the unknown list
         * to the list associated with the resolved process.
         */
        if( NULL == peer_ref ) {
            src = status->MPI_SOURCE;

            if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: recv: Failed to resolve peer_ref (rank %d)\n",
                            src);
                exit_status = ret;
                goto DONE;
            }
            if( NULL == peer_ref ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: recv: Failed to resolve peer_ref (rank %d) - peer_ref is NULL\n",
                            src);
                exit_status = ret;
                goto DONE;
            }
        
            msg_ref->proc_name.jobid  = peer_ref->proc_name.jobid;
            msg_ref->proc_name.vpid   = peer_ref->proc_name.vpid;

            opal_list_remove_item(&(unknown_recv_from_list), &(msg_ref->super));
            opal_list_append(&(peer_ref->recv_list), &(msg_ref->super));
        }

        /* 
         * Do the update
         * - some Booleans already set... 
         *   msg_ref->matched         = false;
         *   msg_ref->already_posted  = true;
         */
        msg_ref->done              = true;
        msg_ref->active            = false;

        peer_ref->total_recv_msgs += 1;
        current_msg_id = 0;
        current_msg_type = COORD_MSG_TYPE_UNKNOWN;

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}


/**************** Start *****************/
/* Start is connected to irecv_start or isend_start */
static ompi_request_type_t * coord_start_req_types = NULL;

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_start(
                                  size_t count,
                                  ompi_request_t** requests, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_peer_ref_t     *peer_ref = NULL;
    ompi_crcp_coord_pml_message_ref_t  *msg_ref  = NULL;
    mca_pml_base_request_t *breq = NULL;
    size_t tmp_ddt_size  = 0;
    size_t iter_req;
    int exit_status = OMPI_SUCCESS;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_start()");

    /*
     * Check from drained recv message before PML gets a chance to do anything
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state ) {
        if( 0 >=  opal_list_get_size(&drained_msg_list)) {
            exit_status = OMPI_SUCCESS;
            goto DONE;
        }

        coord_start_req_types = (ompi_request_type_t *)malloc(sizeof(ompi_request_type_t) * count);
        for(iter_req = 0; iter_req < count; iter_req++) {
            coord_start_req_types[iter_req] = OMPI_REQUEST_MAX;
        }

        for(iter_req = 0; iter_req < count; iter_req++) {
            breq = (mca_pml_base_request_t *)requests[iter_req];
            tmp_ddt_size = (breq->req_datatype)->size;

            if( breq->req_type == MCA_PML_REQUEST_RECV ) {
                ompi_crcp_coord_pml_message_ref_t *drain_msg_ref = NULL;
                
                /*
                 * If peer rank is given then find the peer reference
                 */
                if( 0 <= breq->req_peer ) {
                    if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm,
                                                                 breq->req_peer,
                                                                 &peer_ref) ) ){
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Failed to find peer_ref\n");
                        exit_status = ret;
                        goto DONE;
                    }
                    if( NULL == peer_ref ) {
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Failed to find peer_ref - peer_ref is NULL\n");
                        exit_status = ret;
                        goto DONE;
                    }
                    
                    if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->recv_init_list),
                                                                  breq->req_count,
                                                                  breq->req_tag,
                                                                  breq->req_peer,
                                                                  tmp_ddt_size,
                                                                  &msg_ref,
                                                                  FIND_MSG_UNKNOWN,
                                                                  FIND_MSG_FALSE,
                                                                  FIND_MSG_FALSE,
                                                                  FIND_MSG_TRUE
                                                                  ) ) ) {
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Unable to find the proper (recv) message ref for this recv\n");
                        exit_status = ret;
                        goto DONE;
                    }

                    if( NULL != msg_ref ) {
                        goto MSG_FOUND;
                    }
                }
                /*
                 * Otherwise peer is not known
                 */
                else {
                    if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_persist_recv_list),
                                                                  breq->req_count,
                                                                  breq->req_tag,
                                                                  INVALID_INT,
                                                                  tmp_ddt_size,
                                                                  &msg_ref,
                                                                  FIND_MSG_UNKNOWN,
                                                                  FIND_MSG_FALSE,
                                                                  FIND_MSG_FALSE,
                                                                  FIND_MSG_TRUE
                                                                  ) ) ) {
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Unable to find the proper (recv) message ref for this recv\n");
                        exit_status = ret;
                        goto DONE;
                    }

                    if( NULL != msg_ref ) {
                        goto MSG_FOUND;
                    }
                }

            MSG_FOUND:
                if( NULL != msg_ref ) {
                    /*
                     * See if this mesage was already drained.
                     */
                    if( OMPI_SUCCESS != (ret = find_drained_msg(msg_ref->datatype->size,
                                                                msg_ref->count,
                                                                msg_ref->tag,
                                                                msg_ref->rank,
                                                                &drain_msg_ref) ) ) {
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Failed trying to find a drained message."
                                    " This should never happen. (%d)",
                                    ret);
                        exit_status = ret;
                        goto DONE;
                    }
                    if( NULL != drain_msg_ref ) {
                        /*
                         * If a peer was not specified, find the coorect peer
                         */
                        if( NULL == peer_ref ) {
                            if( OMPI_SUCCESS != (ret = find_peer_in_comm(drain_msg_ref->comm,
                                                                         drain_msg_ref->rank,
                                                                         &peer_ref) ) ){
                                opal_output(mca_crcp_coord_component.super.output_handle,
                                            "crcp:coord: pml_start: Failed to find peer_ref\n");
                                exit_status = ret;
                                goto DONE;
                            }
                            if( NULL == peer_ref ) {
                                opal_output(mca_crcp_coord_component.super.output_handle,
                                            "crcp:coord: pml_start: Failed to find peer_ref - peer_ref is NULL\n");
                                exit_status = ret;
                                goto DONE;
                            }
                        }

                        opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                            "crcp:coord: pml_start: Matched a Recv_init: total = %d",
                                            peer_ref->total_recv_init_msgs);


                        /* Copy the drained message */
                        if( 0 != ompi_ddt_copy_content_same_ddt(msg_ref->datatype, msg_ref->count,
                                                                msg_ref->buffer, drain_msg_ref->buffer) ) {
                            opal_output( mca_crcp_coord_component.super.output_handle,
                                         "crcp:coord: pml_start(): Datatype copy failed (%d)",
                                         ret);
                        }

                        coord_start_req_types[iter_req] = requests[iter_req]->req_type;
                        requests[iter_req]->req_type = OMPI_REQUEST_MAX;

                        /* Remove the message from the list */
                        opal_list_remove_item(&drained_msg_list, &(drain_msg_ref->super));
                        drain_msg_ref->request = NULL;
                        OBJ_RELEASE(drain_msg_ref);
                        
                        peer_ref->total_drained_msgs -= 1;
                        msg_ref->matched = true; /* So we don't count this message a second time -- JJH Check if needed */
                    }
                }
            }
        }

        exit_status = OMPI_SUCCESS;
        goto DONE;
    }
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        for(iter_req = 0; iter_req < count; iter_req++) {
            /*
             * If this was a drained message it will have it's type set to
             * OMPI_REQUEST_MAX so the PML does not try to start it again. 
             * So we need to replace it with the original type
             */
            if( NULL != coord_start_req_types ) {
                if( OMPI_REQUEST_MAX != coord_start_req_types[iter_req] ) {
                    requests[iter_req]->req_type = coord_start_req_types[iter_req];
                    continue;
                }
            }

            breq = (mca_pml_base_request_t *)requests[iter_req];
            tmp_ddt_size = (breq->req_datatype)->size;

            /*
             * If the message was a send operation
             */
            if(breq->req_type == MCA_PML_REQUEST_SEND ) {
                /*
                 * Find the peer reference
                 */
                if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm,
                                                             breq->req_peer,
                                                             &peer_ref) ) ){
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: req_start(): Failed to find peer_ref\n");
                    exit_status = ret;
                    goto DONE;
                }
                if( NULL == peer_ref ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: req_start(): Failed to find peer_ref - peer_ref is NULL\n");
                    exit_status = ret;
                    goto DONE;
                }

                /* Check the send_init list */
                if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->send_init_list),
                                                              breq->req_count,
                                                              breq->req_tag,
                                                              breq->req_peer,
                                                              tmp_ddt_size,
                                                              &msg_ref,
                                                              FIND_MSG_UNKNOWN,
                                                              FIND_MSG_FALSE,
                                                              FIND_MSG_FALSE,
                                                              FIND_MSG_TRUE
                                                              ) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_start(): Unable to find the proper (send_init) message ref for this recv\n");
                    exit_status = ret;
                    goto DONE;
                }
                    
                if( NULL != msg_ref ) {
                    ompi_crcp_coord_pml_message_ref_t *dup_msg_ref  = NULL;
                    
                    /* Duplicate the message so it can complete, and original can be matched again */
                    DUP_MSG(dup_msg_ref, msg_ref);
                    
                    /* Known states
                     *  matched = false
                     *  done    = false
                     *  active  = false (-> true for dup'ed msg)
                     *  already_posted = true;
                     */
                    dup_msg_ref->matched = false;
                    dup_msg_ref->done    = false;
                    dup_msg_ref->active  = true;
                    dup_msg_ref->already_posted  = true;
                    
                    /* We must re-point to the request since there is a chance it
                     * may have been updated by the PML. */
                    dup_msg_ref->request = requests[iter_req];
                    
                    opal_list_append(&(peer_ref->send_init_list), &(dup_msg_ref->super));
                    
                    /* Account for this inflight send */
                    peer_ref->total_send_init_msgs += 1;
                    continue;
                }
                else {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_start(): Could not find message ref - THIS SHOULD NEVER HAPPEN"
                                "[file %s line %d]\n",
                                __FILE__, __LINE__);
                    pml_state->error_code = OMPI_ERROR;
                    goto DONE;
                }
            }
            /*
             * If this was a receive operation then check those lists.
             */
            else if (breq->req_type == MCA_PML_REQUEST_RECV) {
                /*
                 * If peer rank is given then find the peer reference
                 */
                if( 0 <= breq->req_peer ) {
                    if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm,
                                                                 breq->req_peer,
                                                                 &peer_ref) ) ){
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Failed to find peer_ref\n");
                        exit_status = ret;
                        goto DONE;
                    }
                    if( NULL == peer_ref ) {
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Failed to find peer_ref - peer_ref is NULL\n");
                        exit_status = ret;
                        goto DONE;
                    }
                    
                    if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->recv_init_list),
                                                                  breq->req_count,
                                                                  breq->req_tag,
                                                                  breq->req_peer,
                                                                  tmp_ddt_size,
                                                                  &msg_ref,
                                                                  FIND_MSG_UNKNOWN,
                                                                  FIND_MSG_FALSE,
                                                                  FIND_MSG_FALSE,
                                                                  FIND_MSG_TRUE
                                                                  ) ) ) {
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Unable to find the proper (recv) message ref for this recv\n");
                        exit_status = ret;
                        goto DONE;
                    }
                    
                    if( NULL != msg_ref ) {
                        ompi_crcp_coord_pml_message_ref_t *dup_msg_ref  = NULL;

                        /* Duplicate the message so it can complete, and original can be matched again */
                        DUP_MSG(dup_msg_ref, msg_ref);

                        /* Known states
                         *  matched = false
                         *  done    = false
                         *  active  = false (-> true for dup'ed msg)
                         *  already_posted = true;
                         */
                        dup_msg_ref->matched = false;
                        dup_msg_ref->done    = false;
                        dup_msg_ref->active  = true;
                        dup_msg_ref->already_posted  = true;

                        /* We must re-point to the request since there is a chance it
                         * may have been updated by the PML. */
                        dup_msg_ref->request = requests[iter_req];
                            
                        opal_list_append(&(peer_ref->recv_init_list), &(dup_msg_ref->super));

                        continue;
                    }
                    else {
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Could not find message ref - THIS SHOULD NEVER HAPPEN"
                                    "[file %s line %d]\n",
                                    __FILE__, __LINE__);
                        pml_state->error_code = OMPI_ERROR;
                        goto DONE;
                    }
                }
                /*
                 * Else peer is not known
                 */
                else {
                    if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_persist_recv_list),
                                                                  breq->req_count,
                                                                  breq->req_tag,
                                                                  INVALID_INT,
                                                                  tmp_ddt_size,
                                                                  &msg_ref,
                                                                  FIND_MSG_UNKNOWN,
                                                                  FIND_MSG_FALSE,
                                                                  FIND_MSG_FALSE,
                                                                  FIND_MSG_TRUE
                                                                  ) ) ) {
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Unable to find the proper (recv) message ref for this recv\n");
                        exit_status = ret;
                        goto DONE;
                    }

                    if( NULL != msg_ref ) {
                        ompi_crcp_coord_pml_message_ref_t *dup_msg_ref  = NULL;

                        /* Duplicate the message so it can complete, and original can be matched again */
                        DUP_MSG(dup_msg_ref, msg_ref);

                        /* Known states
                         *  matched = false
                         *  done    = false
                         *  active  = false (-> true for dup'ed msg)
                         *  already_posted = true;
                         */
                        dup_msg_ref->matched = false;
                        dup_msg_ref->done    = false;
                        dup_msg_ref->active  = true;
                        dup_msg_ref->already_posted  = true;

                        /* We must re-point to the request since there is a chance it
                         * may have been updated by the PML. */
                        dup_msg_ref->request = requests[iter_req];

                        opal_list_append(&(unknown_persist_recv_list), &(dup_msg_ref->super));

                        continue;
                    }
                    else {
                        opal_output(mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(): Could not find message ref - THIS SHOULD NEVER HAPPEN"
                                    "[file %s line %d]\n",
                                    __FILE__, __LINE__);
                        pml_state->error_code = OMPI_ERROR;
                        goto DONE;
                    }
                }
            }
            /*
             * Unkonwn request type...
             */
            else {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: pml_start(POST): Unknown Request type (%d)",
                            breq->req_type);
                pml_state->error_code = OMPI_ERROR;
                goto DONE;
            }
        }

        /*
         * Clear out the temporary drain type structure.
         */
        if( NULL != coord_start_req_types ) {
            free(coord_start_req_types);
            coord_start_req_types = NULL;
        }
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

/**************** Request Completed ********/
int ompi_crcp_coord_request_complete(struct ompi_request_t *request)
{
    ompi_crcp_coord_pml_peer_ref_t    *peer_ref = NULL;
    ompi_crcp_coord_pml_message_ref_t *msg_ref  = NULL;
    mca_pml_base_request_t *breq;
    size_t tmp_ddt_size  = 0;
    int ret, exit_status = OMPI_SUCCESS;
    int src;
    int tag;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_request_complete()");

    /*
     * Extract the PML version of the request
     */
    breq = (mca_pml_base_request_t *)request;

    /* Check for the NULL Request */
    if( (breq->req_type   != MCA_PML_REQUEST_SEND &&
         breq->req_type   != MCA_PML_REQUEST_RECV ) || /* JJH YYY -- req_state = OMPI_REQUEST_INACTIVE ??? */
        request->req_type == OMPI_REQUEST_NOOP ||
        request->req_type == OMPI_REQUEST_NULL) {
        exit_status = OMPI_SUCCESS;
        goto DONE;
    }

    /* Extract source/tag/ddt_size */
    src = breq->req_peer;
    tag = breq->req_tag;
    tmp_ddt_size = (breq->req_datatype)->size;

    /*************
     * First find the peer reference
     *************/
    if( MPI_ANY_SOURCE == src ) {
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm,
                                                     request->req_status.MPI_SOURCE,
                                                     &peer_ref) ) ){
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: req_complete(): Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }
    } else {
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm,
                                                     src,
                                                     &peer_ref) ) ){
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: req_complete(): Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }
    }
    if( NULL == peer_ref ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: req_complete(): Failed to find peer_ref - peer_ref is NULL\n");
        exit_status = ret;
        goto DONE;
    }

    /*******************************
     * A send request is completing
     ******************************/
    if(breq->req_type == MCA_PML_REQUEST_SEND ) {
        /*
         * ISEND Case:
         */
        if( false == request->req_persistent ) {
            /* Check the isend list */
            if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->isend_list),
                                                          breq->req_count,
                                                          tag, src,
                                                          tmp_ddt_size,
                                                          &msg_ref,
                                                          FIND_MSG_UNKNOWN,
                                                          FIND_MSG_FALSE,
                                                          FIND_MSG_TRUE,
                                                          FIND_MSG_TRUE
                                                          ) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: req_complete: Unable to find the proper (isend) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL != msg_ref ) {
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: req_complete: Matched an iSend: total = %d",
                                    peer_ref->total_isend_msgs);
                goto FOUND;
            }
            else {
                /*
                 * It is possible that we did not 'find' the message because 
                 * we could have previously marked it as done. Due to the
                 * logic in the Request Wait/Test routines we could
                 * receive multiple request complete calls for the
                 * same request.
                 */
                exit_status = OMPI_SUCCESS;
                goto DONE;
            }
        }
        /*
         * SEND_INIT/START Case
         */
        else {
            /* Check the isend_init list */
            if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->send_init_list),
                                                          breq->req_count,
                                                          tag, src,
                                                          tmp_ddt_size,
                                                          &msg_ref,
                                                          FIND_MSG_UNKNOWN,
                                                          FIND_MSG_FALSE,
                                                          FIND_MSG_TRUE,
                                                          FIND_MSG_TRUE
                                                          ) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: req_complete: Unable to find the proper (send_init) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }

            if( NULL != msg_ref ) {
                /* Known states
                 *  matched = false
                 *  done    = false
                 *  active  = true
                 *  already_posted = true;
                 */
                goto FOUND;
            }
            else {
                /*
                 * It is possible that we did not 'find' the message because 
                 * we could have previously marked it as done. Due to the
                 * logic in the Request Wait/Test routines we could
                 * receive multiple request complete calls for the
                 * same request.
                 */
                exit_status = OMPI_SUCCESS;
                goto DONE;
            }
        }
    }
    /***********************************
     * A receive request is completing
     ***********************************/
    else if(breq->req_type == MCA_PML_REQUEST_RECV) {
        /*
         * IRECV Case:
         */
        if( false == request->req_persistent ) {
            /* Check the irecv list */
            if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->irecv_list),
                                                          breq->req_count,
                                                          tag, src,
                                                          tmp_ddt_size,
                                                          &msg_ref,
                                                          FIND_MSG_UNKNOWN,
                                                          FIND_MSG_FALSE,
                                                          FIND_MSG_TRUE,
                                                          FIND_MSG_TRUE
                                                          ) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: req_complete: Unable to find the proper (irecv) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL != msg_ref ) {
                /*
                 * Only increment if this message has not been matched yet
                 */
                if( !(msg_ref->matched) ) {
                    peer_ref->total_irecv_msgs += 1;
                }

                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: req_complete: Matched an iRecv: total = %d",
                                    peer_ref->total_irecv_msgs);
                goto FOUND;
            }

            /*
             * At this point the message was either completed and this is the second time we
             * have seen this request *or* it is in the unknown receive from list.
             */
            if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_recv_from_list),
                                                          breq->req_count,
                                                          tag,
                                                          INVALID_INT,
                                                          tmp_ddt_size,
                                                          &msg_ref,
                                                          FIND_MSG_UNKNOWN,
                                                          FIND_MSG_FALSE,
                                                          FIND_MSG_TRUE,
                                                          FIND_MSG_TRUE
                                                          ) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: req_complete: Unable to find the proper (recv_init) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL != msg_ref ) {
                opal_list_remove_item(&(unknown_recv_from_list), &(msg_ref->super));
                opal_list_append(&(peer_ref->irecv_list), &(msg_ref->super));

                /*
                 * Only increment if this message has not been matched yet
                 */
                if( !(msg_ref->matched) ) {
                    peer_ref->total_irecv_msgs += 1;
                }

                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: req_complete: Matched an iRecv: total = %d",
                                    peer_ref->total_irecv_msgs);
                goto FOUND;
            }
            else {
                /*
                 * It is possible that we did not 'find' the message because 
                 * we could have previously marked it as done. Due to the
                 * logic in the Request Wait/Test routines we could
                 * receive multiple request complete calls for the
                 * same request.
                 */
                exit_status = OMPI_SUCCESS;
                goto DONE;
            }
        }
        /*
         * IRECV_INIT/START Case:
         */
        else {
            /* Check the irecv_init list */
            if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->recv_init_list),
                                                          breq->req_count,
                                                          tag, src,
                                                          tmp_ddt_size,
                                                          &msg_ref,
                                                          FIND_MSG_UNKNOWN,
                                                          FIND_MSG_FALSE,
                                                          FIND_MSG_TRUE,
                                                          FIND_MSG_TRUE
                                                          ) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: req_complete: Unable to find the proper (recv_init) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL != msg_ref ) {
                /*
                 * Only increment if this message has not been matched yet
                 */
                if( !(msg_ref->matched) ) {
                    peer_ref->total_recv_init_msgs += 1;
                }
                goto FOUND;
            }
            else {
                /* The message was not on the known list, check the unkown list */
                if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_persist_recv_list),
                                                              breq->req_count,
                                                              tag,
                                                              INVALID_INT,
                                                              tmp_ddt_size,
                                                              &msg_ref,
                                                              FIND_MSG_UNKNOWN,
                                                              FIND_MSG_FALSE,
                                                              FIND_MSG_TRUE,
                                                              FIND_MSG_TRUE
                                                              ) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: requ_complete: Unable to find the proper (recv_init) message ref for this complete\n");
                    exit_status = ret;
                    goto DONE;
                }

                if( NULL != msg_ref ) {
                    /* Move it from the unknown list to this comm list */
                    opal_list_remove_item(&(unknown_persist_recv_list), &(msg_ref->super));
                    opal_list_append(     &(peer_ref->recv_init_list),  &(msg_ref->super));

                    /*
                     * Only increment if this message has not been matched yet
                     */
                    if( !(msg_ref->matched) ) {
                        peer_ref->total_recv_init_msgs += 1;
                    }
                    goto FOUND;
                }
                else {
                    /*
                     * It is possible that we did not 'find' the message because 
                     * we could have previously marked it as done. Due to the
                     * logic in the Request Wait/Test routines we could
                     * receive multiple request complete calls for the
                     * same request.
                     */
                    exit_status = OMPI_SUCCESS;
                    goto DONE;
                }
            }
        }
    }
    /*
     * An unknown type of request is completing.
     */
    else {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: req_complete: Unknown request type... %d",
                    breq->req_type);
    }
    
 FOUND:
    if( NULL != msg_ref ) {
        /*
         * - some Booleans already set... 
         *   msg_ref->matched         = false;
         *   msg_ref->done            = false;
         *   msg_ref->already_posted  = true;
         */
        msg_ref->done   = true;
        msg_ref->active = false;

        opal_output_verbose(25, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: req_complete: Marked Message... ( %d, %d )\n",
                            peer_ref->total_isend_msgs, peer_ref->total_irecv_msgs);
    }
    else {
        /*
         * It is possible that we have 'completed' this message previously,
         *  so this case can occur during normal operation.
         * This is caused by us checking for completeness twice in ompi_request_wait_all.
         */
        opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: request_complete: No match found for this request :( %d, %d ): [%d/%d,%d]\n",
                            peer_ref->total_isend_msgs, peer_ref->total_irecv_msgs,
                            breq->req_peer, src, breq->req_comm->c_contextid);
    }

 DONE:
    return exit_status;
}

/**************** FT Event *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_ft_event(
                                  int state, 
                                  ompi_crcp_base_pml_state_t* pml_state)
{
    static int step_to_return_to = 0;
    int exit_status = OMPI_SUCCESS;
    int ret;

    ft_event_state = state;

    if( step_to_return_to == 1 ) {
        goto STEP_1;
    }

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_ft_event()");

    /**************************
     * Prepare for a Checkpoint
     **************************/
    if(OPAL_CRS_CHECKPOINT == state) {
        if( OMPI_CRCP_PML_PRE != pml_state->state){
            goto DONE;
        }

        START_TIMER(CRCP_TIMER_CKPT);
    STEP_1:
        step_to_return_to = 0;

        /* Coordinate Peers:
         * When we return from this function we know that all of our
         * channels have been flushed.
         */
        if( OMPI_SUCCESS != (ret = ft_event_coordinate_peers()) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: ft_event: Checkpoint Coordination Failed %d",
                        ret);
            exit_status = ret;
            goto DONE;
        }

        if( stall_for_completion ) {
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_ft_event: STALLING PID %d\n",
                                getpid());

            stall_for_completion = false;
            opal_cr_stall_check  = true;
            step_to_return_to    = 1;

            exit_status = OMPI_EXISTS;
            goto DONE_STALL;
        }
        END_TIMER(CRCP_TIMER_CKPT);
    }
    /*****************************
     * Continue after a checkpoint
     ******************************/
    else if(OPAL_CRS_CONTINUE == state) {
        if( OMPI_CRCP_PML_POST != pml_state->state){
            goto DONE;
        }

        START_TIMER(CRCP_TIMER_CONT);
        /*
         * Finish the coord protocol
         */
        if( OMPI_SUCCESS != (ret = ft_event_finalize_exchange() ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_ft_event: Checkpoint Finalization Failed %d",
                        ret);
            exit_status = ret;
            goto DONE;
        }
        END_TIMER(CRCP_TIMER_CONT);
    }
    /*****************************
     * Restart from a checkpoint
     *****************************/
    else if(OPAL_CRS_RESTART == state) {
        if( OMPI_CRCP_PML_POST != pml_state->state){
            goto DONE;
        }

        /*
         * Finish the coord protocol
         */
        if( OMPI_SUCCESS != (ret = ft_event_finalize_exchange() ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_ft_event: Checkpoint Finalization Failed %d",
                        ret);
            exit_status = ret;
            goto DONE;
        }
    }
    /*****************************
     * Terminating the process post checkpoint
     *****************************/
    else if(OPAL_CRS_TERM == state ) {
        goto DONE;
    }
    /****************************
     * Reached an error
     ****************************/
    else {
        goto DONE;
    }

 DONE:
    step_to_return_to = 0;
    ft_event_state = OPAL_CRS_RUNNING;

    DISPLAY_ALL_TIMERS(state);
    clear_timers();

 DONE_STALL:
    pml_state->error_code = exit_status;
    return pml_state;
}

/******************
 * Local Utility functions
 ******************/

static ompi_crcp_coord_pml_peer_ref_t * find_peer(orte_process_name_t proc)
{
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(&ompi_crcp_coord_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_coord_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_coord_pml_peer_ref_t *cur_peer_ref;
        cur_peer_ref = (ompi_crcp_coord_pml_peer_ref_t*)item;

        if( 0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL,
                                        &(cur_peer_ref->proc_name),
                                        &proc) ) {
            return cur_peer_ref;
        }
    }

    return NULL;
}

static int find_peer_in_comm(struct ompi_communicator_t* comm, int proc_idx,
                             ompi_crcp_coord_pml_peer_ref_t **peer_ref)
{
    *peer_ref = find_peer(comm->c_remote_group->grp_proc_pointers[proc_idx]->proc_name);

    if( NULL == *peer_ref) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static int find_drained_msg(size_t ddt_size,
                            size_t count, int tag, int peer,
                            ompi_crcp_coord_pml_message_ref_t ** found_msg_ref)
{
    opal_list_item_t* item = NULL;

    *found_msg_ref = NULL;

    /* Dummy Check:
     * If the list is empty...
     */
    if( 0 >= opal_list_get_size(&drained_msg_list) ) {
        return OMPI_SUCCESS;
    }
    
    for(item  = opal_list_get_first(&drained_msg_list);
        item != opal_list_get_end(&drained_msg_list);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_coord_pml_message_ref_t * drain_msg = NULL;

        drain_msg = (ompi_crcp_coord_pml_message_ref_t*)item;

        opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: find_drain_msg(): Compare [%d, %d, %d, %d] to %c-[%d, %d, %d, %d]",
                            (int)ddt_size, (int)count, tag, peer,
                            (NULL == drain_msg->buffer ? 'T' : 'F'),
                            (int)drain_msg->ddt_size, (int)drain_msg->count, (int)drain_msg->tag, (int)drain_msg->rank);

        /* If the buffer is invalid then this is not a valid message or
         * has not been completed draining just yet */
        if(NULL == drain_msg->buffer) {
            continue;
        }

        /* If a specific tag was requested, then make sure this messages matches */
        if( MPI_ANY_TAG    != tag &&
            drain_msg->tag != tag) {
            continue;
        }

        /* If a specific rank was requested, then make sure this messages matches */
        if( INVALID_INT != peer ) {
            if( MPI_ANY_SOURCE  != peer && 
                drain_msg->rank != peer) {
                continue;
            }
        }

        /* Check the datatype size, if specified for a match */
        if( ddt_size != PROBE_ANY_SIZE &&
            count    != PROBE_ANY_COUNT) {
            /* Check the datatype size and count to make sure it matches   */
            if((drain_msg->count   ) != count   || 
               (drain_msg->ddt_size) != ddt_size) {
                continue;
            }
        }

        /* If we get here then the message matches */
        *found_msg_ref = drain_msg;
        break;
    }

    return OMPI_SUCCESS;
}
/******************
 * Local FT Event functions
 ******************/
static int ft_event_coordinate_peers(void)
{
    static int step_to_return_to = 0;
    int exit_status = OMPI_SUCCESS;
    int ret;
    
    if( step_to_return_to == 1 ) {
        goto STEP_1;
    }

    /*
     * Exchange Bookmarks with peers
     */
    START_TIMER(CRCP_TIMER_CKPT_EX_B);
    if( OMPI_SUCCESS != (ret = ft_event_exchange_bookmarks() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Bookmark Exchange Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }
    END_TIMER(CRCP_TIMER_CKPT_EX_B);

    /*
     * Check exchanged bookmarks 
     */
    START_TIMER(CRCP_TIMER_CKPT_CK_B);
    if( OMPI_SUCCESS != (ret = ft_event_check_bookmarks() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Bookmark Check Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }
    END_TIMER(CRCP_TIMER_CKPT_CK_B);

    /*
     * Post Drain Acks and Msgs
     */
    START_TIMER(CRCP_TIMER_CKPT_POST);
    if( OMPI_SUCCESS != (ret = ft_event_post_drain_acks() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Bookmark Post Drain ACKS Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }

    if( OMPI_SUCCESS != (ret = ft_event_post_drained() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Bookmark Post Drain Msgs Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }
    END_TIMER(CRCP_TIMER_CKPT_POST);

    /*
     * Check if we need to stall for completion of tasks
     */
    if( stall_for_completion ) {
        opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: ft_event_coordinate_peers: %s **** STALLING ***",
                            ORTE_NAME_PRINT(orte_process_info.my_name));
        step_to_return_to = 1;
        exit_status = OMPI_SUCCESS;
        goto DONE;
    }

 STEP_1:
    step_to_return_to = 0;

    /*
     * Wait for any messages that needed resolved.
     * - Outstanding Receives (to drain wire) -- Receiver
     * - Outstanding Irecvs (for drain ack)   -- Sender
     */
    START_TIMER(CRCP_TIMER_CKPT_WAIT);
    if( OMPI_SUCCESS != (ret = ft_event_wait_quiesce() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Wait Quiesce Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }
    END_TIMER(CRCP_TIMER_CKPT_WAIT);

    /*
     * Exchange Bookmarks with peers -- JJH -- Improve this
     *  - This serves as finish barrier
     *  We need a barrier here to keep peers from sending to us before we have taken
     *  our checkpoint. Ideally this would not happen, but needs some futher investigation.
     */
    START_TIMER(CRCP_TIMER_CKPT_BARR);
    if( OMPI_SUCCESS != (ret = coord_basic_barrier() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Basic Barrier Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }
    END_TIMER(CRCP_TIMER_CKPT_BARR);

    opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: ft_event_coordinate_peers: %s Coordination Finished...\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name) );

    /*
     * Now that all our peer channels are marked as drained
     * continue with the checkpoint.
     * Note: This does not guarentee that all of the peers
     *       are at this same position, but that our
     *       checkpoint will be consistent with all of the
     *       peers once they finish the protocol.
     */

 DONE:
    return exit_status;
}

static int ft_event_finalize_exchange(void)
{
    int exit_status = OMPI_SUCCESS;
    opal_list_item_t* item = NULL;

    /*
     * Clear bookmark totals
     */
    for(item  = opal_list_get_first(&ompi_crcp_coord_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_coord_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_coord_pml_peer_ref_t *peer_ref;
        peer_ref = (ompi_crcp_coord_pml_peer_ref_t*)item;

        peer_ref->total_send_msgs          = 0;
        peer_ref->total_isend_msgs         = 0;
        peer_ref->total_send_init_msgs     = 0;
        peer_ref->total_recv_msgs          = 0;
        peer_ref->total_irecv_msgs         = 0;
        peer_ref->total_recv_init_msgs     = 0;

        peer_ref->matched_send_msgs        = 0;
        peer_ref->matched_isend_msgs       = 0;
        peer_ref->matched_send_init_msgs   = 0;
        peer_ref->matched_recv_msgs        = 0;
        peer_ref->matched_irecv_msgs       = 0;
        peer_ref->matched_recv_init_msgs   = 0;

#if 0
        /*
         * Clean out the message lists
         */
        while( NULL != (rm_item = opal_list_remove_first(&peer_ref->send_list)) ) {
            OBJ_RELEASE(rm_item);
        }

        while( NULL != (rm_item = opal_list_remove_first(&peer_ref->isend_list)) ) {
            OBJ_RELEASE(rm_item);
        }

        while( NULL != (rm_item = opal_list_remove_first(&peer_ref->send_init_list)) ) {
            OBJ_RELEASE(rm_item);
        }


        while( NULL != (rm_item = opal_list_remove_first(&peer_ref->recv_list)) ) {
            OBJ_RELEASE(rm_item);
        }

        while( NULL != (rm_item = opal_list_remove_first(&peer_ref->irecv_list)) ) {
            OBJ_RELEASE(rm_item);
        }

        while( NULL != (rm_item = opal_list_remove_first(&peer_ref->recv_init_list)) ) {
            OBJ_RELEASE(rm_item);
        }
#endif
    }

    return exit_status;
}

static int ft_event_exchange_bookmarks(void)
{
    int peer_idx  = 0;
    int my_idx    = orte_process_info.my_name->vpid;
    int iter      = 0;
    int num_peers = 0;
    
    num_peers = opal_list_get_size(&ompi_crcp_coord_pml_peer_refs);

    for( peer_idx = (num_peers - my_idx - 1), iter = 0;
         iter < num_peers;
         peer_idx = (peer_idx + 1) % num_peers, ++iter) 
        {
            if(my_idx > peer_idx) {
                /* Send our bookmark status */
                send_bookmarks(peer_idx);
                /* Recv peer bookmark status */
                recv_bookmarks(peer_idx);
            }
            else if(my_idx < peer_idx) {
                /* Recv peer bookmark status */
                recv_bookmarks(peer_idx);
                /* Send our bookmark status */
                send_bookmarks(peer_idx);
            }
        }
    
    /* Wait for all bookmarks to arrive */
    START_TIMER(CRCP_TIMER_CKPT_WAIT_B);
    while( total_recv_bookmarks > 0 ) {
        opal_progress();
        opal_event_loop(OPAL_EVLOOP_NONBLOCK);
    }
    total_recv_bookmarks = 0;
    END_TIMER(CRCP_TIMER_CKPT_WAIT_B);
    DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_WAIT_B, 0);

    return OMPI_SUCCESS;
}

static int ft_event_check_bookmarks(void)
{
    opal_list_item_t* item = NULL;
    int ret;
    int p_n_to_p_m   = 0;
    int p_n_from_p_m = 0;

    if( 10 <= mca_crcp_coord_component.super.verbose ) {
        sleep(orte_process_info.my_name->vpid);
        opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                            "Process %s Match Table",
                            ORTE_NAME_PRINT(orte_process_info.my_name));
        opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                            "%s  %5s | %7s | %7s | %7s | %7s |",
                            ORTE_NAME_PRINT(orte_process_info.my_name),
                            "Vpid", "T_Send", "M_Recv", "M_Send", "T_Recv");

        for(item  = opal_list_get_first(&ompi_crcp_coord_pml_peer_refs);
            item != opal_list_get_end(&ompi_crcp_coord_pml_peer_refs);
            item  = opal_list_get_next(item) ) {
            ompi_crcp_coord_pml_peer_ref_t *peer_ref;
            int t_send, m_send;
            int t_recv, m_recv;
            peer_ref = (ompi_crcp_coord_pml_peer_ref_t*)item;

            t_send = (peer_ref->total_send_msgs        +
                      peer_ref->total_isend_msgs       +
                      peer_ref->total_send_init_msgs   );
            m_send = (peer_ref->matched_send_msgs      +
                      peer_ref->matched_isend_msgs     +
                      peer_ref->matched_send_init_msgs ); 
            t_recv = (peer_ref->total_recv_msgs        +
                      peer_ref->total_irecv_msgs       +
                      peer_ref->total_recv_init_msgs   );
            m_recv = (peer_ref->matched_recv_msgs      +
                      peer_ref->matched_irecv_msgs     +
                      peer_ref->matched_recv_init_msgs );

            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "%s  %5d | %7d | %7d | %7d | %7d |",
                                ORTE_NAME_PRINT(orte_process_info.my_name),
                                peer_ref->proc_name.vpid,
                                t_send, m_recv, m_send, t_recv);
        }
    }

    /*
     * For each peer:
     * - Check bookmarks
     * - if mis-matched then post outstanding recvs.
     */
    for(item  = opal_list_get_first(&ompi_crcp_coord_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_coord_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_coord_pml_peer_ref_t *peer_ref;
        peer_ref = (ompi_crcp_coord_pml_peer_ref_t*)item;

        if( 0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL,
                                        (orte_process_info.my_name),
                                        &(peer_ref->proc_name)) ) {
            continue;
        }

        /* Lowest Rank sends first */
        if( orte_process_info.my_name->vpid < peer_ref->proc_name.vpid ) {
            /********************
             * Check P_n --> P_m
             * Has the peer received all the messages that I have put on the wire?
             ********************/
            p_n_to_p_m   = (peer_ref->total_send_msgs        +
                            peer_ref->total_isend_msgs       +
                            peer_ref->total_send_init_msgs   );
            p_n_from_p_m = (peer_ref->matched_recv_msgs      +
                            peer_ref->matched_irecv_msgs     +
                            peer_ref->matched_recv_init_msgs );

            /* T_Send >= M_Recv */
            if( p_n_to_p_m < p_n_from_p_m ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: check_bookmarks: %s --> %s "
                            "Sent Msgs (%4d) = Received Msgs (%4d) => Diff (%4d). "
                            " WARNING: Peer received more than was sent. :(\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name),
                            ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                            p_n_to_p_m,
                            p_n_from_p_m,
                            (p_n_to_p_m - p_n_from_p_m)
                            );
            }

            /* I've send more than my peer has received,
             * so need to coordinate with peer. */
            if( p_n_to_p_m > p_n_from_p_m) {
                opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: check_bookmarks: %s --> %s "
                                    "Sent Msgs (%4d) = Received Msgs (%4d). Peer needs %4d.\n",
                                    ORTE_NAME_PRINT(orte_process_info.my_name),
                                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    );
                /*
                 * Tell the peer what the outstanding messages looked like.
                 * Since we can't tell which ones they are, we need to send the
                 * information for all of the messages since the last checkpoint
                 */
                if( OMPI_SUCCESS != (ret = send_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: check_bookmarks: Unable to send message details to peer %s: Return %d\n",
                                ORTE_NAME_PRINT(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }

            /********************
             * Check P_n <-- P_m
             * Have I received all the messages that my peer has put on the wire?
             ********************/
            p_n_to_p_m   = (peer_ref->matched_send_msgs      +
                            peer_ref->matched_isend_msgs     +
                            peer_ref->matched_send_init_msgs );
            p_n_from_p_m = (peer_ref->total_recv_msgs        +
                            peer_ref->total_irecv_msgs       +
                            peer_ref->total_recv_init_msgs   );

            /* M_Send >= T_Recv */
            if( p_n_to_p_m < p_n_from_p_m ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: check_bookmarks: %s --> %s "
                            "Sent Msgs (%4d) = Received Msgs (%4d) => Diff (%4d). "
                            " WARNING: I received more than the peer sent. :(\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name),
                            ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                            p_n_to_p_m,
                            p_n_from_p_m,
                            (p_n_to_p_m - p_n_from_p_m)
                            );
            }

            /* I've recv'ed less than my peer has sent,
             * so need to coordinate with peer. */
            if( p_n_to_p_m > p_n_from_p_m) {
                opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: check_bookmarks: %s <-- %s "
                                    "Received Msgs (%4d) = Sent Msgs (%4d). I need %4d.\n",
                                    ORTE_NAME_PRINT(orte_process_info.my_name),
                                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    );
                /*
                 * Receive from peer the datatypes of the outstanding sends
                 *  As we figure out what they are post Irecv's for them into a drained buffer list.
                 */
                if( OMPI_SUCCESS != (ret = recv_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: check_bookmarks: Unable to recv message details from peer %s: Return %d\n",
                                ORTE_NAME_PRINT(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }
        }
        /* Highest rank recvs first */
        else {
            /********************
             * Check P_n <-- P_m
             * Have I received all the messages that my peer has put on the wire?
             ********************/
            p_n_to_p_m   = (peer_ref->matched_send_msgs      +
                            peer_ref->matched_isend_msgs     +
                            peer_ref->matched_send_init_msgs );
            p_n_from_p_m = (peer_ref->total_recv_msgs        +
                            peer_ref->total_irecv_msgs       +
                            peer_ref->total_recv_init_msgs   );

            /* M_Send >= T_Recv */
            if( p_n_to_p_m < p_n_from_p_m ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: check_bookmarks: %s --> %s "
                            "Sent Msgs (%4d) = Received Msgs (%4d) => Diff (%4d). "
                            " WARNING: I received more than the peer sent. :(\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name),
                            ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                            p_n_to_p_m,
                            p_n_from_p_m,
                            (p_n_to_p_m - p_n_from_p_m)
                            );
            }

            /* I've recv'ed less than my peer has sent,
             * so need to coordinate with peer. */
            if( p_n_to_p_m > p_n_from_p_m) {
                opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: check_bookmarks: %s <-- %s "
                                    "Received Msgs (%4d) = Sent Msgs (%4d). I need %4d.\n",
                                    ORTE_NAME_PRINT(orte_process_info.my_name),
                                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    );
                /*
                 * Receive from peer the datatypes of the outstanding sends
                 *  As we figure out what they are post Irecv's for them into a drained buffer list.
                 */
                if( OMPI_SUCCESS != (ret = recv_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: check_bookmarks: Unable to recv message details from peer %s: Return %d\n",
                                ORTE_NAME_PRINT(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }

            /********************
             * Check P_n --> P_m
             * Has the peer received all the messages that I have put on the wire?
             ********************/
            p_n_to_p_m   = (peer_ref->total_send_msgs        +
                            peer_ref->total_isend_msgs       +
                            peer_ref->total_send_init_msgs   );
            p_n_from_p_m = (peer_ref->matched_recv_msgs      +
                            peer_ref->matched_irecv_msgs     +
                            peer_ref->matched_recv_init_msgs );

            /* T_Send >= M_Recv */
            if( p_n_to_p_m < p_n_from_p_m ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: check_bookmarks: %s --> %s "
                            "Sent Msgs (%4d) = Received Msgs (%4d) => Diff (%4d). "
                            " WARNING: Peer received more than was sent. :(\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name),
                            ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                            p_n_to_p_m,
                            p_n_from_p_m,
                            (p_n_to_p_m - p_n_from_p_m)
                            );
            }

            /* I've send more than my peer has received,
             * so need to coordinate with peer. */
            if( p_n_to_p_m > p_n_from_p_m) {
                opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: check_bookmarks: %s --> %s "
                                    "Sent Msgs (%4d) = Received Msgs (%4d). Peer needs %4d.\n",
                                    ORTE_NAME_PRINT(orte_process_info.my_name),
                                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    );
                /*
                 * Tell the peer what the outstanding messages looked like.
                 * Since we can't tell which ones they are, we need to send the
                 * information for all of the messages since the last checkpoint
                 */
                if( OMPI_SUCCESS != (ret = send_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: check_bookmarks: Unable to send message details to peer %s: Return %d\n",
                                ORTE_NAME_PRINT(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }
        }
    }

    /*
     * Always stall if we are in a blocking send operation.
     * JJH -- Doesn't this duplicate the code in send_msg_details?
     */
    if( 0 < current_msg_id &&
        current_msg_type == COORD_MSG_TYPE_B_SEND) {
        stall_for_completion = true;
    }

    return OMPI_SUCCESS;
}

static int ft_event_post_drain_acks(void)
{
    drain_msg_ack_ref_t * drain_msg_ack;
    opal_list_item_t* item = NULL;
    size_t req_size;
    int ret;

    req_size  = opal_list_get_size(&drained_msg_ack_list);
    if(req_size <= 0) {
        return OMPI_SUCCESS;
    }

    opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: post_drain_ack: %s Wait on %d Drain ACK Messages.\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        (int)req_size);

    /*
     * We have loaded our peer with the message information
     * Now wait for the ack from them
     */
    for(item  = opal_list_get_first(&drained_msg_ack_list);
        item != opal_list_get_end(&drained_msg_ack_list);
        item  = opal_list_get_next(item) ) {
        drain_msg_ack = (drain_msg_ack_ref_t*)item;

        /* Post the receive */
        if( OMPI_SUCCESS != (ret = orte_rml.recv_buffer_nb( &drain_msg_ack->peer,
                                                            OMPI_CRCP_COORD_BOOKMARK_TAG,
                                                            0,
                                                            drain_message_ack_cbfunc,
                                                            NULL) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: post_drain_acks: %s Failed to post a RML receive to the peer\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name));
            return ret;
        }
    }

    return OMPI_SUCCESS;
}

static void drain_message_ack_cbfunc(int status,
                                     orte_process_name_t* sender,
                                     orte_buffer_t *buffer,
                                     orte_rml_tag_t tag,
                                     void* cbdata)
{
    int ret, exit_status = OMPI_SUCCESS;
    opal_list_item_t* item = NULL;
    size_t ckpt_status;

    /*
     * Unpack the buffer
     */
    UNPACK_BUFFER(buffer, ckpt_status, 1, ORTE_SIZE, "");

    /*
     * Update the outstanding message queue
     */
    for(item  = opal_list_get_first(&drained_msg_ack_list);
        item != opal_list_get_end(&drained_msg_ack_list);
        item  = opal_list_get_next(item) ) {
        drain_msg_ack_ref_t * drain_msg_ack;
        drain_msg_ack = (drain_msg_ack_ref_t*)item;
        
        /* If this ACK has not completed yet */
        if(!drain_msg_ack->complete) {
            /* If it is the correct peer */
            if(drain_msg_ack->peer.jobid  == sender->jobid &&
               drain_msg_ack->peer.vpid   == sender->vpid ) {
                /* We found it! */
                drain_msg_ack->complete = true;
                opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: drain_message_ack_cbfunc: %s --> %s Received ACK of FLUSH from peer\n",
                                    ORTE_NAME_PRINT(orte_process_info.my_name),
                                    ORTE_NAME_PRINT(sender) );
                return;
            }
        }
    }

    opal_output(mca_crcp_coord_component.super.output_handle,
                "crcp:coord: drain_message_ack_cbfunc: %s --> %s ERROR: Uable to match ACK to peer\n",
                ORTE_NAME_PRINT(orte_process_info.my_name),
                ORTE_NAME_PRINT(sender) );

 cleanup:
    return;
}

static int ft_event_post_drained(void)
{
    opal_list_item_t* item = NULL;
    size_t req_size;
    int ret;

    req_size  = opal_list_get_size(&drained_msg_list);
    if(req_size <= 0) {
        return OMPI_SUCCESS;
    }

    opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: post_drained: %s Draining %d Messages.\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        (int)req_size);

    /*
     * For each message in the drained list,
     * post a PML iRecv
     */
    for(item  = opal_list_get_first(&drained_msg_list);
        item != opal_list_get_end(&drained_msg_list);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_coord_pml_message_ref_t * drain_msg;
        drain_msg = (ompi_crcp_coord_pml_message_ref_t*)item;

        drain_msg->active = true;

        /* Do not repost those that are already posted, and 
         * we have requests for
         */
        if( drain_msg->already_posted ) {
            opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: post_drained: %s Found a message that we don't need to post.\n",
                                ORTE_NAME_PRINT(orte_process_info.my_name));
            continue;
        }
        /*
         * Post a receive to drain this message
         */
        else {
            opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: post_drained: %s Posting a message to be drained from %d.\n",
                                ORTE_NAME_PRINT(orte_process_info.my_name),
                                drain_msg->rank);
            if( OMPI_SUCCESS != (ret = wrapped_pml_module->pml_irecv(drain_msg->buffer, 
                                                                     (drain_msg->count * drain_msg->ddt_size),
                                                                     drain_msg->datatype, 
                                                                     drain_msg->rank,
                                                                     drain_msg->tag,
                                                                     drain_msg->comm,
                                                                     &(drain_msg->request) ) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: post_drained: %s Failed to post the Draining PML iRecv\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name) );
                return ret;
            }
        }
    }

    return OMPI_SUCCESS;
}

static int ft_event_wait_quiesce(void)
{
    int exit_status = OMPI_SUCCESS;
    int ret;

    /*********************************************
     * Wait for all draining receives to complete
     **********************************************/
    if( OMPI_SUCCESS != (ret = wait_quiesce_drained() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: wait_quiesce: %s Failed to quiesce drained messages\n",
                    ORTE_NAME_PRINT(orte_process_info.my_name) );
        exit_status = ret;
        goto cleanup;
    }

    /*******************************************************************
     * If we are waiting for All Clear messages from peers wait on them.
     *******************************************************************/
    if( OMPI_SUCCESS != (ret = wait_quiesce_drain_ack() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: wait_quiesce: %s Failed to recv all drain ACKs\n",
                    ORTE_NAME_PRINT(orte_process_info.my_name) );
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int wait_quiesce_drained(void)
{
    ompi_crcp_coord_pml_message_ref_t * drain_msg;
    ompi_request_t ** wait_any_requests = NULL;
    ompi_status_public_t ** wait_any_status = NULL;
    orte_process_name_t *proc_names = NULL;
    opal_list_item_t* item = NULL;
    opal_list_item_t* next = NULL;
    bool prev_stall = false;
    size_t wait_any_count = 0;
    size_t req_size;
    size_t last_proc_idx;
    size_t i;
    int exit_status = OMPI_SUCCESS;
    int ret;

    req_size  = opal_list_get_size(&drained_msg_list);
    if(req_size <= 0) {
        /*  Nothing to do */
        return OMPI_SUCCESS;
    }

    opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: wait_quiesce_drained: %s  Waiting on %d messages to drain\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        (int)req_size);

    /*
     * Create an array of requests to wait upon, and associated statuses
     */
    wait_any_requests = (ompi_request_t **)malloc( (req_size) * sizeof(ompi_request_t *));
    if( NULL == wait_any_requests){
        exit_status = OMPI_ERROR;
        goto cleanup;
    }
    wait_any_status = (ompi_status_public_t **)malloc( (req_size) * sizeof(ompi_status_public_t *));
    if( NULL == wait_any_status){
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    /*
     * Create an array of processes to respond to
     */
    proc_names = (orte_process_name_t *)malloc((req_size) * sizeof(orte_process_name_t));
    if( NULL == proc_names){
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    /* Initalize to invalid values */
    for(i = 0; i < (req_size); ++i) {
        wait_any_requests[i] = &ompi_request_null;
        wait_any_status[i]   = &ompi_status_empty;

        proc_names[i].jobid  = ORTE_JOBID_INVALID;
        proc_names[i].vpid   = ORTE_VPID_INVALID;
    }

    /*
     * Generate a list of messages to wait upon
     * No need to wait on those messages 'already_posted' since they will
     *    complete without our help.
     * Only wait on those messages that we have posted here.
     */
    wait_any_count = 0;
    last_proc_idx = 0;
    for(item  = opal_list_get_first(&drained_msg_list);
        item != opal_list_get_end(&drained_msg_list);
        item  = opal_list_get_next(item) ) {
        bool found = false;
        drain_msg = (ompi_crcp_coord_pml_message_ref_t*)item;

        /*
         * Create the array of requests to wait on
         */
        if( drain_msg->already_posted && NULL == drain_msg->request) {
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: wait_quiesce_drained: %s - %s Already posted this msg.\n",
                                ORTE_NAME_PRINT(orte_process_info.my_name),
                                ORTE_NAME_PRINT(&(drain_msg->proc_name)) );
        }
        else {
            opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: wait_quiesce_drained: %s - %s Waiting on message. (index = %d)\n",
                                ORTE_NAME_PRINT(orte_process_info.my_name),
                                ORTE_NAME_PRINT(&(drain_msg->proc_name)),
                                (int)wait_any_count);

            wait_any_requests[wait_any_count] = drain_msg->request;
            wait_any_status[wait_any_count]   = &drain_msg->status;
            wait_any_count++;
        }

        /*
         * Create a list of processes to send ACK responses to
         */
        /* Add proc to response queue if it is not already there */
        found = false;
        for(i = 0; i < last_proc_idx; ++i) {
            if(proc_names[i].jobid  == drain_msg->proc_name.jobid &&
               proc_names[i].vpid   == drain_msg->proc_name.vpid ) {
                found = true;
                break;
            }
        }
        if( !found ) {
            opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: wait_quiesce: %s - %s Add process to response list [idx %d]\n",
                                ORTE_NAME_PRINT(orte_process_info.my_name),
                                ORTE_NAME_PRINT(&(drain_msg->proc_name)),
                                (int)last_proc_idx);
                
            proc_names[last_proc_idx].jobid  = drain_msg->proc_name.jobid;
            proc_names[last_proc_idx].vpid   = drain_msg->proc_name.vpid;
            last_proc_idx++;
        }
    }

    /*
     * Wait on all of the message to complete in any order
     */
    prev_stall = opal_cr_stall_check;
    opal_cr_stall_check = true;
    if( OMPI_SUCCESS != (ret = coord_request_wait_all(wait_any_count,
                                                      wait_any_requests,
                                                      wait_any_status) ) ) {
        exit_status = ret;
        goto cleanup;
    }
    opal_cr_stall_check = prev_stall;
    
    /*
     * Send ACKs to all peers
     */
    opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: wait_quiesce: %s  Send ACKs to all Peers\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name));

    for(i = 0; i < last_proc_idx; ++i) {
        orte_buffer_t *buffer = NULL;
        size_t response = 1;
            
        /* Send All Clear to Peer */
        if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
            exit_status = OMPI_ERROR;
            goto cleanup;
        }

        PACK_BUFFER(buffer, response, 1, ORTE_SIZE, "");

        if ( 0 > ( ret = orte_rml.send_buffer(&(proc_names[i]), buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
            exit_status = ret;
            goto cleanup;
        }
        if( NULL != buffer) {
            OBJ_RELEASE(buffer);
            buffer = NULL;
        }
    }

    /*
     * Remove only the already posted members of the drained list.
     * All other elements need to be left in the list since we need
     * to match them as new receives come in.
     */
    for(item  = opal_list_get_first(&drained_msg_list);
        item != opal_list_get_end(&drained_msg_list);
        item  = next) {
        drain_msg = (ompi_crcp_coord_pml_message_ref_t*)item;
        next = opal_list_get_next(item);

        if( drain_msg->already_posted ) {
            opal_list_remove_item(&drained_msg_list, &(drain_msg->super) );
            OBJ_RELEASE(drain_msg);
        }
    }

 cleanup:
    if( NULL != wait_any_requests ) {
        free(wait_any_requests);
        wait_any_requests = NULL;
    }

    if( NULL != wait_any_status ) {
        free(wait_any_status);
        wait_any_status = NULL;
    }

    if( NULL != proc_names ) {
        free(proc_names);
        proc_names = NULL;
    }

    return exit_status;
}

static int coord_request_wait_all( size_t count,
                                   ompi_request_t ** requests,
                                   ompi_status_public_t ** statuses )
{
    int exit_status = OMPI_SUCCESS;
    ompi_status_public_t * status;
    ompi_request_t *req;
    size_t i;

    /*
     * Just wait on each request in order
     */
    for( i = 0; i < count; ++i) {
        req    = requests[i];
        status = statuses[i];

        coord_request_wait(req, status);

        opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: request_wait_all: %s  Done with idx %d of %d\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name),
                            (int)i, (int)count);
    }

    return exit_status;
}

static int coord_request_wait( ompi_request_t * req,
                               ompi_status_public_t * status)
{
    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_waiting++;
    while (req->req_complete == false) {
        opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
    }
    ompi_request_waiting--;
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    if( MPI_STATUS_IGNORE != status ) {
        status->MPI_TAG    = req->req_status.MPI_TAG;
        status->MPI_SOURCE = req->req_status.MPI_SOURCE;
        status->_count     = req->req_status._count;
        status->_cancelled = req->req_status._cancelled;
    }

    return OMPI_SUCCESS;
}

static int wait_quiesce_drain_ack(void)
{
    opal_list_item_t* item = NULL;
    opal_list_item_t* next = NULL;
    drain_msg_ack_ref_t * drain_msg_ack;
    int num_outstanding;

    num_outstanding = opal_list_get_size(&drained_msg_ack_list);
    if(num_outstanding <= 0) {
        /*  Nothing to do */
        return OMPI_SUCCESS;
    }

    opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: wait_quiesce_drain_ack: %s Waiting on %d Drain ACK messages\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        num_outstanding);

    while(0 < num_outstanding) {
        for(item  = opal_list_get_first(&drained_msg_ack_list);
            item != opal_list_get_end(&drained_msg_ack_list);
            item = next) {
            drain_msg_ack = (drain_msg_ack_ref_t*)item;
            next = opal_list_get_next(item);

            if(drain_msg_ack->complete) {
                num_outstanding--;
                opal_list_remove_item(&drained_msg_ack_list, &(drain_msg_ack->super) );
                break;
            }
        }

        opal_progress();
        opal_event_loop(OPAL_EVLOOP_NONBLOCK);
    }
        
    /* Clear the ack queue if it isn't already clear (it should already be) */
    while (NULL != (item = opal_list_remove_first(&drained_msg_ack_list) ) ) {
        OBJ_RELEASE(item);
    }

    return OMPI_SUCCESS;
}

/* Paired with recv_bookmarks */
static int send_bookmarks(int peer_idx)
{
    ompi_crcp_coord_pml_peer_ref_t *peer_ref;
    orte_process_name_t peer_name;
    orte_buffer_t *buffer = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    START_TIMER(CRCP_TIMER_CKPT_PEER_S);
    /*
     * Find the peer structure for this peer
     */
    peer_name.jobid  = orte_process_info.my_name->jobid;
    peer_name.vpid   = peer_idx;

    if( NULL == (peer_ref = find_peer(peer_name))) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: send_bookmarks: Could not find peer indexed %d\n",
                    peer_idx);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: send_bookmarks: %s -> %s Sending bookmark  S[%4d,%4d,%4d] R[%4d,%4d,%4d]\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(&peer_name),
                        peer_ref->total_send_msgs,
                        peer_ref->total_isend_msgs,
                        peer_ref->total_send_init_msgs,
                        peer_ref->total_recv_msgs,
                        peer_ref->total_irecv_msgs,
                        peer_ref->total_recv_init_msgs);

    /*
     * Send the bookmarks to peer
     */
    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    PACK_BUFFER(buffer, (peer_ref->total_send_msgs),      1, ORTE_UINT32,
                "crcp:coord: send_bookmarks: Unable to pack total_send_msgs");
    PACK_BUFFER(buffer, (peer_ref->total_isend_msgs),     1, ORTE_UINT32,
                "crcp:coord: send_bookmarks: Unable to pack total_isend_msgs");
    PACK_BUFFER(buffer, (peer_ref->total_send_init_msgs), 1, ORTE_UINT32,
                "crcp:coord: send_bookmarks: Unable to pack total_send_init_msgs");

    PACK_BUFFER(buffer, (peer_ref->total_recv_msgs),      1, ORTE_UINT32,
                "crcp:coord: send_bookmarks: Unable to pack total_recv_msgs");
    PACK_BUFFER(buffer, (peer_ref->total_irecv_msgs),     1, ORTE_UINT32,
                "crcp:coord: send_bookmarks: Unable to pack total_irecv_msgs");
    PACK_BUFFER(buffer, (peer_ref->total_recv_init_msgs), 1, ORTE_UINT32,
                "crcp:coord: send_bookmarks: Unable to pack total_recv_init_msgs");

    if ( 0 > ( ret = orte_rml.send_buffer(&peer_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: send_bookmarks: Failed to send bookmark to peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_name),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if(NULL != buffer) {
        OBJ_RELEASE(buffer);
    }

    END_TIMER(CRCP_TIMER_CKPT_PEER_S);
    DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_PEER_S, peer_idx);

    return exit_status;
}

/* JJH -- Blocking vs Non-Blocking Bookmark exchange */
#if 1
/* Paired with send_bookmarks */
static int recv_bookmarks(int peer_idx)
{
    orte_process_name_t peer_name;
    int exit_status = OMPI_SUCCESS;
    int ret;

    START_TIMER(CRCP_TIMER_CKPT_PEER_R);

    peer_name.jobid  = orte_process_info.my_name->jobid;
    peer_name.vpid   = peer_idx;

    if ( 0 > (ret = orte_rml.recv_buffer_nb(&peer_name,
                                            OMPI_CRCP_COORD_BOOKMARK_TAG,
                                            0,
                                            recv_bookmarks_cbfunc,
                                            NULL) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_bookmarks: Failed to post receive bookmark from peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_name),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    ++total_recv_bookmarks;

 cleanup:
    END_TIMER(CRCP_TIMER_CKPT_PEER_R);
    DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_PEER_R, peer_idx);

    return exit_status;
}
#else
/* Paired with send_bookmarks */
static int recv_bookmarks(int peer_idx)
{
    ompi_crcp_coord_pml_peer_ref_t *peer_ref;
    orte_process_name_t peer_name;
    orte_buffer_t * buffer = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret, tmp_int;

    START_TIMER(CRCP_TIMER_CKPT_PEER_R);

    /*
     * Find the peer structure for this peer
     */
    peer_name.jobid  = orte_process_info.my_name->jobid;
    peer_name.vpid   = peer_idx;

    if( NULL == (peer_ref = find_peer(peer_name))) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_bookmarks: Could not find peer indexed %d\n",
                    peer_idx);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    /*
     * Receive the bookmark from peer
     */
    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if ( 0 > (ret = orte_rml.recv_buffer(&peer_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0) ) , 0) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_bookmarks: Failed to receive bookmark from peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_name),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_send_msgs");
    peer_ref->matched_send_msgs = tmp_int;
    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_isend_msgs");
    peer_ref->matched_isend_msgs = tmp_int;
    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_send_init_msgs");
    peer_ref->matched_send_init_msgs = tmp_int;

    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_recv_msgs");
    peer_ref->matched_recv_msgs = tmp_int;
    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_irecv_msgs");
    peer_ref->matched_irecv_msgs = tmp_int;
    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_recv_init_msgs");
    peer_ref->matched_recv_init_msgs = tmp_int;

    opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_bookmarks: %s <- %s Received bookmark S[%4d,%4d,%4d] R[%4d,%4d,%4d]\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(&peer_name),
                        peer_ref->matched_send_msgs,
                        peer_ref->matched_isend_msgs,
                        peer_ref->matched_send_init_msgs,
                        peer_ref->matched_recv_msgs,
                        peer_ref->matched_irecv_msgs,
                        peer_ref->matched_recv_init_msgs);

 cleanup:
    if(NULL != buffer) {
        OBJ_RELEASE(buffer);
    }

    END_TIMER(CRCP_TIMER_CKPT_PEER_R);
    DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_PEER_R, peer_idx);

    return exit_status;
}
#endif

static void recv_bookmarks_cbfunc(int status,
                                  orte_process_name_t* sender,
                                  orte_buffer_t *buffer,
                                  orte_rml_tag_t tag,
                                  void* cbdata)
{
    ompi_crcp_coord_pml_peer_ref_t *peer_ref;
    int exit_status = OMPI_SUCCESS;
    int ret, tmp_int;
    orte_vpid_t peer_idx;

    START_TIMER(CRCP_TIMER_CKPT_PEER_R);

    peer_idx = sender->vpid;

    /*
     * Find the peer structure for this peer
     */
    if( NULL == (peer_ref = find_peer(*sender))) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_bookmarks: Could not find peer indexed %d\n",
                    peer_idx);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_send_msgs");
    peer_ref->matched_send_msgs = tmp_int;
    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_isend_msgs");
    peer_ref->matched_isend_msgs = tmp_int;
    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_send_init_msgs");
    peer_ref->matched_send_init_msgs = tmp_int;

    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_recv_msgs");
    peer_ref->matched_recv_msgs = tmp_int;
    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_irecv_msgs");
    peer_ref->matched_irecv_msgs = tmp_int;
    UNPACK_BUFFER(buffer, tmp_int, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_recv_init_msgs");
    peer_ref->matched_recv_init_msgs = tmp_int;

    opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_bookmarks: %s <- %s Received bookmark S[%4d,%4d,%4d] R[%4d,%4d,%4d]\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(sender),
                        peer_ref->matched_send_msgs,
                        peer_ref->matched_isend_msgs,
                        peer_ref->matched_send_init_msgs,
                        peer_ref->matched_recv_msgs,
                        peer_ref->matched_irecv_msgs,
                        peer_ref->matched_recv_init_msgs);

 cleanup:
    END_TIMER(CRCP_TIMER_CKPT_PEER_R);
    DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_PEER_R, peer_idx);
    --total_recv_bookmarks;

    return;
}

static int send_msg_details(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                            int total_sent, int total_matched)
{
    drain_msg_ack_ref_t * d_msg_ack = NULL;
    opal_list_t *search_list = NULL;
    opal_list_item_t* msg_item  = NULL;
    bool found_match, finished;
    int pass_num = 1;
    int need, found;
    int exit_status = OMPI_SUCCESS;
    int ret;

    need = total_sent - total_matched;
    found = 0;
    finished = false;
    assert(need > 0);

    /*
     * Check the 'send_list' for this peer
     */
    search_list = &(peer_ref->send_list);
    pass_num = 1;

 SEARCH_AGAIN:
    for(msg_item  = opal_list_get_last(search_list);
        msg_item != opal_list_get_begin(search_list);
        msg_item  = opal_list_get_prev(msg_item) ) {
        ompi_crcp_coord_pml_message_ref_t * msg_ref;
        msg_ref = (ompi_crcp_coord_pml_message_ref_t*)msg_item;

        /* If this message has been matched, then skip it */
        if( msg_ref->matched ) {
            continue;
        }

        found_match = false;
        if(OMPI_SUCCESS != (ret = do_send_msg_detail(peer_ref, msg_ref, &found_match, &finished)) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: send_msg_details: %s --> %s Failed to send message details to peer. Return %d\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                        ret);
        }
        if(found_match) {
            found++;
        }
        if(finished) {
            goto ALL_SENT;
        }
    }

    /*
     * We tried the 'send_list' and need more,
     * so match off the 'isend_list'
     */
    if( 1 == pass_num ) {
        search_list = &(peer_ref->isend_list);
        pass_num = 2;
        goto SEARCH_AGAIN;
    }

    /*
     * We tried the 'send_list' and 'isend_list' and need more,
     * so match off the 'send_init_list'
     */
    if( 2 == pass_num ) {
        search_list = &(peer_ref->send_init_list);
        pass_num = 3;
        goto SEARCH_AGAIN;
    }

 ALL_SENT:
    if( need > found ) {
        opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: send_msg_details: ERROR: ****** Need (%d) vs Found (%d)",
                            need, found);
    }
    assert(need <= found);

    /* Prepare to post a Recv for the ACK All Clear signal from the peer
     * which is sent when they have finished receiving all of the 
     * inflight messages into a local buffer
     */
    d_msg_ack = OBJ_NEW(drain_msg_ack_ref_t);
    d_msg_ack->peer.jobid  = peer_ref->proc_name.jobid;
    d_msg_ack->peer.vpid   = peer_ref->proc_name.vpid;
    d_msg_ack->complete    = false;
    opal_list_append(&drained_msg_ack_list, &(d_msg_ack->super));
    opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: send_msg_details: %s <--> %s Will wait on ACK from this peer.\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)));

    /*
     * If we know that we are in the middle of a blocking send/recv then we
     * need to stall the coordination algorithm while we wait for this to 
     * complete.
     */
    if( 0 < current_msg_id && 
        COORD_MSG_TYPE_B_SEND == current_msg_type) {
        stall_for_completion = true;
    }

    return exit_status;
}

static int do_send_msg_detail(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                              ompi_crcp_coord_pml_message_ref_t*msg_ref,
                              bool *found_match,
                              bool *finished)
{
    orte_buffer_t *buffer = NULL;
    int32_t req_more = -1;
    int comm_my_rank = -1;
    int exit_status = OMPI_SUCCESS;
    int ret;

    *found_match = false;
    *finished    = false;

    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    /*
     * Send:
     * - Communicator Context ID
     * - My Rank in Communicator
     */
    comm_my_rank  = ompi_comm_rank(msg_ref->comm);

    PACK_BUFFER(buffer, msg_ref->comm->c_contextid, 1, ORTE_UINT32,
                "crcp:coord: send_msg_details: Unable to pack communicator ID");
    PACK_BUFFER(buffer, comm_my_rank, 1, ORTE_INT,
                "crcp:coord: send_msg_details: Unable to pack comm rank ID");

    /*
     * Send:
     * - Message tag
     * - Message count
     * - Message Datatype size
     */
    PACK_BUFFER(buffer, msg_ref->tag, 1, ORTE_INT,
                "crcp:coord: send_msg_details: Unable to pack tag");
    PACK_BUFFER(buffer, msg_ref->count, 1, ORTE_SIZE,
                "crcp:coord: send_msg_details: Unable to pack count");
    PACK_BUFFER(buffer, msg_ref->ddt_size, 1, ORTE_SIZE,
                "crcp:coord: send_msg_details: Unable to pack datatype size");

    /*
     * Do the send...
     */
    if ( 0 > ( ret = orte_rml.send_buffer(&peer_ref->proc_name, buffer,
                                          OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: do_send_msg_detail: Unable to send message details to peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_ref->proc_name),
                    ret);
            
        exit_status = OMPI_ERROR;
        goto cleanup;
    }
        
    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }
            
    /*
     * Check return value from peer to see if we found a match.
     */
    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
        
    /*
     * Recv the ACK msg
     */
    if ( 0 > (ret = orte_rml.recv_buffer(&peer_ref->proc_name, buffer,
                                         OMPI_CRCP_COORD_BOOKMARK_TAG, 0) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: do_send_msg_detail: %s --> %s Failed to receive ACK buffer from peer. Return %d\n",
                    ORTE_NAME_PRINT(orte_process_info.my_name),
                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    UNPACK_BUFFER(buffer, req_more, 1, ORTE_UINT32,
                  "crcp:coord: send_msg_details: Failed to unpack the ACK from peer buffer.");
    
    /* Mark message as matched */
    msg_ref->matched = true;

    /*
     * -1 = Accounted for this message
     *  0 = Found a message to drain
     *  1 = All finished.
     */
    if( -1 >= req_more ) {
        *found_match = false;
        *finished    = false;
    }
    else if( 1 <= req_more ) {
        *found_match = true;
        *finished    = true;
    }
    else {
        *found_match = true;
        *finished    = false;
    }

 cleanup:
    return exit_status;
}

/*
 * Recv message details from peer
 * - matched with send_msg_details()
 */
static int recv_msg_details(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                            int total_recv, int total_matched)
{
    int need, found;
    bool found_match;
    int response;
    int exit_status = OMPI_SUCCESS;
    int ret;

    need = total_recv - total_matched;
    found = 0;
    assert(need > 0);

    /*
     * While we are still looking for messages to drain
     */
    while(need > found) {
        uint32_t p_comm_id;
        size_t p_count;
        size_t p_datatype_size;
        int p_rank;
        int p_tag;

        /*
         * Receive the message details from peer
         */
        if( OMPI_SUCCESS != (ret = do_recv_msg_detail(peer_ref,
                                                      &p_rank, &p_comm_id,
                                                      &p_tag, &p_count,
                                                      &p_datatype_size)) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_msg_details: %s <-- %s "
                        "Failed to receive message detail from peer. Return %d\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                        ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Determine if we have matched this message or not.
         * Also take approprate action.
         */
        found_match = false;
        if( OMPI_SUCCESS != (ret = do_recv_msg_detail_check(peer_ref,
                                                            p_rank, p_comm_id,
                                                            p_tag, p_count,
                                                            p_datatype_size,
                                                            &found_match) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_msg_details: %s <-- %s "
                        "Failed to check message detail from peer. Return %d\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                        ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Respond to peer regarding the message detail they have sent.
         */
        if( found_match ) {
            found++;
        }

        if( need <= found ) {
            response = 1; /* All done */
        }
        else if(found_match) {
            response = 0; /* Found a match */
        }
        else {
            response = -1; /* Accounted for this message */
        }

        if(OMPI_SUCCESS != (ret = do_recv_msg_detail_resp(peer_ref, response))) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_msg_details: %s <-- %s Failed to respond to peer. Return %d\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                        ret);
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:

    return exit_status;
}

static int do_recv_msg_detail(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                              int *rank, uint32_t *comm_id, int *tag,
                              size_t *count, size_t *datatype_size)
{
    orte_buffer_t * buffer = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * Recv the msg
     */
    if ( 0 > (ret = orte_rml.recv_buffer(&peer_ref->proc_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: do_recv_msg_detail: %s <-- %s Failed to receive buffer from peer. Return %d\n",
                    ORTE_NAME_PRINT(orte_process_info.my_name),
                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    /* Pull out the communicator ID */
    UNPACK_BUFFER(buffer, (*comm_id), 1, ORTE_UINT32,
                  "crcp:coord: recv_msg_details: Failed to unpack the communicator ID");
    UNPACK_BUFFER(buffer, (*rank), 1, ORTE_INT,
                  "crcp:coord: recv_msg_details: Failed to unpack the communicator rank ID");
    
    /* Pull out the message details */
    UNPACK_BUFFER(buffer, (*tag), 1, ORTE_INT,
                  "crcp:coord: recv_msg_details: Failed to unpack the tag");
    UNPACK_BUFFER(buffer, (*count), 1, ORTE_SIZE,
                  "crcp:coord: recv_msg_details: Failed to unpack the count");
    UNPACK_BUFFER(buffer, (*datatype_size), 1, ORTE_SIZE,
                  "crcp:coord: recv_msg_details: Failed to unpack the datatype size");

 cleanup:
    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    return exit_status;
}

static int do_recv_msg_detail_check(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                                    int rank, uint32_t comm_id, int tag,
                                    size_t count, size_t datatype_size,
                                    bool *found_match)
{
    ompi_crcp_coord_pml_message_ref_t *posted_msg_ref = NULL;
    opal_list_t *found_on_this_list = NULL;
    bool msg_found, msg_complete, msg_already_posted;
    int exit_status = OMPI_SUCCESS;
    int ret;

    *found_match       = false;
    msg_found          = false;
    msg_complete       = false;
    msg_already_posted = false;

    /*
     * Have we received this message?
     *  - Yes: Mark as matched, continue looking
     *  - No : Put the message on the to be drained list, notify peer
     *  - In progress: This receive is currently in the process of being received, stall.
     *                 Will only reach this state inside a blocking recv.
     */
    ret = have_received_msg(peer_ref, rank, comm_id, tag, count, datatype_size,
                            &posted_msg_ref, &found_on_this_list,
                            &msg_found,           /* Did we find a matching recv for this message? */
                            &msg_complete,        /* Is the recv of the message already finished? */
                            &msg_already_posted); /* Has the recv already been posted? */
    if( OMPI_SUCCESS != ret) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_msg_detail_check: %s -- %s "
                    "Failed to determine if we have received this message. Return %d\n",
                    ORTE_NAME_PRINT(orte_process_info.my_name),
                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_msg_detail_check: %s -- %s"
                        " found %s, complete %s, posted %s, peer_rank=[%d vs %d]\n",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                        (true == msg_found ? "True " : "False"), 
                        (true == msg_complete ? "True " : "False"), 
                        (true == msg_already_posted ? "True " : "False"), 
                        (NULL == posted_msg_ref ? -1 : posted_msg_ref->rank),
                        peer_ref->proc_name.vpid);

    /*
     * The message was not found
     *  - Create a new message to drain
     *  - Notify peer of resolution of one message
     */
    if( !msg_found ) {
        ompi_crcp_coord_pml_message_ref_t *d_msg = NULL;

        opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: recv_msg_detail_check: %s Found a message that needs to be drained\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name) );

        /*
         * Construct a message for draining
         */
        CREATE_NEW_MSG(d_msg, COORD_MSG_TYPE_I_RECV,
                       NULL,
                       0, NULL, /* Setup the datatype outside of this */
                       tag, rank, ompi_comm_lookup(comm_id),
                       NULL,
                       peer_ref->proc_name.jobid,
                       peer_ref->proc_name.vpid);
        /*
         * Put the true count here so we can properly match the drain.
         * The post_drained() will properly handle the packed datatype
         * by changing the count to (count * ddt_size).
         */
        d_msg->count     = count;
        ompi_ddt_duplicate(&ompi_mpi_packed, &(d_msg->datatype));
        d_msg->ddt_size  = datatype_size;

        d_msg->matched        = true;
        d_msg->done           = false;
        d_msg->active         = false;
        d_msg->already_posted = false;

        /* Create a buffer of the necessary type/size */
        if(d_msg->count > 0 ) {
            d_msg->buffer = (void *) malloc(d_msg->count * d_msg->ddt_size);
        }
        else {
            /* JJH - Check 0 Byte message */
            d_msg->buffer = (void *) malloc(1 * d_msg->ddt_size);
        }

        /* Save it so we can post and poll on it later */
        opal_list_append(&drained_msg_list, &(d_msg->super));
        peer_ref->total_drained_msgs += 1;

        *found_match = true;
    }
    /*
     * The message was found, and completed. Do nothing.
     */
    else if( msg_complete ) {
        ; /* Do nothing... */
    }
    /*
     * The message was found, not complete, and already posted. (irecv)
     * - Create a drain message
     * - Point the 'request' at it
     * - Make sure not to post this message to be drained, but just wait on the request.
     */
    else if( msg_already_posted ) {
        ompi_crcp_coord_pml_message_ref_t *d_msg = NULL;

        opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: recv_msg_detail_check: %s "
                            "Found a message already posted! Prepare to drain.\n",
                            ORTE_NAME_PRINT(orte_process_info.my_name));

        /*
         * If this is the current blocking recv,
         * we need to stall so it can complete properly.
         */
        if( current_msg_id         == posted_msg_ref->msg_id  &&
            COORD_MSG_TYPE_B_RECV  == posted_msg_ref->msg_type) {
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv_msg_detail_check: %s "
                                "Found a message already posted! Prepare to STALL.\n",
                                ORTE_NAME_PRINT(orte_process_info.my_name));
            stall_for_completion = true;
        }
        /*
         * Only increment the counter if we do not know it is the 
         * current blocking recv
         */
        else {
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv_msg_detail_check: %s "
                                "Found a message already posted! No stall required [%3d, %3d, %3d, %3d].\n",
                                ORTE_NAME_PRINT(orte_process_info.my_name),
                                (int)current_msg_id, 
                                (int)current_msg_type, 
                                (int)posted_msg_ref->msg_id,
                                (int)posted_msg_ref->msg_type);
            ; /* JJH CHECK - peer_ref->total_drained_msgs += 1; */
        }

        /*
         * If the message has been posted, but not from this peer.
         * This means that the message was matched off the unknown_list and 
         * src = MPI_ANY_SOURCE. Since we need to make sure we drain the 
         * message from this peer.
         *  - Add the suggested peer flag to match this peer
         *  - Make sure we wait for this message to complete when draining 
         *    messages.
         * JJH -- When do we use this?
         */
        if (posted_msg_ref->rank != peer_ref->proc_name.vpid) {
            posted_msg_ref->suggested_rank = rank;
        }

        /*
         * Construct a message for draining
         * This message will *not* be posted for draining since it is already
         * posted in the system. We will simply wait for it to complete.
         */
        CREATE_NEW_MSG(d_msg, COORD_MSG_TYPE_I_RECV,
                       NULL,
                       count, NULL,
                       tag, rank, ompi_comm_lookup(comm_id),
                       posted_msg_ref->request,
                       peer_ref->proc_name.jobid,
                       peer_ref->proc_name.vpid);

        d_msg->matched        = true;
        d_msg->done           = false;
        d_msg->active         = true;
        d_msg->already_posted = true;

        /* Save it so we can post and poll on it later */
        opal_list_append(&drained_msg_list, &(d_msg->super));

        *found_match = true;
    }
    else {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_msg_detail_check: ***** ERROR ***** %s Failed to find an action to use. This should never happen!\n",
                    ORTE_NAME_PRINT(orte_process_info.my_name));
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int have_received_msg(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                             int rank, uint32_t comm_id, int tag,
                             size_t count, size_t datatype_size,
                             ompi_crcp_coord_pml_message_ref_t ** found_msg_ref,
                             opal_list_t **found_on_this_list,
                             bool *found, bool *complete, bool *already_posted)
{
    int ret;

    *found_on_this_list = NULL;
    *found_msg_ref      = NULL;
    *found              = false;
    *complete           = false;
    *already_posted     = false;

    /*
     * Check the recv_list
     */
    if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->recv_list),
                                                  count, tag, INVALID_INT,
                                                  datatype_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE,   /* Matched?        */
                                                  FIND_MSG_UNKNOWN, /* Done?           */
                                                  FIND_MSG_UNKNOWN, /* Active?         */
                                                  FIND_MSG_TRUE     /* Already Posted? */
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }
    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(peer_ref->recv_list);
        goto FOUND;
    }

    /*
     * Check the irecv_list
     */
    if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->irecv_list),
                                                  count, tag, INVALID_INT,
                                                  datatype_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE,   /* Matched?        */
                                                  FIND_MSG_UNKNOWN, /* Done?           */
                                                  FIND_MSG_UNKNOWN, /* Active?         */
                                                  FIND_MSG_TRUE     /* Already Posted? */
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }
    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(peer_ref->irecv_list);
        goto FOUND;
    }

    /*
     * Check the recv_init_list
     * 1) (done = true , active = false)
     * 2) (done = false, active = true)
     */
    if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->recv_init_list),
                                                  count, tag, INVALID_INT,
                                                  datatype_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE,   /* Matched?        */
                                                  FIND_MSG_TRUE,    /* Done?           */
                                                  FIND_MSG_FALSE,   /* Active?         */
                                                  FIND_MSG_TRUE     /* Already Posted? */
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }
    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(peer_ref->recv_init_list);
        goto FOUND;
    }

    if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->recv_init_list),
                                                  count, tag, INVALID_INT,
                                                  datatype_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE,   /* Matched?        */
                                                  FIND_MSG_FALSE,   /* Done?           */
                                                  FIND_MSG_TRUE,    /* Active?         */
                                                  FIND_MSG_TRUE     /* Already Posted? */
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }
    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(peer_ref->recv_init_list);
        goto FOUND;
    }

    /*
     * Check the unknown list of non-persistant
     */
    if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_recv_from_list),
                                                  count, tag, INVALID_INT,
                                                  datatype_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE,   /* Matched?        */
                                                  FIND_MSG_UNKNOWN, /* Done?           */
                                                  FIND_MSG_UNKNOWN, /* Active?         */
                                                  FIND_MSG_TRUE     /* Already Posted? */
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }
    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(unknown_recv_from_list);
        goto FOUND;
    }

    /*
     * Check the unknown list of persistant
     * 1) (done = true , active = false)
     * 2) (done = false, active = true)
     */
    if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_persist_recv_list),
                                                  count, tag, INVALID_INT,
                                                  datatype_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE,   /* Matched?        */
                                                  FIND_MSG_TRUE,    /* Done?           */
                                                  FIND_MSG_FALSE,   /* Active?         */
                                                  FIND_MSG_TRUE     /* Already Posted? */
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }
    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(unknown_persist_recv_list);
        goto FOUND;
    }

    if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_persist_recv_list),
                                                  count, tag, INVALID_INT,
                                                  datatype_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE,   /* Matched?        */
                                                  FIND_MSG_FALSE,   /* Done?           */
                                                  FIND_MSG_TRUE,    /* Active?         */
                                                  FIND_MSG_TRUE     /* Already Posted? */
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }
    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(unknown_persist_recv_list);
        goto FOUND;
    }

    /*
     * JJH -- Should we check the drained list?
     * If we checkpoint again before dimishing the drained list, then
     * the peer could be requesting that a drained send complete...
     */

    /* Message was not found. */

 FOUND:
    /* If we found a matching message */
    if( NULL != *found_msg_ref) {
        (*found_msg_ref)->matched = true;
        *found          = true;
        *complete       = (*found_msg_ref)->done;
        *already_posted = (*found_msg_ref)->already_posted;
    }
    else {
        *found_on_this_list = NULL;
        *found_msg_ref      = NULL;
        *found              = false;
        *complete           = false;
        *already_posted     = false;
    }

    return OMPI_SUCCESS;
}

static int find_message_named(opal_list_t * search_list,
                              size_t count, int tag, int peer,                              
                              size_t ddt_size,
                              ompi_crcp_coord_pml_message_ref_t ** found_msg_ref,
                              int matched, int done, int active, int already_posted)
{
    opal_list_item_t* item = NULL;

    *found_msg_ref = NULL;

    /*
     * Dummy checks
     */
    if( NULL == search_list) {
        return OMPI_ERROR;
    }
    if( 0 >= opal_list_get_size(search_list) ) {
        return OMPI_SUCCESS;
    }

    /*
     * Check the search list
     */
    for(item  = opal_list_get_last(search_list);
        item != opal_list_get_begin(search_list);
        item  = opal_list_get_prev(item) ) {

        ompi_crcp_coord_pml_message_ref_t * msg_ref;
        msg_ref = (ompi_crcp_coord_pml_message_ref_t*)item;

        /* Check constraints */
        if( matched != FIND_MSG_UNKNOWN ) {
            if( (matched == FIND_MSG_TRUE  && msg_ref->matched == false) ||
                (matched == FIND_MSG_FALSE && msg_ref->matched == true) ) {
                continue;
            }
        }
        if( done != FIND_MSG_UNKNOWN ) {
            if( (done == FIND_MSG_TRUE  && msg_ref->done == false) ||
                (done == FIND_MSG_FALSE && msg_ref->done == true) ) {
                continue;
            }
        }
        if( active != FIND_MSG_UNKNOWN ) {
            if( (active == FIND_MSG_TRUE  && msg_ref->active == false) ||
                (active == FIND_MSG_FALSE && msg_ref->active == true) ) {
                continue;
            }
        }
        if( already_posted != FIND_MSG_UNKNOWN ) {
            if( (already_posted == FIND_MSG_TRUE  && msg_ref->already_posted == false) ||
                (already_posted == FIND_MSG_FALSE && msg_ref->already_posted == true) ) {
                continue;
            }
        }

        if(msg_ref->count == count  &&
           (msg_ref->tag  == MPI_ANY_TAG || msg_ref->tag  == tag)   &&
           (peer          == INVALID_INT || msg_ref->rank == peer)  &&
           msg_ref->ddt_size == ddt_size) {

            opal_output_verbose(30, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: find_message_named: Found Message -- Comm list (%d, %d)\n",
                                tag, peer);

            *found_msg_ref = msg_ref;
            return OMPI_SUCCESS;
        }
    }

    return OMPI_SUCCESS;
}

static int do_recv_msg_detail_resp(ompi_crcp_coord_pml_peer_ref_t *peer_ref,
                                   int resp)
{
    orte_buffer_t * buffer = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    PACK_BUFFER(buffer, resp, 1, ORTE_UINT32,
                "crcp:coord: recv_msg_details: Unable to ask peer for more messages");
        
    if ( 0 > ( ret = orte_rml.send_buffer(&peer_ref->proc_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_msg_detail_resp: Unable to send message detail response to peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_ref->proc_name),
                    ret);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    return exit_status;
}

/* Staggered alltoall */
static int coord_basic_barrier(void)
{
    int peer_idx  = 0;
    int my_idx    = orte_process_info.my_name->vpid;
    int iter      = 0;
    int num_peers = 0;
    
    num_peers = opal_list_get_size(&ompi_crcp_coord_pml_peer_refs);

    for( peer_idx = (num_peers - my_idx - 1), iter = 0;
         iter < num_peers;
         peer_idx = (peer_idx + 1) % num_peers, ++iter) 
        {
            if(my_idx > peer_idx) {
                /* Send our bookmark status */
                coord_basic_barrier_send(peer_idx);
                /* Recv peer bookmark status */
                coord_basic_barrier_recv(peer_idx);
            }
            else if(my_idx < peer_idx) {
                /* Recv peer bookmark status */
                coord_basic_barrier_recv(peer_idx);
                /* Send our bookmark status */
                coord_basic_barrier_send(peer_idx);
            }
        }

    return OMPI_SUCCESS;
}

/* Paired with coord_basic_barrier_recv */
static int coord_basic_barrier_send(int peer_idx)
{
    orte_process_name_t peer_name;
    orte_buffer_t *buffer = NULL;
    int value = 1234;
    int exit_status = OMPI_SUCCESS;
    int ret;

    /*
     * Find the peer structure for this peer
     */
    peer_name.jobid  = orte_process_info.my_name->jobid;
    peer_name.vpid   = peer_idx;

    /*
     * Send the bookmarks to peer
     */
    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    PACK_BUFFER(buffer, (value),      1, ORTE_UINT32,
                "crcp:coord: coord_basic_barrier_send: Unable to pack ACK");

    /* JJH -- Really Establish TAG in rml_types.h */
    if ( 0 > ( ret = orte_rml.send_buffer(&peer_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG+1, 0)) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: coord_basic_barrier_send: Failed to send ACK to peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_name),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if(NULL != buffer) {
        OBJ_RELEASE(buffer);
    }

    return exit_status;
}

/* Paired with coord_basic_barrier_send */
static int coord_basic_barrier_recv(int peer_idx)
{
    orte_process_name_t peer_name;
    orte_buffer_t * buffer = NULL;
    int value = 0;
    int exit_status = OMPI_SUCCESS;
    int ret;

    /*
     * Find the peer structure for this peer
     */
    peer_name.jobid  = orte_process_info.my_name->jobid;
    peer_name.vpid   = peer_idx;

    /*
     * Receive the bookmark from peer
     */
    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if ( 0 > (ret = orte_rml.recv_buffer(&peer_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG+1, 0) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_bookmarks: Failed to receive bookmark from peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_name),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    UNPACK_BUFFER(buffer, value, 1, ORTE_UINT32,
                  "crcp:coord: recv_bookmarks: Unable to unpack total_send_msgs");

 cleanup:
    if(NULL != buffer) {
        OBJ_RELEASE(buffer);
    }

    return exit_status;
}

/**************** Timing functionality ********************/
static void start_time(int idx) {
    if(idx < CRCP_TIMER_MAX ) {
        timer_start[idx] = get_time();
    }
}

static void end_time(int idx) {
    if(idx < CRCP_TIMER_MAX ) {
        timer_end[idx] = get_time();
    }
}

static double get_time() {
    double wtime;

#if OPAL_TIMER_USEC_NATIVE
    wtime = (double)opal_timer_base_get_usec() / 1000000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif

    return wtime;
}

static void clear_timers(void) {
    int i;
    for(i = 0; i < CRCP_TIMER_MAX; ++i) {
        timer_start[i] = 0.0;
        timer_end[i]   = 0.0;
    }
}

static void display_all_timers(int state) {
    int i;

    for( i = 0; i <= CRCP_TIMER_CONT; ++i) {
        display_indv_timer(i, state);
    }
}

static void display_indv_timer(int idx, int var) {
    double diff = timer_end[idx] - timer_start[idx];

    if( 0 != orte_process_info.my_name->vpid ) {
        return;
    }

    if( diff < 0.001 ) {
        return;
    }
    opal_output(0,
                "crcp:coord: timing(%3d): %15s = %10.2f [%15.2f - %15.2f]\n",
                var,
                timer_label[idx],
                diff,
                timer_end[idx],
                timer_start[idx]);
}
