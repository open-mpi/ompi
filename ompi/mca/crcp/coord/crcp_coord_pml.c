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
#include "ompi/mca/pml/base/pml_base_module_exchange.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"

#include "crcp_coord.h"
#include "crcp_coord_pml.h"

/*
 * TEMP DEBUG stmt
 */
#define JJH_DEBUG 1

opal_list_t unknown_recv_from_list;
opal_list_t unknown_persist_recv_list;
opal_list_t drained_msg_ack_list;
opal_list_t drained_msg_list;
opal_list_t ompi_crcp_coord_pml_procs;

/*
 * Convenience References
 */
static mca_pml_base_component_t  *wrapped_pml_component = NULL;
static mca_pml_base_module_t     *wrapped_pml_module    = NULL;
static uint64_t message_seq_num = 0;
static bool stall_for_completion;

#define INVALID_INT -123456789

/************************************
 * Locally Global vars & functions :)
 ************************************/
OBJ_CLASS_INSTANCE(ompi_crcp_coord_pml_message_ref_t,
                   opal_list_item_t,
                   ompi_crcp_coord_pml_message_ref_construct,
                   ompi_crcp_coord_pml_message_ref_destruct);

void ompi_crcp_coord_pml_message_ref_construct(ompi_crcp_coord_pml_message_ref_t *msg_ref) {
    msg_ref->msg_id     = 0;
    msg_ref->buffer     = NULL;
    msg_ref->count      = 0;

    msg_ref->datatype   = NULL;
    msg_ref->ddt_size   = 0;

    msg_ref->tag        = 0;
    msg_ref->rank       = 0;
    msg_ref->comm       = NULL;
    msg_ref->mode       = MCA_PML_BASE_SEND_STANDARD;
    msg_ref->async      = false;
    msg_ref->request    = NULL;

    msg_ref->proc_name.cellid = ORTE_CELLID_INVALID;
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

    if( NULL != msg_ref->buffer ) {
        free(msg_ref->buffer);
    }
    msg_ref->buffer     = NULL;
    msg_ref->count      = 0;

    if( NULL != msg_ref->datatype ) {
        OBJ_RELEASE(msg_ref->datatype);
    }
    msg_ref->datatype   = NULL;
    msg_ref->ddt_size   = 0;

    msg_ref->tag        = 0;
    msg_ref->rank       = 0;
    msg_ref->comm       = NULL;
    msg_ref->mode       = MCA_PML_BASE_SEND_STANDARD;
    msg_ref->async      = false;
    if( NULL != msg_ref->request ) {
        OBJ_RELEASE(msg_ref->request);
    }
    msg_ref->request    = NULL;

    msg_ref->proc_name.cellid = ORTE_CELLID_INVALID;
    msg_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    msg_ref->proc_name.vpid   = ORTE_VPID_INVALID;

    msg_ref->matched        = false;
    msg_ref->done           = false;
    msg_ref->active         = false;
    msg_ref->already_posted = false;

    msg_ref->suggested_rank = INVALID_INT;
}

OBJ_CLASS_INSTANCE(ompi_crcp_coord_pml_bookmark_proc_t,
                   opal_list_item_t,
                   ompi_crcp_coord_pml_bookmark_proc_construct,
                   ompi_crcp_coord_pml_bookmark_proc_destruct);

void ompi_crcp_coord_pml_bookmark_proc_construct(ompi_crcp_coord_pml_bookmark_proc_t *bkm_proc) {
    bkm_proc->proc_name.cellid = 0;
    bkm_proc->proc_name.jobid  = 0;
    bkm_proc->proc_name.vpid   = 0;

    OBJ_CONSTRUCT(&(bkm_proc->lock), opal_mutex_t);
    OBJ_CONSTRUCT(&(bkm_proc->cond), opal_condition_t);

    OBJ_CONSTRUCT(&bkm_proc->send_list,       opal_list_t);
    OBJ_CONSTRUCT(&bkm_proc->isend_list,      opal_list_t);
    OBJ_CONSTRUCT(&bkm_proc->send_init_list,  opal_list_t);

    OBJ_CONSTRUCT(&bkm_proc->recv_list,       opal_list_t);
    OBJ_CONSTRUCT(&bkm_proc->irecv_list,      opal_list_t);
    OBJ_CONSTRUCT(&bkm_proc->recv_init_list,  opal_list_t);

    bkm_proc->total_send_msgs           = 0;
    bkm_proc->total_isend_msgs          = 0;
    bkm_proc->total_send_init_msgs      = 0;
    bkm_proc->matched_send_msgs         = 0;
    bkm_proc->matched_isend_msgs        = 0;
    bkm_proc->matched_send_init_msgs    = 0;

    bkm_proc->total_recv_msgs           = 0;
    bkm_proc->total_irecv_msgs          = 0;
    bkm_proc->total_recv_init_msgs      = 0;
    bkm_proc->matched_recv_msgs         = 0;
    bkm_proc->matched_irecv_msgs        = 0;
    bkm_proc->matched_recv_init_msgs    = 0;
}

void ompi_crcp_coord_pml_bookmark_proc_destruct( ompi_crcp_coord_pml_bookmark_proc_t *bkm_proc) {
    bkm_proc->proc_name.cellid = 0;
    bkm_proc->proc_name.jobid  = 0;
    bkm_proc->proc_name.vpid   = 0;
    
    OBJ_DESTRUCT(&(bkm_proc->lock));
    OBJ_DESTRUCT(&(bkm_proc->cond));

    OBJ_DESTRUCT(&bkm_proc->send_list);
    OBJ_DESTRUCT(&bkm_proc->isend_list);
    OBJ_DESTRUCT(&bkm_proc->send_init_list);

    OBJ_DESTRUCT(&bkm_proc->recv_list);
    OBJ_DESTRUCT(&bkm_proc->irecv_list);
    OBJ_DESTRUCT(&bkm_proc->recv_init_list);

    bkm_proc->total_send_msgs           = 0;
    bkm_proc->total_isend_msgs          = 0;
    bkm_proc->total_send_init_msgs      = 0;
    bkm_proc->matched_send_msgs         = 0;
    bkm_proc->matched_isend_msgs        = 0;
    bkm_proc->matched_send_init_msgs    = 0;

    bkm_proc->total_recv_msgs           = 0;
    bkm_proc->total_irecv_msgs          = 0;
    bkm_proc->total_recv_init_msgs      = 0;
    bkm_proc->matched_recv_msgs         = 0;
    bkm_proc->matched_irecv_msgs        = 0;
    bkm_proc->matched_recv_init_msgs    = 0;
}

#define CREATE_COORD_STATE(coord_state, pml_state)            \
 {                                                            \
   coord_state = (ompi_crcp_coord_pml_state_t *)              \
                 malloc(sizeof(ompi_crcp_coord_pml_state_t)); \
   coord_state->prev_ptr         = pml_state;                 \
   coord_state->super.super      = pml_state->super;          \
   coord_state->super.state      = pml_state->state;          \
   coord_state->super.error_code = pml_state->error_code;     \
   coord_state->super.wrapped_pml_component = pml_state->wrapped_pml_component; \
   coord_state->super.wrapped_pml_module    = pml_state->wrapped_pml_module;    \
 }

#define EXTRACT_COORD_STATE(pml_state, v_coord_state, v_rtn_state, v_peer_ref, v_msg_ref) \
 {                                                           \
   v_coord_state = (ompi_crcp_coord_pml_state_t*)pml_state;  \
   v_rtn_state   = v_coord_state->prev_ptr;                  \
   v_peer_ref    = v_coord_state->peer_ref;                  \
   v_msg_ref     = v_coord_state->msg_ref;                   \
 }


#define CREATE_NEW_MSG(msg_ref, v_count, v_datatype, v_tag, v_rank, v_comm, v_mode, v_async, v_request, p_cellid, p_jobid, p_vpid) \
 {                                                       \
   msg_ref = OBJ_NEW(ompi_crcp_coord_pml_message_ref_t); \
   msg_ref->msg_id   = message_seq_num;                  \
   message_seq_num++;                                    \
   msg_ref->buffer   = NULL;                             \
   msg_ref->count    = v_count;                          \
   msg_ref->datatype = v_datatype;                       \
   if( NULL != msg_ref->datatype ) {                     \
      OBJ_RETAIN(msg_ref->datatype);                     \
      msg_ref->ddt_size = msg_ref->datatype->size;       \
   } else {                                              \
      msg_ref->ddt_size = 0;                             \
   }                                                     \
                                                         \
   msg_ref->tag     = v_tag;                             \
   msg_ref->rank    = v_rank;                            \
   msg_ref->comm    = v_comm;                            \
   msg_ref->mode    = v_mode;                            \
   msg_ref->async   = v_async;                           \
   msg_ref->request = v_request;                         \
   if( NULL != msg_ref->request ) {                      \
      OBJ_RETAIN(msg_ref->request);                      \
   }                                                     \
                                                         \
   msg_ref->proc_name.cellid = p_cellid;                 \
   msg_ref->proc_name.jobid  = p_jobid;                  \
   msg_ref->proc_name.vpid   = p_vpid;                   \
 }

#define DUP_MSG(dup_msg_ref, msg_ref)                             \
 {                                                                \
    dup_msg_ref = OBJ_NEW(ompi_crcp_coord_pml_message_ref_t);     \
    dup_msg_ref->msg_id = message_seq_num;                        \
    message_seq_num++;                                            \
    dup_msg_ref->buffer = NULL;                                   \
    dup_msg_ref->count    = msg_ref->count;                       \
    dup_msg_ref->datatype = msg_ref->datatype;                    \
    OBJ_RETAIN(msg_ref->datatype);                                \
    dup_msg_ref->ddt_size = msg_ref->ddt_size;                    \
                                                                  \
    dup_msg_ref->tag  = msg_ref->tag;                             \
    dup_msg_ref->rank = msg_ref->rank;                            \
    dup_msg_ref->comm = msg_ref->comm;                            \
    dup_msg_ref->mode = msg_ref->mode;                            \
    dup_msg_ref->async   = msg_ref->async;                        \
    dup_msg_ref->request = msg_ref->request;                      \
    OBJ_RETAIN(msg_ref->request);                                 \
                                                                  \
    dup_msg_ref->proc_name.cellid = msg_ref->proc_name.cellid;    \
    dup_msg_ref->proc_name.jobid  = msg_ref->proc_name.jobid;     \
    dup_msg_ref->proc_name.vpid   = msg_ref->proc_name.vpid;      \
 }

/************************
 * Local Variables
 ************************/
static uint64_t current_msg_id = 1;

/*
 * List of pending ACKs to drained messages
 *  -- ompi_crcp_coord_pml_drain_msg_ack_ref_t
 */
struct ompi_crcp_coord_pml_drain_msg_ack_ref_t {
    /** This is a list object */
    opal_list_item_t super;
    /** Complete flag */
    bool complete;
    /** Peer which we received from */
    orte_process_name_t peer;
};
typedef struct ompi_crcp_coord_pml_drain_msg_ack_ref_t ompi_crcp_coord_pml_drain_msg_ack_ref_t;
    
OBJ_CLASS_DECLARATION(ompi_crcp_coord_pml_drain_msg_ack_ref_t);
void ompi_crcp_coord_pml_drain_msg_ack_ref_construct(ompi_crcp_coord_pml_drain_msg_ack_ref_t *msg_ack_ref);
void ompi_crcp_coord_pml_drain_msg_ack_ref_destruct( ompi_crcp_coord_pml_drain_msg_ack_ref_t *msg_ack_ref);

OBJ_CLASS_INSTANCE(ompi_crcp_coord_pml_drain_msg_ack_ref_t,
                   opal_list_item_t,
                   ompi_crcp_coord_pml_drain_msg_ack_ref_construct,
                   ompi_crcp_coord_pml_drain_msg_ack_ref_destruct);

void ompi_crcp_coord_pml_drain_msg_ack_ref_construct(ompi_crcp_coord_pml_drain_msg_ack_ref_t *msg_ack_ref) {
    msg_ack_ref->complete    = false;

    msg_ack_ref->peer.cellid = 0;
    msg_ack_ref->peer.jobid  = 0;
    msg_ack_ref->peer.vpid   = 0;
}

void ompi_crcp_coord_pml_drain_msg_ack_ref_destruct( ompi_crcp_coord_pml_drain_msg_ack_ref_t *msg_ack_ref) {
    msg_ack_ref->complete   = false;

    msg_ack_ref->peer.cellid = 0;
    msg_ack_ref->peer.jobid  = 0;
    msg_ack_ref->peer.vpid   = 0;
}


/************************
 * Fuction Definitions
 ************************/
static void drain_message_ack_cbfunc(int status,
                                     orte_process_name_t* sender,
                                     orte_buffer_t *buffer,
                                     orte_rml_tag_t tag,
                                     void* cbdata);
#define FIND_MSG_TRUE     0
#define FIND_MSG_FALSE    1
#define FIND_MSG_UNKNOWN  2

static int find_message_named(opal_list_t *search_list,
                              size_t count, int tag, int src,
                              size_t ddt_size,
                              ompi_crcp_coord_pml_message_ref_t ** found_msg_ref,
                              int matched, int done, int active, int posted);
static ompi_crcp_coord_pml_bookmark_proc_t * find_peer_by_orte_name(orte_process_name_t proc);
static int find_peer_in_comm_named(struct ompi_communicator_t* comm, int proc_idx,
                                    ompi_crcp_coord_pml_bookmark_proc_t **peer_ref);
static int find_drained_message_named(size_t ddt_size,
                                      size_t count, int tag, int peer,
                                      ompi_crcp_coord_pml_message_ref_t ** found_msg_ref);

static int ft_event_coordinate_peers(void);
static int ft_event_exchange_bookmarks(void);
static int ft_event_check_bookmarks(void);
static int ft_event_wait_quiesce(void);
static int ft_event_post_drained(void);
static int ft_event_post_drain_acks(void);
static int ft_event_clear_bookmarks(void);
static int ft_event_finalize_exchange(void);
static int ft_event_send_msg_details(ompi_crcp_coord_pml_bookmark_proc_t *peer_ref, int total_sent, int total_matched);
static int ft_event_recv_msg_details(ompi_crcp_coord_pml_bookmark_proc_t *peer_ref, int total_recv, int total_matched);
static bool ft_event_have_received_msg(uint32_t comm_id, int tag, size_t count, size_t ddt_size,
                                       ompi_crcp_coord_pml_bookmark_proc_t *peer_ref,
                                       ompi_crcp_coord_pml_message_ref_t ** found_msg_ref,
                                       opal_list_t **found_on_this_list,
                                       bool *found, bool *complete, bool *already_posted);

static int send_bookmarks(int peer_idx);
static int recv_bookmarks(int peer_idx);
static int send_recv_local_bookmarks(int peer_idx);


/****************
 * PML Wrapper Init/Finalize
 ****************/
int ompi_crcp_coord_pml_init(void) {
    message_seq_num = 1;
    stall_for_completion = false;

    OBJ_CONSTRUCT(&ompi_crcp_coord_pml_procs, opal_list_t);

    OBJ_CONSTRUCT(&unknown_recv_from_list, opal_list_t);
    OBJ_CONSTRUCT(&unknown_persist_recv_list, opal_list_t);

    OBJ_CONSTRUCT(&drained_msg_list, opal_list_t);
    OBJ_CONSTRUCT(&drained_msg_ack_list, opal_list_t);

    OMPI_CRCP_COORD_FT_GLOBAL_INIT();
    OMPI_CRCP_COORD_FT_RECV_INIT();
    OMPI_CRCP_COORD_FT_SEND_INIT();

    return OMPI_SUCCESS;
}

int ompi_crcp_coord_pml_finalize(void) {
    OBJ_DESTRUCT(&ompi_crcp_coord_pml_procs);

    OBJ_DESTRUCT(&unknown_recv_from_list);

    OBJ_DESTRUCT(&drained_msg_list);
    OBJ_DESTRUCT(&drained_msg_ack_list);

    OMPI_CRCP_COORD_FT_GLOBAL_FINALIZE();
    OMPI_CRCP_COORD_FT_RECV_FINALIZE();
    OMPI_CRCP_COORD_FT_SEND_FINALIZE();

    return OMPI_SUCCESS;
}

/****************
 * PML Wrapper
 ****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_enable( bool enable, ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_enable()");

    pml_state->error_code = OMPI_SUCCESS;

    return pml_state;
}

/**************** Communicator *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_add_comm( struct ompi_communicator_t* comm, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_add_comm()");

    pml_state->error_code = OMPI_SUCCESS;

    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_del_comm( struct ompi_communicator_t* comm, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_add_comm()");

    pml_state->error_code = OMPI_SUCCESS;

    return pml_state;
}

/**************** Processes *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_add_procs( struct ompi_proc_t **procs, size_t nprocs, 
                                   ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_bookmark_proc_t *new_ref;
    size_t i;

    wrapped_pml_component = pml_state->wrapped_pml_component;
    wrapped_pml_module    = pml_state->wrapped_pml_module;

    if( OMPI_CRCP_PML_PRE != pml_state->state){
        pml_state->error_code = OMPI_SUCCESS;
        return pml_state;
    }

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_add_procs(%d)",
                        (int)nprocs);
    
    for(i = 0; i < nprocs; ++i) {
        new_ref = OBJ_NEW(ompi_crcp_coord_pml_bookmark_proc_t);

        new_ref->proc_name.cellid = procs[i]->proc_name.cellid;
        new_ref->proc_name.jobid  = procs[i]->proc_name.jobid;
        new_ref->proc_name.vpid   = procs[i]->proc_name.vpid;

        opal_list_append(&ompi_crcp_coord_pml_procs, &(new_ref->super));
    }

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_del_procs( struct ompi_proc_t **procs, size_t nprocs, 
                                   ompi_crcp_base_pml_state_t* pml_state )
{
    size_t i;
    opal_list_item_t* item = NULL;
    ompi_crcp_coord_pml_bookmark_proc_t *old_ref;

    if( OMPI_CRCP_PML_PRE != pml_state->state){
        pml_state->error_code = OMPI_SUCCESS;
        return pml_state;
    }

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_del_procs(%d)", 
                        (int)nprocs);

    for(i = 0; i < nprocs; ++i) {
        item = (opal_list_item_t*)find_peer_by_orte_name(procs[i]->proc_name);
        if(NULL == item) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_del_procs: Unable to find peer [%lu,%lu,%lu]\n",
                        ORTE_NAME_ARGS(&(procs[i]->proc_name)));
            pml_state->error_code = OMPI_ERROR;
            return pml_state;
        }

        opal_list_remove_item(&ompi_crcp_coord_pml_procs, item);
        old_ref = (ompi_crcp_coord_pml_bookmark_proc_t *)item;
        OBJ_RELEASE(old_ref);
    }

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Progress *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_progress(ompi_crcp_base_pml_state_t* pml_state)
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_progress()");

    /* Nothing to do */

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Probe *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_iprobe(int dst, int tag, 
                                                       struct ompi_communicator_t* comm, 
                                                       int *matched, ompi_status_public_t* status, 
                                                       ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_iprobe()");

    /* Nothing to do */

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_probe( int dst, int tag, 
                                                       struct ompi_communicator_t* comm, 
                                                       ompi_status_public_t* status, 
                                                       ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_probe()");

    /* Nothing to do */

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Send *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_isend_init( void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag, 
                                    mca_pml_base_send_mode_t mode, struct ompi_communicator_t* comm, struct ompi_request_t **request, 
                                    ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_bookmark_proc_t    *peer_ref = NULL;
    ompi_crcp_coord_pml_message_ref_t      *msg_ref  = NULL;
    ompi_crcp_coord_pml_state_t            *coord_state = NULL;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_isend_init(%d)", pml_state->state);

    /**********************************
     * Before the PML gets the message
     *  - Setup some structures for tracking the message.
     **********************************/
    if ( OMPI_CRCP_PML_PRE == pml_state->state ) {
        /*
         * Find the peer reference
         */
        if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(comm, dst, &peer_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_isend_init: Unable to find the proper structure refs\n");
            pml_state->error_code = ret;
            return pml_state;
        }
        if( peer_ref == NULL ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_isend_init: Unable to find the proper structure refs: peer is NULL\n");
            pml_state->error_code = OMPI_ERROR;
            return pml_state;
        }

        /*
         * Create a new Message Object
         */
        CREATE_NEW_MSG(msg_ref, count, datatype, tag, dst, comm, mode, true, NULL,
                       peer_ref->proc_name.cellid,
                       peer_ref->proc_name.jobid,
                       peer_ref->proc_name.vpid);

        msg_ref->matched        = false;
        msg_ref->done           = false;
        msg_ref->active         = false;
        msg_ref->already_posted = true;
        
        opal_list_append(&(peer_ref->send_init_list), &(msg_ref->super));
        
        /* Save the pointers */
        CREATE_COORD_STATE(coord_state, pml_state);
        coord_state->peer_ref = peer_ref;
        coord_state->msg_ref  = msg_ref;
        coord_state->super.error_code = OMPI_SUCCESS;

        return &coord_state->super;
    }
    /******************************
     * After the PML is done, update our structures.
     ******************************/
    else if ( OMPI_CRCP_PML_POST == pml_state->state ) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state, peer_ref,  msg_ref);

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

        free(coord_state);
        rtn_state->error_code = OMPI_SUCCESS;

        return rtn_state;
    }
    /***************************
     ***************************/
    else {
        ;
    }

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_isend( void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                               mca_pml_base_send_mode_t mode, struct ompi_communicator_t* comm, struct ompi_request_t **request, 
                               ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_bookmark_proc_t    *peer_ref = NULL;
    ompi_crcp_coord_pml_message_ref_t      *msg_ref  = NULL;
    ompi_crcp_coord_pml_state_t            *coord_state = NULL;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_isend(%d)", pml_state->state);

    /**********************************
     * Before the PML gets the message
     *  - Setup some structures for tracking the message.
     **********************************/
    if ( OMPI_CRCP_PML_PRE == pml_state->state ) {

        /*
         * Find the peer reference
         */
        if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(comm, dst, &peer_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_isend: Unable to find the proper structure refs\n");
            pml_state->error_code = ret;
            return pml_state;
        }
        if( peer_ref == NULL) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_isend: Unable to find the proper structure refs: peer is NULL\n");
            pml_state->error_code = OMPI_ERROR;
            return pml_state;
        }

        /*
         * Create a new Message Object
         */
        CREATE_NEW_MSG(msg_ref, count, datatype, tag, dst, comm, mode, true, NULL,
                       peer_ref->proc_name.cellid,
                       peer_ref->proc_name.jobid,
                       peer_ref->proc_name.vpid);

        msg_ref->matched        = false;
        msg_ref->done           = false;
        msg_ref->active         = true;
        msg_ref->already_posted = true;
        
        opal_list_append(&(peer_ref->isend_list), &(msg_ref->super));
        
        /* A bit of bookkeeping */
        peer_ref->total_isend_msgs  += 1;

        /* Save the pointers */
        CREATE_COORD_STATE(coord_state, pml_state);
        coord_state->peer_ref = peer_ref;
        coord_state->msg_ref  = msg_ref;
        coord_state->super.error_code = OMPI_SUCCESS;

        return &coord_state->super;
    }
    /******************************
     * After the PML is done, update our structures.
     ******************************/
    else if ( OMPI_CRCP_PML_POST == pml_state->state ) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state, peer_ref,  msg_ref);

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

        free(coord_state);
        rtn_state->error_code = OMPI_SUCCESS;

        return rtn_state;
    }
    /***************************
     ***************************/
    else {
        ;
    }

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_send(  void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                               mca_pml_base_send_mode_t mode, struct ompi_communicator_t* comm, 
                               ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_bookmark_proc_t    *peer_ref = NULL;
    ompi_crcp_coord_pml_message_ref_t      *msg_ref  = NULL;
    ompi_crcp_coord_pml_state_t            *coord_state = NULL;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_send( %d )", pml_state->state);

    /**********************************
     * Before the PML gets the message
     *  - Enter the critical section.
     *  - Setup some structures for tracking the message.
     **********************************/
    if ( OMPI_CRCP_PML_PRE == pml_state->state ) {
        /*
         * Find the peer reference
         */
        if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(comm, dst, &peer_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_send: Unable to find the proper structure refs\n");
            pml_state->error_code = ret;
            return pml_state;
        }
        if( peer_ref == NULL ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_send: Unable to find the proper structure refs: peer is NULL\n");
            pml_state->error_code = OMPI_ERROR;
            return pml_state;
        }

        /*
         * Create a new Message Object
         */
        CREATE_NEW_MSG(msg_ref, count, datatype, tag, dst, comm, mode, false, NULL,
                       peer_ref->proc_name.cellid,
                       peer_ref->proc_name.jobid,
                       peer_ref->proc_name.vpid);

        msg_ref->matched        = false;
        msg_ref->done           = false;
        msg_ref->active         = true;
        msg_ref->already_posted = true;
        
        opal_list_append(&(peer_ref->send_list), &(msg_ref->super));
        
        /* A bit of bookkeeping */
        peer_ref->total_send_msgs += 1;
        current_msg_id = msg_ref->msg_id;

        /* Save the pointers */
        CREATE_COORD_STATE(coord_state, pml_state);
        coord_state->peer_ref = peer_ref;
        coord_state->msg_ref  = msg_ref;
        coord_state->super.error_code = OMPI_SUCCESS;

        return &coord_state->super;
    }
    /******************************
     * After the PML is done, update our structures.
     ******************************/
    else if ( OMPI_CRCP_PML_POST == pml_state->state ) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state, peer_ref, msg_ref);

        /*
         * Mark the messages as complete
         * - some Booleans already set...
         *   msg_ref->matched        = false;
         *   msg_ref->already_posted = true;
         */
        msg_ref->done   = true;
        msg_ref->active = false;

        current_msg_id = 0;

        free(coord_state);
        rtn_state->error_code = OMPI_SUCCESS;

        return rtn_state;
    }
    /***************************
     ***************************/
    else {
        ;
    }

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Recv *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_irecv_init( void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                                    struct ompi_communicator_t* comm,  struct ompi_request_t **request, 
                                    ompi_crcp_base_pml_state_t* pml_state)
{
    ompi_crcp_coord_pml_bookmark_proc_t    *peer_ref = NULL;
    ompi_crcp_coord_pml_message_ref_t      *msg_ref  = NULL;
    ompi_crcp_coord_pml_state_t            *coord_state = NULL;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_recv_init(%d)",
                        pml_state->state);

    /************************
     * Pre PML Call
     ************************/
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * A message will never be on the drained list for this function since
         * it does not actually receive anything, just sets up the system.
         * The receive for these reqeusts are done in the start() and wait()
         * commands.
         */

        /*
         * Create a new message for the receive list
         */
        CREATE_NEW_MSG(msg_ref, count, datatype, tag, src, comm, MCA_PML_BASE_SEND_COMPLETE, true, NULL,
                       ORTE_CELLID_INVALID,
                       ORTE_JOBID_INVALID,
                       ORTE_VPID_INVALID);
        
        msg_ref->matched         = false;
        msg_ref->done            = false;
        msg_ref->active          = false;
        msg_ref->already_posted  = true;

        CREATE_COORD_STATE(coord_state, pml_state);

        /*
         * If we don't know the peer add the message to the unknown list
         */
        if( src < 0 || src >= comm->c_remote_group->grp_proc_count) {
            opal_list_append(&(unknown_persist_recv_list), &(msg_ref->super));
            
            coord_state->peer_ref = NULL;
            coord_state->msg_ref  = msg_ref;
        }
        else {
            if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(comm, src, &peer_ref) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: pml_recv_init: Unable to find the proper structure refs\n");
                pml_state->error_code = ret;
                return pml_state;
            }

            msg_ref->proc_name.cellid = peer_ref->proc_name.cellid;
            msg_ref->proc_name.jobid  = peer_ref->proc_name.jobid;
            msg_ref->proc_name.vpid   = peer_ref->proc_name.vpid;
            
            opal_list_append(&(peer_ref->recv_init_list), &(msg_ref->super));
            
            coord_state->peer_ref = peer_ref;
            coord_state->msg_ref  = msg_ref;
        }

        coord_state->super.error_code = OMPI_SUCCESS;
        return &coord_state->super;
    }
    /************************
     * Post PML Call
     * Do some bookkeeping
     ************************/
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state, peer_ref,  msg_ref);

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

        free(coord_state);
        rtn_state->error_code = OMPI_SUCCESS;

        return rtn_state;
    }
    /************************
     ************************/
    else {
        ;
    }

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_irecv( void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                               struct ompi_communicator_t* comm, struct ompi_request_t **request, 
                               ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_bookmark_proc_t    *peer_ref = NULL;
    ompi_crcp_coord_pml_message_ref_t      *msg_ref  = NULL;
    ompi_crcp_coord_pml_message_ref_t      *drain_msg_ref = NULL;
    ompi_crcp_coord_pml_state_t            *coord_state = NULL;
    size_t tmp_ddt_size  = 0;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_irecv(%d)",
                        pml_state->state);

    /************************
     * Pre PML Call
     ************************/
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        tmp_ddt_size = datatype->size;
        if( OMPI_SUCCESS != (ret = find_drained_message_named(tmp_ddt_size,
                                                              count, tag, src,
                                                              &drain_msg_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_irecv(PRE): An error occured when trying to find a drained message."
                        " This should never happen. (%d)",
                        ret);
            pml_state->error_code = ret;
            return pml_state;
        }

        /*
         * If the message was not drained previously (common case)
         */
        if( NULL == drain_msg_ref) {
            /*
             * Create a new message for the receive list
             */
            CREATE_NEW_MSG(msg_ref, count, datatype, tag, src, comm, MCA_PML_BASE_SEND_COMPLETE, true, NULL,
                           ORTE_CELLID_INVALID,
                           ORTE_JOBID_INVALID,
                           ORTE_VPID_INVALID);

            msg_ref->matched         = false;
            msg_ref->done            = false;
            msg_ref->active          = true;
            msg_ref->already_posted  = true;

            CREATE_COORD_STATE(coord_state, pml_state);

            /*
             * If we don't know the peer add the message to the unknown list
             */
            if( src < 0 || src >= comm->c_remote_group->grp_proc_count) {
                opal_output_verbose(30, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_irecv: Adding to unknown recv list\n");

                opal_list_append(&(unknown_recv_from_list), &(msg_ref->super));
                
                coord_state->peer_ref = NULL;
                coord_state->msg_ref  = msg_ref;
            }
            else {
                if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(comm, src, &peer_ref) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_irecv: Unable to find the proper structure refs\n");
                    pml_state->error_code = ret;
                    return pml_state;
                }

                msg_ref->proc_name.cellid = peer_ref->proc_name.cellid;
                msg_ref->proc_name.jobid  = peer_ref->proc_name.jobid;
                msg_ref->proc_name.vpid   = peer_ref->proc_name.vpid;

                opal_list_append(&(peer_ref->irecv_list), &(msg_ref->super));

                coord_state->peer_ref = peer_ref;
                coord_state->msg_ref  = msg_ref;
            }
            coord_state->super.error_code = OMPI_SUCCESS;
            return &coord_state->super;
        }
        /*
         * If the message was a drained message, complete it right away
         * Note: We do not need to increment any counters here since we already have
         *       when we originally drained the message.
         */
        else {
            opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_irecv(PRE): Matched a drained message.");

            /* Copy the drained message */
            src = drain_msg_ref->rank;
            tag = drain_msg_ref->tag;
            memcpy(buf,    drain_msg_ref->buffer,  tmp_ddt_size * count);

            /* Remove the message from the list */
            if( NULL != drain_msg_ref->datatype ) {
                OBJ_RELEASE(drain_msg_ref->datatype);
                drain_msg_ref->datatype = NULL;
            }
            opal_list_remove_item(&drained_msg_list, &(drain_msg_ref->super));
            OBJ_RELEASE(drain_msg_ref); 

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
    }
    /************************
     * Post PML Call
     * Do some bookkeeping
     ************************/
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state, peer_ref,  msg_ref);

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

        free(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }
    /************************
     ************************/
    else {
        ;
    }

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/*
 * If we are not in a multithreaded FT build then we need to 
 * convert MPI_RECV to MPI_IRECV due to buffering issues that may
 * lead to deadlock in the single process case.
 */
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_recv(  void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                               struct ompi_communicator_t* comm,  ompi_status_public_t* status, 
                               ompi_crcp_base_pml_state_t* pml_state)
{
    ompi_crcp_coord_pml_bookmark_proc_t    *peer_ref;
    ompi_crcp_coord_pml_message_ref_t      *msg_ref;
    ompi_crcp_coord_pml_message_ref_t      *drain_msg_ref = NULL;
    ompi_crcp_coord_pml_state_t            *coord_state = NULL;
    size_t tmp_ddt_size  = 0;
    int ret;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_recv(%d)",
                        pml_state->state);

    /************************
     * Pre PML Call
     ************************/
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        tmp_ddt_size = datatype->size;
        if( OMPI_SUCCESS != (ret = find_drained_message_named(tmp_ddt_size,
                                                              count, tag, src,
                                                              &drain_msg_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_recv(PRE): An error occured when trying to find a drained message."
                        " This should never happen. (%d)",
                        ret);
            pml_state->error_code = ret;
            return pml_state;
        }

        /*
         * If the message was not drained previously (common case)
         */
        if( NULL == drain_msg_ref) {
            /*
             * Create a new message for the receive list
             */
            CREATE_NEW_MSG(msg_ref, count, datatype, tag, src, comm, MCA_PML_BASE_SEND_COMPLETE, false, NULL,
                           ORTE_CELLID_INVALID,
                           ORTE_JOBID_INVALID,
                           ORTE_VPID_INVALID);

            msg_ref->matched         = false;
            msg_ref->done            = false;
            msg_ref->active          = true;
            msg_ref->already_posted  = true;

            current_msg_id = msg_ref->msg_id;

            CREATE_COORD_STATE(coord_state, pml_state);

            /*
             * If we don't know the peer add the message to the unknown list
             */
            if( src < 0 || src >= comm->c_remote_group->grp_proc_count) {
                opal_list_append(&(unknown_recv_from_list), &(msg_ref->super));

                coord_state->peer_ref = NULL;
                coord_state->msg_ref  = msg_ref;
            }
            else {
                if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(comm, src, &peer_ref) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_recv: Unable to find the proper structure refs\n");
                    pml_state->error_code = ret;
                    return pml_state;
                }

                msg_ref->proc_name.cellid = peer_ref->proc_name.cellid;
                msg_ref->proc_name.jobid  = peer_ref->proc_name.jobid;
                msg_ref->proc_name.vpid   = peer_ref->proc_name.vpid;

                opal_list_append(&(peer_ref->recv_list), &(msg_ref->super));

                coord_state->peer_ref = peer_ref;
                coord_state->msg_ref  = msg_ref;
            }
            
            coord_state->super.error_code = OMPI_SUCCESS;
            return &coord_state->super;
        }
        /*
         * If the message was a drained message, complete it right away
         * Note: We do not need to increment any counters here since we already have
         *       when we originally drained the message.
         */
        else {
            opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_recv(PRE): Matched a drained message.");

            /* Copy the drained message */
            src = drain_msg_ref->rank;
            tag = drain_msg_ref->tag;
            memcpy(buf,    drain_msg_ref->buffer,  tmp_ddt_size * count);
            memcpy(status, &drain_msg_ref->status, sizeof(ompi_status_public_t)); 

            /* Remove the message from the list */
            if( NULL != drain_msg_ref->datatype ) {
                OBJ_RELEASE(drain_msg_ref->datatype);
                drain_msg_ref->datatype = NULL;
            }
            opal_list_remove_item(&drained_msg_list, &(drain_msg_ref->super));
            OBJ_RELEASE(drain_msg_ref);

            /* Since we originally mark all drained received as irecvs
             * we need to update our counters since this is a full recv
             */
            peer_ref->total_irecv_msgs -= 1;
            peer_ref->total_recv_msgs  += 1;

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
    }
    /************************
     * Post PML Call
     * Do some bookkeeping
     ************************/
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state, peer_ref,  msg_ref);

        /*
         * If the peer was not specified previously then we want to move the message
         * from the unknown_list to the appropriate peer list.
         */
        if( peer_ref == NULL ) {
            src = status->MPI_SOURCE;
            
            if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(comm, src, &peer_ref) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: pml_recv: Unable to find the proper structure refs\n");
                pml_state->error_code = ret;
                return pml_state;
            }

            msg_ref->proc_name.cellid = peer_ref->proc_name.cellid;
            msg_ref->proc_name.jobid  = peer_ref->proc_name.jobid;
            msg_ref->proc_name.vpid   = peer_ref->proc_name.vpid;

            opal_list_remove_item(&(unknown_recv_from_list), &(msg_ref->super));
            opal_list_append(     &(peer_ref->recv_list),    &(msg_ref->super));
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

        free(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }
    /************************
     ************************/
    else {
        ;
    }

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Dump *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_dump( struct ompi_communicator_t* comm, int verbose, 
                              ompi_crcp_base_pml_state_t* pml_state )
{
    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_dump()");

    /* Nothing to do */

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Start *****************/
/* Start is connected to irecv_start or isend_start */
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_start( size_t count, ompi_request_t** requests, 
                               ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_coord_pml_bookmark_proc_t    *peer_ref = NULL;
    ompi_crcp_coord_pml_message_ref_t      *msg_ref  = NULL;
    size_t tmp_ddt_size  = 0;
    int ret;
    size_t c;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_start(%d)",
                        pml_state->state);

    /*
     * Only take action once the PML has things started
     */
    if( OMPI_CRCP_PML_POST != pml_state->state) {
        pml_state->error_code = OMPI_SUCCESS;
        return pml_state;
    }

    for(c = 0; c < count; c++) {
        mca_pml_base_request_t *breq;
        breq = (mca_pml_base_request_t *)requests[c];

        tmp_ddt_size = (breq->req_datatype)->size;

        /*
         * If the message was a send operation
         */
        if(breq->req_type == MCA_PML_REQUEST_SEND ) {
            /* 
             * Find the peer reference
             */
            if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(breq->req_comm, breq->req_peer, &peer_ref) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: pml_start(POST): Unable to find the proper structure refs\n");
                pml_state->error_code = ret;
                return pml_state;
            }

            /* Check the send list */
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
                            "crcp:coord: pml_start(POST): (b) Unable to find the proper (send) message ref for this recv\n");
                pml_state->error_code = ret;
                return pml_state;
            }
                    
            if( NULL != msg_ref ) {
                /* Known states
                 *  matched = false
                 *  done    = false
                 *  active  = false -> true
                 *  already_posted = true;
                 */
                msg_ref->active = true;
            }
#if JJH_DEBUG == 1
            else {
                printf("JJH THIS SHOULD NEVER HAPPEN file %s line %d\n", __FILE__, __LINE__);
                pml_state->error_code = OMPI_ERROR;
                goto DONE;
            }
#endif
        }
        /*
         * If this was a receive operation then check those lists.
         */
        else if (breq->req_type == MCA_PML_REQUEST_RECV) {
            /*
             * Unknown peer, so check the unknown send list.
             */
            if( breq->req_peer < 0 ||
                breq->req_peer >= breq->req_comm->c_remote_group->grp_proc_count) {
                opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: pml_start(POST): Peer not in communicator src = %d.\n",
                                    breq->req_peer);

                if( OMPI_SUCCESS != (ret = find_message_named(&unknown_persist_recv_list,
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
                                "crcp:coord: pml_start(POST): (a) Unable to find the proper message ref for this recv\n");
                    pml_state->error_code = ret;
                    return pml_state;
                }
            }
            /*
             * Known peer, so find in its information
             */
            else {
                /* 
                 * Find the peer reference
                 */
                if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(breq->req_comm, breq->req_peer, &peer_ref) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_start(POST): Unable to find the proper structure refs\n");
                    pml_state->error_code = ret;
                    return pml_state;
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
                                "crcp:coord: pml_start(POST): (b) Unable to find the proper (recv) message ref for this recv\n");
                    pml_state->error_code = ret;
                    return pml_state;
                }
            }

            if( NULL != msg_ref ) {
                /* Known states
                 *  matched = false
                 *  done    = false
                 *  active  = false -> true
                 *  already_posted = true;
                 */
                msg_ref->active = true;
            }
#if JJH_DEBUG == 1
            else {
                printf("JJH THIS SHOULD NEVER HAPPEN file %s line %d\n", __FILE__, __LINE__);
                pml_state->error_code = OMPI_ERROR;
                goto DONE;
            }
#endif
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

    pml_state->error_code = OMPI_SUCCESS;

 DONE:
    return pml_state;
}

/**************** Request Completed ********/
int ompi_crcp_coord_request_complete(struct ompi_request_t *request)
{
    ompi_crcp_coord_pml_bookmark_proc_t    *peer_ref = NULL;
    ompi_crcp_coord_pml_message_ref_t      *msg_ref  = NULL;
    mca_pml_base_request_t *breq;
    size_t tmp_ddt_size  = 0;
    int ret, exit_status = OMPI_SUCCESS;
    int src;
    int tag;

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: request_complete()");

    /*
     * Extract the PML version of the request
     */
    breq = (mca_pml_base_request_t *)request;

    /* Check for the NULL Request */
    if( (breq->req_type   != MCA_PML_REQUEST_SEND &&
         breq->req_type   != MCA_PML_REQUEST_RECV ) ||
        request->req_type == OMPI_REQUEST_NOOP ||
        request->req_type == OMPI_REQUEST_NULL) {
        exit_status = OMPI_SUCCESS;
        goto DONE;
    }

    /* Extract source/tag/ddt_size */
    tmp_ddt_size = (breq->req_datatype)->size;
    src = breq->req_peer;
    tag = breq->req_tag;

    /*************
     * First find the peer reference
     *************/
    if( MPI_ANY_SOURCE == src ) {
        if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(breq->req_comm, request->req_status.MPI_SOURCE, &peer_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_start(POST): Unable to find the proper structure refs\n");
            exit_status = ret;
            goto DONE;
        }
    } else {
        if( OMPI_SUCCESS != (ret = find_peer_in_comm_named(breq->req_comm, src, &peer_ref) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_start(POST): Unable to find the proper structure refs\n");
            exit_status = ret;
            goto DONE;
        }
    }

    /* Make sure we found the peer reference */
    if( NULL == peer_ref ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: request_complete: Unable to find the peer (%d)\n",
                    src);
        exit_status = OMPI_ERROR;
        goto DONE;
    }

    /*******************************
     * A send request is completing
     ******************************/
    if(breq->req_type == MCA_PML_REQUEST_SEND ) {
        /* ISEND Case: */
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
                            "crcp:coord: request_complete: Unable to find the proper (isend) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL != msg_ref ) {
                peer_ref->total_isend_msgs += 1;

                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: request_complete: Matched an iSend: total = %d",
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
        /* ISEND_INIT/START (persistent) Case: */
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
                            "crcp:coord: request_complete: Unable to find the proper (send_init) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL != msg_ref ) {
                ompi_crcp_coord_pml_message_ref_t      *dup_msg_ref  = NULL;
                
                /* Duplicate the message so it can be matched again */
                DUP_MSG(dup_msg_ref, msg_ref);
                
                dup_msg_ref->matched = false;
                dup_msg_ref->done    = false;
                dup_msg_ref->active  = false;
                dup_msg_ref->already_posted  = true;
                
                opal_list_append(&(peer_ref->send_init_list), &(dup_msg_ref->super));
                
                /* Account for this completed request */
                peer_ref->total_send_init_msgs += 1;
                
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: request_complete: Matched an Send Init: total = %d",
                                    peer_ref->total_send_init_msgs);
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
        /* IRECV Case: */
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
                            "crcp:coord: request_complete: Unable to find the proper (irecv) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL != msg_ref ) {
                peer_ref->total_irecv_msgs += 1;

                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: request_complete: Matched an iRecv: total = %d",
                                    peer_ref->total_irecv_msgs);
                goto FOUND;
            }

            /*
             * At this point the message was either completed and this is the second time we
             * have seen this request *or* it is in the unknown receive from list.
             */
            if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_recv_from_list),
                                                          breq->req_count,
                                                          tag, INVALID_INT,
                                                          tmp_ddt_size,
                                                          &msg_ref,
                                                          FIND_MSG_UNKNOWN,
                                                          FIND_MSG_FALSE,
                                                          FIND_MSG_TRUE,
                                                          FIND_MSG_TRUE
                                                          ) ) ) {
                opal_output(mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: request_complete: Unable to find the proper (recv_init) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL != msg_ref ) {
                opal_list_remove_item(&(unknown_recv_from_list), &(msg_ref->super));
                opal_list_append(&(peer_ref->irecv_list), &(msg_ref->super));

                peer_ref->total_irecv_msgs += 1;

                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: request_complete: Matched an iRecv: total = %d",
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
        /* IRECV_INIT/START Case: */
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
                            "crcp:coord: request_complete: Unable to find the proper (recv_init) message ref for this complete\n");
                exit_status = ret;
                goto DONE;
            }
            if( NULL == msg_ref ) {
                /* The message was not on the known list, check the unkown list */
                if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_persist_recv_list),
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
                                "crcp:coord: request_complete: Unable to find the proper (recv_init) message ref for this complete\n");
                    exit_status = ret;
                    goto DONE;
                }

                if( NULL == msg_ref ) {
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
                else {
                    ompi_crcp_coord_pml_message_ref_t *dup_msg_ref  = NULL;

                    /* Move it from the unknown list to this comm list */
                    opal_list_remove_item(&(unknown_persist_recv_list), &(msg_ref->super));
                    opal_list_append(     &(peer_ref->recv_init_list),  &(msg_ref->super));

                
                    /* Duplicate the message so it can be matched again */
                    DUP_MSG(dup_msg_ref, msg_ref);
                
                    dup_msg_ref->matched = false;
                    dup_msg_ref->done    = false;
                    dup_msg_ref->active  = false;
                    dup_msg_ref->already_posted  = true;

                    opal_list_append(&(unknown_persist_recv_list), &(dup_msg_ref->super));
                
                    /* Account for this completed request */
                    peer_ref->total_recv_init_msgs += 1;

                    opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                        "crcp:coord: request_complete: Matched an Send Init: total = %d",
                                        peer_ref->total_recv_init_msgs);
                    goto FOUND;
                }
            }
            else { /* NULL != msg_ref */
                ompi_crcp_coord_pml_message_ref_t *dup_msg_ref  = NULL;
                
                /* Duplicate the message so it can be matched again */
                DUP_MSG(dup_msg_ref, msg_ref);
                
                dup_msg_ref->matched = false;
                dup_msg_ref->done    = false;
                dup_msg_ref->active  = false;
                dup_msg_ref->already_posted  = true;

                opal_list_append(&(peer_ref->send_init_list), &(dup_msg_ref->super));
                
                /* Account for this completed request */
                peer_ref->total_recv_init_msgs += 1;

                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: request_complete: Matched an Send Init: total = %d",
                                    peer_ref->total_recv_init_msgs);
                goto FOUND;
            }
        }
    }
    /*
     * An unknown type of request is completing.
     */
    else {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: request_complete: Unknown request type... %d",
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
                            "crcp:coord: request_complete: Marked Message... ( %d, %d )\n",
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
ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_ft_event(int state, 
                                                         ompi_crcp_base_pml_state_t* pml_state)
{
    int ret;
    static int step_to_return_to = 0;

    if( step_to_return_to == 1 ) {
        goto STEP_1;
    }

    opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_ft_event()");
    
    /**************************
     * Prepare for a Checkpoint
     **************************/
    if(OPAL_CRS_CHECKPOINT == state) {
        /* We only take action before the real PML is given
         * the opportunity to take action
         */
        if( OMPI_CRCP_PML_PRE != pml_state->state){
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }

#if 0
        /* Stop all new Send operations */
        OMPI_CRCP_COORD_FT_SEND_CS_RESTRICT(false);
        OMPI_CRCP_COORD_FT_GLOBAL_CS_RESTRICT(true);
        OMPI_CRCP_COORD_FT_RECV_CS_RESTRICT(true);
#endif

        /* Coordinate Peers
         * When we return from this function we know that all of our
         * channels have been flushed.
         */
    STEP_1:
        step_to_return_to = 0;
        if( OMPI_SUCCESS != (ret = ft_event_coordinate_peers()) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_ft_event: Checkpoint Coordination Failed %d",
                        ret);
            pml_state->error_code = ret;
            return pml_state;
        }

        if( stall_for_completion ) {
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: pml_ft_event: STALLING PID %d\n",
                                getpid());

            stall_for_completion = false;
            opal_cr_stall_check  = true;
            step_to_return_to = 1;
            pml_state->error_code = OMPI_EXISTS;
            return pml_state;
        }
    }
    /*****************************
     * Continue after a checkpoint
     ******************************/
    else if(OPAL_CRS_CONTINUE == state) {
        if( OMPI_CRCP_PML_POST != pml_state->state){
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }

        /*
         * Finish the coord protocol
         */
        if( OMPI_SUCCESS != (ret = ft_event_finalize_exchange() ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_ft_event: Checkpoint Finalization Failed %d",
                        ret);
            pml_state->error_code = ret;
            return pml_state;
        }

#if 0
        /* All new sends to proceed */
        OMPI_CRCP_COORD_FT_SEND_CS_RELEASE();
        OMPI_CRCP_COORD_FT_RECV_CS_RELEASE();
        OMPI_CRCP_COORD_FT_GLOBAL_CS_RELEASE();
#endif
    }
    /*****************************
     * Restart from a checkpoint
     *****************************/
    else if(OPAL_CRS_RESTART == state) {
        if( OMPI_CRCP_PML_POST != pml_state->state){
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }

        /*
         * Finish the coord protocol
         */
        if( OMPI_SUCCESS != (ret = ft_event_finalize_exchange() ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_ft_event: Checkpoint Finalization Failed %d",
                        ret);
            pml_state->error_code = ret;
            return pml_state;
        }
        /*
         * Re-boot the PML/BML/BTL stack
         * Need to do this because:
         * 1) BTL contact information has changed
         * 2) PML/BML use static lists of BTLs and the list may have changed
         *    so they need to be cleared and refreshed.
         * - Need to:
         *   a) del_procs, del_comm
         *   b) shutdown PML/BML/BTL
         *   c) Startup PML/BML/BTL including reselection
         *   d) add_procs, add_comm
         */
        if( OMPI_SUCCESS != (ret = ompi_crcp_base_reboot_pml(pml_state) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: pml_ft_event: Rebooting the PML/BML/BTL stack Failed %d",
                        ret);
            pml_state->error_code = ret;
            return pml_state;
        }

#if 0
        /* All new sends to proceed */
        OMPI_CRCP_COORD_FT_SEND_CS_RELEASE();
        OMPI_CRCP_COORD_FT_RECV_CS_RELEASE();
        OMPI_CRCP_COORD_FT_GLOBAL_CS_RELEASE();
#endif
    }
    /*****************************
     * Terminating the process post checkpoint
     *****************************/
    else if(OPAL_CRS_TERM == state ) {
        if( OMPI_CRCP_PML_POST != pml_state->state){
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }

#if 0
        OMPI_CRCP_COORD_FT_SEND_CS_RELEASE();
        OMPI_CRCP_COORD_FT_RECV_CS_RELEASE();
        OMPI_CRCP_COORD_FT_GLOBAL_CS_RELEASE();
#endif
    }
    /****************************
     * Reached an error
     ****************************/
    else {
        if( OMPI_CRCP_PML_POST != pml_state->state){
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
    }

    step_to_return_to = 0;

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/******************
 * Local functions
 ******************/
static int ft_event_coordinate_peers(void) {
    int ret = OMPI_SUCCESS;
    static int step_to_return_to = 0;

    if( step_to_return_to == 1 ) {
        goto STEP_1;
    }

    /*
     * Exchange Bookmarks with peers
     */
    if( OMPI_SUCCESS != (ret = ft_event_exchange_bookmarks() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Bookmark Exchange Failed %d",
                    ret);
        return ret;
    }

    /*
     * Check exchanged bookmarks 
     */
    if( OMPI_SUCCESS != (ret = ft_event_check_bookmarks() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Bookmark Check Failed %d",
                    ret);
        return ret;
    }

    if( stall_for_completion ) {
        opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: ft_event_coordinate_peers: [%lu,%lu,%lu] **** STALLING ***",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
        step_to_return_to = 1;
        return OMPI_SUCCESS;
    }

 STEP_1:
    step_to_return_to = 0;

    if( OMPI_SUCCESS != (ret = ft_event_post_drain_acks() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Post Bookmark Drain ACKS Failed %d",
                    ret);
        return ret;
    }

    if( OMPI_SUCCESS != (ret = ft_event_post_drained() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Post Bookmark Drained Failed %d",
                    ret);
        return ret;
    }

    /*
     * Wait for any messages that needed resolved.
     * - Outstanding Receives (to drain wire) -- Receiver
     * - Outstanding Irecvs (for drain ack)   -- Sender
     */
    if( OMPI_SUCCESS != (ret = ft_event_wait_quiesce() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_coordinate_peers: Wait Quiesce Failed %d",
                    ret);
        return ret;
    }

    opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: ft_event_coordinate_peers: [%lu,%lu,%lu] DONE...\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name) );

    /*
     * Now that all our peer channels are marked as drained
     * continue with the checkpoint.
     * Note: This does not guarentee that all of the peers
     *       are at this same position, but that our
     *       checkpoint will be consistent with all of the
     *       peers once they finish the protocol.
     */

    return OMPI_SUCCESS;
}

static int ft_event_clear_bookmarks(void) {
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(&ompi_crcp_coord_pml_procs);
        item != opal_list_get_end(&ompi_crcp_coord_pml_procs);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_coord_pml_bookmark_proc_t *peer_ref;
        peer_ref = (ompi_crcp_coord_pml_bookmark_proc_t*)item;

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
    }

    /*******************
     * Optimization: Clean out all of the message queues so we don't take up as much space
     *******************/

    return OMPI_SUCCESS;
}

/*
 * LAM/MPI used a staggered all-to-all algoritm for bookmark exachange
 *    http://www.lam-mpi.org/papers/lacsi2003/
 */
static int ft_event_exchange_bookmarks(void) {
    int peer_idx  = 0;
    int my_idx    = orte_process_info.my_name->vpid;
    int iter      = 0;
    int num_peers = 0;
    
    num_peers = opal_list_get_size(&ompi_crcp_coord_pml_procs);

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
            else {
                /* Send/Recv to ourself */
                send_recv_local_bookmarks(peer_idx);
            }
        }
    
    return OMPI_SUCCESS;
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

/*
 * Here we send our bookmarks for this peer to that peer.
 * This is our count of what we have seen from this peer.
 */
static int send_bookmarks(int peer_idx) {
    ompi_crcp_coord_pml_bookmark_proc_t *peer_ref;
    orte_process_name_t peer_name;
    orte_buffer_t *buffer = NULL;
    int ret;
    int exit_status = OMPI_SUCCESS;

    /*
     * Find this peer in the structure
     */
    peer_name.cellid = orte_process_info.my_name->cellid;
    peer_name.jobid  = orte_process_info.my_name->jobid;
    peer_name.vpid   = peer_idx;

    if( NULL == (peer_ref = find_peer_by_orte_name(peer_name)) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: send_bookmarks: Could not find peer indexed %d\n",
                    peer_idx);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: send_bookmarks: [%lu,%lu,%lu] Sending bookmark  (%4d,%4d,%4d) (%4d,%4d,%4d) to   peer [%lu,%lu,%lu]\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name),
                        peer_ref->total_send_msgs,
                        peer_ref->total_isend_msgs,
                        peer_ref->total_send_init_msgs,
                        peer_ref->total_recv_msgs,
                        peer_ref->total_irecv_msgs,
                        peer_ref->total_recv_init_msgs,
                        ORTE_NAME_ARGS(&peer_name));

    /*
     * Send the bookmarks to them
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
                    "crcp:coord: send_bookmarks: Could send bookmark (%d,%d,%d %d,%d,%d) to peer [%lu,%lu,%lu]: Return %d\n",
                    peer_ref->total_send_msgs,
                    peer_ref->total_isend_msgs,
                    peer_ref->total_send_init_msgs,
                    peer_ref->total_recv_msgs,
                    peer_ref->total_irecv_msgs,
                    peer_ref->total_recv_init_msgs,
                    ORTE_NAME_ARGS(&peer_name),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if(NULL != buffer)
        OBJ_RELEASE(buffer);

    return exit_status;
}

/*
 * Here we receive our peers bookmarks for us
 * This is the peers count of what they have seen from 
 * us.
 */
static int recv_bookmarks(int peer_idx) {
    ompi_crcp_coord_pml_bookmark_proc_t *peer_ref;
    orte_process_name_t peer_name;
    orte_buffer_t * buffer = NULL;
    int ret;
    int exit_status = OMPI_SUCCESS;
    uint32_t tmp_int;

    /*
     * Find this peer in the structure
     */
    peer_name.cellid = orte_process_info.my_name->cellid;
    peer_name.jobid  = orte_process_info.my_name->jobid;
    peer_name.vpid   = peer_idx;

    if( NULL == (peer_ref = find_peer_by_orte_name(peer_name)) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_bookmarks: Could not find peer indexed %d\n",
                    peer_idx);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    /*
     * Receive the bookmarks from them
     */
    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if ( 0 > (ret = orte_rml.recv_buffer(&peer_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: recv_bookmarks: Bookmark Receive from peer [%lu,%lu,%lu] failed: Return %d\n",
                    ORTE_NAME_ARGS(&peer_name),
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

    opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_bookmarks: [%lu,%lu,%lu] Received bookmark (%4d,%4d,%4d) (%4d,%4d,%4d) from peer [%lu,%lu,%lu]\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name),
                        peer_ref->matched_send_msgs,
                        peer_ref->matched_isend_msgs,
                        peer_ref->matched_send_init_msgs,
                        peer_ref->matched_recv_msgs,
                        peer_ref->matched_irecv_msgs,
                        peer_ref->matched_recv_init_msgs,
                        ORTE_NAME_ARGS(&peer_name));

 cleanup:
    if( NULL != buffer)
        OBJ_RELEASE(buffer);

    return exit_status;
}

/*
 * Here we exchange bookmarks with ourself.
 * This should be a no-op, and is only here for
 * completeness.
 */
static int send_recv_local_bookmarks(int peer_idx) {
    /* No Op */
    return OMPI_SUCCESS;
}

/*
 * Check the bookmark numbers and resolve any mismatches that may have arisen
 */
static int ft_event_check_bookmarks(void) {
    opal_list_item_t* item = NULL;
    int ret;
    int p_n_to_p_m   = 0;
    int p_n_from_p_m = 0;

    if( 15 <= mca_crcp_coord_component.super.verbose ) {
        sleep(orte_process_info.my_name->vpid);
        opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                            "Process [%lu,%lu,%lu] Match Table",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
        opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                            "[%lu,%lu,%lu]  %5s | %7s | %7s | %7s | %7s |",
                            ORTE_NAME_ARGS(orte_process_info.my_name),
                            "Vpid", "T_Send", "M_Recv", "M_Send", "T_Recv");

        for(item  = opal_list_get_first(&ompi_crcp_coord_pml_procs);
            item != opal_list_get_end(&ompi_crcp_coord_pml_procs);
            item  = opal_list_get_next(item) ) {
            ompi_crcp_coord_pml_bookmark_proc_t *peer_ref;
            int t_send, m_send;
            int t_recv, m_recv;
            peer_ref = (ompi_crcp_coord_pml_bookmark_proc_t*)item;

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

            opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                "[%lu,%lu,%lu]  %5d | %7d | %7d | %7d | %7d |",
                                ORTE_NAME_ARGS(orte_process_info.my_name),
                                peer_ref->proc_name.vpid,
                                t_send, m_recv, m_send, t_recv);
        }
    }

    /*
     * For each peer:
     * - Check to make sure channel is quiet
     * - If not post necessary recvs
     */
    for(item  = opal_list_get_first(&ompi_crcp_coord_pml_procs);
        item != opal_list_get_end(&ompi_crcp_coord_pml_procs);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_coord_pml_bookmark_proc_t *peer_ref;
        peer_ref = (ompi_crcp_coord_pml_bookmark_proc_t*)item;
        
        if( 0 == (peer_ref->proc_name.vpid) % 2) {
            /* Check P_n --> P_m
             * Has the peer received all the messages that I have put on the wire?
             */
            p_n_to_p_m   = (peer_ref->total_send_msgs        +
                            peer_ref->total_isend_msgs       +
                            peer_ref->total_send_init_msgs   );
            p_n_from_p_m = (peer_ref->matched_recv_msgs      +
                            peer_ref->matched_irecv_msgs     +
                            peer_ref->matched_recv_init_msgs );
            if( p_n_to_p_m > p_n_from_p_m) {
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: check_bookmarks: [%lu,%lu,%lu] --> [%lu,%lu,%lu] "
                                    "Sent Msgs (%4d) = Received Msgs (%4d). Peer needs %4d.\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name),
                                    ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    );
                /*
                 * Tell the peer what the outstanding messages looked like.
                 * Since we can't tell which ones they are, we need to send the
                 * information for all of the messages since the last checkpoint
                 */
                if( OMPI_SUCCESS != (ret = ft_event_send_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: check_bookmarks: Unable to send message details to peer [%lu,%lu,%lu]: Return %d\n",
                                ORTE_NAME_ARGS(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }

            /* Check P_n <-- P_m
             * Have I received all the messages that my peer has put on the wire?
             */
            p_n_to_p_m   = (peer_ref->matched_send_msgs      +
                            peer_ref->matched_isend_msgs     +
                            peer_ref->matched_send_init_msgs );
            p_n_from_p_m = (peer_ref->total_recv_msgs        +
                            peer_ref->total_irecv_msgs       +
                            peer_ref->total_recv_init_msgs   );
            
            if( p_n_to_p_m > p_n_from_p_m) {
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: check_bookmarks: [%lu,%lu,%lu] <-- [%lu,%lu,%lu] "
                                    "Received Msgs (%4d) = Sent Msgs (%4d). I need %4d.\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name),
                                    ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    );
                /*
                 * Receive from peer the datatypes of the outstanding sends
                 *  As we figure out what they are post Irecv's for them into a drained buffer list.
                 */
                if( OMPI_SUCCESS != (ret = ft_event_recv_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: check_bookmarks: Unable to recv message details from peer [%lu,%lu,%lu]: Return %d\n",
                                ORTE_NAME_ARGS(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }
        }
        else { /* Odd */
            /* Check P_n <-- P_m
             * Have I received all the messages that my peer has put on the wire?
             */
            p_n_to_p_m   = (peer_ref->matched_send_msgs      +
                            peer_ref->matched_isend_msgs     +
                            peer_ref->matched_send_init_msgs );
            p_n_from_p_m = (peer_ref->total_recv_msgs        +
                            peer_ref->total_irecv_msgs       +
                            peer_ref->total_recv_init_msgs   );
            
            if( p_n_to_p_m > p_n_from_p_m) {
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: check_bookmarks: [%lu,%lu,%lu] <-- [%lu,%lu,%lu] "
                                    "Received Msgs (%4d) = Sent Msgs (%4d). I need %4d.\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name),
                                    ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    );
                /*
                 * Receive from peer the datatypes of the outstanding sends
                 *  As we figure out what they are post Irecv's for them into a drained buffer list.
                 */
                if( OMPI_SUCCESS != (ret = ft_event_recv_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: check_bookmarks: Unable to recv message details from peer [%lu,%lu,%lu]: Return %d\n",
                                ORTE_NAME_ARGS(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }

            /* Check P_n --> P_m
             * Has the peer received all the messages that I have put on the wire?
             */
            p_n_to_p_m   = (peer_ref->total_send_msgs        +
                            peer_ref->total_isend_msgs       +
                            peer_ref->total_send_init_msgs   );
            p_n_from_p_m = (peer_ref->matched_recv_msgs      +
                            peer_ref->matched_irecv_msgs     +
                            peer_ref->matched_recv_init_msgs );
            if( p_n_to_p_m > p_n_from_p_m) {
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: check_bookmarks: [%lu,%lu,%lu] --> [%lu,%lu,%lu] "
                                    "Sent Msgs (%4d) = Received Msgs (%4d). Peer needs %4d.\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name),
                                    ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    );
                /*
                 * Tell the peer what the outstanding messages looked like.
                 * Since we can't tell which ones they are, we need to send the
                 * information for all of the messages since the last checkpoint
                 */
                if( OMPI_SUCCESS != (ret = ft_event_send_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: check_bookmarks: Unable to send message details to peer [%lu,%lu,%lu]: Return %d\n",
                                ORTE_NAME_ARGS(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }
        }
    }
    return OMPI_SUCCESS;
}

/*
 * RML callback function called when a peer indicates that they
 * have received all of the messages it had marked as in-flight.
 */
static void drain_message_ack_cbfunc(int status,
                                     orte_process_name_t* sender,
                                     orte_buffer_t *buffer,
                                     orte_rml_tag_t tag,
                                     void* cbdata)
{
    int ret, exit_status = ORTE_SUCCESS;
    size_t ckpt_status;
    opal_list_item_t* item = NULL;

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
        ompi_crcp_coord_pml_drain_msg_ack_ref_t * drain_msg_ack;
        drain_msg_ack = (ompi_crcp_coord_pml_drain_msg_ack_ref_t*)item;
        
        /* If this ACK has not completed yet */
        if(!drain_msg_ack->complete) {
            /* If it is the correct peer */
            if(drain_msg_ack->peer.cellid == sender->cellid &&
               drain_msg_ack->peer.jobid  == sender->jobid &&
               drain_msg_ack->peer.vpid   == sender->vpid ) {
                /* We found it! */
                drain_msg_ack->complete = true;
                opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: drain_message_ack_cbfunc: [%lu,%lu,%lu] --> [%lu,%lu,%lu] Received ACK of FLUSH from peer\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name),
                                    ORTE_NAME_ARGS(sender) );
                return;
            }
        }
    }

    opal_output(mca_crcp_coord_component.super.output_handle,
                "crcp:coord: drain_message_ack_cbfunc: [%lu,%lu,%lu] --> [%lu,%lu,%lu] ERROR: Uable to match ACK to peer\n",
                ORTE_NAME_ARGS(orte_process_info.my_name),
                ORTE_NAME_ARGS(sender) );

 cleanup:
    return;
}

/*
 * Peer noticed that there was a message missing that we believe to have been sent
 * send them a list of message information so they can determine which one they are 
 * missing.
 */
static int ft_event_send_msg_details(ompi_crcp_coord_pml_bookmark_proc_t *peer_ref, int total_sent, int total_matched) {
    int ret, exit_status = OMPI_SUCCESS;
    opal_list_item_t* msg_item  = NULL;
    ompi_crcp_coord_pml_drain_msg_ack_ref_t * d_msg_ack = NULL;
    orte_buffer_t *buffer = NULL;
    int need;
    int32_t req_more = -1;
    int comm_my_rank;
    int ctr = 0;
    opal_list_t *search_list;
    int pass_num = 0;

    need = total_sent - total_matched;
    assert(need > 0);

    /*  For each sent_to_list
     *   Send messages not marked as done...
     * For more datatype magic see: osc_pt2pt_data_move.c:219ish
     */
    /* Try to match off the sent list first */
    search_list = &(peer_ref->send_list);
    pass_num = 1;

    SEARCH_AGAIN:
    for(msg_item  = opal_list_get_last(search_list);
        msg_item != opal_list_get_begin(search_list);
        msg_item  = opal_list_get_prev(msg_item) ) {
        ompi_crcp_coord_pml_message_ref_t * msg_ref;
        msg_ref = (ompi_crcp_coord_pml_message_ref_t*)msg_item;
        
        /*
         * If this message has been matched then proceed to the next message
         */
        if(msg_ref->matched) {
            continue;
        }

        if( NULL != buffer) {
            OBJ_RELEASE(buffer);
            buffer = NULL;
        }

        if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
            exit_status = OMPI_ERROR;
            goto cleanup;
        }

        /*
         * First send the communicator ID
         */
        comm_my_rank  = ompi_comm_rank(msg_ref->comm);

        /* Some Communicator/Rank Details */
        PACK_BUFFER(buffer, msg_ref->comm->c_contextid, 1, ORTE_UINT32,
                    "crcp:coord: send_msg_details: Unable to pack communicator ID");
        PACK_BUFFER(buffer, comm_my_rank, 1, ORTE_INT,
                    "crcp:coord: send_msg_details: Unable to pack comm rank ID");

        /* Message Details */
        PACK_BUFFER(buffer, msg_ref->done, 1, ORTE_BOOL,
                    "crcp:coord: send_msg_details: Unable to pack done flag");
        PACK_BUFFER(buffer, msg_ref->tag, 1, ORTE_INT,
                    "crcp:coord: send_msg_details: Unable to pack tag");
        PACK_BUFFER(buffer, msg_ref->count, 1, ORTE_SIZE,
                    "crcp:coord: send_msg_details: Unable to pack count");
        PACK_BUFFER(buffer, msg_ref->ddt_size, 1, ORTE_SIZE,
                    "crcp:coord: send_msg_details: Unable to pack datatype size");

        /*
         * Do the send...
         */
        if ( 0 > ( ret = orte_rml.send_buffer(&peer_ref->proc_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: send_msg_details: Unable to send message details to peer [%lu,%lu,%lu]: Return %d\n",
                        ORTE_NAME_ARGS(&peer_ref->proc_name),
                        ret);
            
            exit_status = OMPI_ERROR;
            goto cleanup;
        }
        
        if( NULL != buffer) {
            OBJ_RELEASE(buffer);
            buffer = NULL;
        }
            
        /*
         * See if the peer wants more results
         */
        if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
        
        /*
         * Recv the ACK msg
         */
        if ( 0 > (ret = orte_rml.recv_buffer(&peer_ref->proc_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: send_msg_details: [%lu,%lu,%lu] --> [%lu,%lu,%lu] Failed to receive ACK buffer from peer. Return %d\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name),
                        ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                        ret);
            exit_status = ret;
            goto cleanup;
        }
        
        /*
         * Pull out the communicator ID
         */
        UNPACK_BUFFER(buffer, req_more, 1, ORTE_UINT32,
                      "crcp:coord: send_msg_details: Failed to unpack the ACK from peer buffer.");
        
        /* Debug marker */
        msg_ref->matched = true;
        
        if(req_more == 0) {
            /* Need to send more results ... */
            opal_output_verbose(20, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: send_msg_details: [%lu,%lu,%lu] --> [%lu,%lu,%lu] Need to send more results to peer..."
                                "[%d,%d] (Need %d) -[%d]\n",
                                ORTE_NAME_ARGS(orte_process_info.my_name),
                                ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                                msg_ref->comm->c_contextid, pass_num,
                                need,
                                ctr);
        }
        else {
            /* Peer says that is all they need */
            opal_output_verbose(10, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: send_msg_details: [%lu,%lu,%lu] --> [%lu,%lu,%lu] Done sending results to peer... (Need %d) -[%d]\n",
                                ORTE_NAME_ARGS(orte_process_info.my_name),
                                ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                                need,
                                ctr);
            goto ALL_SENT;
        }
        ctr++;
    }

    /*
     * We tried to match off the send list, but not successful,
     * so match off the isend list
     */
    if( pass_num == 1 ) {

        search_list = &(peer_ref->isend_list);
        pass_num = 2;
        goto SEARCH_AGAIN;
    }
    /*
     * We tried to match off the send and isend list, but not successful, 
     * so match off the send_init list
     */
    else if(pass_num == 2) {

        search_list = &(peer_ref->send_init_list);
        pass_num = 3;
        goto SEARCH_AGAIN;
    }

 ALL_SENT:
    assert(req_more != 0);

    /* Prepare to post a Recv for the ACK All Clear signal from the peer
     * which is sent when they have finished receiving all of the 
     * inflight messages into a local buffer
     */
    d_msg_ack = OBJ_NEW(ompi_crcp_coord_pml_drain_msg_ack_ref_t);
    d_msg_ack->peer.cellid = peer_ref->proc_name.cellid;
    d_msg_ack->peer.jobid  = peer_ref->proc_name.jobid;
    d_msg_ack->peer.vpid   = peer_ref->proc_name.vpid;
    d_msg_ack->complete    = false;
    opal_list_append(&drained_msg_ack_list, &(d_msg_ack->super));

 cleanup:
    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    /* JJH: What is this doing for us? Getting us out of a blocking send? what about asych? */
    if( 0 != current_msg_id ) {
        stall_for_completion = true;
    }
    
    return exit_status;
}

/*
 * We need to drain messages. Receive a list of sends from the peer, and determine 
 * which ones we need to post.
 */
static int ft_event_recv_msg_details(ompi_crcp_coord_pml_bookmark_proc_t *peer_ref, int total_recv, int total_matched) {
    int ret, exit_status = OMPI_SUCCESS;
    int need;
    int found = 0;
    int32_t req_more = 0;
    orte_buffer_t * buffer = NULL;
    ompi_crcp_coord_pml_message_ref_t   *posted_msg_ref = NULL;
    opal_list_t *found_on_this_list = NULL;
    bool msg_found;
    bool msg_complete;
    bool msg_already_posted;
    int ctr = 0;

    /*
     * Until our peer has sent us everything that we need keep asking for more
     */
    need = total_recv - total_matched;
    assert(need > 0);

    while(need > found) {
        int      p_comm_rank;
        uint32_t p_comm_id;
        bool     p_msg_done;
        int      p_msg_tag;
        size_t   p_msg_count;
        size_t   p_msg_buffer_size;

        if( NULL != buffer) {
            OBJ_RELEASE(buffer);
            buffer = NULL;
        }
        
        if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }

        /*
         * Recv the msg
         */
        if ( 0 > (ret = orte_rml.recv_buffer(&peer_ref->proc_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_msg_details: [%lu,%lu,%lu] <-- [%lu,%lu,%lu] Failed to receive buffer from peer. Return %d\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name),
                        ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                        ret);
            exit_status = ret;
            goto cleanup;
        }

        /* Pull out the communicator ID */
        UNPACK_BUFFER(buffer, p_comm_id, 1, ORTE_UINT32,
                      "crcp:coord: recv_msg_details: Failed to unpack the communicator ID");
        UNPACK_BUFFER(buffer, p_comm_rank, 1, ORTE_INT,
                      "crcp:coord: recv_msg_details: Failed to unpack the communicator rank ID");

        /* Pull out the message details */
        UNPACK_BUFFER(buffer, p_msg_done, 1, ORTE_BOOL,
                      "crcp:coord: recv_msg_details: Failed to unpack the done flag");
        UNPACK_BUFFER(buffer, p_msg_tag, 1, ORTE_INT,
                      "crcp:coord: recv_msg_details: Failed to unpack the tag");
        UNPACK_BUFFER(buffer, p_msg_count, 1, ORTE_SIZE,
                      "crcp:coord: recv_msg_details: Failed to unpack the count");
        UNPACK_BUFFER(buffer, p_msg_buffer_size, 1, ORTE_SIZE,
                      "crcp:coord: recv_msg_details: Failed to unpack the datatype size");

        /*
         * Have we received this messages?
         *  - Yes: Mark as matched, continue looking
         *  - No : Put the message on the to be drained list, notify peer
         *  - In progress: This receive is currently in the process of being received, stall.
         *                 Will only reach this state inside a blocking recv.
         */
        if( OMPI_SUCCESS != (ret = ft_event_have_received_msg(p_comm_id, p_msg_tag, p_msg_count,
                                                              p_msg_buffer_size,
                                                              peer_ref, 
                                                              &posted_msg_ref,
                                                              &found_on_this_list,
                                                              &msg_found,          /* Did we find a matching recv for this message? */
                                                              &msg_complete,       /* Is the recv of the message already finished? */
                                                              &msg_already_posted) /* Has the recv already been posted? */
                             ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_msg_details: [%lu,%lu,%lu] -- [%lu,%lu,%lu] Failed to determine if we have received this message. Return %d\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name),
                        ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                        ret);
            exit_status = ret;
            goto cleanup;
        }

        opal_output_verbose(25, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: have_received: [%lu,%lu,%lu] -- [%lu,%lu,%lu]"
                            " t=%d, f=%d, found=%d, complete=%d, posted=%d, peer_rank=[%d vs %d]\n",
                            ORTE_NAME_ARGS(orte_process_info.my_name),
                            ORTE_NAME_ARGS(&(peer_ref->proc_name)),
                            true, false,
                            msg_found, msg_complete, msg_already_posted,
                            (NULL == posted_msg_ref ? -1 : posted_msg_ref->rank), peer_ref->proc_name.vpid);
        
        /*
         * The message was not found:
         *  - Create a new message to drain
         *  - notify peer of resolution of one message
         */
        if( !msg_found ) {
            ompi_proc_t *peer_ompi_proc = NULL;
            ompi_crcp_coord_pml_message_ref_t *d_msg = NULL;

            opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv_msg_details: [%lu,%lu,%lu] Found a message that needs to be posted\n",
                                ORTE_NAME_ARGS(orte_process_info.my_name) );

            /*
             * Construct message
             */
            peer_ompi_proc = ompi_proc_find(&(peer_ref->proc_name));

            CREATE_NEW_MSG(d_msg, 0, NULL, /* Setup the datatype outside of this */
                           p_msg_tag, p_comm_rank, ompi_comm_lookup(p_comm_id),
                           MCA_PML_BASE_SEND_COMPLETE, false, NULL,
                           peer_ref->proc_name.cellid,
                           peer_ref->proc_name.jobid,
                           peer_ref->proc_name.vpid);

            d_msg->count     = p_msg_count * p_msg_buffer_size;
            ompi_ddt_duplicate(&ompi_mpi_packed, &(d_msg->datatype));
            d_msg->ddt_size  = (d_msg->datatype)->size;

            d_msg->matched        = true;
            d_msg->done           = p_msg_done;
            d_msg->active         = false;
            d_msg->already_posted = false;

            /*
             * If we have not posted the receive yet regardless of if this is the sender is 
             * currently sending the message or not (d_msg->done = false) go 
             * ahead and post the receive to try and drain that message.
             */
            
            /*
             * Post the recv if is isn't already posted
             */
            /* Create a buffer of the necessary type/size */
            d_msg->buffer = (void *) malloc(d_msg->count * d_msg->ddt_size);

            /* Save it so we can post and poll on it later */
            opal_list_append(&drained_msg_list, &(d_msg->super));
            peer_ref->total_irecv_msgs += 1;

            ++found;
        }
        /*
         * The message was found, and complete. Do nothing
         */
        else if( msg_complete ) {
            ctr++;
        }
        /* 
         * The message was found, not complete, and already posted. (irecv)
         *  - Create a drain message
         *  - point the 'request' at it
         *  - Make sure not to post this message to be drained, but just wait on the request.
         */
        else if( msg_already_posted ) {
            ompi_crcp_coord_pml_message_ref_t *d_msg = NULL;

            opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: recv_msg_details: [%lu,%lu,%lu] Found a message [%lu] that is already posted by %d\n",
                                ORTE_NAME_ARGS(orte_process_info.my_name),
                                posted_msg_ref->msg_id,
                                (int)peer_ref->proc_name.vpid);

            /*
             * If the message has been posted, but not from this peer.
             *  This means that the message was matched off of the unknown_list (src = ANY_SOURCE)
             *  And since we need to make sure we drain the message from this peer,
             *    - Add the suggested peer flag to match this peer
             *    - Make sure we wait for this message to complete when draining messages
             */
            if (posted_msg_ref->rank != peer_ref->proc_name.vpid) {
                opal_output_verbose(1, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: recv_msg_details: [%lu,%lu,%lu] Found a message that is already posted by ANY_PEER. ****JJH****\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name) );
                posted_msg_ref->suggested_rank = p_comm_rank;
            }

            /*
             * This is the current blocking receive.
             * We need to stall so it can complete properly
             */
            if( !posted_msg_ref->async ) {
                opal_output_verbose(1, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: recv_msg_details: [%lu,%lu,%lu] Current blocking recv matched, stall for it to complete",
                                    ORTE_NAME_ARGS(orte_process_info.my_name));
                /* Set the sentinal value */
                stall_for_completion = true;
            }
            /*
             * Only increment the counter if we do not know it is a blocking recv
             */
            else {
                peer_ref->total_irecv_msgs += 1;
            }

            /* Set some dummy values along with the peer reference */
            CREATE_NEW_MSG(d_msg, p_msg_count, NULL,
                           p_msg_tag, p_comm_rank,
                           ompi_comm_lookup(p_comm_id), posted_msg_ref->mode,
                           posted_msg_ref->async, posted_msg_ref->request,
                           peer_ref->proc_name.cellid,
                           peer_ref->proc_name.jobid,
                           peer_ref->proc_name.vpid);

            d_msg->matched        = true;
            d_msg->done           = p_msg_done;
            d_msg->active         = true;
            d_msg->already_posted = true;

            /* Save it so we can post and poll on it later */
            opal_list_append(&drained_msg_list, &(d_msg->super));

            ++found;
        }
        else {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_msg_details: ***** ERROR ***** [%lu,%lu,%lu] Failed to find an action to use. This should never happen!\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name));
            return OMPI_ERROR;
        }

        /*
         * If we need more ask for more, 
         * ow tell peer we are done..
         */
        if( need > found) {
            /* Ask for more */
            req_more = 0;
        }
        else {
            /* Tell them we are all done */
            req_more = 1;
        }

        if( NULL != buffer) {
            OBJ_RELEASE(buffer);
            buffer = NULL;
        }

        if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
            exit_status = OMPI_ERROR;
            goto cleanup;
        }
        
        PACK_BUFFER(buffer, req_more, 1, ORTE_UINT32,
                    "crcp:coord: recv_msg_details: Unable to ask peer for more messages");
        
        if ( 0 > ( ret = orte_rml.send_buffer(&peer_ref->proc_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: recv_msg_details: Unable to send message details to peer [%lu,%lu,%lu]: Return %d\n",
                        ORTE_NAME_ARGS(&peer_ref->proc_name),
                        ret);
            
            exit_status = OMPI_ERROR;
            goto cleanup;
        }
    }

 cleanup:
    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    return exit_status;
}

/*
 * Determine if we have received this message yet or not
 */
static bool ft_event_have_received_msg(uint32_t comm_id, int tag, size_t count,
                                       size_t ddt_size,
                                       ompi_crcp_coord_pml_bookmark_proc_t *peer_ref,
                                       ompi_crcp_coord_pml_message_ref_t ** found_msg_ref,
                                       opal_list_t **found_on_this_list,
                                       bool *found, bool *complete, bool *already_posted)
{
    int ret;

    *found_on_this_list = NULL;
    *found_msg_ref  = NULL;
    *found          = false;
    *complete       = false;
    *already_posted = false;

    /*
     * Check the recv_list
     */
    if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->recv_list),
                                                  count, tag, INVALID_INT,
                                                  ddt_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE, FIND_MSG_UNKNOWN, FIND_MSG_UNKNOWN, FIND_MSG_TRUE
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_have_received_msg: Unable to find the proper message reference.\n");
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
                                                  ddt_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE, FIND_MSG_UNKNOWN, FIND_MSG_UNKNOWN, FIND_MSG_TRUE
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_have_received_msg: Unable to find the proper message reference.\n");
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
                                                  ddt_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE, FIND_MSG_TRUE, FIND_MSG_FALSE, FIND_MSG_TRUE
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(peer_ref->recv_init_list);
        goto FOUND;
    }

    if( OMPI_SUCCESS != (ret = find_message_named(&(peer_ref->recv_init_list),
                                                  count, tag, INVALID_INT,
                                                  ddt_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE, FIND_MSG_FALSE, FIND_MSG_TRUE, FIND_MSG_TRUE
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_have_received_msg: Unable to find the proper message reference.\n");
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
                                                  ddt_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE, FIND_MSG_UNKNOWN, FIND_MSG_UNKNOWN, FIND_MSG_TRUE
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_have_received_msg: Unable to find the proper message reference.\n");
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
                                                  ddt_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE, FIND_MSG_TRUE, FIND_MSG_FALSE, FIND_MSG_TRUE
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(unknown_persist_recv_list);
        goto FOUND;
    }

    if( OMPI_SUCCESS != (ret = find_message_named(&(unknown_persist_recv_list),
                                                  count, tag, INVALID_INT,
                                                  ddt_size,
                                                  found_msg_ref,
                                                  FIND_MSG_FALSE, FIND_MSG_FALSE, FIND_MSG_TRUE, FIND_MSG_TRUE
                                                  ) ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_have_received_msg: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    if( NULL != *found_msg_ref) {
        *found_on_this_list = &(unknown_persist_recv_list);
        goto FOUND;
    }

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
        *found_msg_ref  = NULL;
        *found          = false;
        *complete       = false;
        *already_posted = false;
    }
    
    return OMPI_SUCCESS;;
}

/*
 * Post the RML receives for when the peer says the channel is clear.
 */
static int ft_event_post_drain_acks(void) {
    opal_list_item_t* item = NULL;
    ompi_crcp_coord_pml_drain_msg_ack_ref_t * drain_msg_ack;
    size_t req_size;
    int ret;

    req_size  = opal_list_get_size(&drained_msg_ack_list);
    if(req_size <= 0) {
        return OMPI_SUCCESS;
    }

    /*
     * We have loaded our peer with the message information
     * Now wait for the ack from them
     */
    for(item  = opal_list_get_first(&drained_msg_ack_list);
        item != opal_list_get_end(&drained_msg_ack_list);
        item  = opal_list_get_next(item) ) {
        drain_msg_ack = (ompi_crcp_coord_pml_drain_msg_ack_ref_t*)item;

        /* Post the receive */
        if( OMPI_SUCCESS != (ret = orte_rml.recv_buffer_nb( &drain_msg_ack->peer,
                                                            OMPI_CRCP_COORD_BOOKMARK_TAG,
                                                            0,
                                                            drain_message_ack_cbfunc,
                                                            NULL) ) ) {
            opal_output(mca_crcp_coord_component.super.output_handle,
                        "crcp:coord: post_drain_acks: [%lu,%lu,%lu] Failed to post a RML receive to the peer\n",
                        ORTE_NAME_ARGS(orte_process_info.my_name));

            return ret;
        }
    }

    return OMPI_SUCCESS;
}

/*
 * Post all the outstanding receives. Irecv so we can poll on them later.
 */
static int ft_event_post_drained(void) {
    opal_list_item_t* item = NULL;
    size_t req_size;
    int ret;

    req_size  = opal_list_get_size(&drained_msg_list);
    if(req_size <= 0) {
        return OMPI_SUCCESS;
    }

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
                                "crcp:coord: post_drained: [%lu,%lu,%lu] Found a message that we don't need to post.\n",
                                ORTE_NAME_ARGS(orte_process_info.my_name));
            continue;
        }
        /*
         * Post a receive to drain this message
         */
        else {
            opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: post_drained: [%lu,%lu,%lu] Posting a message to be drained from %d.\n",
                                ORTE_NAME_ARGS(orte_process_info.my_name),
                                drain_msg->rank);
            if( OMPI_SUCCESS != (ret = wrapped_pml_module->pml_irecv(drain_msg->buffer, 
                                                                     drain_msg->count,
                                                                     drain_msg->datatype, 
                                                                     drain_msg->rank,
                                                                     drain_msg->tag,
                                                                     drain_msg->comm,
                                                                     &(drain_msg->request) ) ) ) {
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: post_drained: [%lu,%lu,%lu] Failed to post the Draining PML iRecv\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name) );
                return ret;
            }
        }
    }

    return OMPI_SUCCESS;
}

/*
 * Waiting for 
 * - all the outstanding Recvs posted by the protocol are complete
 * - all the peers that had outstanding Recvs posted 'all clear'
 */
static int ft_event_wait_quiesce(void) {
    int ret, exit_status = OMPI_SUCCESS;
    opal_list_item_t* item = NULL, *next = NULL;
    size_t req_size;
    bool found = false;
    int num_outstanding;
    bool prev_stall = false;
    ompi_crcp_coord_pml_message_ref_t * drain_msg;
    ompi_request_t ** wait_any_requests = NULL;
    ompi_status_public_t ** wait_any_status = NULL;
    size_t wait_any_count = 0;
    orte_process_name_t *proc_names = NULL;
    size_t i, last_idx;

    /*********************************************
     * Wait for all draining receives to complete
     **********************************************/
    req_size  = opal_list_get_size(&drained_msg_list);

    if(req_size > 0) {
        opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: wait_quiesce: [%lu,%lu,%lu]  Waiting on %d messages to drain\n",
                            ORTE_NAME_ARGS(orte_process_info.my_name),
                            (int)req_size);

        /*
         * Create an array of requests to wait upon
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

            proc_names[i].cellid = ORTE_CELLID_INVALID;
            proc_names[i].jobid  = ORTE_JOBID_INVALID;
            proc_names[i].vpid   = ORTE_VPID_INVALID;
        }

        /*
         * Generate a list of messages to wait upon
         */
        wait_any_count = 0;
        for(item  = opal_list_get_first(&drained_msg_list);
            item != opal_list_get_end(&drained_msg_list);
            item  = opal_list_get_next(item) ) {
            drain_msg = (ompi_crcp_coord_pml_message_ref_t*)item;
            
            opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: wait_quiesce: [%lu,%lu,%lu] <-- [%lu,%lu,%lu] Waiting on recv\n",
                                ORTE_NAME_ARGS(orte_process_info.my_name),
                                ORTE_NAME_ARGS(&(drain_msg->proc_name)) );

            /* This message has already been completed. */
            if( drain_msg->already_posted && NULL == drain_msg->request) {
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: wait_quiesce: [%lu,%lu,%lu] - [%lu,%lu,%lu] Already posted this msg\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name),
                                    ORTE_NAME_ARGS(&(drain_msg->proc_name)) );
            }
            /*
             * We know that is is not our current recv.
             * Regardless if this is the message that the peer is currently sending or not
             * we want to wait on the receive since the peer is waiting in progress if
             * we post the receive we will be able to drain it regardless of where the peer is.
             * Once we have drained the message then we tell the peer that the send has completed
             * via the OOB/RML. At which point the peer is free to continue. Once the coordination
             * process is complete then it will return to the user application the notification 
             * that the send has completed.
             *
             * All this means that we don't need to take action on drain_msg->send_done
             */
            /* If it is ours then wait for it to complete */
            else {
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: wait_quiesce: [%lu,%lu,%lu] - [%lu,%lu,%lu] Waiting on message. (index = %d)\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name),
                                    ORTE_NAME_ARGS(&(drain_msg->proc_name)),
                                    (int)wait_any_count);

                wait_any_requests[wait_any_count] = drain_msg->request;
                wait_any_status[wait_any_count]   = &drain_msg->status;
                wait_any_count++;
            }
        }

        /* Add process names */
        last_idx = 0;
        for(item  = opal_list_get_first(&drained_msg_list);
            item != opal_list_get_end(&drained_msg_list);
            item  = opal_list_get_next(item) ) {
            drain_msg = (ompi_crcp_coord_pml_message_ref_t*)item;

            /* Add proc to response queue if it is not already there */
            found = false;
            for(i = 0; i < last_idx; ++i) {
                if(proc_names[i].cellid == drain_msg->proc_name.cellid &&
                   proc_names[i].jobid  == drain_msg->proc_name.jobid &&
                   proc_names[i].vpid   == drain_msg->proc_name.vpid ) {
                    found = true;
                    break;
                }
            }
            if( !found ) {
                opal_output_verbose(15, mca_crcp_coord_component.super.output_handle,
                                    "crcp:coord: wait_quiesce: [%lu,%lu,%lu] - [%lu,%lu,%lu] Add process to response list [idx %d]\n",
                                    ORTE_NAME_ARGS(orte_process_info.my_name),
                                    ORTE_NAME_ARGS(&(drain_msg->proc_name)),
                                    (int)last_idx);
                
                proc_names[last_idx].cellid = drain_msg->proc_name.cellid;
                proc_names[last_idx].jobid  = drain_msg->proc_name.jobid;
                proc_names[last_idx].vpid   = drain_msg->proc_name.vpid;
                last_idx = last_idx + 1;
            }
        }

        /*
         * Wait all of the messages to complete in any order
         */
        prev_stall = opal_cr_stall_check;
        opal_cr_stall_check = true;
        for( i = 0; i < wait_any_count; ++i) {
            ompi_status_public_t   tmp_status;
            int wait_any_idx = 0;

            if( OMPI_SUCCESS != (ret = ompi_request_wait_any(wait_any_count,
                                                             wait_any_requests,
                                                             &wait_any_idx,
                                                             &tmp_status) ) ) {
                exit_status = ret;
                goto cleanup;
            }
            
            opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: wait_quiesce: [%lu,%lu,%lu]  Done with idx %d of %d\n",
                                ORTE_NAME_ARGS(orte_process_info.my_name),
                                (int)wait_any_idx, (int)wait_any_count);

            memcpy(wait_any_status[wait_any_idx], &tmp_status, sizeof(ompi_status_public_t) );
        }
        opal_cr_stall_check = prev_stall;

        opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: wait_quiesce: [%lu,%lu,%lu]  Send ACK to all Peers\n",
                            ORTE_NAME_ARGS(orte_process_info.my_name));

        /* If there were requests needed to be reaped, send ack to all peers */
        for(i = 0; i < last_idx; ++i) {
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
         * All other elements need to be there since we need to match them
         * as new receives come in.
         */
        for(item  = opal_list_get_first(&drained_msg_list);
            item != opal_list_get_end(&drained_msg_list);
            item  = next) {
            drain_msg = (ompi_crcp_coord_pml_message_ref_t*)item;
            next = opal_list_get_next(item);

            if( drain_msg->already_posted) {
                if( NULL != drain_msg->datatype) {
                    OBJ_RELEASE(drain_msg->datatype);
                    drain_msg->datatype = NULL;
                }
                if( NULL != drain_msg->request ) {
                    OBJ_RELEASE(drain_msg->request);
                    drain_msg->request = NULL;
                }
                opal_list_remove_item(&drained_msg_list, &(drain_msg->super) );
                OBJ_RELEASE(drain_msg);
            }
        }
    }

    /*******************************************************************
     * If we are waiting for All Clear messages from peers wait on them.
     *******************************************************************/
    num_outstanding = opal_list_get_size(&drained_msg_ack_list);
    if(num_outstanding > 0) {
        ompi_crcp_coord_pml_drain_msg_ack_ref_t * drain_msg_ack;

        opal_output_verbose(5, mca_crcp_coord_component.super.output_handle,
                            "crcp:coord: wait_quiesce: [%lu,%lu,%lu]  Waiting on %d Drain ACK messages\n",
                            ORTE_NAME_ARGS(orte_process_info.my_name),
                            num_outstanding);

        while(0 < num_outstanding) {
            for(item  = opal_list_get_first(&drained_msg_ack_list);
                item != opal_list_get_end(&drained_msg_ack_list);
                item = next) {
                drain_msg_ack = (ompi_crcp_coord_pml_drain_msg_ack_ref_t*)item;
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
        
        /* Clear the ack queue if it isn't already clear (it should be) */
        while (NULL != (item = opal_list_remove_first(&drained_msg_ack_list) ) ) {
            OBJ_RELEASE(item);
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

static int ft_event_finalize_exchange(void) {
    int ret;

    /*
     * End the 2-phase commit process
     */

    /*
     * Clear the bookmark totals
     */
    if( OMPI_SUCCESS != (ret = ft_event_clear_bookmarks() ) ) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: ft_event_finalize_exchange: Bookmark Release Failed %d",
                    ret);
        return ret;
    }

    return OMPI_SUCCESS;
}

/*
 * Find a peer with a specific name in the global peer list.
 */
static ompi_crcp_coord_pml_bookmark_proc_t * find_peer_by_orte_name(orte_process_name_t proc) {
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(&ompi_crcp_coord_pml_procs);
        item != opal_list_get_end(&ompi_crcp_coord_pml_procs);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_coord_pml_bookmark_proc_t *proc_ref;
        proc_ref = (ompi_crcp_coord_pml_bookmark_proc_t*)item;
        
        if(0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, 
                                       &proc_ref->proc_name,
                                       &proc)) {
            opal_output_verbose(30, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: find_peer_by_orte_name: Found Peer [%lu,%lu,%lu]\n",
                                ORTE_NAME_ARGS(&(proc)));
            return proc_ref;
        }
    }

    return NULL;
}

/*
 * Find a peer and communicator pair
 */
static int find_peer_in_comm_named(struct ompi_communicator_t* comm, int proc_idx, 
                                    ompi_crcp_coord_pml_bookmark_proc_t **peer_ref)
{
    /* Find the peer ref */
    *peer_ref = find_peer_by_orte_name(comm->c_remote_group->grp_proc_pointers[proc_idx]->proc_name);
    if(NULL == *peer_ref) {
        opal_output(mca_crcp_coord_component.super.output_handle,
                    "crcp:coord: find_peer_in_comm_named: Unable to find peer [%lu,%lu,%lu] (%d)\n",
                    ORTE_NAME_ARGS(&(comm->c_remote_group->grp_proc_pointers[proc_idx]->proc_name)),
                    (int)opal_list_get_size(&ompi_crcp_coord_pml_procs));
        return OMPI_ERROR;
    }
    
    return OMPI_SUCCESS;
}

/*
 * Find a message in the supplied list
 */
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
           (msg_ref->tag == MPI_ANY_TAG || msg_ref->tag == tag)   &&
           (peer         == INVALID_INT || msg_ref->rank  == peer)  &&
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

/*
 * Find a message that was previously drained that matches our specifics
 */
static int find_drained_message_named(size_t ddt_size,
                                      size_t count, int tag, int peer,
                                      ompi_crcp_coord_pml_message_ref_t ** found_msg_ref)
{
    opal_list_item_t* item = NULL;
    size_t loc_ddt_size;

    *found_msg_ref = NULL;

    if( 0 >= opal_list_get_size(&drained_msg_list) ) {
        /* Dummy check */
        return OMPI_SUCCESS;
    }
    
    for(item  = opal_list_get_first(&drained_msg_list);
        item != opal_list_get_end(&drained_msg_list);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_coord_pml_message_ref_t * drain_msg = NULL;

        drain_msg = (ompi_crcp_coord_pml_message_ref_t*)item;

        /* If the buffer is invalid then this is not a valid message or
         * has not been completed just yet */
        if(NULL == drain_msg->buffer) {
            continue;
        }

        /* If a specific tag was requested, then make sure this messages matches */
        if( tag != MPI_ANY_TAG && drain_msg->tag != tag) {
            continue;
        }

        /* If a specific rank was requested, then make sure this messages matches */
        if( peer != INVALID_INT ) {
            if( peer != MPI_ANY_SOURCE && drain_msg->rank != peer) {
                continue;
            }
        }

        /* Check the datatype size 
         * Make sure we call get_pack_description first so all variables are initalized.
         */
        loc_ddt_size = drain_msg->ddt_size;

        if(drain_msg->count == count && loc_ddt_size == ddt_size) {
            opal_output_verbose(30, mca_crcp_coord_component.super.output_handle,
                                "crcp:coord: find_drained_message_named: Found Message -- (%d, %d)\n",
                                tag, peer);
            *found_msg_ref = drain_msg;
            return OMPI_SUCCESS;
        }
    }

    return OMPI_SUCCESS;
}
