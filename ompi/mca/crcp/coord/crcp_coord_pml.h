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

/**
 * @file
 * 
 * Coord CRCP component
 *
 */

#ifndef MCA_CRCP_COORD_PML_EXPORT_H
#define MCA_CRCP_COORD_PML_EXPORT_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "ompi/mca/crcp/crcp.h"
#include "ompi/communicator/communicator.h"
#include "opal/runtime/opal_cr.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "ompi/class/ompi_free_list.h"

#include "ompi/mca/crcp/coord/crcp_coord.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /*
     * PML Coordination functions
     */
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_enable
    ( bool enable, ompi_crcp_base_pml_state_t* pml_state );

    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_add_comm
    ( struct ompi_communicator_t* comm, 
      ompi_crcp_base_pml_state_t* pml_state );
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_del_comm
    ( struct ompi_communicator_t* comm, 
      ompi_crcp_base_pml_state_t* pml_state );

    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_add_procs
    ( struct ompi_proc_t **procs, size_t nprocs, 
      ompi_crcp_base_pml_state_t* pml_state );
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_del_procs
    ( struct ompi_proc_t **procs, size_t nprocs, 
      ompi_crcp_base_pml_state_t* pml_state );

    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_progress
    (ompi_crcp_base_pml_state_t* pml_state);
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_iprobe
    (int dst, int tag, struct ompi_communicator_t* comm, 
     int *matched, ompi_status_public_t* status, 
     ompi_crcp_base_pml_state_t* pml_state );

    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_probe
    ( int dst, int tag, struct ompi_communicator_t* comm, 
      ompi_status_public_t* status, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_isend_init
    ( void *buf, size_t count, ompi_datatype_t *datatype, 
      int dst, int tag, mca_pml_base_send_mode_t mode, 
      struct ompi_communicator_t* comm, 
      struct ompi_request_t **request, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_isend
    ( void *buf, size_t count, ompi_datatype_t *datatype, 
      int dst, int tag, mca_pml_base_send_mode_t mode, 
      struct ompi_communicator_t* comm, 
      struct ompi_request_t **request, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_send
    (  void *buf, size_t count, ompi_datatype_t *datatype, 
       int dst, int tag, mca_pml_base_send_mode_t mode, 
       struct ompi_communicator_t* comm, 
       ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_irecv_init
    ( void *buf, size_t count, ompi_datatype_t *datatype, 
      int src, int tag, struct ompi_communicator_t* comm,
      struct ompi_request_t **request, 
      ompi_crcp_base_pml_state_t* pml_state);
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_irecv
    ( void *buf, size_t count, ompi_datatype_t *datatype, 
      int src, int tag, struct ompi_communicator_t* comm, 
      struct ompi_request_t **request, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_recv
    (  void *buf, size_t count, ompi_datatype_t *datatype, 
       int src, int tag, struct ompi_communicator_t* comm,  
       ompi_status_public_t* status, 
       ompi_crcp_base_pml_state_t* pml_state);
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_dump
    ( struct ompi_communicator_t* comm, int verbose, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_start
    ( size_t count, ompi_request_t** requests, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_coord_pml_ft_event
    (int state, ompi_crcp_base_pml_state_t* pml_state);

    /*
     * Request function
     */
    int ompi_crcp_coord_request_complete(struct ompi_request_t *request);

    /***********************************
     * Globally Defined Structures
     ***********************************/
    /*
     * Types of Messages
     */
    enum ompi_crcp_coord_pml_message_type_t {
        COORD_MSG_TYPE_UNKNOWN, /* 0 Unknown type      */
        COORD_MSG_TYPE_B_SEND,  /* 1 Blocking Send     */
        COORD_MSG_TYPE_I_SEND,  /* 2 Non-Blocking Send */
        COORD_MSG_TYPE_P_SEND,  /* 3 Persistent  Send  */
        COORD_MSG_TYPE_B_RECV,  /* 4 Blocking Recv     */
        COORD_MSG_TYPE_I_RECV,  /* 5 Non-Blocking Recv */
        COORD_MSG_TYPE_P_RECV   /* 6 Persistent  Recv  */
    };
    typedef enum ompi_crcp_coord_pml_message_type_t ompi_crcp_coord_pml_message_type_t;

    /*
     * Message Reference
     */
    struct ompi_crcp_coord_pml_message_ref_t {
        /** This is a list object */
        opal_list_item_t super;

        /** Sequence Number of this message */
        uint64_t msg_id;

        /** Type of message this references */
        ompi_crcp_coord_pml_message_type_t msg_type;


        /** Buffer for data */
        void * buffer;

        /** Count for data */
        size_t count;

        /** Datatype */
        struct ompi_datatype_t * datatype;

        /** Quick reference to the size of the datatype */
        size_t ddt_size;

        /** Message Tag */
        int tag;

        /** Peer rank to which it was sent/recv'ed if known */
        int rank;

        /** Communicator pointer */
        ompi_communicator_t* comm;

        /** Receive Request */
        ompi_request_t *request;

        /** Status */
        ompi_status_public_t status;


        /** Peer which we received from */
        orte_process_name_t proc_name;

        /* Sample movement of values (mirrored for send):
         *                     Recv()   iRecv()  irecv_init()  start()  req_complete()
         *   * Pre:
         *     matched        = false   false    false         ---      ---
         *     done           = false   false    false         ---      true
         *     active         = true    true     false         true     false
         *     already_posted = true    true     true          ---      ---
         *   * Post:
         *     matched        = false   false    false         ---      ---
         *     done           = true    false    false         false    true
         *     active         = false   true     false         true     false
         *     already_posted = true    true     true          ---      ---
         *   * Drain
         *     already_posted = false -> true when posted irecv
         */
        /** Has this message been matched by the peer?
         * true = peer confirmed the receipt of this message
         * false = unknown if peer has received this message or not
         */
        bool matched;

        /** Is this message complete WRT PML semantics?
         * true = message done on this side (send or receive)
         * false = message still in process (sending or receiving)
         */
        bool done;

        /** Is the message actively being worked on?
         * true = Message is !done, and is in the progress cycle
         * false = Message is !done and is *not* in the progress cycle ( [send/recv]_init requests)
         */
        bool active;

        /** Has this message been posted?
         * true = message was posted (Send or recv)
         * false = message was not yet posted.
         *   Used when trying to figure out which messages the drain protocol needs to post, and
         *   which message have already been posted for it.
         */
        bool already_posted;

        /** Suggested Rank that this should be matched to
         * This is used when rank = ANY_SOURCE and we need to 
         * drain it to a specific peer
         */
        int suggested_rank;
    };
    typedef struct ompi_crcp_coord_pml_message_ref_t ompi_crcp_coord_pml_message_ref_t;
    
    OBJ_CLASS_DECLARATION(ompi_crcp_coord_pml_message_ref_t);
    void ompi_crcp_coord_pml_message_ref_construct(ompi_crcp_coord_pml_message_ref_t *msg_ref);
    void ompi_crcp_coord_pml_message_ref_destruct( ompi_crcp_coord_pml_message_ref_t *msg_ref);

    /*
     * A structure for a single process
     * Contains:
     *  - List of sent messages to this peer
     *  - List of received message from this peer
     *  - Message totals
     */
    struct ompi_crcp_coord_pml_peer_ref_t {
        /** This is a list object */
        opal_list_item_t super;

        /** Name of peer */
        orte_process_name_t proc_name;

        /** List of messages sent to this peer */
        opal_list_t send_list;      /**< pml_send       */
        opal_list_t isend_list;     /**< pml_isend      */
        opal_list_t send_init_list; /**< pml_isend_init */

        /** List of messages recved from this peer */
        opal_list_t recv_list;      /**< pml_recv       */
        opal_list_t irecv_list;     /**< pml_irecv      */
        opal_list_t recv_init_list; /**< pml_irecv_init */

        /* 
         * These are totals over all communicators provided for convenience.
         *
         * If we are P_n and this structure represent P_m then:
         *  - total_*   = P_n --> P_m
         *  - matched_* = P_n <-- P_m
         * Where P_n --> P_m means:
         *  the number of messages P_n knows that it has sent/recv to/from P_m
         * And P_n --> P_m means:
         *  the number of messages P_m told us that is has sent/recv to/from P_n
         *
         * How total* are used:
         * Send:
         *   Before put on the wire: ++total
         * Recv:
         *   Once completed: ++total
         */
        /** Total Number of messages sent */
        uint32_t  total_send_msgs;
        uint32_t  total_isend_msgs;
        uint32_t  total_send_init_msgs;
        uint32_t  matched_send_msgs;
        uint32_t  matched_isend_msgs;
        uint32_t  matched_send_init_msgs;
        
        /** Total Number of messages received */
        uint32_t  total_recv_msgs;
        uint32_t  total_irecv_msgs;
        uint32_t  total_recv_init_msgs;
        uint32_t  matched_recv_msgs;
        uint32_t  matched_irecv_msgs;
        uint32_t  matched_recv_init_msgs;

        /** Total Number of messages drained */
        uint32_t  total_drained_msgs;
    };
    typedef struct ompi_crcp_coord_pml_peer_ref_t ompi_crcp_coord_pml_peer_ref_t;
    
    OBJ_CLASS_DECLARATION(ompi_crcp_coord_pml_peer_ref_t);
    void ompi_crcp_coord_pml_peer_ref_construct(ompi_crcp_coord_pml_peer_ref_t *bkm_proc);
    void ompi_crcp_coord_pml_peer_ref_destruct( ompi_crcp_coord_pml_peer_ref_t *bkm_proc);

    /*
     * Local version of the PML state
     */
    struct ompi_crcp_coord_pml_state_t {
        ompi_crcp_base_pml_state_t p_super;
        ompi_crcp_base_pml_state_t *prev_ptr;

        ompi_crcp_coord_pml_peer_ref_t     *peer_ref;
        ompi_crcp_coord_pml_message_ref_t  *msg_ref;
    };
    typedef struct ompi_crcp_coord_pml_state_t ompi_crcp_coord_pml_state_t;
    OBJ_CLASS_DECLARATION(ompi_crcp_coord_pml_state_t);

    /***********************************
     * Globally Defined Variables
     ***********************************/
    /*
     * List of known peers
     */
    extern opal_list_t ompi_crcp_coord_pml_peer_refs;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_CRCP_COORD_PML_EXPORT_H */
