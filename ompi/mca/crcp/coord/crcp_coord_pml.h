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
#include "orte/mca/ns/ns.h"
#include "opal/runtime/opal_cr.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

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
     * Globally Defined Variables
     ***********************************/
    /*
     * List of Messages received from ANY_SOURCES
     *  -- ompi_crcp_coord_pml_message_ref_t
     */
    OMPI_MODULE_DECLSPEC extern opal_list_t unknown_recv_from_list;
    OMPI_MODULE_DECLSPEC extern opal_list_t unknown_persist_recv_list;

    /*
     * List of pending ACKs to drained messages
     *  -- ompi_crcp_coord_pml_drain_msg_ack_ref_t
     */
    OMPI_MODULE_DECLSPEC extern opal_list_t drained_msg_ack_list;

    /*
     * List of drained messages to match against
     *  -- ompi_crcp_coord_pml_message_ref_t
     */
    OMPI_MODULE_DECLSPEC extern opal_list_t drained_msg_list;

    /*
     * List of processes known
     *  -- ompi_crcp_coord_pml_bookmark_proc_t
     */
    OMPI_MODULE_DECLSPEC extern opal_list_t ompi_crcp_coord_pml_procs;

    /*
     * Message reference
     */
    struct ompi_crcp_coord_pml_message_ref_t {
        /** This is a list object */
        opal_list_item_t super;

        /** Sequence Number of this message */
        uint64_t msg_id;

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

        /** Message Mode */
        mca_pml_base_send_mode_t mode;

        /* Is this an asynchronous message */
        bool async;

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
    struct ompi_crcp_coord_pml_bookmark_proc_t {
        /** This is a list object */
        opal_list_item_t super;

        /** Name of peer */
        orte_process_name_t proc_name;

        /*
         * Just to control concurrent access to some of these counters,
         * and the PML
         */
        opal_mutex_t     lock;
        opal_condition_t cond;

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
    };
    typedef struct ompi_crcp_coord_pml_bookmark_proc_t ompi_crcp_coord_pml_bookmark_proc_t;
    
    OBJ_CLASS_DECLARATION(ompi_crcp_coord_pml_bookmark_proc_t);
    void ompi_crcp_coord_pml_bookmark_proc_construct(ompi_crcp_coord_pml_bookmark_proc_t *bkm_proc);
    void ompi_crcp_coord_pml_bookmark_proc_destruct( ompi_crcp_coord_pml_bookmark_proc_t *bkm_proc);

    struct ompi_crcp_coord_pml_state_t {
        ompi_crcp_base_pml_state_t super;
        ompi_crcp_base_pml_state_t *prev_ptr;
        ompi_crcp_coord_pml_bookmark_proc_t    *peer_ref;
        ompi_crcp_coord_pml_message_ref_t      *msg_ref;
    };
    typedef struct ompi_crcp_coord_pml_state_t ompi_crcp_coord_pml_state_t;

    /***************
     * A bit of locking, it's good for you
     ***************/
    /*
     * Any thread can call this when entering a critical section
     * This in not strictly a critical section, but a protected
     * section of code while checkpointing is occuring.
     */
#if OPAL_ENABLE_FT == 1
#define OMPI_CRCP_COORD_CS_ENTER(bvar, ctr, lock, cond) \
  {                                                  \
     opal_mutex_lock(&lock);                         \
     while( bvar ) {                                 \
         opal_condition_wait(&cond,                  \
                             &lock);                 \
     }                                               \
     ctr++;                                          \
     opal_mutex_unlock(&lock);                       \
  }
#else
#define OMPI_CRCP_COORD_CS_ENTER(bvar, ctr, lock, cond) ;
#endif

    /*
     * Any thread can call this when exiting a critical section
     */
#if OPAL_ENABLE_FT == 1
#define OMPI_CRCP_COORD_CS_EXIT(ctr, lock, cond)    \
  {                                                 \
     opal_mutex_lock(&lock);                        \
     ctr--;                                         \
     opal_condition_signal(&cond);                  \
     opal_mutex_unlock(&lock);                      \
  }
#else
#define OMPI_CRCP_COORD_CS_EXIT(ctr, lock, cond) ;
#endif

    /*
     * Checkpoint protocol calls this to restrict processes
     * from entering a specific critical section.
     */
#if OPAL_ENABLE_FT == 1
#define OMPI_CRCP_COORD_CS_RESTRICT(bvar, ctr, lock, cond, wait, dbg_str)  \
  {                                                      \
     opal_mutex_lock(&lock);                             \
     bvar = true;                                        \
     while(ctr > 0 && wait) {                            \
        opal_condition_wait(&cond,                       \
                            &lock);                      \
     }                                                   \
     opal_mutex_unlock(&lock);                           \
  }
#else
#define OMPI_CRCP_COORD_CS_RESTRICT(bvar, ctr, lock, cond, wait, dbg_str) ;
#endif

    /*
     * Checkpoint protocol calls this to release all the blocking
     * threads so that they may enter the critical section.
     */
#if OPAL_ENABLE_FT == 1
#define OMPI_CRCP_COORD_CS_RELEASE(bvar, lock, cond)   \
  {                                                    \
     opal_mutex_lock(&lock);                           \
     bvar = false;                                     \
     opal_condition_signal(&cond);                     \
     opal_mutex_unlock(&lock);                         \
  }
#else
#define OMPI_CRCP_COORD_CS_RELEASE(bvar, lock, cond) ;
#endif


    /*
     * Some short cuts.
     */
    OPAL_DECLSPEC extern opal_mutex_t     ompi_crcp_coord_ft_global_cs_lock;
    OPAL_DECLSPEC extern opal_condition_t ompi_crcp_coord_ft_global_cs_cond;
    OPAL_DECLSPEC extern int              ompi_crcp_coord_ft_global_cs_count;
    OPAL_DECLSPEC extern bool             ompi_crcp_coord_ft_global_cs_block;

    OPAL_DECLSPEC extern opal_mutex_t     ompi_crcp_coord_ft_send_cs_lock;
    OPAL_DECLSPEC extern opal_condition_t ompi_crcp_coord_ft_send_cs_cond;
    OPAL_DECLSPEC extern int              ompi_crcp_coord_ft_send_cs_count;
    OPAL_DECLSPEC extern bool             ompi_crcp_coord_ft_send_cs_block;

    OPAL_DECLSPEC extern opal_mutex_t     ompi_crcp_coord_ft_recv_cs_lock;
    OPAL_DECLSPEC extern opal_condition_t ompi_crcp_coord_ft_recv_cs_cond;
    OPAL_DECLSPEC extern int              ompi_crcp_coord_ft_recv_cs_count;
    OPAL_DECLSPEC extern bool             ompi_crcp_coord_ft_recv_cs_block;

#if OPAL_ENABLE_FT == 1 && OPAL_ENABLE_FT_THREAD == 1
    /* Global stuff */
#define OMPI_CRCP_COORD_FT_GLOBAL_INIT()                                 \
  {                                                                      \
    OBJ_CONSTRUCT(&ompi_crcp_coord_ft_global_cs_lock, opal_mutex_t);     \
    OBJ_CONSTRUCT(&ompi_crcp_coord_ft_global_cs_cond, opal_condition_t); \
    ompi_crcp_coord_ft_global_cs_count = 0;                              \
    ompi_crcp_coord_ft_global_cs_block = false;                          \
  }

#define OMPI_CRCP_COORD_FT_GLOBAL_FINALIZE()          \
  {                                                   \
    OBJ_DESTRUCT(&ompi_crcp_coord_ft_global_cs_lock); \
    OBJ_DESTRUCT(&ompi_crcp_coord_ft_global_cs_cond); \
    ompi_crcp_coord_ft_global_cs_count = 0;           \
    ompi_crcp_coord_ft_global_cs_block = false;       \
  }

#define OMPI_CRCP_COORD_FT_GLOBAL_CS_ENTER()                   \
  OMPI_CRCP_COORD_CS_ENTER(ompi_crcp_coord_ft_global_cs_block, \
                           ompi_crcp_coord_ft_global_cs_count, \
                           ompi_crcp_coord_ft_global_cs_lock,  \
                           ompi_crcp_coord_ft_global_cs_cond);

#define OMPI_CRCP_COORD_FT_GLOBAL_CS_EXIT()                    \
  OMPI_CRCP_COORD_CS_EXIT(ompi_crcp_coord_ft_global_cs_count,  \
                          ompi_crcp_coord_ft_global_cs_lock,   \
                          ompi_crcp_coord_ft_global_cs_cond);

#define OMPI_CRCP_COORD_FT_GLOBAL_CS_RESTRICT(wait)               \
  OMPI_CRCP_COORD_CS_RESTRICT(ompi_crcp_coord_ft_global_cs_block, \
                              ompi_crcp_coord_ft_global_cs_count, \
                              ompi_crcp_coord_ft_global_cs_lock,  \
                              ompi_crcp_coord_ft_global_cs_cond,  \
                              wait,                               \
                              "CRCP GLOBAL");

#define OMPI_CRCP_COORD_FT_GLOBAL_CS_RELEASE()                   \
  OMPI_CRCP_COORD_CS_RELEASE(ompi_crcp_coord_ft_global_cs_block, \
                             ompi_crcp_coord_ft_global_cs_lock,  \
                             ompi_crcp_coord_ft_global_cs_cond);

    /* Send stuff */
#define OMPI_CRCP_COORD_FT_SEND_INIT()                                 \
  {                                                                    \
    OBJ_CONSTRUCT(&ompi_crcp_coord_ft_send_cs_lock, opal_mutex_t);     \
    OBJ_CONSTRUCT(&ompi_crcp_coord_ft_send_cs_cond, opal_condition_t); \
    ompi_crcp_coord_ft_send_cs_count = 0;                              \
    ompi_crcp_coord_ft_send_cs_block = false;                          \
  }

#define OMPI_CRCP_COORD_FT_SEND_FINALIZE()          \
  {                                                 \
    OBJ_DESTRUCT(&ompi_crcp_coord_ft_send_cs_lock); \
    OBJ_DESTRUCT(&ompi_crcp_coord_ft_send_cs_cond); \
    ompi_crcp_coord_ft_send_cs_count = 0;           \
    ompi_crcp_coord_ft_send_cs_block = false;       \
  }

#define OMPI_CRCP_COORD_FT_SEND_CS_ENTER()                   \
  OMPI_CRCP_COORD_CS_ENTER(ompi_crcp_coord_ft_send_cs_block, \
                           ompi_crcp_coord_ft_send_cs_count, \
                           ompi_crcp_coord_ft_send_cs_lock,  \
                           ompi_crcp_coord_ft_send_cs_cond);

#define OMPI_CRCP_COORD_FT_SEND_CS_EXIT()                    \
  OMPI_CRCP_COORD_CS_EXIT(ompi_crcp_coord_ft_send_cs_count,  \
                          ompi_crcp_coord_ft_send_cs_lock,   \
                          ompi_crcp_coord_ft_send_cs_cond);

#define OMPI_CRCP_COORD_FT_SEND_CS_RESTRICT(wait)               \
  OMPI_CRCP_COORD_CS_RESTRICT(ompi_crcp_coord_ft_send_cs_block, \
                              ompi_crcp_coord_ft_send_cs_count, \
                              ompi_crcp_coord_ft_send_cs_lock,  \
                              ompi_crcp_coord_ft_send_cs_cond,  \
                              wait,                             \
                              "CRCP SEND");

#define OMPI_CRCP_COORD_FT_SEND_CS_RELEASE()                   \
  OMPI_CRCP_COORD_CS_RELEASE(ompi_crcp_coord_ft_send_cs_block, \
                             ompi_crcp_coord_ft_send_cs_lock,  \
                             ompi_crcp_coord_ft_send_cs_cond);

    /* Receive stuff */
#define OMPI_CRCP_COORD_FT_RECV_INIT()                                 \
  {                                                                    \
    OBJ_CONSTRUCT(&ompi_crcp_coord_ft_recv_cs_lock, opal_mutex_t);     \
    OBJ_CONSTRUCT(&ompi_crcp_coord_ft_recv_cs_cond, opal_condition_t); \
    ompi_crcp_coord_ft_recv_cs_count = 0;                              \
    ompi_crcp_coord_ft_recv_cs_block = false;                          \
  }

#define OMPI_CRCP_COORD_FT_RECV_FINALIZE()          \
  {                                                 \
    OBJ_DESTRUCT(&ompi_crcp_coord_ft_recv_cs_lock); \
    OBJ_DESTRUCT(&ompi_crcp_coord_ft_recv_cs_cond); \
    ompi_crcp_coord_ft_recv_cs_count = 0;           \
    ompi_crcp_coord_ft_recv_cs_block = false;       \
  }

#define OMPI_CRCP_COORD_FT_RECV_CS_ENTER()                   \
  OMPI_CRCP_COORD_CS_ENTER(ompi_crcp_coord_ft_recv_cs_block, \
                           ompi_crcp_coord_ft_recv_cs_count, \
                           ompi_crcp_coord_ft_recv_cs_lock,  \
                           ompi_crcp_coord_ft_recv_cs_cond);

#define OMPI_CRCP_COORD_FT_RECV_CS_EXIT()                    \
  OMPI_CRCP_COORD_CS_EXIT(ompi_crcp_coord_ft_recv_cs_count,  \
                          ompi_crcp_coord_ft_recv_cs_lock,   \
                          ompi_crcp_coord_ft_recv_cs_cond);

#define OMPI_CRCP_COORD_FT_RECV_CS_RESTRICT(wait)               \
  OMPI_CRCP_COORD_CS_RESTRICT(ompi_crcp_coord_ft_recv_cs_block, \
                              ompi_crcp_coord_ft_recv_cs_count, \
                              ompi_crcp_coord_ft_recv_cs_lock,  \
                              ompi_crcp_coord_ft_recv_cs_cond,  \
                              wait,                             \
                              "CRCP RECV");

#define OMPI_CRCP_COORD_FT_RECV_CS_RELEASE()                   \
  OMPI_CRCP_COORD_CS_RELEASE(ompi_crcp_coord_ft_recv_cs_block, \
                             ompi_crcp_coord_ft_recv_cs_lock,  \
                             ompi_crcp_coord_ft_recv_cs_cond);

#else

#define OMPI_CRCP_COORD_FT_GLOBAL_INIT()      ;
#define OMPI_CRCP_COORD_FT_GLOBAL_FINALIZE()  ;
#define OMPI_CRCP_COORD_FT_GLOBAL_CS_ENTER()  ;
#define OMPI_CRCP_COORD_FT_GLOBAL_CS_EXIT()   ;
#define OMPI_CRCP_COORD_FT_GLOBAL_CS_RESTRICT(wait)  ;
#define OMPI_CRCP_COORD_FT_GLOBAL_CS_RELEASE() ;

#define OMPI_CRCP_COORD_FT_SEND_INIT()      ;
#define OMPI_CRCP_COORD_FT_SEND_FINALIZE()  ;
#define OMPI_CRCP_COORD_FT_SEND_CS_ENTER()  ;
#define OMPI_CRCP_COORD_FT_SEND_CS_EXIT()   ;
#define OMPI_CRCP_COORD_FT_SEND_CS_RESTRICT(wait)  ;
#define OMPI_CRCP_COORD_FT_SEND_CS_RELEASE() ;

#define OMPI_CRCP_COORD_FT_RECV_INIT()      ;
#define OMPI_CRCP_COORD_FT_RECV_FINALIZE()  ;
#define OMPI_CRCP_COORD_FT_RECV_CS_ENTER()  ;
#define OMPI_CRCP_COORD_FT_RECV_CS_EXIT()   ;
#define OMPI_CRCP_COORD_FT_RECV_CS_RESTRICT(wait)  ;
#define OMPI_CRCP_COORD_FT_RECV_CS_RELEASE() ;


#endif /* ENABLE_FT */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_CRCP_COORD_PML_EXPORT_H */
