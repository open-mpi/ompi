/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_GM_H
#define MCA_PTL_GM_H

#include "class/ompi_free_list.h"
#include "mca/ptl/ptl.h"

#define MCA_PTL_GM_STATISTICS 0
#define THRESHOLD 16384 
#define MAX_RECV_TOKENS 256
#define PTL_GM_ADMIN_SEND_TOKENS 0
#define PTL_GM_ADMIN_RECV_TOKENS 0
#define GM_SIZE  30
#define NUM_RECV_FRAGS 256 
#define MCA_PTL_GM_FRAG_CACHED
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * GM PTL component
     */
    struct mca_ptl_gm_component_t {
        mca_ptl_base_component_1_0_0_t super; /**< base PTL module */
        struct mca_ptl_gm_module_t **gm_ptl_modules; /**< array of available PTL modules */
        size_t      gm_num_ptl_modules;      /**< number of ptls actually used */
        size_t      gm_max_ptl_modules;      /**< maximum number of ptls - available */
        uint32_t    gm_max_port_number;      /**< maximum number of ports by board */
        uint32_t    gm_max_boards_number;    /**< maximum number of boards on the node */
        uint32_t    gm_free_list_num;        /**< initial size of free lists */
        uint32_t    gm_free_list_max;        /**< maximum size of free lists */
        uint32_t    gm_free_list_inc;        /**< number of elements to alloc when growing free lists */
	uint32_t    gm_segment_size;         /**< size of the allocated segment */
        uint32_t    gm_eager_limit;          /**< number of bytes before the rendez-vous protocol. If the
                                              **< size of the message is less than this number then GM 
                                              **< use a eager protocol.  */
        uint32_t    gm_rdma_frag_size;       /**< maximum fragment size used to transfer data over RDMA */
        char*       gm_port_name;            /**< the name used to get the port */

        struct mca_ptl_gm_proc_t* gm_local;
        ompi_list_t gm_procs;
        ompi_list_t gm_send_req;

        ompi_mutex_t gm_lock;                 /**< lock for accessing module state */
    };

    typedef struct mca_ptl_gm_component_t mca_ptl_gm_component_t;
    extern mca_ptl_gm_component_t mca_ptl_gm_component;

    /**
     * GM PTL Interface
     */
    struct mca_ptl_gm_module_t {
        mca_ptl_base_module_t super;    /**< base PTL module interface */
        struct gm_port *gm_port;
        unsigned int local_id;
        unsigned int global_id;
        unsigned int port_id;
        unsigned int num_send_tokens;
        unsigned int num_recv_tokens;
        unsigned int max_send_tokens;
        unsigned int max_recv_tokens;
        void*        gm_send_dma_memory; /**< pointer to the send DMA registered memory attached to the PTL */
        void*        gm_recv_dma_memory; /**< pointer to the recv DMA registered memory attached to the PTL */
        struct mca_ptl_gm_send_frag_t* gm_send_fragments;
        struct mca_ptl_gm_recv_frag_t* gm_recv_fragments;

        ompi_free_list_t gm_send_frags;
        ompi_free_list_t gm_send_dma_frags;
        ompi_free_list_t gm_recv_frags_free;
        ompi_list_t gm_send_frags_queue;
        ompi_list_t gm_pending_acks;
        ompi_list_t gm_recv_outstanding_queue;

        ompi_thread_t thread;
#if MCA_PTL_GM_STATISTICS
        size_t      ptl_bytes_sent;
        size_t      ptl_bytes_recv;
#endif
    };

    typedef struct mca_ptl_gm_module_t mca_ptl_gm_module_t;
    extern mca_ptl_gm_module_t mca_ptl_gm_module;

    /**
     * Register GM module parameters with the MCA framework
     */
    extern int  mca_ptl_gm_component_open (void);

    /**
     * Any final cleanup before being unloaded.
     */
    extern int  mca_ptl_gm_component_close (void);

    /**
     * GM module initialization.
     * 
     * @param num_ptls (OUT)                  Number of PTLs returned in PTL array.
     * @param allow_multi_user_threads (OUT)  Flag indicating wether PTL supports user threads (TRUE)
     * @param have_hidden_threads (OUT)       Flag indicating wether PTL uses threads (TRUE)
     */
    extern mca_ptl_base_module_t **mca_ptl_gm_component_init (int *num_ptl_modules,
                                                              bool * allow_multi_user_threads,
                                                              bool * have_hidden_threads);

    /**
     * GM module control.
     */
    extern int  mca_ptl_gm_component_control (int param, void *value, size_t size);

    /**
     * GM module progress.
     */
    extern int  mca_ptl_gm_component_progress (mca_ptl_tstamp_t tstamp);

    /**
     *  GM put
     */
    extern int  mca_ptl_gm_put( struct mca_ptl_base_module_t *ptl,
                                struct mca_ptl_base_peer_t *ptl_peer,
                                struct mca_pml_base_send_request_t *sendreq,
                                size_t offset, size_t size, int flags);

    /**
     *  GM get
     */
    extern int  mca_ptl_gm_get (struct mca_ptl_base_module_t *ptl,
                                struct mca_ptl_base_peer_t *ptl_peer,
                                struct mca_pml_base_recv_request_t *sendreq,
                                size_t offset, size_t size, int flags);

    /**
     * PML->PTL notification of change in the process list.
     * 
     * @param ptl (IN)
     * @param nprocs (IN)     Number of processes
     * @param procs (IN)      Set of processes
     * @param peers (OUT)     Set of (optional) peer addressing info.
     * @param peers (IN/OUT)  Set of processes that are reachable via this PTL.
     * @return     OMPI_SUCCESS or error status on failure.
     * 
     */
    extern int  mca_ptl_gm_add_procs (struct mca_ptl_base_module_t *ptl,
                                      size_t nprocs,
                                      struct ompi_proc_t **procs,
                                      struct mca_ptl_base_peer_t **peers,
                                      struct ompi_bitmap_t * reachable);

    /**
     * PML->PTL notification of change in the process list.
     *
     * @param ptl (IN)     PTL instance
     * @param nproc (IN)   Number of processes.
     * @param procs (IN)   Set of processes.
     * @param peers (IN)   Set of peer data structures.
     * @return             Status indicating if cleanup was successful
     *
     */
    extern int  mca_ptl_gm_del_procs( struct mca_ptl_base_module_t *ptl,
                                      size_t nprocs,
                                      struct ompi_proc_t **procs,
                                      struct mca_ptl_base_peer_t **peers );

    /**
     * PML->PTL Allocate a send request from the PTL modules free list.
     *
     * @param ptl (IN)       PTL instance
     * @param request (OUT)  Pointer to allocated request.
     * @return               Status indicating if allocation was successful.
     *
     */
    extern int  mca_ptl_gm_request_init( struct mca_ptl_base_module_t* ptl,
                                         struct mca_pml_base_send_request_t* req);

    /**
     *
     */
    extern void mca_ptl_gm_request_fini( struct mca_ptl_base_module_t *ptl,
                                         struct mca_pml_base_send_request_t* req);

    /**
     * PML->PTL Notification that a receive fragment has been matched.
     *
     * @param ptl (IN)          PTL instance
     * @param recv_frag (IN)    Receive fragment
     *
     */
    extern void mca_ptl_gm_matched (struct mca_ptl_base_module_t *ptl,
                                    struct mca_ptl_base_recv_frag_t *frag);

    /**
     *
     */
    extern int  mca_ptl_gm_finalize (struct mca_ptl_base_module_t *ptl);

    /**
     * Internally allocate memory for the unexpected messages. We will manage a list
     * of such buffers in order to avoid too many memory allocations.
     */
    extern char* gm_get_local_buffer( void );
    extern void gm_release_local_buffer( char* ptr );

    union mca_ptl_base_header_t;
    void mca_ptl_gm_dump_header( char* str, union mca_ptl_base_header_t* hdr );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
