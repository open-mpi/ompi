/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_ELAN_H
#define MCA_PTL_ELAN_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "mem/free_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"

/*#include "elan/sys/init_sys.h"*/
/*#include "elan/elan.h"*/

#define MCA_PTL_ELAN_STATISTICS 0

/**
 * ELAN PTL module.
 */

struct mca_ptl_elan_state_t {

    /* User configurable parameters */

    char        *elan_version;   /**< Version of the elan library */
    uint64_t     elan_debug;     /**< elan debug tracing output */
    uint64_t     elan_traced;    /**< elan TRACE output */
    uint64_t     elan_flags;
    FILE         elan_debugfile;
    int          elan_signalnum;

#ifdef ENABLE_ELAN_MEMORY_ALLOCATOR
    size_t       main_size;   /**< size of Main memory allocator heap */
    size_t       elan_size;   /**< size of Elan memory allocator heap */
    void        *main_base;   /**< Main memory allocator heap base */
    void        *elan_base;   /**< Elan memory allocator heap base */
#endif

    /* other state parameters */

    int          elan_attached;    /**< 0 until elan_attach() called */
    unsigned int elan_vp;          /**< elan vpid, not ompi vpid */
    unsigned int elan_nvp;         /**< total # of elan vpid */
    int         *elan_localvps;    /**< mapping of localId to elan vp */
    int          elan_localid;   /**< # of local elan vpids */
    int          elan_numlocals;   /**< # of local elan vpids */
    int          elan_maxlocals;   /**< maximum # of local elan vpids */
    int          elan_nrails;      /**< # of rails elan vpids */
    int          elan_rmsid;       /**< rms resource id */
    long         elan_pagesize;
    pid_t        elan_pid;

    /* TODO:
     *   Even though the elan threads are not utilized for now. 
     *   We provide memory/state control structures for later extensions.
     *   A simple type casting of ELAN_ESTATE can bring
     *   the complete structure of the ELAN_EPRIVSATE.
     */
    void         *elan_cap;    /**< job capability */
    void         *elan_estate; /**< Elan state of the 0th rail */

#ifdef ELAN_COMP
    ELAN_CTX     *elan_ctx;    /**< Elan ctx of the 0th rail */
    ELAN_RAIL   **elan_rail;   /**< Rail control struct for all rails */
    RAIL        **all_rails;   /**< all rails */
    ELAN_ESTATE **all_estates; /**< elan (priv)states of all rails */
#endif
    ompi_mutex_t  state_lock;  /**< lock for elan state */
};
typedef struct mca_ptl_elan_state_t mca_ptl_elan_state_t;

struct mca_ptl_elan_module_1_0_0_t {

    mca_ptl_base_module_1_0_0_t super;       /**< base PTL module */

    /* These parameters does not provided good freedom,
     * It does not hurt to skip them */
#if  1
    int   elan_free_list_num;     /**< initial size of free lists */
    int   elan_free_list_max;     /**< maximum size of free lists */
    int   elan_free_list_inc;     /**< # to alloc when growing lists */
#endif

    /* 
     * We create our own simplified structure for managing elan state
     * although libelan already provides one. We do not need
     * all that tport, group structures.
     */
    struct mca_ptl_elan_state_t *elan_state;  /**< elan state */
    struct mca_ptl_elan_t **elan_ptls;  /**< array of available PTLs */
    size_t elan_num_ptls;               /**< number of ptls activated */

    ompi_list_t elan_reqs;        /**< all elan requests */
    ompi_list_t elan_prog_events; /**< events in progress */
    ompi_list_t elan_comp_events; /**< events completed, but to reclaim */

    ompi_free_list_t elan_events_free;  /**< free events */
    ompi_free_list_t elan_reqs_free;  /**< all elan requests */

    ompi_event_t elan_send_event;  /**< event structure for sends */
    ompi_event_t elan_recv_event;  /**< event structure for recvs */
    ompi_list_t  elan_procs;       /**< elan proc's */
    ompi_mutex_t elan_lock;        /**< lock for module state */
};
typedef struct mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module_1_0_0_t;

struct mca_ptl_elan_send_frag_t;
struct mca_ptl_elan_recv_frag_t;

extern mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module;

/**
 * Register ELAN module parameters with the MCA framework
 */
extern int  mca_ptl_elan_module_open (void);

/**
 * Any final cleanup before being unloaded.
 */
extern int  mca_ptl_elan_module_close (void);

/**
 * ELAN module initialization.
 * 
 * @param num_ptls (OUT)
 *        Number of PTLs returned in PTL array.
 * @param allow_multi_user_threads (OUT)  
 *        Flag indicating wether PTL supports user threads (TRUE)
 * @param have_hidden_threads (OUT)       
 *        Flag indicating whether PTL uses threads (TRUE)
 *
 */
extern mca_ptl_t **mca_ptl_elan_module_init (int *num_ptls,
                                             bool * multi_user_threads,
                                             bool * have_hidden_threads);

/**
 * ELAN module control.
 */
extern int  mca_ptl_elan_module_control (int param,
                                         void *value,
                                         size_t size);

/**
 * ELAN module progress.
 */
extern int  mca_ptl_elan_module_progress (mca_ptl_tstamp_t tstamp);

/**
 * ELAN PTL Interface
 */
struct mca_ptl_elan_t {

	/**< The elan progress related interface */

    mca_ptl_t   super;          /**< base PTL interface */

	/**< The following are elan-related control structures */

#ifdef ELAN_COMP
    ELAN_RAIL   *elan_rail;     /**< Pointer to this Rail */
    ELAN_CTX    *elan_ctx;      /**< Elan ctx of this rail */
#endif
    int         ptl_ni_local;   /**< PTL NI local rank */
    int         ptl_ni_total;   /**< PTL NI total */

    int         elan_sten_size; /**< sten packet len */
    int         elan_rdma_size; /**< qdma packet length */
    int         elan_qdma_size; /**< qdma packet length */

    int         sten_total;     /**< total sten descriptors */
    int         rdma_total;     /**< total rdma descriptors */
    int         qdma_total;     /**< total rdma descriptors */
                                
    int         sten_num;       /**< num of outstanding sten packets */
    int         rdma_num;       /**< num of outstanding rdma packets */
    int         qdma_num;       /**< num of outstanding rdma packets */
                                
    int         max_num_dmas;   /**< total rdma descriptors */
                                
    ompi_list_t elan_stens;     /**< used elan sten descriptors*/
    ompi_list_t elan_dmas;      /**< used elan dma descriptors*/
    ompi_list_t elan_rdmas;     /**< used elan rdma descriptors */
    ompi_list_t elan_frags;     /**< used elan fragments */

    ompi_free_list_t elan_dmas_free;   /**< free elan dma descriptors*/
    ompi_free_list_t elan_stens_free;  /**< free elan sten descriptors*/
    ompi_free_list_t elan_rdmas_free;  /**< free elan rdma descriptors */
    ompi_free_list_t elan_frags_free;  /**< free elan rdma fragments */

#if MCA_PTL_ELAN_STATISTICS        /* some statistics */
    size_t      ptl_bytes_sent;
    size_t      ptl_bytes_recv;
#endif

};
typedef struct mca_ptl_elan_t mca_ptl_elan_t;
extern mca_ptl_elan_t mca_ptl_elan;

/**
 * Cleanup any resources held by the PTL.
 * 
 * @param ptl  PTL instance.
 * @return     OMPI_SUCCESS or error status on failure.
 */
extern int  mca_ptl_elan_finalize (struct mca_ptl_t *ptl);


/**
 * PML->PTL notification of change in the process list.
 * 
 * @param ptl  (IN)
 * @param proc (IN)  
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 */
extern int  mca_ptl_elan_add_proc (struct mca_ptl_t *ptl,
                                   struct ompi_proc_t *proc,
                                   struct mca_ptl_base_peer_t **peer);

/**
 * PML->PTL notification of change in the process list.
 *
 * @param ptl  (IN)     PTL instance
 * @param proc (IN)    Peer process
 * @param peer (IN)    Peer addressing information.
 * @return             Status indicating if cleanup was successful
 */
extern int  mca_ptl_elan_del_proc (struct mca_ptl_t *ptl,
                                   struct ompi_proc_t *procs,
                                   struct mca_ptl_base_peer_t *addr);

/**
 * PML->PTL Allocate a send request from the PTL modules free list.
 *
 * @param ptl (IN)       PTL instance
 * @param request (OUT)  Pointer to allocated request.
 * @return               Status indicating if allocation was successful.
 */
extern int  mca_ptl_elan_req_alloc (struct mca_ptl_t *ptl,
									struct mca_ptl_base_send_request_t **);

/**
 * PML->PTL Return a send request to the PTL modules free list.
 *
 * @param ptl (IN)       PTL instance
 * @param request (IN)   Pointer to allocated request.
 */
extern void mca_ptl_elan_req_return (struct mca_ptl_t *ptl,
                                     struct mca_ptl_base_send_request_t *);

/**
 * PML->PTL Notification that a receive fragment has been matched.
 *
 * @param ptl (IN)          PTL instance
 * @param recv_frag (IN)    Receive fragment
 */
extern void mca_ptl_elan_matched (struct mca_ptl_t *ptl,
                                  struct mca_ptl_base_recv_frag_t *frag);

/**
 * PML->PTL Initiate an isend of the specified size.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via 
 *        mca_ptl_base_request_alloc_fn_t)
 * @param size (IN)              
 *        Number of bytes PML is requesting PTL to deliver
 * @param flags (IN)             
 *        Flags that should be passed to the peer via the message header.
 * @param request (OUT)          
 *        OMPI_SUCCESS if the PTL was able to queue one or more fragments
 */
extern int  mca_ptl_elan_isend (struct mca_ptl_t *ptl,
                                struct mca_ptl_base_peer_t *ptl_peer,
                                struct mca_ptl_base_send_request_t *,
                                size_t offset,
                                size_t * size,
                                int flags);

/**
 * PML->PTL Initiate an irecv of the specified size.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via 
 *        mca_ptl_base_request_alloc_fn_t)
 * @param size (IN)              
 *        Number of bytes PML is requesting PTL to deliver
 * @param flags (IN)             
 *        Flags that should be passed to the peer via the message header.
 * @param request (OUT)          
 *        OMPI_SUCCESS if the PTL was able to queue one or more fragments
 */
extern int  mca_ptl_elan_irecv (struct mca_ptl_t *ptl,
                                struct mca_ptl_base_peer_t *ptl_peer,
                                struct mca_ptl_base_send_request_t *,
                                size_t offset,
                                size_t * size,
                                int flags);

/**
 * PML->PTL Initiate a put of the specified size.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via 
 *        mca_ptl_base_request_alloc_fn_t)
 * @param size (IN)              
 *        Number of bytes PML is requesting PTL to deliver
 * @param flags (IN)             
 *        Flags that should be passed to the peer via the message header.
 * @param request (OUT)          
 *        OMPI_SUCCESS if the PTL was able to queue one or more fragments
 */
extern int  mca_ptl_elan_put (struct mca_ptl_t *ptl,
                              struct mca_ptl_base_peer_t *ptl_peer,
                              struct mca_ptl_base_send_request_t *,
                              size_t offset,
                              size_t * size,
                              int flags);

/**
 * PML->PTL Initiate a get of the specified size.
 *
 * @param ptl (IN)               PTL instance
 * @param ptl_base_peer (IN)     PTL peer addressing
 * @param send_request (IN/OUT)  Send request (allocated by PML via 
 *        mca_ptl_base_request_alloc_fn_t)
 * @param size (IN)              
 *        Number of bytes PML is requesting PTL to deliver
 * @param flags (IN)             
 *        Flags that should be passed to the peer via the message header.
 * @param request (OUT)          
 *        OMPI_SUCCESS if the PTL was able to queue one or more fragments
 */
extern int  mca_ptl_elan_get (struct mca_ptl_t *ptl,
                              struct mca_ptl_base_peer_t *ptl_peer,
                              struct mca_ptl_base_send_request_t *,
                              size_t offset,
                              size_t * size,
                              int flags);

/**
 * Return a recv fragment to the modules free list.
 *
 * @param ptl (IN)   PTL instance
 * @param frag (IN)  ELAN receive fragment
 */
extern void mca_ptl_elan_recv_frag_return (struct mca_ptl_t *ptl,
                                           struct mca_ptl_elan_recv_frag_t
                                           *frag);

/**
 * Return a send fragment to the modules free list.
 *
 * @param ptl (IN)   PTL instance
 * @param frag (IN)  ELAN send fragment
 */
extern void mca_ptl_elan_send_frag_return (struct mca_ptl_t *ptl,
                                           struct mca_ptl_elan_send_frag_t
                                           *frag);

#endif
