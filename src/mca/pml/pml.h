/*
 * $HEADER$
 */
/**
 * @file
 * 
 * P2P Management Layer (PML)
 *
 * An MCA component type that provides the P2P interface functionality required 
 * by the MPI layer. The PML is a relatively thin layer that primarily provides 
 * for the fragmentation and scheduling of messages over multiple transports 
 * (instances of the P2P Transport Layer (PTL) MCA component type) as depicted below:
 *
 *   ------------------------------------
 *   |                MPI               |
 *   ------------------------------------
 *   |                PML               |
 *   ------------------------------------
 *   | PTL (TCP) | PTL (SM) | PTL (...) |
 *   ------------------------------------
 *
 * A single PML module is selected by the MCA framework during library
 * initialization. Initially, all available PMLs are loaded (potentially 
 * as shared libraries) and their module open and init functions called.
 * The MCA framework selects the module returning the highest priority and
 * closes/unloads any other PML modules that may have been opened.
 *
 * After the PML is selected, the MCA framework loads and initalize
 * all available PTLs. The PML is notified of the selected PTLs via the
 * the mca_pml_base_add_ptls_fn_t downcall from the MCA.
 * 
 * After all of the MCA components are initialized, the MPI/RTE will make 
 * downcalls into the PML to provide the initial list of processes 
 * (lam_proc_t instances), and notification of changes (add/delete).
 * 
 * The PML module must select the set of PTL modules that are to be used
 * to reach a given destination. These should be cached on a PML specific
 * data structure that is hung off the lam_proc_t.
 *
 * The PML should then apply a scheduling algorithm (round-robin,
 * weighted distribution, etc), to schedule the delivery of messages
 * over the available PTLs.
 *
 */
                                                                                         
#ifndef MCA_PML_H
#define MCA_PML_H

#include "lam_config.h"
#include "include/lam.h"
#include "lfc/lam_list.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "request/request.h"
#include "mca/mca.h"
#include "mpi.h" /* needed for MPI_ANY_TAG */


/*
 * PML module types
 */

struct mca_ptl_t;
struct mca_ptl_addr_t;


typedef enum {
    MCA_PML_BASE_SEND_STANDARD,
    MCA_PML_BASE_SEND_BUFFERED,
    MCA_PML_BASE_SEND_SYNCHRONOUS,
    MCA_PML_BASE_SEND_READY
} mca_pml_base_send_mode_t;

/* renamed MPI constants */
#define LAM_ANY_TAG    MPI_ANY_TAG
#define LAM_ANY_SOURCE MPI_ANY_SOURCE
#define LAM_PROC_NULL  MPI_PROC_NULL

/**
 * MCA->PML Called by MCA framework to initialize the module.
 * 
 * @param priority (OUT) Relative priority or ranking used by MCA to
 * selected a module.
 *
 * @param allow_multi_user_threads (OUT) Whether this module can run
 * at MPI_THREAD_MULTIPLE or not.
 *
 * @param have_hidden_threads (OUT) Whether this module may use
 * hidden threads (e.g., progress threads) or not.
 */
typedef struct mca_pml_1_0_0_t * (*mca_pml_base_module_init_fn_t)(
    int *priority, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads);

typedef int (*mca_pml_base_module_finalize_fn_t)(void);

/**
 * PML module version and interface functions.
 */

struct mca_pml_base_module_1_0_0_t {
   mca_base_module_t pmlm_version;
   mca_base_module_data_1_0_0_t pmlm_data;
   mca_pml_base_module_init_fn_t pmlm_init;
   mca_pml_base_module_finalize_fn_t pmlm_finalize;
};
typedef struct mca_pml_base_module_1_0_0_t mca_pml_base_module_1_0_0_t;
typedef mca_pml_base_module_1_0_0_t mca_pml_base_module_t;


/**
 * MCA management functions.
 */


/**
 * Downcall from MPI/RTE layer when new processes are created.
 *
 * @param  procs   Array of new processes
 * @param  nprocs  Size of process array
 * @return         LAM_SUCCESS or failure status.
 *
 * Provides a notification to the PML that new processes have been
 * created, and provides the PML the opportunity to cache data
 * (e.g. list of PTLs to use) on the lam_proc_t data structure.
 */
typedef int (*mca_pml_base_add_procs_fn_t)(struct lam_proc_t **procs, size_t nprocs);


/**
 * Downcall from MPI/RTE layer when processes are terminated.
 *
 * @param  procs   Array of processes
 * @param  nprocs  Size of process array
 * @return         LAM_SUCCESS or failure status.
 *
 * Provides a notification to the PML that processes have 
 * gone away, and provides the PML the opportunity to cleanup
 * any data cached on the lam_proc_t data structure.
 */
typedef int (*mca_pml_base_del_procs_fn_t)(struct lam_proc_t **procs, size_t nprocs);


/**
 * Downcall from MCA layer after all PTLs have been loaded/selected.
 *
 * @param  ptls    List of selected PTLs
 * @return         LAM_SUCCESS or failure status.
 *
 * Provides a notification to the PML that processes have 
 * gone away, and provides the PML the opportunity to cleanup
 * any data cached on the lam_proc_t data structure.
 */
typedef int (*mca_pml_base_add_ptls_fn_t)(lam_list_t *ptls);


/**
 * MPI Interface Functions
 */


/**
 * Downcall from MPI layer when a new communicator is created.
 *
 * @param comm  Communicator
 * @return      LAM_SUCCESS or failure status.
 *
 * Provides the PML the opportunity to initialize/cache a data structure
 * on the communicator.
 */
typedef int (*mca_pml_base_add_comm_fn_t)(struct lam_communicator_t* comm);


/**
 * Downcall from MPI layer when a communicator is destroyed.
 *
 * @param comm  Communicator
 * @return      LAM_SUCCESS or failure status.
 *
 * Provides the PML the opportunity to cleanup any datastructures
 * associated with the communicator.
 */
typedef int (*mca_pml_base_del_comm_fn_t)(struct lam_communicator_t* comm);

/**
 *  Initialize a persistent receive request. 
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param src (IN)         Source rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 LAM_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_irecv_init_fn_t)(
    void *buf,                           
    size_t count,                         
    lam_datatype_t *datatype,              
    int src,
    int tag,                                
    struct lam_communicator_t* comm,
    struct lam_request_t **request           
);

/**
 *  Post a receive request. 
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param src (IN)         Source rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 LAM_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_irecv_fn_t)(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int src,
    int tag,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);

/**
 *  Post a receive and wait for completion. 
 *
 *  @param buf (IN)         User buffer
 *  @param count (IN)       Number of elements of the specified datatype
 *  @param datatype (IN)    User defined datatype
 *  @param src (IN)         Source rank w/in communicator
 *  @param tag (IN)         User defined tag
 *  @param comm (IN)        Communicator
 *  @param status (OUT)     Completion status
 *  @return                 LAM_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_recv_fn_t)(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int src,
    int tag,
    struct lam_communicator_t* comm,
    lam_status_public_t* status
);

/**
 *  Initialize a persistent send request. 
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param dst (IN)         Peer rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param mode (IN)        Send mode (STANDARD,BUFFERED,SYNCHRONOUS,READY)
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 LAM_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_isend_init_fn_t)(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);


/**
 *  Post a send request. 
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param dst (IN)         Peer rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param mode (IN)        Send mode (STANDARD,BUFFERED,SYNCHRONOUS,READY)
 *  @param comm (IN)        Communicator.
 *  @param request (OUT)    Request handle.
 *  @return                 LAM_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_isend_fn_t)(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);


/**
 *  Post a send request and wait for completion.
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       Number of elements of the specified datatype.
 *  @param datatype (IN)    User defined datatype.
 *  @param dst (IN)         Peer rank w/in communicator.
 *  @param tag (IN)         User defined tag.
 *  @param mode (IN)        Send mode (STANDARD,BUFFERED,SYNCHRONOUS,READY)
 *  @param comm (IN)        Communicator.
 *  @return                 LAM_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_send_fn_t)(
    void *buf,
    size_t count,
    lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct lam_communicator_t* comm
);

/**
 * Initiate one or more persistent requests.
 *
 * @param count    Number of requests
 * @param request  Array of persistent requests
 * @return         LAM_SUCCESS or failure status.
 */
typedef int (*mca_pml_base_start_fn_t)(
    size_t count,
    lam_request_t** requests
);

/**
 * Non-blocking test for request completion.
 *
 * @param count (IN)     Number of requests
 * @param request (IN)   Array of requests
 * @param index (OUT)    Index of first completed request.
 * @param complete (OUT) Flag indicating if index is valid (a request completed).
 * @param status (OUT)   Status of completed request.
 * @return               LAM_SUCCESS or failure status.
 *
 * Note that upon completion, the request is freed, and the
 * request handle at index set to NULL.
 */
typedef int (*mca_pml_base_test_fn_t)(
    size_t count,
    lam_request_t** requests,
    int *index,
    int *completed,
    lam_status_public_t* status
);


/**
 * Non-blocking test for request completion.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param completed (OUT) Flag indicating wether all requests completed.
 * @param statuses (OUT)  Array of completion statuses.
 * @return                LAM_SUCCESS or failure status.
 *
 * This routine returns completed==true if all requests have completed.
 * The statuses parameter is only updated if all requests completed. Likewise, 
 * the requests array is not modified (no requests freed), unless all requests 
 * have completed.
 */
typedef int (*mca_pml_base_test_all_fn_t)(
    size_t count,
    lam_request_t** requests,
    int *completed,
    lam_status_public_t* statuses
);


/**
 * Wait (blocking-mode) for one of N requests to complete.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param index (OUT)     Index into request array of completed request.
 * @param status (OUT)    Status of completed request.
 * @return                LAM_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_wait_fn_t)(
    size_t count,
    lam_request_t** request,
    int *index,
    lam_status_public_t* status
);


/**
 * Wait (blocking-mode) for all of N requests to complete.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param statuses (OUT)  Array of completion statuses.
 * @return                LAM_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_wait_all_fn_t)(
    size_t count,                
    lam_request_t** request,    
    lam_status_public_t *status
);


/**
 * Release resources held by a persistent mode request.
 *
 * @param request (IN)    Request
 * @return                LAM_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_free_fn_t)(
    lam_request_t** request
);


/**
 * A special NULL request handle.
 *
 * @param request (OUT)   Request
 * @return                LAM_SUCCESS or failure status.
 *
 */
typedef int (*mca_pml_base_null_fn_t)(
    lam_request_t** request
);


/**
 *  PML instance.
 */

struct mca_pml_1_0_0_t {

    /* downcalls from MCA to PML */
    mca_pml_base_add_procs_fn_t    pml_add_procs;
    mca_pml_base_del_procs_fn_t    pml_del_procs;
    mca_pml_base_add_ptls_fn_t     pml_add_ptls;

    /* downcalls from MPI to PML */
    mca_pml_base_add_comm_fn_t     pml_add_comm;
    mca_pml_base_del_comm_fn_t     pml_del_comm;
    mca_pml_base_irecv_init_fn_t   pml_irecv_init;
    mca_pml_base_irecv_fn_t        pml_irecv;
    mca_pml_base_recv_fn_t         pml_recv;
    mca_pml_base_isend_init_fn_t   pml_isend_init;
    mca_pml_base_isend_fn_t        pml_isend;
    mca_pml_base_send_fn_t         pml_send;
    mca_pml_base_start_fn_t        pml_start;
    mca_pml_base_test_fn_t         pml_test;
    mca_pml_base_test_all_fn_t     pml_test_all;
    mca_pml_base_wait_fn_t         pml_wait;
    mca_pml_base_wait_all_fn_t     pml_wait_all;
    mca_pml_base_free_fn_t         pml_free;
    mca_pml_base_null_fn_t         pml_null;
};
typedef struct mca_pml_1_0_0_t mca_pml_1_0_0_t;
typedef mca_pml_1_0_0_t mca_pml_t;
extern mca_pml_t mca_pml;

/*
 * Macro for use in modules that are of type pml v1.0.0
 */
#define MCA_PML_BASE_VERSION_1_0_0 \
  /* pml v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* pml v1.0 */ \
  "pml", 1, 0, 0

#endif /* MCA_PML_H */
