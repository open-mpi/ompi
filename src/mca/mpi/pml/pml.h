/*
 * $HEADER$
 */

#ifndef MCA_PML_H
#define MCA_PML_H

#include "lam_config.h"
#include "lam/lam.h"
#include "lam/lfc/list.h"
#include "mpi/communicator/communicator.h"
#include "mpi/datatype/datatype.h"
#include "mpi/request/request.h"
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

#define LAM_ANY_TAG MPI_ANY_TAG

/**
 * MCA->PML Called by MCA framework to initialize the module.
 * 
 * @param priority (OUT)   Relative priority or ranking used by MCA to selected a module.
 * @param thread_min (OUT) Minimum thread level supported by the module.
 * @param thread_max (OUT) Maximum thread level supported by the module.
 */

typedef struct mca_pml_1_0_0_t * (*mca_pml_base_module_init_fn_t)(
    int* priority, 
    int* min_thread, 
    int* max_thread);

/**
 * PML module version and interface functions.
 */

struct mca_pml_base_module_1_0_0_t {
   mca_base_module_t pmlm_version;
   mca_base_module_data_1_0_0_t pmlm_data;
   mca_pml_base_module_init_fn_t pmlm_init;
};
typedef struct mca_pml_base_module_1_0_0_t mca_pml_base_module_1_0_0_t;
typedef mca_pml_base_module_1_0_0_t mca_pml_base_module_t;


/*
 * PML instance interface functions and datatype
 */

typedef int (*mca_pml_base_add_comm_fn_t)(struct lam_communicator_t*);
typedef int (*mca_pml_base_del_comm_fn_t)(struct lam_communicator_t*);
typedef int (*mca_pml_base_add_procs_fn_t)(struct lam_proc_t **procs, size_t nprocs);
typedef int (*mca_pml_base_del_procs_fn_t)(struct lam_proc_t **procs, size_t nprocs);
typedef int (*mca_pml_base_add_ptls_fn_t)(struct mca_ptl_t **ptls, size_t nptls);
typedef int (*mca_pml_base_fini_fn_t)(void);
typedef int (*mca_pml_base_progress_fn_t)(void);

typedef int (*mca_pml_base_irecv_init_fn_t)(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int src,
    int tag,
    bool persistent,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);

typedef int (*mca_pml_base_irecv_fn_t)(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int src,
    int tag,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);

typedef int (*mca_pml_base_isend_init_fn_t)(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    bool persistent,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);

typedef int (*mca_pml_base_isend_fn_t)(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);

typedef int (*mca_pml_base_start_fn_t)(
    lam_request_t* request
);

typedef int (*mca_pml_base_test_fn_t)(
    lam_request_t** request,
    int count,
    int *completed
);

typedef int (*mca_pml_base_wait_fn_t)(
    lam_request_t* request,
    lam_status_public_t* status
);


/**
 *  PML instance interface functions.
 */

struct mca_pml_1_0_0_t {

    /* downcalls from MCA to PML */
    mca_pml_base_add_comm_fn_t     pml_add_comm;
    mca_pml_base_del_comm_fn_t     pml_del_comm;
    mca_pml_base_add_procs_fn_t    pml_add_procs;
    mca_pml_base_del_procs_fn_t    pml_del_procs;
    mca_pml_base_add_ptls_fn_t     pml_add_ptls;
    mca_pml_base_fini_fn_t         pml_fini;
    mca_pml_base_progress_fn_t     pml_progress;

    /* downcalls from MPI to PML */
    mca_pml_base_irecv_init_fn_t   pml_irecv_init;
    mca_pml_base_irecv_fn_t        pml_irecv;
    mca_pml_base_isend_init_fn_t   pml_isend_init;
    mca_pml_base_isend_fn_t        pml_isend;
    mca_pml_base_start_fn_t        pml_start;
    mca_pml_base_test_fn_t         pml_test;
    mca_pml_base_wait_fn_t         pml_wait;
};
typedef struct mca_pml_1_0_0_t mca_pml_1_0_0_t;
typedef mca_pml_1_0_0_t mca_pml_t;

/*
 * Macro for use in modules that are of type pml v1.0.0
 */
#define MCA_PML_BASE_VERSION_1_0_0 \
  /* pml v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* pml v1.0 */ \
  "pml", 1, 0, 0


/*
 * Global functions for MCA: overall PML open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_pml_base_open(void);
  int mca_pml_base_select(void);
  int mca_pml_base_close(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Globals
 */
extern int mca_pml_base_output;
extern lam_list_t mca_pml_base_modules_available;
extern mca_pml_base_module_t mca_pml_base_selected_module;
extern mca_pml_t mca_pml;

#endif /* MCA_PML_H */
