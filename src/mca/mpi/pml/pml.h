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
 * PML module functions
 */

typedef struct mca_pml_t * (*mca_pml_base_init_fn_t)(int* priority, int* min_thread, int* max_thread);
typedef int (*mca_pml_base_fini_fn_t)(void);

/*
 * PML types
 */

struct mca_ptl_t;

typedef uint64_t mca_pml_base_tstamp_t;

typedef enum {
    MCA_PML_BASE_SEND_STANDARD,
    MCA_PML_BASE_SEND_BUFFERED,
    MCA_PML_BASE_SEND_SYNCHRONOUS,
    MCA_PML_BASE_SEND_READY
} mca_pml_base_send_mode_t;


struct mca_pml_base_status_t {
};
typedef struct mca_pml_base_status_t mca_pml_base_status_t;

#define LAM_ANY_TAG MPI_ANY_TAG


/*
 * PML interface functions
 */

typedef int (*mca_pml_base_add_comm_fn_t)(struct lam_communicator_t*);
typedef int (*mca_pml_base_del_comm_fn_t)(struct lam_communicator_t*);
typedef int (*mca_pml_base_add_procs_fn_t)(struct lam_proc_t **procs, size_t nprocs);
typedef int (*mca_pml_base_del_procs_fn_t)(struct lam_proc_t **procs, size_t nprocs);
typedef int (*mca_pml_base_add_ptls_fn_t)(struct mca_ptl_t **ptls, size_t nptls);
typedef int (*mca_pml_base_progress_fn_t)(mca_pml_base_tstamp_t);

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
    mca_pml_base_status_t* status
);

/*
 * PML module definition.
 */

struct mca_pml_base_module_1_0_0_t {
   mca_base_module_t pmlm_version;
   mca_base_module_data_1_0_0_t pmlm_data;
   mca_pml_base_init_fn_t pmlm_init;
};
typedef struct mca_pml_base_module_1_0_0_t mca_pml_base_module_1_0_0_t;


/*
 * Struct that represents the common state and interface functions
 * provided by a PML.
 */

struct mca_pml_t {

    mca_pml_base_add_comm_fn_t     pml_add_comm;
    mca_pml_base_del_comm_fn_t     pml_del_comm;
    mca_pml_base_add_procs_fn_t    pml_add_procs;
    mca_pml_base_del_procs_fn_t    pml_del_procs;
    mca_pml_base_add_ptls_fn_t     pml_add_ptls;
    mca_pml_base_fini_fn_t         pml_fini;

    mca_pml_base_irecv_init_fn_t   pml_irecv_init;
    mca_pml_base_irecv_fn_t        pml_irecv;
    mca_pml_base_isend_init_fn_t   pml_isend_init;
    mca_pml_base_isend_fn_t        pml_isend;
    mca_pml_base_progress_fn_t     pml_progress;
    mca_pml_base_start_fn_t        pml_start;
    mca_pml_base_test_fn_t         pml_test;
    mca_pml_base_wait_fn_t         pml_wait;

};
typedef struct mca_pml_t mca_pml_t;

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
  int mca_pml_base_close(void);
  int mca_pml_base_open(lam_cmd_line_t *cmd);
  int mca_pml_base_init(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Public variables
 */

extern lam_list_t *mca_pml_base_opened;
extern lam_list_t *mca_pml_base_available;

/*
 * Global instance of array of pointers to mca_base_module_t.  Will
 * effectively be filled in by configure.
 */

extern const mca_base_module_t **mca_pml_base_modules;

#endif /* MCA_PML_H */
