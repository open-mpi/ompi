/*
 * $HEADER$
 */

#ifndef MCA_PML_H
#define MCA_PML_H

#include "lam_config.h"

#include "lam/lam.h"
#include "lam/lfc/list.h"
#include "mpi/proc/proc.h"
#include "mpi/communicator/communicator.h"
#include "mpi/datatype/datatype.h"
#include "mpi/request/request.h"
#include "mca/mca.h"


/*
 * PML module functions
 */

typedef int (*mca_pml_base_query_fn_t)(int *priority, int *min_thread,
                                       int* max_thread);
typedef struct mca_pml_1_0_0_t * 
  (*mca_pml_base_init_1_0_0_fn_t)
  (struct lam_proc_t **procs, int nprocs, int *max_tag, int *max_cid);

/*
 * PML types
 */

typedef uint64_t mca_pml_base_sequence_t;
typedef uint64_t mca_pml_base_tstamp_t;
typedef lam_list_t mca_pml_base_queue_t;

typedef enum {
    MCA_PML_BASE_SEND_STANDARD,
    MCA_PML_BASE_SEND_BUFFERED,
    MCA_PML_BASE_SEND_SYNCHRONOUS,
    MCA_PML_BASE_SEND_READY
} mca_pml_base_send_type_t;


/*
 * PML interface functions
 */

typedef int (*mca_pml_base_progress_fn_t)(mca_pml_base_tstamp_t);

typedef int (*mca_pml_base_isend_fn_t)(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int dest,
    int tag,
    struct lam_communicator_t* comm,
    mca_pml_base_send_type_t type,
    struct lam_request_t **request
);

typedef int (*mca_pml_base_addprocs_fn_t)(lam_proc_t **procs, int nprocs);


/*
 * PML module definition.
 */

struct mca_pml_base_module_1_0_0_t {
  mca_base_module_t pmlm_version;
  mca_base_module_data_1_0_0_t pmlm_data;

  mca_pml_base_query_fn_t pmlm_query;
  mca_pml_base_init_1_0_0_fn_t pmlm_init;
};
typedef struct mca_pml_base_module_1_0_0_t mca_pml_base_module_1_0_0_t;


/*
 * Struct that represents the common state and interface functions
 * provided by a PML.
 */

struct mca_pml_1_0_0_t {
  mca_pml_base_addprocs_fn_t pml_addprocs;
  mca_pml_base_isend_fn_t pml_isend;
  mca_pml_base_progress_fn_t pml_progress;
};
typedef struct mca_pml_1_0_0_t mca_pml_1_0_0_t;


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
  int mca_pml_base_query(void);
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
