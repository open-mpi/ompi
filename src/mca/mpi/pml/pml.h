/*
 * $HEADER$
 */

#ifndef LAM_MCA_PML_H
#define LAM_MCA_PML_H

#include "lam/lam.h"
#include "lam/proc.h"
#include "lam/lfc/list.h"
#include "mca/mca.h"

/*
 * PML module functions
 */

typedef int (*mca_pml_query_fn_t)(int *priority);
typedef struct mca_pml_1_0_0 * (*mca_pml_init_1_0_0_fn_t)(
    struct lam_proc_t **procs, 
    int nprocs, 
    int *max_tag, 
    int *max_cid
);

/*
 * PML types
 */

typedef uint64_t mca_pml_sequence_t;
typedef uint64_t mca_pml_tstamp_t;
typedef lam_list_t mca_pml_queue_t;

typedef enum {
    MCA_PML_REQUEST_TYPE_RECV,
    MCA_PML_REQUEST_TYPE_SEND_STANDARD,
    MCA_PML_REQUEST_TYPE_SEND_BUFFERED,
    MCA_PML_REQUEST_TYPE_SEND_SYNCHRONOUS,
    MCA_PML_REQUEST_TYPE_SEND_READY
} mca_pml_request_type_t;


/*
 * PML interface functions
 */

struct lam_communicator_t;
struct lam_datatype_t;
struct lam_proc_t;
struct lam_request_t;

typedef int (*mca_pml_progress_fn_t)(mca_pml_tstamp_t);

typedef int (*mca_pml_isend_fn_t)(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int dest,
    int tag,
    struct lam_communicator_t* comm,
    mca_pml_request_type_t req_type,
    struct lam_request_t **request
);

typedef int (*mca_pml_addprocs_fn_t)(lam_proc_t **procs, int nprocs);


/*
 * PML module definition.
 */

struct mca_pml_module_1_0_0_t {

  mca_1_0_0_t super;
  mca_pml_query_fn_t pmlm_query;
  mca_pml_init_1_0_0_fn_t pmlm_init;

};
typedef struct mca_pml_module_1_0_0_t mca_pml_module_1_0_0_t;


/*
 * Struct that represents the common state and interface functions
 * provided by a PML.
 */
                                                                                                         
struct mca_pml_1_0_0_t {

  mca_pml_addprocs_fn_t pml_addprocs;
  mca_pml_isend_fn_t pml_isend;
  mca_pml_progress_fn_t pml_progress;

};
typedef struct mca_pml_1_0_0_t mca_pml_1_0_0_t;


/*
 * Set the default type to use version 1.1.0 of the PML
 */

typedef struct mca_pml_module_1_1_0_t mca_pml_module_t;
typedef struct mca_pml_1_1_0_t mca_pml_t;


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
 * Global instance of array of pointers to lam_ssi_rpi_t.  Will
 * effectively be filled in by configure.
 */

extern const mca_t **mca_pml_modules;

#endif /* LAM_MCA_PML_H */
