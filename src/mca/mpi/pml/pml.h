/*
 * $HEADER$
 */

#ifndef LAM_MCA_PML_H
#define LAM_MCA_PML_H

#include "mca/mca.h"
#include "proc.h"
#include "lam.h"
#include "lam/lfc/list.h"


/*
 * PML module functions
 */

typedef int (*mca_pml_query_fn_t)(int *priority);
typedef struct mca_pml_1_0_0 * (*mca_pml_init_1_0_0_fn_t)(
    struct lam_proc **procs, 
    int nprocs, 
    int *max_tag, 
    int *max_cid
);


/*
 * PML interface functions
 */

typedef enum {
    MCA_PTM_REQUEST_TYPE_RECV,
    MCA_PTM_REQUEST_TYPE_SEND_STANDARD,
    MCA_PTM_REQUEST_TYPE_SEND_BUFFERED,
    MCA_PTM_REQUEST_TYPE_SEND_SYNCHRONOUS,
    MCA_PTM_REQUEST_TYPE_SEND_READY
} mca_pml_request_type_t;


typedef int (*mca_pml_progress_fn_t)(lam_time_t);

typedef int (*mca_pml_isend_fn_t)(
    void *buf,
    size_t size,
    lam_datatype_t *datatype,
    int dest,
    int tag,
    lam_communicator_t* comm,
    mca_pml_request_type_t req_type,
    lam_request_t **request
);

typedef int (*mci_pma_addprocs_fn_t)(lam_proc_t **procs, int nprocs);


/*
 * PML module definition.
 */

typedef struct mca_pml_module_1_0_0 {
  mca_1_0_0_t mp_meta_info;

  /* pml API function pointers */

  mca_pml_query_fn_t mp_query;
  mca_pml_init_1_0_0_fn_t mp_init;
} mca_pml_module_1_0_0_t;


/*
 * Struct that represents the common state and interface functions
 * provided by a PML.
 */
                                                                                                         
typedef struct mca_pml_1_0_0 {

  mca_pml_addprocs_fn_t pml_addprocs;
  mca_pml_isend_fn_t pml_isend;
  mca_pml_progress_fn_t pml_progress;

} mca_pml_1_0_0_t;


/*
 * Set the default type to use version 1.1.0 of the PML
 */

typedef mca_pml_module_1_1_0_t mca_pml_t;
typedef mca_pml_1_1_0_t mca_pml_t;


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
