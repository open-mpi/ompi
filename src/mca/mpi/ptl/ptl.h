/*
 * $HEADER$
 */

#ifndef LAM_MCA_PTL_H
#define LAM_MCA_PTL_H

#include "mca/mca.h"
#include "lam/lam.h"
#include "lam/lfc/list.h"
#include "mpi/proc/proc.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/pml/base/pml_base_sendreq.h"


/*
 * Set the default type to use version 1.0.0 of the PTL 
 */

typedef struct mca_ptl_module_1_0_0_t mca_ptl_module_t;
typedef struct mca_ptl_1_0_0_t mca_ptl_t;


/*
 * PTL module functions.
 */

typedef struct mca_ptl_1_0_0_t** (*mca_ptl_init_1_0_0_fn_t)(int* num_ptls, int *thread_min, int *thread_max);
typedef int (*mca_ptl_fini_1_0_0_fn_t)(struct mca_ptl_1_0_0_t*);
                                                                                                         
/*
 * PTL action functions.
 */

typedef mca_pml_base_send_request_t* (*mca_ptl_request_alloc_fn_t)(mca_ptl_t*);
typedef int (*mca_ptl_fragment_fn_t)(mca_ptl_t*, mca_pml_base_send_request_t*, size_t);
typedef int (*mca_ptl_progress_fn_t)(mca_ptl_t*, mca_pml_base_tstamp_t);

/*
 * Struct used to pass PTL module information from the each PTL 
 * instance back to the MCA framework.
 */

struct mca_ptl_module_1_0_0_t {
  mca_base_module_t ptlm_version;
  mca_base_module_data_1_0_0_t ptlm_data;
  mca_ptl_init_1_0_0_fn_t ptlm_init;
};
typedef struct mca_ptl_module_1_0_0_t mca_ptl_module_1_0_0_t;

/*
 * Struct that represents the common state and interface functions
 * provided by a PTL.
 */

struct mca_ptl_1_0_0_t {

    /* PTL common attributes */
    size_t       ptl_frag_first_size;     /* maximum size of first fragment */
    size_t       ptl_frag_min_size;       /* threshold below which the CDI will not fragment */
    size_t       ptl_frag_max_size;       /* maximum fragment size supported by the CDI */
    uint32_t     ptl_endpoint_latency;    /* relative/absolute measure of latency */
    uint64_t     ptl_endpoint_bandwidth;  /* bandwidth (bytes/sec) supported by each endpoint */
    size_t       ptl_endpoint_count;      /* number endpoints supported by this CDI */

    /* PTL function table */
    mca_ptl_request_alloc_fn_t ptl_request_alloc;
    mca_ptl_fragment_fn_t ptl_fragment;
    mca_ptl_progress_fn_t ptl_progress;

};
typedef struct mca_ptl_1_0_0_t mca_ptl_1_0_0_t;

/*
 * Global functions for MCA: overall PTL open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_ptl_base_close(void);
  int mca_ptl_base_open(lam_cmd_line_t *cmd);
  int mca_ptl_base_query(void);
  int mca_ptl_base_init(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Public variables
 */

extern lam_list_t *mca_ptl_base_opened;
extern lam_list_t *mca_ptl_base_available;

/*
 * Global instance of array of pointers to mca_t.  Will
 * effectively be filled in by configure.
 */

extern const mca_base_module_t **mca_ptl_base_modules;

#endif /* LAM_MCA_PTL_H */
