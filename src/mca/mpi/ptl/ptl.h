/*
 * $HEADER$
 */

#ifndef LAM_MCA_PTL_H
#define LAM_MCA_PTL_H

#include "mca/mca.h"
#include "proc.h"
#include "lam.h"
#include "lam/lfc/list.h"


/*
 * PTL module functions.
 */

typedef int (*mca_ptl_query_fn_t)(int *priority, int *thread_min, int *thread_max);
typedef struct mca_ptl_1_0_0* (*mca_ptl_init_1_0_0_fn_t)();
                                                                                                         
/*
 * PTL action functions.
 */

typedef int (*mca_ptl_fragment_fn_t)(mca_ptl_send_request_t*, size_t);
typedef int (*mca_ptl_progress_fn_t)(mca_time_t);

/*
 * Struct used to pass PTL module information from the each PTL 
 * instance back to the MCA framework.
 */

typedef struct mca_ptl_module_1_0_0 {
  mca_1_0_0_t super;
  mca_ptl_query_fn_t ptlm_query;
  mca_ptl_init_1_0_0_fn_t ptlm_init;
} mca_ptl_module_1_0_0_t;

/*
 * Struct that represents the common state and interface functions
 * provided by a PTL.
 */

typedef struct mca_ptl_1_0_0 {

    /* PTL common attributes */
    size_t       ptl_frag_first_size;     /* maximum size of first fragment */
    size_t       ptl_frag_min_size;       /* threshold below which the CDI will not fragment */
    size_t       ptl_frag_max_size;       /* maximum fragment size supported by the CDI */
    uint32_t     ptl_endpoint_latency;    /* relative/absolute measure of latency */
    uint64_t     ptl_endpoint_bandwidth;  /* bandwidth (bytes/sec) supported by each endpoint */
    size_t       ptl_endpoint_count;      /* number endpoints supported by this CDI */

    /* PTL function table */
    mca_ptl_fragment_fn_t  ptl_fragment;
    mca_ptl_progress_fn_t  ptl_progress;

} mca_ptl_1_0_0_t;


/*
 * Set the default type to use version 1.1.0 of the PTL 
 */

typedef mca_ptl_module_1_1_0_t mca_ptl_module_t;
typedef mca_ptl_1_1_0_t mca_ptl_t;


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

extern const mca_t **mca_ptl_modules;

#endif /* LAM_MCA_PTL_H */
