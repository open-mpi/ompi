/*
 * $HEADER$
 */

#ifndef MCA_PTL_BASE_H
#define MCA_PTL_BASE_H

#include "lam_config.h"

#include "mca/mca.h"
#include "mca/mpi/ptl/ptl.h"


/*
 * Global functions for MCA: overall PTL open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_ptl_base_open(void);
  int mca_ptl_base_select(lam_list_t *available);
  int mca_ptl_base_close(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Globals
 */
extern int mca_ptl_base_output;
extern lam_list_t mca_ptl_base_modules_available;

#endif /* MCA_PTL_BASE_H */
