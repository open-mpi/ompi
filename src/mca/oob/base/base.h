/* -*- C -*-
 *
 * $HEADER$
 */

#ifndef MCA_OOB_BASE_H_
#define MCA_OOB_BASE_H_

#include "lam_config.h"

#include "lam/types.h"
#include "mca/mca.h"
#include "mca/lam/oob/oob.h"

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_oob_base_open(void);
  int mca_oob_base_select(bool *allow_multi_user_threads, 
                          bool *have_hidden_threads);
  int mca_oob_base_close(void);

  bool mca_oob_base_is_checkpointable(void);

  int mca_oob_base_checkpoint(void);
  int mca_oob_base_continue(void);
  int mca_oob_base_restart(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Global struct holding the selected module's function pointers
 */
extern int mca_oob_base_output;
extern lam_list_t mca_oob_base_modules_available;
extern mca_oob_base_module_t mca_oob_base_selected_module;
extern mca_oob_t mca_oob;

#endif
