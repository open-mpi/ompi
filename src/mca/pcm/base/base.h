/* -*- C -*-
 *
 * $HEADER$
 */

#ifndef MCA_PCM_BASE_H_
#define MCA_PCM_BASE_H_

#include "lam_config.h"

#include "lam/types.h"
#include "mca/mca.h"
#include "mca/lam/pcm/pcm.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_pcm_base_open(void);
  int mca_oob_base_select(bool *allow_multi_user_threads, 
                          bool *have_hidden_threads);
  int mca_pcm_base_close(void);

  bool mca_pcm_base_is_checkpointable(void);

  int mca_pcm_base_checkpoint(void);
  int mca_pcm_base_continue(void);
  int mca_pcm_base_restart(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */
extern int mca_pcm_base_output;
extern lam_list_t mca_pcm_base_modules_available;
extern mca_pcm_base_module_t mca_pcm_base_selected_module;
extern mca_pcm_t mca_pcm;

#endif /* MCA_PCM_BASE_H */
