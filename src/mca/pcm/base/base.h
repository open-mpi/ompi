/* -*- C -*-
 *
 * $HEADER$
 */

#ifndef MCA_PCM_BASE_H_
#define MCA_PCM_BASE_H_

#include "ompi_config.h"

#include "include/types.h"
#include "mca/mca.h"
#include "mca/pcm/pcm.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_pcm_base_open(void);
  int mca_pcm_base_select(bool *allow_multi_user_threads, 
                          bool *have_hidden_threads);
  int mca_pcm_base_close(void);

    char* mca_pcm_base_no_unique_name(void);


    int mca_pcm_base_send_schedule(FILE *fd, 
                                   ompi_rte_node_schedule_t *sched,
                                   ompi_list_t *nodelist);

    int  mca_pcm_base_recv_schedule(FILE *fd, 
                                    ompi_rte_node_schedule_t *sched,
                                    ompi_list_t *nodelist);

    int mca_pcm_base_build_base_env(char **in_env, char ***out_envp);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */
extern int mca_pcm_base_output;
extern ompi_list_t mca_pcm_base_components_available;
extern mca_pcm_base_component_t mca_pcm_base_selected_component;
extern mca_pcm_base_module_t mca_pcm;

#endif /* MCA_PCM_BASE_H */
