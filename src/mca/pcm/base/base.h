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
#include "mca/llm/base/base_internal.h"

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int mca_pcm_base_open(void);
    /* modules is a pointer to an array of pointers to mca_pcm_base_module_t structs */
    int mca_pcm_base_select(bool *allow_multi_user_threads, 
                            bool *have_hidden_threads,
                            int constraint,
                            mca_pcm_base_module_t ***modules,
                            int *modules_len);

    int mca_pcm_base_close(void);

    int mca_pcm_base_send_schedule(FILE *fd, 
                                   mca_ns_base_jobid_t jobid,
                                   ompi_rte_node_schedule_t *sched,
                                   int num_procs);

    int  mca_pcm_base_recv_schedule(FILE *fd, 
                                    mca_ns_base_jobid_t *jobid,
                                    ompi_rte_node_schedule_t *sched,
                                    int *num_procs);

    int mca_pcm_base_build_base_env(char **in_env, int *envc, char ***out_envp);

    int mca_pcm_base_ioexecvp(char **cmdv, int showout, char *outbuff, 
                                   int outbuffsize, int stderr_is_err);

    char* mca_pcm_base_get_username(mca_llm_base_hostfile_node_t *node);

    char *mca_pcm_base_get_unique_id(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */
extern int mca_pcm_base_output;
extern ompi_list_t mca_pcm_base_components_available;

#endif /* MCA_PCM_BASE_H */
