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
    int mca_pcm_base_select(bool have_threads,
                            int constraint,
                            mca_pcm_base_module_t ***modules,
                            size_t *modules_len);

    int mca_pcm_base_close(void);

    /* communicate the important parts of our structs around  */
    int mca_pcm_base_send_schedule(FILE *fd, 
                                   mca_ns_base_jobid_t jobid,
                                   ompi_rte_node_schedule_t *sched,
                                   int num_procs);

    int  mca_pcm_base_recv_schedule(FILE *fd, 
                                    mca_ns_base_jobid_t *jobid,
                                    ompi_rte_node_schedule_t *sched,
                                    int *num_procs);

    /**
     * Create pushable environment
     *
     * Copy the parts of \c in_env that should be moved to remote
     * nodes when spawning processes into \c out_envp.  All variables
     * starting with OMPI_ are copied.
     */
    int mca_pcm_base_build_base_env(char **in_env, int *envc, char ***out_envp);

    int mca_pcm_base_ioexecvp(char **cmdv, int showout, char *outbuff, 
                                   int outbuffsize, int stderr_is_err);

    char* mca_pcm_base_get_username(mca_llm_base_hostfile_node_t *node);

    /**
     * Get unique id string available from PCM components
     *
     * PCM components may be able to provide some unique identifier
     * that should be used in seperating universes (the Batch ID in PBS,
     * for example).  This function provides an interface for
     * retriving that information from the components.
     */
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
