/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "include/types.h"
#include "mca/llm/llm.h"

#include <sys/types.h>

#ifndef MCA_PCM_RSH_H_
#define MCA_PCM_RSH_H_

#ifdef __cplusplus
extern "C" {
#endif

    /*
     * Module open / close
     */
    int mca_pcm_rsh_component_open(void);
    int mca_pcm_rsh_component_close(void);

    /*
     * Startup / Shutdown
     */
    struct mca_pcm_base_module_1_0_0_t* mca_pcm_rsh_init(int *priority, 
                                                         bool *allow_multi_user_threads,
                                                         bool *have_hidden_threads,
                                                         int constraints);
    int mca_pcm_rsh_finalize(struct mca_pcm_base_module_1_0_0_t* me);

    /*
     * Interface
     */
    ompi_list_t* mca_pcm_rsh_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                                mca_ns_base_jobid_t jobid,
                                                int nodes, int procs);
    bool mca_pcm_rsh_can_spawn(struct mca_pcm_base_module_1_0_0_t* me);
    int mca_pcm_rsh_spawn_procs(struct mca_pcm_base_module_1_0_0_t* me, 
                                mca_ns_base_jobid_t jobid, ompi_list_t *schedule_list);
    int mca_pcm_rsh_kill_proc(struct mca_pcm_base_module_1_0_0_t* me, 
                              ompi_process_name_t *name, int flags);
    int mca_pcm_rsh_kill_job(struct mca_pcm_base_module_1_0_0_t* me,
                             mca_ns_base_jobid_t jobid, int flags);
    int mca_pcm_rsh_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                         mca_ns_base_jobid_t jobid,
                                         ompi_list_t *nodelist);

#ifdef __cplusplus
}
#endif

/*
 * Module variables
 */
/* should we avoid running .profile, even if the shell says we should */
extern int mca_pcm_rsh_no_profile;
/* should we assume same shell on remote as locally? */
extern int mca_pcm_rsh_fast;
/* should we ignore things on stderr? */
extern int mca_pcm_rsh_ignore_stderr;
/* how should we fire procs up on the remote side? */
extern char *mca_pcm_rsh_agent;

extern int mca_pcm_rsh_output;

extern int mca_pcm_rsh_use_ns;
extern mca_llm_base_module_t mca_pcm_rsh_llm;

#endif /* MCA_PCM_RSH_H_ */
