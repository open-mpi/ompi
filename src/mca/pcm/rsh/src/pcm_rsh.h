/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "include/types.h"

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
                                          bool *have_hidden_threads);
    int mca_pcm_rsh_finalize(void);

    /*
     * Interface
     */
    bool mca_pcm_rsh_can_spawn(void);
    int mca_pcm_rsh_spawn_procs(int jobid, ompi_list_t *schedule_list);
    int mca_pcm_rsh_kill_proc(ompi_process_name_t *name, int flags);
    int mca_pcm_rsh_kill_job(int jobid, int flags);

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

#endif /* MCA_PCM_RSH_H_ */
