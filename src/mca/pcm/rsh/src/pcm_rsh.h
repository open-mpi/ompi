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
    char *mca_pcm_rsh_get_unique_name(void);
    ompi_list_t* mca_pcm_rsh_allocate_resources(int jobid, int nodes, 
                                                int procs);
    int mca_pcm_rsh_register_monitor(int jobid,
                                     ompi_rte_monitor_fn_t func);
    bool mca_pcm_rsh_can_spawn(void);
    int mca_pcm_rsh_spawn_procs(int jobid, ompi_list_t *schedule_list);
    int mca_pcm_rsh_kill_proc(ompi_process_name_t *name, int flags);
    int mca_pcm_rsh_kill_job(int jobid, int flags);
    int mca_pcm_rsh_deallocate_resources(int jobid, ompi_list_t *nodelist);

#ifdef __cplusplus
}
#endif

#endif /* MCA_PCM_RSH_H_ */
