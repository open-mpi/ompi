/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "include/types.h"

#include <sys/types.h>

#ifndef MCA_PCM_RMS_H_
#define MCA_PCM_RMS_H_

#ifdef __cplusplus
extern "C" {
#endif

    /*
     * Module open / close
     */
    int mca_pcm_rms_component_open(void);
    int mca_pcm_rms_component_close(void);

    /*
     * Startup / Shutdown
     */
    struct mca_pcm_base_module_1_0_0_t* mca_pcm_rms_init(int *priority, 
                                          bool *allow_multi_user_threads,
                                          bool *have_hidden_threads);
    int mca_pcm_rms_finalize(void);

    /*
     * Interface
     */
    ompi_list_t* mca_pcm_rms_allocate_resources(int jobid,
                                                int nodes, int procs);
    bool mca_pcm_rms_can_spawn(void);
    int mca_pcm_rms_spawn_procs(int jobid, ompi_list_t *schedule_list);
    int mca_pcm_rms_kill_proc(ompi_process_name_t *name, int flags);
    int mca_pcm_rms_kill_job(int jobid, int flags);
    int mca_pcm_rms_deallocate_resources(int jobid,
                                         ompi_list_t *nodelist);

#ifdef __cplusplus
}
#endif

/*
 * Module variables
 */
extern int mca_pcm_rms_output;

#endif /* MCA_PCM_RMS_H_ */
