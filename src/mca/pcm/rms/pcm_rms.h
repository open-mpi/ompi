/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "include/types.h"
#include "class/ompi_list.h"

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
    ompi_list_t* mca_pcm_rms_allocate_resources(mca_ns_base_jobid_t jobid,
                                                int nodes, int procs);
    bool mca_pcm_rms_can_spawn(void);
    int mca_pcm_rms_spawn_procs(mca_ns_base_jobid_t jobid, ompi_list_t *schedule_list);
    int mca_pcm_rms_kill_proc(ompi_process_name_t *name, int flags);
    int mca_pcm_rms_kill_job(mca_ns_base_jobid_t jobid, int flags);
    int mca_pcm_rms_deallocate_resources(mca_ns_base_jobid_t jobid,
                                         ompi_list_t *nodelist);

    struct mca_pcm_rms_pids_t {
        ompi_list_item_t super;
        mca_ns_base_vpid_t lower;
        mca_ns_base_vpid_t upper;
        pid_t child;
    };
    typedef struct mca_pcm_rms_pids_t mca_pcm_rms_pids_t;
    OBJ_CLASS_DECLARATION(mca_pcm_rms_pids_t);

    struct mca_pcm_rms_job_item_t {
        ompi_list_item_t super;
        mca_ns_base_jobid_t jobid;
        ompi_list_t *pids;
    };
    typedef struct mca_pcm_rms_job_item_t mca_pcm_rms_job_item_t;
    OBJ_CLASS_DECLARATION(mca_pcm_rms_job_item_t);

#ifdef __cplusplus
}
#endif

/*
 * Module variables
 */
extern int mca_pcm_rms_output;
extern ompi_list_t mca_pcm_rms_jobs;
extern int mca_pcm_rms_use_ns;

#endif /* MCA_PCM_RMS_H_ */
