/* -*- C -*-
 * 
 * $HEADER$
 *
 * BWB: COMPONENT TODO
 *
 *  - add process reaping code
 *  - trigger status change events on process death
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "include/types.h"
#include "class/ompi_list.h"

#include <sys/types.h>

#ifndef MCA_PCM_BPROC_H_
#define MCA_PCM_BPROC_H_

#ifdef __cplusplus
extern "C" {
#endif

    /*
     * Module open / close
     */
    int mca_pcm_bproc_component_open(void);
    int mca_pcm_bproc_component_close(void);

    /*
     * Startup / Shutdown
     */
    struct mca_pcm_base_module_1_0_0_t* mca_pcm_bproc_init(int *priority, 
                                                         bool *allow_multi_user_threads,
                                                         bool *have_hidden_threads,
                                                         int constraints);
    int mca_pcm_bproc_finalize(struct mca_pcm_base_module_1_0_0_t* me);

    /*
     * Interface
     */
    ompi_list_t* mca_pcm_bproc_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                                mca_ns_base_jobid_t jobid,
                                                int nodes, int procs);
    int mca_pcm_bproc_spawn_procs(struct mca_pcm_base_module_1_0_0_t* me,
                                mca_ns_base_jobid_t jobid, ompi_list_t *schedule_list);
    int mca_pcm_bproc_kill_proc(struct mca_pcm_base_module_1_0_0_t* me,
                              ompi_process_name_t *name, int flags);
    int mca_pcm_bproc_kill_job(struct mca_pcm_base_module_1_0_0_t* me,
                             mca_ns_base_jobid_t jobid, int flags);
    int mca_pcm_bproc_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                         mca_ns_base_jobid_t jobid,
                                         ompi_list_t *nodelist);

    /*
     * Job management code
     */
    void mca_pcm_bproc_job_list_init(void);
    void mca_pcm_bproc_job_list_fini(void);

    int mca_pcm_bproc_add_started_pids(mca_ns_base_jobid_t jobid, pid_t child_pid,
                         mca_ns_base_vpid_t lower, mca_ns_base_vpid_t upper);
    pid_t mca_pcm_bproc_get_started_pid(mca_ns_base_jobid_t jobid, mca_ns_base_vpid_t vpid,
                          bool remove_started_pid);
    int mca_pcm_bproc_get_started_pid_list(mca_ns_base_jobid_t jobid, pid_t **pids, size_t *len,
                             bool remove_started_pids);
    int mca_pcm_bproc_remove_job(mca_ns_base_jobid_t jobid);

    struct mca_pcm_bproc_pids_t {
        ompi_list_item_t super;
        mca_ns_base_vpid_t lower;
        mca_ns_base_vpid_t upper;
        pid_t child;
    };
    typedef struct mca_pcm_bproc_pids_t mca_pcm_bproc_pids_t;
    OBJ_CLASS_DECLARATION(mca_pcm_bproc_pids_t);

    struct mca_pcm_bproc_job_item_t {
        ompi_list_item_t super;
        mca_ns_base_jobid_t jobid;
        ompi_list_t *pids;
    };
    typedef struct mca_pcm_bproc_job_item_t mca_pcm_bproc_job_item_t;
    OBJ_CLASS_DECLARATION(mca_pcm_bproc_job_item_t);

#ifdef __cplusplus
}
#endif

/*
 * Module variables
 */
extern int mca_pcm_bproc_output;

#endif /* MCA_PCM_BPROCx_H_ */
