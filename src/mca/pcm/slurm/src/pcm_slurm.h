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

#ifndef MCA_PCM_SLURM_H_
#define MCA_PCM_SLURM_H_

#ifdef __cplusplus
extern "C" {
#endif

    /*
     * Module open / close
     */
    int mca_pcm_slurm_component_open(void);
    int mca_pcm_slurm_component_close(void);

    /*
     * Startup / Shutdown
     */
    struct mca_pcm_base_module_1_0_0_t* mca_pcm_slurm_init(int *priority, 
                                                         bool have_threads,
                                                         int constraints);
    int mca_pcm_slurm_finalize(struct mca_pcm_base_module_1_0_0_t* me);

    /*
     * Interface
     */
    ompi_list_t* mca_pcm_slurm_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                                mca_ns_base_jobid_t jobid,
                                                int nodes, int procs);
    int mca_pcm_slurm_spawn_procs(struct mca_pcm_base_module_1_0_0_t* me,
                                mca_ns_base_jobid_t jobid, ompi_list_t *schedule_list);
    int mca_pcm_slurm_kill_proc(struct mca_pcm_base_module_1_0_0_t* me,
                              ompi_process_name_t *name, int flags);
    int mca_pcm_slurm_kill_job(struct mca_pcm_base_module_1_0_0_t* me,
                             mca_ns_base_jobid_t jobid, int flags);
    int mca_pcm_slurm_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                         mca_ns_base_jobid_t jobid,
                                         ompi_list_t *nodelist);

#ifdef __cplusplus
}
#endif

/*
 * Module variables
 */
extern int mca_pcm_slurm_output;

#endif /* MCA_PCM_SLURM_H_ */
