/* -*- C -*-
 *
 * $HEADER$
 */

/**
 * @file@
 *
 *
 */

#ifndef MCA_PCM_BASE_JOB_TRACK_H_
#define MCA_PCM_BASE_JOB_TRACK_H_

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "mca/ns/ns.h"
#include "runtime/runtime_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    struct mca_pcm_base_job_list_t;
    typedef struct mca_pcm_base_job_list_t mca_pcm_base_job_list_t;

    /*
     * Job management code
     */
    mca_pcm_base_job_list_t* mca_pcm_base_job_list_init(void);
    void mca_pcm_base_job_list_fini(mca_pcm_base_job_list_t *me);

    int mca_pcm_base_job_list_add_job_info(mca_pcm_base_job_list_t *me,
                                           mca_ns_base_jobid_t jobid,
                                           pid_t starter_pid,
                                           mca_ns_base_vpid_t lower, 
                                           mca_ns_base_vpid_t upper);

    pid_t mca_pcm_base_job_list_get_starter(mca_pcm_base_job_list_t *me,
                                            mca_ns_base_jobid_t jobid,
                                            mca_ns_base_vpid_t vpid,
                                            bool remove_started_pid);

    int mca_pcm_base_job_list_get_starters(mca_pcm_base_job_list_t *me,
                                           mca_ns_base_jobid_t jobid, 
                                           pid_t **pids, size_t *len,
                                           bool remove_started_pids);

    int mca_pcm_base_job_list_get_all_starters(mca_pcm_base_job_list_t *me,
                                               pid_t **pids, size_t *len,
                                               bool remove_started_pids);

    int mca_pcm_base_job_list_get_job_info(mca_pcm_base_job_list_t *me,
                                           pid_t pid, 
                                           mca_ns_base_jobid_t *jobid,
                                           mca_ns_base_vpid_t *lower,
                                           mca_ns_base_vpid_t *upper, 
                                           bool remove_started_pids);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
