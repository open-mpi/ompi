/* -*- C -*-
 *
 * $HEADER$
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

    /*
     * Job management code
     */
    void mca_pcm_base_job_list_init(void);
    void mca_pcm_base_job_list_fini(void);

    int mca_pcm_base_add_started_pids(mca_ns_base_jobid_t jobid,
                                      pid_t child_pid,
                                     mca_ns_base_vpid_t lower, 
                                     mca_ns_base_vpid_t upper);
    pid_t mca_pcm_base_get_started_pid(mca_ns_base_jobid_t jobid,
                                      mca_ns_base_vpid_t vpid,
                                      bool remove_started_pid);
    int mca_pcm_base_get_started_pid_list(mca_ns_base_jobid_t jobid, 
                                         pid_t **pids, size_t *len,
                                         bool remove_started_pids);
    int mca_pcm_base_get_job_info(pid_t pid, mca_ns_base_jobid_t *jobid,
                                  mca_ns_base_vpid_t *lower,
                                  mca_ns_base_vpid_t *upper);
    int mca_pcm_base_remove_job(mca_ns_base_jobid_t jobid);

    struct mca_pcm_base_pids_t {
        ompi_list_item_t super;
        mca_ns_base_vpid_t lower;
        mca_ns_base_vpid_t upper;
        pid_t child;
    };
    typedef struct mca_pcm_base_pids_t mca_pcm_base_pids_t;
    OBJ_CLASS_DECLARATION(mca_pcm_base_pids_t);

    struct mca_pcm_base_job_item_t {
        ompi_list_item_t super;
        mca_ns_base_jobid_t jobid;
        ompi_list_t *pids;
    };
    typedef struct mca_pcm_base_job_item_t mca_pcm_base_job_item_t;
    OBJ_CLASS_DECLARATION(mca_pcm_base_job_item_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
