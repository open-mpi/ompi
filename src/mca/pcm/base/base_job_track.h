/* -*- C -*-
 *
 * $HEADER$
 */

/**
 * @file@
 *
 * \brief PCM code for tracking starter process handles
 * 
 * PCM helper code for tracking starter process handles.  In the UNIX
 * world, this means keeping the association of a jobid,vpid to a unix
 * pid.  This is useful for situations like RMS, where sending a
 * signal to prun can result in the entire job receiving the signal.
 *
 * It is intended that a \c mca_pcm_base_job_list_t is used within one
 * module, and not shared among multiple modules.  While it should be
 * possible to share further, such a setup is not well tested.  The
 * code is multi-thread safe.  There is some serialization, but
 * nothing that should lead to deadlock.
 */

#ifndef MCA_PCM_BASE_JOB_TRACK_H_
#define MCA_PCM_BASE_JOB_TRACK_H_

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "mca/ns/ns.h"
#include "runtime/runtime_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Opaque handle for job tracking
     *
     * Opaque handle for a particular job tracking data set.  Add /
     * delete operations are local to the given \c
     * mca_pcm_base_job_list_t.
     */
    struct mca_pcm_base_job_list_t;
    typedef struct mca_pcm_base_job_list_t mca_pcm_base_job_list_t;


    /**
     * Initialize job tracking handle
     *
     * Create and initialize a job tracking handle.  The returned
     * handle will have no data associated with it, but can be used
     * for further calls to the job tracking interface.  At the end of
     * the data's lifespan, \c mca_pcm_base_job_list_fini should be
     * called with the given handle to avoid memory leaks.
     */
    mca_pcm_base_job_list_t* mca_pcm_base_job_list_init(void);


    /**
     * Finalize job tracking handle
     *
     * Finalize a job tracking handle, freeing all associated memory.
     * After a call to \c mca_pcm_base_job_list_fini, the given handle
     * may no longer be used.
     */
    void mca_pcm_base_job_list_fini(mca_pcm_base_job_list_t *me);


    /**
     * Add information about a set of processes
     *
     * Add information to the given job tracking handle about the
     * given jobid and vpid range.
     */
    int mca_pcm_base_job_list_add_job_info(mca_pcm_base_job_list_t *me,
                                           mca_ns_base_jobid_t jobid,
                                           pid_t starter_pid,
                                           mca_ns_base_vpid_t lower, 
                                           mca_ns_base_vpid_t upper);


    /**
     * Get starter handle for a single process
     *
     * Get starter handle for a single process.  Optionally remove the
     * data from the job list.
     */
    pid_t mca_pcm_base_job_list_get_starter(mca_pcm_base_job_list_t *me,
                                            mca_ns_base_jobid_t jobid,
                                            mca_ns_base_vpid_t vpid,
                                            bool remove_started_pid);


    /**
     * Get starter handles for a given jobid
     *
     * Get all starter handles in the given data store for the given
     * jobid.  Note that there may be other starter handles for the
     * given jobid in other data stores.  Optionally remove the
     * returned data from the job list.
     */
    int mca_pcm_base_job_list_get_starters(mca_pcm_base_job_list_t *me,
                                           mca_ns_base_jobid_t jobid, 
                                           pid_t **pids, size_t *len,
                                           bool remove_started_pids);

    /**
     * Get all starter handles in the data store
     *
     * Get all starter handles in the given data store, optionally
     * removing them from the data store.
     */
    int mca_pcm_base_job_list_get_all_starters(mca_pcm_base_job_list_t *me,
                                               pid_t **pids, size_t *len,
                                               bool remove_started_pids);


    /**
     * Get all information associated with a given starter handle
     *
     * Get the jobid and vpid range associated with a given starter
     * handle.  Optionally remove the information from the data store.
     */
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
