/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file@
 *
 * \brief PCM code for handling backend to ompi_rte_kill_ functions
 *
 * \note This really isn't working yet.  Sorry :(
 */

#ifndef MCA_PCM_BASE_KILL_TRACK_H_
#define MCA_PCM_BASE_KILL_TRACK_H_

#include "mca/ns/ns.h"
#include "runtime/runtime_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Send kill message for an entire job
     *
     * Send a kill message to all processes associated with \c jobid.
     * This may require OOB communication with one or more remote
     * processes.  An attempt is made to deliver the signal
     * asynchronously in a timely manner, but there may be times where
     * a delay is required in single threaded code (particularily when
     * portions of the job were started by MPI_COMM_SPAWN).
     */
    int mca_pcm_base_kill_send_job_msg(mca_ns_base_jobid_t jobid,
                                       int sig, int errorcode, int flags);

    /**
     * Send kill message for a single process
     *
     * Send a kill message to process \c name.  As a side effect,
     * other processes in the same job as \c name may be killed.  This
     * may require OOB communication with one or more remote
     * processes.  An attempt is made to deliver the signal
     * asynchronously in a timely manner, but there may be times where
     * a delay is required in single threaded code (particularily when
     * portions of the job were started by MPI_COMM_SPAWN).
     */
    int mca_pcm_base_kill_send_proc_msg(ompi_process_name_t name,
                                        int sig, int errorcode, int flags);


    /**
     * Register the pcm to receive kill messages
     *
     * Register pcm module \c pcm to receive kill messages for job \c
     * jobid, processes with vpids of \c lower_vpid to \c upper_vpid.
     *
     * \note This function should only be called within a pcm module.
     */
    int mca_pcm_base_kill_register(mca_pcm_base_module_t* pcm,
                                   mca_ns_base_jobid_t jobid,
                                   mca_ns_base_vpid_t lower_vpid,
                                   mca_ns_base_vpid_t upper_vpid);


    /**
     * Unregister the pcm to receive kill messages
     *
     * Unregister pcm module \c pcm to receive kill messages for job \c
     * jobid, processes with vpids of \c lower_vpid to \c upper_vpid.
     *
     * \note This function should only be called within a pcm module.
     */
    int mca_pcm_base_kill_unregister(mca_pcm_base_module_t* pcm,
                                     mca_ns_base_jobid_t jobid,
                                     mca_ns_base_vpid_t lower_vpid,
                                     mca_ns_base_vpid_t upper_vpid);


    /**
     * Initialize the kill message code
     *
     * \note This function should only be called from mca_pcm_base_open.
     */
    int mca_pcm_base_kill_init(void);


    /**
     * Finalize the kill message code
     *
     * \note This function should only be called from mca_pcm_base_close.
     */
    int mca_pcm_base_kill_fini(void);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* #ifndef MCA_PCM_BASE_KILL_TRACK_H_ */
