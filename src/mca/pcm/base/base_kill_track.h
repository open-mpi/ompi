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


    int mca_pcm_base_kill(int how, ompi_process_name_t *name,
                          int signal, int flags);


    /**
     * Register the pcm to receive kill messages
     *
     * Register pcm module \c pcm to receive kill messages for job \c
     * jobid, processes with vpids of \c lower_vpid to \c upper_vpid.
     *
     * \note This function should only be called within a pcm module.
     */
    int mca_pcm_base_kill_register(mca_pcm_base_module_t* pcm,
				   ompi_process_name_t *name);


    /**
     * Unregister the pcm to receive kill messages
     *
     * Unregister pcm module \c pcm to receive kill messages for job \c
     * jobid, processes with vpids of \c lower_vpid to \c upper_vpid.
     *
     * \note This function should only be called within a pcm module.
     */
    int mca_pcm_base_kill_unregister(ompi_process_name_t *name);


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
