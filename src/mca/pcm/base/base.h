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

#ifndef MCA_PCM_BASE_H_
#define MCA_PCM_BASE_H_

#include "ompi_config.h"

#include "include/types.h"
#include "mca/mca.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/debug.h"
#include "mca/llm/base/base_internal.h"

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC    int mca_pcm_base_open(void);
    /* modules is a pointer to an array of pointers to mca_pcm_base_module_t structs */
OMPI_DECLSPEC    int mca_pcm_base_select(bool have_threads,
                            int constraint,
                            mca_pcm_base_module_t ***modules,
                            size_t *modules_len);

OMPI_DECLSPEC    int mca_pcm_base_close(void);

    /* communicate the important parts of our structs around  */
OMPI_DECLSPEC    int mca_pcm_base_send_schedule(FILE *fd, 
                                   mca_ns_base_cellid_t cellid,
                                   mca_ns_base_jobid_t jobid,
                                   mca_ns_base_vpid_t global_start_vpid,
                                   int global_spawn_size,
                                   ompi_rte_node_schedule_t *sched,
                                   int num_procs);

OMPI_DECLSPEC    int  mca_pcm_base_recv_schedule(FILE *fd, 
                                    mca_ns_base_cellid_t *cellid,
                                    mca_ns_base_jobid_t *jobid,
                                    mca_ns_base_vpid_t *starting_vpid,
                                    int *spawn_size,
                                    ompi_rte_node_schedule_t *sched,
                                    int *num_procs);

    /**
     * Create pushable environment
     *
     * Copy the parts of \c in_env that should be moved to remote
     * nodes when spawning processes into \c out_envp.  All variables
     * starting with OMPI_ are copied.
     */
OMPI_DECLSPEC    int mca_pcm_base_build_base_env(char **in_env, int *envc, char ***out_envp);

OMPI_DECLSPEC    char* mca_pcm_base_get_username(mca_llm_base_hostfile_node_t *node);

    /**
     * Get unique id string available from PCM components
     *
     * PCM components may be able to provide some unique identifier
     * that should be used in seperating universes (the Batch ID in PBS,
     * for example).  This function provides an interface for
     * retriving that information from the components.
     */
OMPI_DECLSPEC    char *mca_pcm_base_get_unique_id(void);



/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_pcm_base_output;
OMPI_DECLSPEC extern ompi_list_t mca_pcm_base_components_available;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PCM_BASE_H */
