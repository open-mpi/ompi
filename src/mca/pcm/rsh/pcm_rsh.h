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
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "include/types.h"
#include "mca/llm/llm.h"
#include "mca/pcm/base/base_job_track.h"

#include <sys/types.h>

#ifndef MCA_PCM_RSH_H_
#define MCA_PCM_RSH_H_

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /*
     * Module open / close
     */
    int mca_pcm_rsh_component_open(void);
    int mca_pcm_rsh_component_close(void);

    /*
     * Startup / Shutdown
     */
    struct mca_pcm_base_module_1_0_0_t* mca_pcm_rsh_init(int *priority, 
                                                         bool have_threads,
                                                         int constraints);
    int mca_pcm_rsh_finalize(struct mca_pcm_base_module_1_0_0_t* me);

    /*
     * Interface
     */
    ompi_list_t* mca_pcm_rsh_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                                mca_ns_base_jobid_t jobid,
                                                int nodes, int procs);
    int mca_pcm_rsh_spawn_procs(struct mca_pcm_base_module_1_0_0_t* me, 
                                mca_ns_base_jobid_t jobid, ompi_list_t *schedule_list);
    int mca_pcm_rsh_kill_proc(struct mca_pcm_base_module_1_0_0_t* me, 
                              ompi_process_name_t *name, int flags);
    int mca_pcm_rsh_kill_job(struct mca_pcm_base_module_1_0_0_t* me,
                             mca_ns_base_jobid_t jobid, int flags);
    int mca_pcm_rsh_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                         mca_ns_base_jobid_t jobid,
                                         ompi_list_t *nodelist);

    int mca_pcm_rsh_ioexecvp(char **cmdv, int showout, char *outbuff, 
                                   int outbuffsize, int stderr_is_err);

    struct mca_pcm_rsh_module_t {
        mca_pcm_base_module_t super;

        mca_llm_base_module_t *llm;
        mca_pcm_base_job_list_t *jobs;

        int no_profile;
        int fast_boot;
        int ignore_stderr;
        char* rsh_agent;
        int constraints;
        unsigned int delay_time;
    	bool debug_callback;
    };
    typedef struct mca_pcm_rsh_module_t mca_pcm_rsh_module_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_PCM_RSH_H_ */
