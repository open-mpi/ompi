/* -*- C -*-
 *
 * $HEADER$
 */
/** @file **/

/** 
 *  \brief OMPI/MPI Interface for Parallel Job & Process Control (pcm)
 *
 * OMPI/MPI assumes it is running under a fully operational parallel
 * run-time environment (RTE).  This environment may be provided by
 * batch schedulers such as PBS and LSF, single system image tools
 * such as bproc, or specially designed MPI control daemons (the MPICH
 * mpd or the included OMPI daemon).  The functionality provided
 * through the process control interface is dependant on the support
 * of the underlying infrastructure.  For example, ompi_pcm_spawn
 * (essentially, the "go do it" part of MPI_COMM_SPAWN) is not
 * available for jobs running under the Qadrics/RMS RTE.  The OMPI
 * daemons will always provide the complete pcm interface.
 *
 * Like the other OMPI run-time interfaces, the pcm interface is
 * implemented through mca modules (pcm).  For details on the
 * capabilities of a particular module, please see the individual
 * module's documentation.
 *
 * A run-time environment suitable for use by OMPI/MPI must provide the
 * following capabilities:
 *
 *  - Remote process startup at job-start time with the ability to:
 *    - push an environment (or a large chunk of an environment) to the started process
 *    - redirect the stdout and stderr of the process to either a file (batch 
 *      schedulers) or the mpirun application (OMPI daemons) without interaction from the
 *      started process
 *  - A working registry interface
 *  - A "unique" job id for each parallel job
 *  - The ability to "clean up" after a job when provided with that job id
 *  - The ability to receive UNIX wait-style notification of parallel job termination
 *
 * A run-time environment should proivde the following capabilities if supported:
 *  - Remote process spawning for MPI_SPAWN and friends
 *  - Fine-grained control over process cleanup (for example, only do final cleanup 
 *    of resources when all applications are unavailable, kill on first death, kill on
 *    the 3rd process exit, etc.)
 *
 * The pcm interface is responsible for ensuring that each process
 * started is capable of performing peer discovery during MPI_INIT.
 * It is intended that mpirun will be actively calling into the pcm
 * interface so that mpirun can be used as a redezvous point.  Using
 * mpirun is certainly not required, but it is anticipated this will
 * be a common module design.
 *
 */

#ifndef MCA_PCM_H_
#define MCA_PCM_H_

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/ns/ns.h"
#include "include/types.h"

#include <sys/param.h>

/* Module functions */

typedef int (*mca_pcm_base_module_get_peers_fn_t)
     (ompi_process_name_t **peers, size_t *npeers);
typedef ompi_process_name_t* (*mca_pcm_base_module_get_self_fn_t)(void);

/* Component functions */

typedef struct mca_pcm_base_module_1_0_0_t *(*mca_pcm_base_component_init_fn_t)
     (int *priority, bool *allow_multi_user_threads,
      bool *have_hidden_threads);
typedef int (*mca_pcm_base_component_finalize_fn_t)(void);

/* Ver 1.0.0 */
struct mca_pcm_base_component_1_0_0_t {
  mca_base_component_t pcmm_version;
  mca_base_component_data_1_0_0_t pcmm_data;

  mca_pcm_base_component_init_fn_t pcmm_init;
  mca_pcm_base_component_finalize_fn_t pcmm_finalize;
};
typedef struct mca_pcm_base_component_1_0_0_t mca_pcm_base_component_1_0_0_t;
typedef struct mca_pcm_base_component_1_0_0_t mca_pcm_base_component_t;

struct mca_pcm_base_module_1_0_0_t {
  mca_pcm_base_module_get_peers_fn_t pcm_peers;
  mca_pcm_base_module_get_self_fn_t  pcm_self;
};
typedef struct mca_pcm_base_module_1_0_0_t mca_pcm_base_module_1_0_0_t;
typedef struct mca_pcm_base_module_1_0_0_t mca_pcm_base_module_t;

/*
 * Macro for use in modules that are of type pcm v1.0.0
 */
#define MCA_PCM_BASE_VERSION_1_0_0 \
  /* pcm v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* pcm v1.0 */ \
  "pcm", 1, 0, 0

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_pcm_base_open(void);
  int mca_pcm_base_select(bool *allow_multi_user_threads, 
                          bool *have_hidden_threads);
  int mca_pcm_base_close(void);

  bool mca_pcm_base_is_checkpointable(void);

  int mca_pcm_base_checkpoint(void);
  int mca_pcm_base_continue(void);
  int mca_pcm_base_restart(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Macros
 */

/*
 * Globals
 */
extern int mca_pcm_base_output;
extern ompi_list_t mca_pcm_base_components_available;
extern mca_pcm_base_component_t mca_pcm_base_selected_component;
extern mca_pcm_base_module_t mca_pcm;

#endif
