/* -*- C -*-
 *
 * $HEADER$
 */
/** @file **/

/** 
 *  \brief Open MPI Interface for Parallel Job & Process Control (pcm)
 *
 * Open MPI assumes it is running under a fully operational parallel
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
 * A run-time environment suitable for use by Open MPI must provide
 * the following capabilities:
 *
 *  - Remote process startup at job-start time with the ability to:
 *    - push an environment (or a large chunk of an environment) to 
 *      the started process
 *    - redirect the stdout and stderr of the process to either a file
 *      (batch schedulers) or the mpirun application (OMPI daemons)
 *      without interaction from the started process
 *  - A working registry interface
 *  - A "unique" job id for each parallel job
 * - The ability to "clean up" after a job when provided with that job
      id
 *  - The ability to receive UNIX wait-style notification of parallel
 *    job termination
 *
 * A run-time environment should proivde the following capabilities 
 * if supported:
 *  - Remote process spawning for MPI_SPAWN and friends
 *  - Fine-grained control over process cleanup (for example, only do
 *    final cleanup of resources when all applications are unavailable,
 *    kill on first death, kill on the 3rd process exit, etc.)
 *
 * The pcm interface is responsible for ensuring that each process
 * started is capable of performing peer discovery during MPI_INIT.
 * It is intended that mpirun will be actively calling into the pcm
 * interface so that mpirun can be used as a redezvous point.  Using
 * mpirun is certainly not required, but it is anticipated this will
 * be a common module design.
 *
 * \note Users should not use this interface directly, instead using
 * the RTE interface defined in runtime.h
 */

#ifndef MCA_PCM_H_
#define MCA_PCM_H_

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/ns/ns.h"
#include "include/types.h"
#include "runtime/runtime_types.h"

/*
 * MCA component management functions
 */

/**
 * PCM initialization function
 *
 * Called by the MCA framework to initialize the component.  Will
 * be called exactly once in the lifetime of the process.
 *
 * @param priority (OUT) Relative priority or ranking use by MCA to
 *                       select a module.
 * @param allow_multiple_user_threads (OUT) Whether this module can
 *                       run with multiple threads making calls into
 *                       the library (equivalent of MPI_THREAD_MULTIPLE 
 *                       from MPI-land).
 * @param have_hidden_threads (OUT) Whether this module needs to start
 *                       a background thread for operation.
 * @param constrains (IN) Bit-wise mask of constraints put on the PCM.
 *                       List of available constants is in the run-time interface - 
 *                       constants start with \c OMPI_RTE_SPAWN_.
 *
 * \warning The only requirement on the returned type is that the
 * first sizeof(struct mca_pcm_base_module_1_0_0_t) bytes have the
 * same structure as a struct mca_pcm_base_module_1_0_0_t.  The pcm is
 * free to return a pointer to a larger structure in order to maintain
 * per-module information it may need.  Therefore, the caller should
 * never copy the structure or assume its size.
 */
typedef struct mca_pcm_base_module_1_0_0_t* 
(*mca_pcm_base_component_init_fn_t)(int *priority, 
                                    bool *allow_multiple_user_threads,
                                    bool *have_hidden_threads,
                                    int constraints);


/**
 * PCM finalization function
 *
 * Called by the MCA framework to finalize the component.  Will be
 * called once per successful call to pcm_base_compoenent_init.
 *
 * @param me (IN)       Pointer to the module being finalized
 */
typedef int (*mca_pcm_base_component_finalize_fn_t)(struct mca_pcm_base_module_1_0_0_t* me);


/**
 * Return a string uniquely identifying the environment
 *
 * Return a string that provides some uniqueness that the system
 * run-time environment makes available.  In RSH, this function won't
 * provide any help.  In a PBS job, however, this function will return
 * the concatentaion of the PBS variables required for uniquely
 * identifying the PBS job environment (the same is true of LSF, SGE,
 * etc.).
 *
 * @param id_string (OUT) Available uniqueness string (NULL terminated)
 * @param priority (OUT) Similar to \c init priority.  Used if multiple
 *           components return useful information.
 *
 * @returns OMPI_SUCCESS if there is information in id_string.
 * OMPI_ERR_NOT_SUPPORTED if there is no information available.
 *
 * \note This is a component-level call, so it can (and will) be
 * called before any calls to module_init.
 */
typedef int
(*mca_pcm_base_component_get_unique_id_fn_t)(char **id_string,
                                             int *priority);


/** 
 * PCM module version and interface functions
 *
 * \note pcm_get_unique_id can be NULL if no unique information is
 * possibly going to be available from the pcm component.
 */
struct mca_pcm_base_component_1_0_0_t {
    mca_base_component_t pcm_version;
    mca_base_component_data_1_0_0_t pcm_data;
    mca_pcm_base_component_init_fn_t pcm_init;
    mca_pcm_base_component_get_unique_id_fn_t pcm_get_unique_id;
};
typedef struct mca_pcm_base_component_1_0_0_t mca_pcm_base_component_1_0_0_t;
typedef mca_pcm_base_component_1_0_0_t mca_pcm_base_component_t;


/*
 * PCM interface functions
 */



/**
 * Allocate requested resources
 *
 * Allocate the specified nodes / processes for use in a new job.
 * Requires a jobid from the PCM interface.  The allocation returned
 * may be smaller than requested - it is up to the caller to proceed
 * as appropriate should this occur.  This function should only be
 * called once per jobid.
 *
 * @param me (IN)    Pointer to the module struct
 * @param jobid (IN) Jobid with which to associate the given resources.
 * @param nodes (IN) Number of nodes to try to allocate. If 0, 
 *                   the PCM will try to allocate <code>procs</code>
 *                   processes on as many nodes as are needed.  If non-zero, 
 *                   will try to fairly distribute <code>procs</code> 
 *                   processes over the nodes.  If <code>procs</code> is 0, 
 *                   will attempt to allocate all cpus on
 *                   <code>nodes</code> nodes
 * @param procs (IN) Number of processors to try to allocate.  See the note
 *                   for <code>nodes</code> for usage.
 * @param nodelist (OUT) List of <code>mca_pcm_node_t</code>s describing
 *                   the allocated resources.
 */
typedef ompi_list_t*
(*mca_pcm_base_allocate_resources_fn_t)(struct mca_pcm_base_module_1_0_0_t* me,
                                        mca_ns_base_jobid_t jobid,
                                        int nodes,
                                        int procs);


/**
 * Spawn a job
 *
 * Start a job with given jobid and starting vpid (should probably be
 * 0 for the forseeable future).  The job is specified using an array
 * of \c mca_pcm_base_schedule_t structures, which give both process
 * and location information.
 *
 * @param me (IN)    Pointer to the module struct
 * @param jobid (IN) Jobid under which the job should be started
 * @param schedule_list (IN) A list of \c mca_pcm_base_schedule_t
 *                   structures describing the job to start.
 */
typedef int
(*mca_pcm_base_spawn_procs_fn_t)(struct mca_pcm_base_module_1_0_0_t* me,
                                 mca_ns_base_jobid_t jobid, 
                                 ompi_list_t *schedule_list);


/**
 * Kill a specific process in this cell
 *
 * @param me (IN)    Pointer to the module struct
 * @param process_name (IN) Which process needs to be killed.
 *
 * @return Error code
 *
 * @warning flags is currently ignored, but should be set to 0 for
 * future compatibility.  Will be used to specify how to kill
 * processes (0 will be same as a "kill <pid>"
 */
typedef int
(*mca_pcm_base_kill_proc_fn_t)(struct mca_pcm_base_module_1_0_0_t* me,
                               ompi_process_name_t *name, int flags);


/**
 * Kill all the processes in a job. This will probably find out all
 * the processes in the job by contacting the registry and then call
 * mca_pcm_kill_process for each process in the job (for a cell)
 *
 * @param me (IN)    Pointer to the module struct
 * @param jobid (IN) Job id 
 *
 * @return Error code
 *
 * @warning flags is currently ignored, but should be set to 0 for
 * future compatibility.  Will be used to specify how to kill
 * processes (0 will be same as a "kill <pid>"
 */
typedef int
(*mca_pcm_base_kill_job_fn_t)(struct mca_pcm_base_module_1_0_0_t* me,
                              mca_ns_base_jobid_t jobid, int flags);


/**
 * Deallocate requested resources
 *
 * Return the resources for the given jobid to the system.
 *
 * @param me (IN)    Pointer to the module struct
 * @param jobid (IN) Jobid associated with the resources to be freed.
 * @param nodes (IN) Nodelist from associated allocate_resource call.
 *                   All associated memory will be freed as appropriate.
 */
typedef int
(*mca_pcm_base_deallocate_resources_fn_t)(struct mca_pcm_base_module_1_0_0_t* me,
                                          mca_ns_base_jobid_t jobid,
                                          ompi_list_t *nodelist);


/**
 * Base module structure for the PCM
 *
 * Base module structure for the PCM - presents the required function
 * pointers to the calling interface. 
 */
struct mca_pcm_base_module_1_0_0_t {
    mca_pcm_base_allocate_resources_fn_t pcm_allocate_resources;
    mca_pcm_base_spawn_procs_fn_t pcm_spawn_procs;
    mca_pcm_base_kill_proc_fn_t pcm_kill_proc;
    mca_pcm_base_kill_job_fn_t pcm_kill_job;
    mca_pcm_base_deallocate_resources_fn_t pcm_deallocate_resources;
    mca_pcm_base_component_finalize_fn_t pcm_finalize;
};
typedef struct mca_pcm_base_module_1_0_0_t mca_pcm_base_module_1_0_0_t;
typedef struct mca_pcm_base_module_1_0_0_t mca_pcm_base_module_t;


/**
 * Macro for use in modules that are of type pcm v1.0.0
 */
#define MCA_PCM_BASE_VERSION_1_0_0 \
  /* pcm v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* pcm v1.0 */ \
  "pcm", 1, 0, 0

#endif
