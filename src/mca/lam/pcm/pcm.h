/* -*- C -*-
 *
 * $HEADER$
 */

/** 
 *  \brief LAM/MPI Interface for Parallel Job & Process Control (pcm)
 *
 * LAM/MPI assumes it is running under a fully operational parallel
 * run-time environment (RTE).  This environment may be provided by
 * batch schedulers such as PBS and LSF, single system image tools
 * such as bproc, or specially designed MPI control daemons (the MPICH
 * mpd or the included LAM daemon).  The functionality provided
 * through the process control interface is dependant on the support
 * of the underlying infrastructure.  For example, lam_pcm_spawn
 * (essentially, the "go do it" part of MPI_COMM_SPAWN) is not
 * available for jobs running under the Qadrics/RMS RTE.  The LAM
 * daemons will always provide the complete pcm interface.
 *
 * Like the other LAM run-time interfaces, the pcm interface is
 * implemented through mca modules (lam/pcm).  For details on the
 * capabilities of a particular module, please see the individual
 * module's documentation.
 *
 * A run-time environment suitable for use by LAM/MPI must provide the
 * following capabilities:
 *
 *  - Remote process startup at job-start time with the ability to:
 *    - push an environment (or a large chunk of an environment) to the started process
 *    - redirect the stdout and stderr of the process to either a file (batch 
 *      schedulers) or the mpirun application (LAM daemons) without interaction from the
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

#include "lam_config.h"

#include "mca/mca.h"
#include "lam/types.h"

/*
 * "PCM" global types
 */
#define LAM_PCM_PROC_MPIRUN 0
#define LAM_PCM_PROC_MPIAPP 1
#define LAM_PCM_PROC_OTHER  2

typedef struct lam_pcm_node {
  int32_t node_num;
  int32_t num_procs;
} lam_pcm_node_t;

typedef struct lam_pcm_control_args {
  char* request;
  char* value;
} lam_pcm_control_args_t;

/*
 * functions every module must provide
 */

typedef int (*mca_pcm_query_fn_t)(int *priority);
typedef struct mca_pcm_1_0_0* (*mca_pcm_init_fn_t)(void);

  /**
   * \func lam_pcm_query_get_nodes
   *
   * Get list of nodes available for execution
   *
   * @param nodes Pointer that will contain array of nodes available
   * @param nodes_len Length of nodes array
   * @param available_procs Number of available processors in the RTE
   *
   * @retval LAM_SUCCESS success
   * @retval LAM_NOT_SUPPORTED Not available
   *
   * Obtain a list of nodes available for execution.  No promises are
   * made that such information is available - for some environments
   * (Quadrics/RMS, etc) nothing is really known about the environment
   * until you ask for it.  If a node/host mapping is unavailable,
   * *nodes will be NULL and nodes_len will be 0.  If the total number
   * of available processors for MPI applications is not available, it
   * will be set to -1 and the function will return "Not available".
   * In the case where both are available, available_procs will be
   * equal to the sum of nodes[0...n].num_procs.
   */
typedef int (*mca_pcm_query_get_nodes)(lam_pcm_node_t **nodes, size_t *nodes_len, 
                                       int available_procs);


  /**
   * Get new parallel job handle
   *
   * @param parent Parent job handle (NULL if not in parallel job)
   *
   * @retval NULL failure
   * @retval non-NULL sucess
   *
   * The run-time environment tracks MPI applications through a
   * parallel job handle, which is a char* string.  This string can be
   * used to request information about the status of a currently
   * running job, kill the job, etc.
   *
   * The parent parameter allows the run-time system to provide a
   * process tree (spawn, etc.) if the user really wants such
   * information.  For mpirun, it should just be NULL.
   *
   * \warning The handle must be released using lam_pcm_handle_free
   */
typedef lam_job_handle_t (*mca_pcm_handle_new_fn_t)(lam_job_handle_t parent);


  /**
   * Get my parallel job handle
   *
   * @retval NULL failure - environment not properly initialized
   * @retval non-NULL success
   *
   * Return the parallel job handle for the currently running process
   *
   * \warning The handle must be released using lam_pcm_handle_free
   */
typedef lam_job_handle_t (*mca_pcm_handle_get_fn_t)(void);


  /**
   * Free a job handle
   *
   * @param job_handle Poiner to a lam_job_handle_t
   * 
   * Free a job handle returned by lam_pcm_handle_new or
   * lam_pcm_handle_get.
   */
typedef void (*mca_pcm_handle_free_fn_t)(lam_job_handle_t *job_handle);


  /**
   * Ask if mca module can spawn processes
   * 
   * @param job_handle Parallel job handle of running process
   *
   * @retval LAM_SUCCESS LAM can spawn more jobs
   * @retval LAM_NOT_SUPPORTED LAM can not spawn more jobs
   *
   * Ask the currently running mca module for the runtime environment
   * if it supports spawning more processes.  This question should
   * always return LAM_SUCCESS (yes) if called from mpirun.  Useful
   * for asking if MPI_SPAWN and friends can run.
   */
typedef int (*mca_pcm_job_can_sapwn_fn_t)(lam_job_handle_t job_handle);


  /**
   * Configure arguments for the parallel job to be started
   *
   * @param job_handle Parallel job handle to configure
   * @param opts Array of key=value structures requesting job behaviour
   * @param opts_len Length of opts array
   *
   * @retval LAM_SUCCESS Sucess
   * @retval LAM_ERROR Unkonwn failure
   *
   * Configure the job using key=value arguments.  The meanings of the
   * arguments are up to the specific mca module providing run-time support.
   *
   * Common key values will be provided here once MCAs begin to use
   * this function.  The only existing module no-ops this entire
   * function.
   * 
   * \Warning It is an error to call this function more than once on a single
   * job handle.
   */
typedef int (*mca_pcm_job_set_arguments_fn_t)(lam_job_handle_t job_handle, 
                                              lam_pcm_control_args_t* opts, 
                                              size_t opts_len);


  /**
   * Launch processes across parallel job
   *
   * @param job_handle Parallel job handle within which to launch processes
   * @param nodes Array of nodes structures describing targets of launch
   * @param nodes_len Length of nodes
   * @param file Process to laucnh (does not have to be equal to argv[0])
   * @param argc Length of argv
   * @param argv Argv array for launched processes
   * @param env Environment array for launched process.  See note below
   *
   * @retval LAM_SUCCESS Success
   * @retval LAM_ERR_RESOURCE_BUSY Try again real soon now
   * @retval LAM_ERR_NOT_SUPPORTED non-MPIRUN process can not spawn jobs
   * @retval LAM_FAILURE Unkonwn failure
   *
   * Launch num_procs nodes[?].processes on nodes[?].node_num for each
   * nodes entry, as part of job_handle's job.  The env array should
   * contain any environment variables that should be pushed to the
   * remote processes.  The mca may provide a more detailed
   * environment if necessary (bporc, etc.).
   *
   * LAM_ERR_NOT_SUPPORTED will be returned if the mca module does not
   * support spawning of new applications from
   */
typedef int (*mca_pcm_job_launch_procs_fn_t)(lam_job_handle_t job_handle, 
                                             lam_pcm_node_t *nodes, 
                                             size_t nodes_len, const char* file, 
                                             int argc, const char* argv[], 
                                             const char *env[]);


  /**
   * Do rendezvous duties after launching parallel job
   *
   * @param job_handle Parallel job handle to run through startup
   *
   * @retval LAM_SUCCESS Success
   * @retval LAM_FAILURE Unknown failure
   *
   * Do the civic duties required to complete the rendezvous part of
   * the startup protocol.  After this, the MPI application should
   * know who all its neighbors are.  It is, of course, completely
   * possible that the MCA module has been in the background doing
   * this all along and didn't bother to tell you.  When this function
   * returns, it is safe to assume that all rendezvous is complete
   * (ie, you can exit and not mess anything up
   */
typedef int (*mca_pcm_job_rendezvous_fn_t)(lam_job_handle_t job_handle);


  /**
   * Wait for job completion
   *
   * @param job_handle Parallel job handle to wait on
   *
   * @retval LAM_SUCCESS Success
   * @retval LAM_ERR_INTERUPTED Interupted (due to signal, etc.)
   *
   * The LAM parallel version of "wait".  It is not required to wait
   * on a job at termination, as job results will be expunged over
   * time as resource limits dictate.
   */
typedef int (*mca_pcm_job_wait_fn_t)(lam_job_handle_t job_handle);


  /**
   * Request job status
   *
   * @param job_handle Parallel job handle to query
   * @param running Job is running, if true
   *
   * @retval LAM_SUCCESS Success
   * @retval LAM_ERR_BAD_PARAM Invalid job handle
   *
   * Ask if job is running.  If job has recently finished, this does
   * not imply wait the pcm interface will call wait for you.
   */
typedef int (*mca_pcm_job_running_fn_t)(lam_job_handle_t job_handle, 
                                        int* running);


  /**
   * Request list of job handles running in current environment
   *
   * @param handles Pointer to job handles array
   * @param handles_len length of handles array
   *
   * @retval LAM_ERR_NOT_IMPLEMENTED Not implemented
   *
   * Query the environment about currently running jobs.  Intended for
   * applications outside MPI and mpirun, to be user friendly and all
   * those things.  mca modules are not required to support this function.
   *
   * \warning This function is not yet implemented.
   */
typedef int (*mca_pcm_job_list_running_fn_t)(lam_job_handle_t **handles, 
                                             size_t handles_len);


  /**
   * Do process startup code
   *
   * @retval LAM_SUCCESS Success
   * @retval LAM_ERR_FATAL Fatal error occurred
   * @retval LAM_ERROR Unkonwn failure
   *
   * Do all communication work required to get peer list and establish
   * the out of band communictaion mechanism.  If a pcm interface uses
   * fork()/exec() to start other processes on the current node, it
   * should do so and complete all rendezvous before returning from
   * this function.
   *
   * The mca module is free to start the oob interface as soon as it
   * as provided the oob interface enough information to do so (tight
   * integration with the oob mca module is probably required to meet
   * this constraint).
   */
typedef int (*mca_pcm_proc_startup_fn_t)(void);


  /**
   * Get peers list
   *
   * @retval LAM_ERR_NOT_IMPLEMENTED Function not implemented
   *
   * Get list of peers in the parallel job.  Should not require any
   * communication with other nodes (communication with processes on
   * this node are allowed).
   *
   * \warning This function is not implemented and its argument list
   * will obviously change in the very near future.
   */
typedef int (*mca_pcm_proc_get_peers_fn_t)(void);


  /**
   * Get my entry in the peers list
   *
   * @retval LAM_ERR_NOT_IMPLEMENTED Function not implemented
   *
   * Get my entry in the peers list
   *
   * \warning This function is not implemented and its argument list
   * will obviously change in the very near future.
   */
typedef  int (*mca_pcm_proc_get_me_fn_t)(void);

  /**
   * Get my entry in the peers list
   *
   * @retval LAM_ERR_NOT_IMPLEMENTED Function not implemented
   *
   * Get my entry in the peers list
   *
   * \warning This function is not implemented and its argument list
   * will obviously change in the very near future.
   */
typedef int (*mca_pcm_proc_get_parent_fn_t)(void);

typedef int (*mca_pcm_finalize_fn_t)(void);


/*
 * Ver 1.0.0
 */
typedef struct mca_pcm_module_1_0_0 {
  mca_module_1_0_0_t super;

  mca_pcm_query_fn_t pcmm_query;
  mca_pcm_init_fn_t pcmm_init;
  mca_pcm_finalize_fn_t pcmm_finalize;
} mca_pcm_module_1_0_0_t;

typedef struct mca_pcm_1_0_0 {
  mca_1_0_0_t super;

  mca_pcm_query_get_nodes_fn_t pcm_query_get_nodes;

  mca_pcm_handle_new_fn_t pcm_handle_new;
  mca_pcm_handle_get_fn_t pcm_handle_get;
  mca_pcm_handle_free_fn_t pcm_handle_free;

  mca_pcm_job_can_spwan_fn_t pcm_job_can_spawn;
  mca_pcm_job_set_arguments_fn_t pcm_job_set_arguments;
  mca_pcm_job_launch_procs_fn_t pcm_job_launch_procs;
  mca_pcm_job_rendezvous_fn_t pcm_job_rendezvous;
  mca_pcm_job_wait_fn_t pcm_job_wait;
  mca_pcm_job_running_fn_t pcm_job_running;
  mca_pcm_job_list_running_fn_t pcm_job_list_running;

  mca_pcm_proc_startup_fn_t pcm_proc_startup;
  mca_pcm_proc_get_peers_fn_t pcm_proc_get_peers;
  mca_pcm_proc_get_me_fn_t pcm_proc_get_me;
  mca_pcm_proc_get_parent_fn_t pcm_proc_get_parent;
} mca_pcm_module_1_0_0_t;

typedef mca_pcm_module_1_0_0_t mca_pcm_module_t;
typedef mca_pcm_1_0_0_t mca_pcm_t;

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_pcm_base_open(lam_cmd_line_t *cmd);
  int mca_pcm_base_close(void);

  bool mca_pcm_base_is_checkpointable(void)

  int mca_pcm_base_checkpoint(void);
  int mca_pcm_base_continue(void);
  int mca_pcm_base_restart(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Global struct holding the selected module's function pointers
 */
extern mca_pcm_t mca_pcm;

#endif
