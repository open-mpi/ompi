/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Interface into the Open MPI Run Time Environment
 */
#ifndef OMPI_RUNTIME_H
#define OMPI_RUNTIME_H

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "mca/gpr/gpr.h"
#include "util/cmd_line.h"

#include "runtime/runtime_types.h"
#include "mca/ns/ns.h"

/* For backwards compatibility.  If you only need MPI stuff, please include
   mpiruntime/mpiruntime.h directly */
#include "mpi/runtime/mpiruntime.h"

/* constants defining runtime-related segment naming conventions for the
 * registry
 */
#define OMPI_RTE_JOB_STATUS_SEGMENT "ompi-job-status"
#define OMPI_RTE_OOB_SEGMENT        "ompi-oob"
#define OMPI_RTE_VM_STATUS_SEGMENT  "ompi-vm-status"
#define OMPI_RTE_SCHED_SEGMENT      "ompi-sched"
#define OMPI_RTE_MODEX_SEGMENT      "ompi_modex"

/* constants for spawn constraints */

/** Spawn constraint - require multi-cell support.  The selected spawn
    system must be capable of starting across multiple cells.  This
    allows multiple pcms to be used to satisfy a single resource
    allocation request */
#define OMPI_RTE_SPAWN_MULTI_CELL 0x0001
/** Spawn constraint - require ability to launch daemons.  The
    selected spawn system must be capable of starting daemon process.
    Setting this flag will result in a spawn service that does not
    neccessarily provide process monitoring or standard I/O
    forwarding.  The calling process may exit before all children have
    exited. */
#define OMPI_RTE_SPAWN_DAEMON     0x0002
/** Spawn constraint - require quality of service support.  The
    selected spawn system must provide I/O forwarding, quick process
    shutdown, and process status monitoring. */
#define OMPI_RTE_SPAWN_HIGH_QOS   0x0004
/** Spawn constraint - caller is an MPI process.  The caller is an MPI
    application (has called MPI_Init).  This should be used only for
    MPI_COMM_SPAWN and MPI_COMM_SPAWN_MULTIPLE.  The calling process
    will follow the semantics of the MPI_COMM_SPAWN_* functions. */
#define OMPI_RTE_SPAWN_FROM_MPI   0x0008

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /* globals used by RTE - instanced in ompi_rte_init.c */

    OMPI_DECLSPEC extern int ompi_rte_debug_flag;

    /* Define the info structure underlying the Open MPI universe system
    * instanced in ompi_rte_init.c */

    struct ompi_universe_t {
	char *name;
	char *host;
	char *uid;
	pid_t pid;
	bool persistence;
	char *scope;
	bool probe;
	bool console;
        char *ns_replica;   /**< OOB contact info for name server */
        char *gpr_replica;  /**< OOB contact info for GPR */
	char *seed_contact_info;  /**< OOB contact info for universe seed */
	bool console_connected;   /**< Indicates if console is connected */
	char *scriptfile;   /**< Name of file containing commands to be executed */
	char *hostfile;   /**< Name of file containing list of hosts to be built into virtual machine */
    };
    typedef struct ompi_universe_t ompi_universe_t;

OMPI_DECLSPEC extern ompi_universe_t ompi_universe_info;


    struct ompi_rte_process_status_t {
	ompi_status_key_t status_key;
	ompi_exit_code_t exit_code;
    };
    typedef struct ompi_rte_process_status_t ompi_rte_process_status_t;


    struct ompi_rte_vm_status_t {
	char *nodename;
	ompi_list_t processes;
    };
    typedef struct ompi_rte_vm_status_t ompi_rte_vm_status_t;

    struct ompi_rte_vm_process_t {
	ompi_list_item_t *item;
	ompi_process_name_t *name;
	int32_t local_pid;
    };
    typedef struct ompi_rte_vm_process_t ompi_rte_vm_process_t;

    /**
     * Initialize the Open MPI support code
     *
     * This function initializes the Open MPI support code, including
     * malloc debugging and threads.  It should be called exactly once
     * by every application that utilizes any of the Open MPI support
     * libraries (including MPI applications, mpirun, and mpicc).
     *
     * This function should be called before \code ompi_rte_init, if
     * \code ompi_rte_init is to be called.
     */
OMPI_DECLSPEC    int ompi_init(int argc, char* argv[]);

    /**
     * Finalize the Open MPI support code
     *
     * Finalize the Open MPI support code.  Any function calling \code
     * ompi_init should call \code ompi_finalize.  This function should
     * be called after \code ompi_rte_finalize, if \code
     * ompi_rte_finalize is called.
     */
OMPI_DECLSPEC    int ompi_finalize(void);

    /**
     * Abort the current application with a pretty-print error message
     *
     * Aborts currently running application with \code abort(), pretty
     * printing an error message if possible.  Error message should be
     * specified using the standard \code printf() format.
     */
OMPI_DECLSPEC    int ompi_abort(int status, char *fmt, ...);


    /**
     * Initialize the Open MPI run time environment
     *
     * Initlize the Open MPI run time environment, including process
     * control and out of band messaging.  This function should be
     * called exactly once, after \code ompi_init.  This function should
     * be called by every application using the RTE interface, including
     * MPI applications and mpirun.
     */
OMPI_DECLSPEC    int ompi_rte_init(ompi_cmd_line_t *cmd_line, bool *allow_multi_user_threads, bool *have_hidden_threads);

    /**
     * Finalize the Open MPI run time environment
     *
     */
OMPI_DECLSPEC    int ompi_rte_finalize(void);


    /**
     * Request a handle for spawning jobs
     *
     * Request a handle for allocating resources and spawning a job.
     * This is the first step in starting a new set of processes.  It
     * will load the best available set of pcm components for starting
     * a job according to the \c criteria provided.
     *
     * The returned job handle should be OBJ_RELEASE()'ed when no
     * further use of the particular job handle is needed.  It is
     * possible that consecutive calls to this function with the same
     * \c criteria will return a pointer to the same object.  In these
     * situations, the reference count on the object will be adjusted
     * as appropriate.
     *
     * The returned handle can be used to call the process startup
     * related functions multiple times, both in the same job and in
     * different jobs.
     *
     * @param criteria (IN) Selection criteria.  A bitmask of the
     *                      constants defined in \c runtime.h starting
     *                      with \c OMPI_RTE_SPAWN_*
     * @param have_threads (IN) Whether the current running process is
     *                      multi-threaded or not.  true means there
     *                      may be concurrent access into the
     *                      underlying components *and* that the
     *                      components may launch new threads.
     * @return jobhandle (OUT) Pointer to an \c ompi_rte_jobhandle.
     *                      If no available pcm components are capable
     *                      of meeting criteria, \c NULL is returned.
     */
OMPI_DECLSPEC    ompi_rte_spawn_handle_t* ompi_rte_get_spawn_handle(int criteria,
                                                       bool have_threads);


    /**
     * Allocate requested resources
     *
     * Allocate the specified nodes / processes for use in a new job.
     * This function should be called exactly once per call to \c
     * ompi_rte_spawn_procs.
     *
     * @param handle (IN) Handle from \c ompi_rte_get_spawn_handle
     * @param jobid (IN) Jobid with which to associate the given resources.
     * @param nodes (IN) Number of ndoes to try to allocate.  If 0, the
     *                   allocator will try to allocate \c procs processes
     *                   on as many nodes as are needed.  If positive, 
     *                   will try to allocate \c procs process slots 
     *                   per node.  If both nodes and procs are 0,
     *                   will attempt to return as many resources as
     *                   possible
     * @param procs (IN) Number of processors to try to allocate.  See the note
     *                   for \c nodes for usage.
     * @return List of <code>ompi_rte_node_allocation_t</code>s
     *                   describing the allocated resources or NULL on
     *                   error (error will be in errno).  If the
     *                   number of requested resources is not
     *                   available, errno will be set to \c
     *                   OMPI_ERR_OUT_OF_RESOURCE.  This is not a
     *                   fatal error - \c ompi_rte_allocate_resources
     *                   can be called again, but with a smaller
     *                   resource request.
     *
     * @note In the future, a more complex resource allocation
     *       function may be added, which allows for complicated
     *       resource requests.  This function will continue to exist
     *       as a special case of that function.
     *
     *       Some systems are not capable of providing a maximum
     *       available resource count and there is an inherent race
     *       condition to do so in many other systems.  On these
     *       systems, errno will be set to \c OMPI_ERR_NOT_SUPPORTED.
     *       This is not a fatal error - \c
     *       ompi_rte_allocate_resources can be called again, but
     *       without nodes = 0, procs = 0.
     */
OMPI_DECLSPEC    ompi_list_t* ompi_rte_allocate_resources(ompi_rte_spawn_handle_t* handle,
                                             mca_ns_base_jobid_t jobid, 
                                             int nodes, int procs);


    /**
     * Spawn a job
     *
     * Start a job with given jobid and starting vpid (should probably be
     * 0 for the forseeable future).  The job is specified using an array
     * of \c mca_pcm_base_schedule_t structures, which give both process
     * and location information.
     *
     * @param handle (IN) Handle from \c ompi_rte_get_spawn_handle
     */
OMPI_DECLSPEC    int ompi_rte_spawn_procs(ompi_rte_spawn_handle_t* handle,
                             mca_ns_base_jobid_t jobid, 
                             ompi_list_t *schedule_list);


    /**
     * Get my name
     *
     * @return my name
     */
OMPI_DECLSPEC    ompi_process_name_t* ompi_rte_get_self(void);


    /**
     * Get names of peer processes which have been launched
     *
     * @param peers (OUT) Pointer to a pointer of
     *                    ompi_process_name_t. \c *peers will be set
     *                    to point to a statically allocated buffer
     *                    containing the array of peer processes
     *                    started with the current process.  If \c
     *                    peers is NULL, then only \c npeers is
     *                    updated.
     * @param npeers (OUT) pointer to an integer that will be updated
     *                    with the total number of peers started with
     *                    the current process.  Also the length of \c
     *                    *peers array if \c peers is not \c NULL
     *
     * @return OMPI_SUCCESS on success
     *         OMPI_ERR_NOT_IMPLEMENTED if the underlying module is
     *                     not properly loaded.
     *
     */
OMPI_DECLSPEC    int ompi_rte_get_peers(ompi_process_name_t **peers, size_t *npeers);

    /**
     * Get current status of the process
     */
OMPI_DECLSPEC    ompi_rte_process_status_t *ompi_rte_get_process_status(ompi_process_name_t *proc);

    /**
     * Set process status
     */

OMPI_DECLSPEC    int ompi_rte_set_process_status(ompi_rte_process_status_t *status,
				                 ompi_process_name_t *proc);

    /**
     * Unpack the process status structure stored on the registry
     */
OMPI_DECLSPEC    ompi_rte_process_status_t *ompi_rte_unpack_process_status(ompi_registry_value_t *value);

    /**
     * Hold for startup message to arrive, then decode it.
     */

OMPI_DECLSPEC    int ompi_rte_wait_startup_msg(void);

    /**
     * Hold for shutdown message to arrive, then decode it.
     */

OMPI_DECLSPEC    int ompi_rte_wait_shutdown_msg(void);

    /**
     * Change state as processes complete registration/unregistration
     */

OMPI_DECLSPEC    void ompi_rte_all_procs_registered(ompi_registry_notify_message_t* match, void* cbdata);

OMPI_DECLSPEC    void ompi_rte_all_procs_unregistered(ompi_registry_notify_message_t* match, void* cbdata);

OMPI_DECLSPEC	 int ompi_rte_monitor_procs_registered(void);

OMPI_DECLSPEC    int ompi_rte_monitor_procs_unregistered(void);

    /**
     * Kill a specific process in this cell
     *
     * @param process_name Which process needs to be killed.
     * @return Error code
     *
     * @warning flags is currently ignored, but should be set to 0 for
     * future compatibility.  Will be used to specify how to kill
     * processes (0 will be same as a "kill <pid>"
     */
    OMPI_DECLSPEC int ompi_rte_kill_proc(ompi_process_name_t *name, 
                                         int signal, 
                                         int errorcode, 
                                         int flags);


    /**
     * Kill all the processes in a job. This will probably find out all
     * the processes in the job by contacting the registry and then call
     * mca_pcm_kill_process for each process in the job (for a cell)
     *
     * @param jobid Job id 
     * @return Error code
     *
     * @warning flags is currently ignored, but should be set to 0 for
     * future compatibility.  Will be used to specify how to kill
     * processes (0 will be same as a "kill <pid>"
     */
    OMPI_DECLSPEC int ompi_rte_kill_job(mca_ns_base_jobid_t jobid, 
                                        int signal,
                                        int errorcode,
                                        int flags);


    /**
     * Deallocate requested resources
     *
     * Return the resources for the given jobid to the system.
     *
     * @param handle (IN) Handle from \c ompi_rte_get_spawn_handle
     * @param jobid (IN) Jobid associated with the resources to be freed.
     * @param nodes (IN) Nodelist from associated allocate_resource call.
     *                   All associated memory will be freed as appropriate.
     */
OMPI_DECLSPEC    int ompi_rte_deallocate_resources(ompi_rte_spawn_handle_t *handle,
                                      mca_ns_base_jobid_t jobid, 
                                      ompi_list_t *nodelist);


    /**
     * Setup rte command line options
     *
     * Defines the command line options specific to the rte/seed daemon
     *
     * @param cmd_line Pointer to an ompi_cmd_line_t object
     * @retval None
     */
OMPI_DECLSPEC    void ompi_rte_cmd_line_setup(ompi_cmd_line_t *cmd_line);


    /**
     * Parse the rte command line for options
     *
     * Parses the specified command line for rte specific options.
     * Fills the relevant global structures with the information obtained.
     *
     * @param cmd_line Command line to be parsed.
     * @retval None
     */
OMPI_DECLSPEC    void ompi_rte_parse_cmd_line(ompi_cmd_line_t *cmd_line);

    /**
     * Parse the rte command line for daemon-specific options
     *
     * Parses the specified command line for rte daemon-specific options.
     * Fills the relevant global structures with the information obtained.
     *
     * @param cmd_line Command line to be parsed.
     * @retval None
     */
OMPI_DECLSPEC    void ompi_rte_parse_daemon_cmd_line(ompi_cmd_line_t *cmd_line);

    /**
     * Check for universe existence
     *
     * Checks to see if a specified universe exists. If so, attempts
     * to connect to verify that the universe is accepting connections.
     * If both ns and gpr replicas provided, first checks for those
     * connections. Gets any missing info from the universe contact.
     *
     * @param None Reads everything from the process_info and system_info
     * structures
     *
     * @retval OMPI_SUCCESS Universe found and connection accepted
     * @retval OMPI_NO_CONNECTION_ALLOWED Universe found, but not persistent or
     * restricted to local scope
     * @retval OMPI_CONNECTION_FAILED Universe found, but connection attempt
     * failed. Probably caused by unclean termination of the universe seed
     * daemon.
     * @retval OMPI_CONNECTION_REFUSED Universe found and contact made, but
     * universe refused to allow connection.
     */
OMPI_DECLSPEC    int ompi_rte_universe_exists(void);

    /**
     * Parse the RTE environmental variables
     *
     * Checks the environmental variables and passes their info (where
     * set) into the respective info structures. Sets ALL Open MPI
     * default values in universe, process, and system structures.
     *
     * @param None
     *
     * @retval None
     */
OMPI_DECLSPEC    void ompi_rte_parse_environ(void);


    /**
     * Register a daemon on the virtual machine segment.
     */
OMPI_DECLSPEC    int ompi_vm_register(void);

    /**
     * Startup a job - notify processes that all ready to begin
     */
OMPI_DECLSPEC   int ompi_rte_job_startup(mca_ns_base_jobid_t jobid);

    /**
     * Shutdown a job - notify processes that all ready to stop
     */
OMPI_DECLSPEC   int ompi_rte_job_shutdown(mca_ns_base_jobid_t jobid);

    /**
     * Complete initialization of the RTE
     */
OMPI_DECLSPEC   int ompi_rte_init_cleanup(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_RUNTIME_H */
