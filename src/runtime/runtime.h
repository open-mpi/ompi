/*
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

#include <sys/types.h>

#include "runtime/runtime_types.h"
#include "mca/ns/ns.h"

/* For backwards compatibility.  If you only need MPI stuff, please include
   mpiruntime/mpiruntime.h directly */
#include "mpi/runtime/mpiruntime.h"

#ifdef __cplusplus
extern "C" {
#endif

    /* globals used by RTE - instanced in ompi_rte_init.c */

    extern int ompi_rte_debug_flag;

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
	bool silent_mode;
        bool ns_replica;
        bool gpr_replica;
	bool web_server;
	char *socket_contact_info;
	char *oob_contact_info;
	bool console_connected;
	char *scriptfile;
	char *hostfile;
    };
    typedef struct ompi_universe_t ompi_universe_t;

    extern ompi_universe_t ompi_universe_info;

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
    int ompi_init(int argc, char* argv[]);

    /**
     * Finalize the Open MPI support code
     *
     * Finalize the Open MPI support code.  Any function calling \code
     * ompi_init should call \code ompi_finalize.  This function should
     * be called after \code ompi_rte_finalize, if \code
     * ompi_rte_finalize is called.
     */
    int ompi_finalize(void);

    /**
     * Abort the current application with a pretty-print error message
     *
     * Aborts currently running application with \code abort(), pretty
     * printing an error message if possible.  Error message should be
     * specified using the standard \code printf() format.
     */
    int ompi_abort(int status, char *fmt, ...);


    /**
     * Initialize the Open MPI run time environment
     *
     * Initlize the Open MPI run time environment, including process
     * control and out of band messaging.  This function should be
     * called exactly once, after \code ompi_init.  This function should
     * be called by every application using the RTE interface, including
     * MPI applications and mpirun.
     */
    int ompi_rte_init_stage1(bool *allow_multi_user_threads, bool *have_hidden_threads);
    int ompi_rte_init_stage2(bool *allow_multi_user_threads, bool *have_hidden_threads);

    /**
     * Finalize the Open MPI run time environment
     *
     */
    int ompi_rte_finalize(void);

   
    /**
     * Allocate requested resources
     *
     * Allocate the specified nodes / processes for use in a new job.
     * Requires a newly created jobid.  The allocation returned may be
     * smaller than requested - it is up to the caller to proceed as
     * appropriate should this occur.  This function should only be called
     * once per jobid.
     *
     * @param jobid (IN) Jobid with which to associate the given resources.
     * @param nodes (IN) Number of ndoes to try to allocate.  If 0, the
     *                   allocator will try to allocate \c procs processes
     *                   on as many nodes as are needed.  If non-zero, 
     *                   will try to allocate \c procs process slots 
     *                   per node.
     * @param procs (IN) Number of processors to try to allocate.  See the note
     *                   for <code>nodes</code> for usage.
     * @return List of <code>ompi_rte_node_allocation_t</code>s
     *                   describing the allocated resources.
     *
     * @note In the future, a more complex resource allocation
     *       function may be added, which allows for complicated
     *       resource requests.  This function will continue to exist
     *       as a special case of that function.
     */
    ompi_list_t* ompi_rte_allocate_resources(mca_ns_base_jobid_t jobid, 
                                             int nodes, int procs);


    /** 
     * This tells you whether the runtime is capable of spawning new
     * processes or not
     *
     * @return True/False
     */
    bool ompi_rte_can_spawn(void);


    /**
     * Spawn a job
     *
     * Start a job with given jobid and starting vpid (should probably be
     * 0 for the forseeable future).  The job is specified using an array
     * of \c mca_pcm_base_schedule_t structures, which give both process
     * and location information.
     *
     */
    int ompi_rte_spawn_procs(mca_ns_base_jobid_t jobid, 
                             ompi_list_t *schedule_list);


    /**
     * Get my name
     *
     * @return my name
     */
    ompi_process_name_t* ompi_rte_get_self(void);


    /**
     * Get names of peer processes which have been launched
     *
     * @param Nothing
     * @return An array of peer names, including me
     */
    int ompi_rte_get_peers(ompi_process_name_t **peers, size_t *npeers);

    /**
     * Setup process info in the registry.
     */

    int ompi_rte_register(void);

    /**
     * Monitor a job - currently implemented by monitoring process 
     * registration/deregistration to/from the GPR.
     */
    
    int ompi_rte_notify(mca_ns_base_jobid_t job, int num_procs);
    int ompi_rte_monitor(void);

    /**
     * Remove process registration.
     */

    int ompi_rte_unregister(void);

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
    int ompi_rte_kill_proc(ompi_process_name_t *name, int flags);


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
    int ompi_rte_kill_job(mca_ns_base_jobid_t jobid, int flags);


    /**
     * Deallocate requested resources
     *
     * Return the resources for the given jobid to the system.
     *
     * @param jobid (IN) Jobid associated with the resources to be freed.
     * @param nodes (IN) Nodelist from associated allocate_resource call.
     *                   All associated memory will be freed as appropriate.
     */
    int ompi_rte_deallocate_resources(mca_ns_base_jobid_t jobid, 
                                      ompi_list_t *nodelist);


    /**
     * Setup rte command line options
     *
     * Defines the command line options specific to the rte/seed daemon
     *
     * @param cmd_line Pointer to an ompi_cmd_line_t object
     * @retval None
     */
    void ompi_rte_cmd_line_setup(ompi_cmd_line_t *cmd_line);


    /**
     * Parse the rte command line for options
     *
     * Parses the specified command line for rte specific options.
     * Fills the relevant global structures with the information obtained.
     *
     * @param cmd_line Command line to be parsed.
     * @retval None
     */
    void ompi_rte_parse_cmd_line(ompi_cmd_line_t *cmd_line);

    /**
     * Parse the rte command line for daemon-specific options
     *
     * Parses the specified command line for rte daemon-specific options.
     * Fills the relevant global structures with the information obtained.
     *
     * @param cmd_line Command line to be parsed.
     * @retval None
     */
    void ompi_rte_parse_daemon_cmd_line(ompi_cmd_line_t *cmd_line);

    /**
     * Check for universe existence
     *
     * Checks to see if a specified universe exists on the local host. If so, attempts
     * to connect to verify that the universe is accepting connections.
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
    int ompi_rte_local_universe_exists(void);

#ifdef __cplusplus
}
#endif

#endif /* OMPI_RUNTIME_H */
