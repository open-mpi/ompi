/*
 * $HEADER$
 */

/** @file:
 *
 * Populates global structure with process-specific information.
 *
 *
 */


#include <stdbool.h>
#include <sys/types.h>
#include "mca/ns/ns.h"

/**
 * Process information structure
 *
 * The ompi_proc_info() function fills the pid field and obtains the process name,
 * storing that information in the global
 * structure. The structure also holds path names to the universe, job, and process
 * session directories, and to the stdin, stdout, and
 * stderr temp files - however, these are all initialized elsewhere.
 *
 */
struct ompi_proc_info_t {
    bool init;             /**< Certifies that values have been filled.
			    * Certifies that the ompi_sys_info() function has been
			    * called at least once so fields have valid values
			    */
    pid_t pid;             /**< Local process ID for this process */
    ompi_process_name_t *name;  /**< Process name structure */
    bool seed;             /**< Indicate whether or not this is seed daemon */
    char *universe_session_dir;  /**< Location of universe  temp dir.
			    * The session directory has the form
			    * <prefix><openmpi-sessions-user><universe>, where the prefix
			    * can either be provided by the user via the
			    * --tmpdir command-line flag, the use of one of several
			    * environmental variables, or else a default location.
			    */

    char *job_session_dir;  /**< Session directory for job */

    char *proc_session_dir;    /**< Session directory for the process */

    char *sock_stdin;      /**< Path name to temp file for stdin. */
    char *sock_stdout;     /**< Path name to temp file for stdout. */
    char *sock_stderr;     /**< Path name to temp file for stderr. */
};
typedef struct ompi_proc_info_t ompi_proc_info_t;

extern ompi_proc_info_t ompi_process_info;

/**
 * Global structure to store a wide range of information about the process.
 * ompi_proc_info populates a global variable with information about the process
 * being executing. This function should be
 * called only once to setup the information.
 *
 * @param None.
 *
 * @retval OMPI_SUCCESS Successfully initialized the various fields.
 * @retval OMPI_ERROR Failed to initialize one or more fields.
 */

int ompi_proc_info(void);
