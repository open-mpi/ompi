/*
 * $HEADER$
 */

/** @file:
 *
 * Populates global structure with system-specific information.
 */


#include <stdbool.h>
#include <sys/utsname.h>
#include <sys/types.h>

/**
 * System information structure
 *
 * The ompi_sys_info() function fills the sysname, nodename, release, version, machine,
 * path_sep, and user fields, but does not populate
 * the session_dir, enviro, suffix, or sock_* fields. These latter fields are populated by other
 * functions as required.
 *
 */
struct ompi_sys_info_t {
    bool init;             /**< Certifies that values have been filled.
			    * Certifies that the ompi_sys_info() function has been
			    * called at least once so fields have valid values
			    */
    pid_t pid;             /* Process ID for this process */
    char *sysname;         /**< Name of OS in use on this node. */
    char *nodename;        /**< Fully qualified host name on the network. */
    char *release;	   /**< Release level of the operating system. */
    char *version;	   /**< Version of the operating system release. */
    char *machine;	   /**< Type of hardware composing this node. */
    char *path_sep;        /**< Path separation char, saved as string.
			    * The character used to separate directories in the path - 
			    * a value that is usually either a '\' or '/', depending
			    * upon the operating system
			    */
    char *user;            /**< User id on this system. */
    char *session_dir;     /**< Location of user writable temp dir.
			    * The session directory has the form
			    * <prefix><openmpi-sessions>, where the prefix
			    * can either be provided by the user via the
			    * --tmpdir command-line flag, the use of one of several
			    * environmental variables, or else a default location. The
			    * function ompi_session_dir_init() develops
			    * the name of this directory, creates it, and stores the name
			    * in this location.
			    */
    char *enviro;          /**< Computing environment employed on this system.
			    * Indicates the local computing environment for managing
			    * and scheduling resources - e.g., SLURM, PBS, LSF, or BProc
			    */
    char *suffix;          /**< Automatic suffix added to file names.
			    * Some computing environments automatically "tag" files
			    * created by applications with a computer-generated suffix
			    * to ensure uniqueness of the file name. This field records
			    * that value for future use.
			    */
    char *sock_stdin;      /**< Path name to temp file for stdin. */
    char *sock_stdout;     /**< Path name to temp file for stdout. */
    char *sock_stderr;     /**< Path name to temp file for stderr. */
};
typedef struct ompi_sys_info_t ompi_sys_info_t;

extern ompi_sys_info_t ompi_system_info;

/**
 * Discover and record a wide range of information about the system upon which
 * this code is executing. ompi_sys_info populates a global variable with information about the system
 * upon which the process is executing. This function can be executed multiple times - the universe
 * process through ompi_init(), and each application process via mpi_init().
 *
 * @retval None
 */

void ompi_sys_info(void);
