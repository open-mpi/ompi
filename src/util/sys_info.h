/*
 * $HEADER$
 */

/** @file **/

/* sys_info populates a global variable with information about the system
 * upon which the process is executing. Each process executes this function - the universe
 * process through ompi_init(), and application processes via mpi_init().
 *
 * The structure contains the following fields:
 *
 * bool valid     - certifies that the ompi_sys_info has been called at least once
 *                  so fields have valid values
 *
 * char sysname[] - the name of the operating system in use
 *
 * char release[] - current release level of the operating system
 *
 * char version[] - current version level of the operating system
 *
 * char machine[] - type of hardware in use
 *
 * char nodename[] - the network node name of the host, same as that
 *                   returned by gethostname()
 *
 * char path_sep - the character used to separate directories in the path,
 *                 a value that is usually either a '\' or '/', depending
 *                 upon the operating system
 *
 * char tmp_dir - location of user-writable temp directory on this system.
 *                Space for string is allocated in ompi_sys_info().
 *
 */

#include <stdbool.h>
#include <sys/utsname.h>

struct ompi_sys_info_t {
    bool init;             /** Certifies that values have been filled */
    char *sysname;         /** Name of OS */
    char *nodename;        /** Name of this network node */
    char *release;	   /** Release level */
    char *version;	   /** Version level */
    char *machine;	   /** Hardware type */
    /** ---------- above line = fields in utsname structure --------- 
     *  Following fields are added to support Open MPI-specific needs
     */
    char *path_sep;        /** path separation char, saved as string */
    char *user;            /** user id on this system */
    char *session_dir;     /** location of user writable temp dir for storing session info */
    char *enviro;          /** computing environment employed on this system */
    char *suffix;          /** automatic suffix added to file names, if system does this */
    char *sock_stdin;      /** path name to temp file for stdin */
    char *sock_stdout;     /** path name to temp file for stdout */
    char *sock_stderr;     /** path name to temp file for stderr */
};
typedef struct ompi_sys_info_t ompi_sys_info_t;

extern ompi_sys_info_t ompi_system_info;

/**
 * Discover and record a wide range of information about the system upon which
 * this code is executing.
 *
 * @retval None

void ompi_sys_info(void);
