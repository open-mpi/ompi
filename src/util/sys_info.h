/*
 * $HEADER$
 */

/** @file **/

/* sys_info populates a global variable with information about the system
 * upon which the program is executing. The structure contains the following
 * fields:
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
 */

#include <sys/utsname.h>

struct ompi_sys_info_t {
    char sysname[_SYS_NAMELEN];    /* Name of OS */
    char nodename[_SYS_NAMELEN];   /* Name of this network node */
    char release[_SYS_NAMELEN];	   /* Release level */
    char version[_SYS_NAMELEN];	   /* Version level */
    char machine[_SYS_NAMELEN];	   /* Hardware type */
    char path_sep;                 /* path separation char */
};
typedef struct ompi_sys_info_t ompi_sys_info_t;

extern ompi_sys_info_t ompi_system_info;

int ompi_sys_info(void);
