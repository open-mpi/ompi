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
 */


#include <stdio.h>
#include <sys/utsname.h>


typedef struct utsname ompi_sys_info_t;

extern ompi_sys_info_t ompi_system_info;

int ompi_sys_info(void);
