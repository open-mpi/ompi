/*
 * $HEADER$
 */

/** @file **/

#include <stdio.h>
#include <sys/utsname.h>


typedef struct utsname ompi_sys_info_t;

extern ompi_sys_info_t ompi_system_info;

int ompi_sys_info(void);
