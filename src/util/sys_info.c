/*
 * $HEADER$
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>

#include "constants.h"
#include "util/sys_info.h"

ompi_sys_info_t ompi_system_info;

int ompi_sys_info(void)
{
    struct utsname sys_info;
    char *path_name;

    if (0 > uname(&sys_info)) {  /* have an error - set values to indicate */
	strcpy(ompi_system_info.sysname, "NULL");
	strcpy(ompi_system_info.nodename, "NULL");
	strcpy(ompi_system_info.release, "NULL");
	strcpy(ompi_system_info.version, "NULL");
	strcpy(ompi_system_info.machine, "NULL");
	ompi_system_info.path_sep = NULL;
	return(LAM_ERROR);
    }
    strcpy(ompi_system_info.sysname, sys_info.sysname);
    strcpy(ompi_system_info.nodename, sys_info.nodename);
    strcpy(ompi_system_info.release, sys_info.release);
    strcpy(ompi_system_info.version, sys_info.version);
    strcpy(ompi_system_info.machine, sys_info.machine);

    if (NULL == (path_name = getcwd(NULL, 0))) {
	ompi_system_info.path_sep = NULL;
	return(LAM_ERROR);
    }

    if (strlen(path_name) > 1) {
        ompi_system_info.path_sep = path_name[strlen(path_name)-strlen(basename(path_name))-1];
    }
    else {
	ompi_system_info.path_sep = path_name[0];
    }

    return(LAM_SUCCESS);
}
