/*
 * $HEADER$
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>
#include <stdlib.h>
#include <pwd.h>
#include <sys/stat.h>

#include "ompi_config.h"

#include "include/constants.h"
#include "util/output.h"

#include "util/sys_info.h"

ompi_sys_info_t ompi_system_info = {
                 /* .init =        */            false,
                 /* .sysname =     */            NULL,
	         /* .nodename =    */            NULL,
                 /* .release =     */            NULL,
                 /* .version =     */            NULL,
                 /* .machine =     */            NULL,
                 /* .path_sep =    */            NULL,
                 /* .user =        */            NULL,
                 /* .enviro =      */            NULL,
                 /* .suffix =      */            NULL};

int ompi_sys_info(void)
{
    struct utsname sys_info;
    char *path_name, sep[2];
    struct passwd *pwdent;

    if (ompi_system_info.init) {
	return OMPI_SUCCESS;
    }

    if (0 > uname(&sys_info)) {  /* have an error - set utsname values to indicate */
        if (NULL != ompi_system_info.sysname) {
            free(ompi_system_info.sysname);
            ompi_system_info.sysname = NULL;
        }
        if (NULL != ompi_system_info.nodename) {
            free(ompi_system_info.nodename);
            ompi_system_info.nodename = NULL;
        }
        if (NULL != ompi_system_info.release) {
            free(ompi_system_info.release);
            ompi_system_info.release = NULL;
        }
        if (NULL != ompi_system_info.version) {
            free(ompi_system_info.version);
            ompi_system_info.version = NULL;
        }
        if (NULL != ompi_system_info.machine) {
            free(ompi_system_info.machine);
            ompi_system_info.machine = NULL;
        }
        return OMPI_ERROR;
    } else {
        ompi_system_info.sysname = strdup(sys_info.sysname);
        ompi_system_info.nodename = strdup(sys_info.nodename);
        ompi_system_info.release = strdup(sys_info.release);
        ompi_system_info.version = strdup(sys_info.version);
        ompi_system_info.machine = strdup(sys_info.machine);

	ompi_output(0, "sysname: %s", sys_info.sysname);
    }

    if (NULL != (path_name = getcwd(NULL, 0))) {
        if (strlen(path_name) > 1) {
            sep[0] = path_name[strlen(path_name)-strlen(basename(path_name))-1];
        }
        else {
            sep[0] = path_name[0];
        }
        sep[1] = '\0';
        ompi_system_info.path_sep = strdup(sep);
    }

    /* get the name of the user */
    if ((pwdent = getpwuid(getuid())) != 0) {
	ompi_system_info.user = strdup(pwdent->pw_name);
    } else {
	ompi_system_info.user = strdup("unknown");
    }

    /* set the init flag */
    ompi_system_info.init = true;  /* only indicates that we have been through here once - still have to test for NULL values */

    return(OMPI_SUCCESS);
}
