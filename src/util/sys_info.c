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
#include "util/sys_info.h"
#include "util/os_session_dir.h"
#include "util/cmd_line.h"
#include "util/common_cmd_line.h"

ompi_sys_info_t ompi_system_info = {
                 /* .init =        */     false,
		 /* .pid =         */     0,
                 /* .sysname =     */     NULL,
	         /* .nodename =    */     NULL,
                 /* .release =     */     NULL,
                 /* .version =     */     NULL,
                 /* .machine =     */     NULL,
                 /* .path_sep =    */     NULL,
                 /* .user =        */     NULL,
                 /* .session_dir = */     NULL,
                 /* .enviro =      */     NULL,
                 /* .suffix =      */     NULL,
                 /* .sock_stdin =  */     NULL,
                 /* .sock_stdout = */     NULL,
                 /* .sock_stderr = */     NULL};


int ompi_sys_info(void)
{
    struct utsname sys_info;
    char *path_name, sep[2];
    char *universe = NULL;
    char *tmpdir = NULL;
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

    /* get the process id */
    ompi_system_info.pid = getpid();

    /* set the init flag so that session_dir_init knows not to come back here */
    ompi_system_info.init = true;  /* only indicates that we have been through here once - still have to test for NULL values */

    /* see if user specified universe name */
    if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "universe")) {
	if (NULL == ompi_cmd_line_get_param(ompi_common_cmd_line, "universe", 0, 0)) {
	    return(OMPI_ERROR);
	}
	universe = strdup(ompi_cmd_line_get_param(ompi_common_cmd_line, "universe", 0, 0));
    }

    /* see if user specified session directory prefix */
    if (ompi_cmd_line_is_taken(ompi_common_cmd_line, "tmpdir")) {
	if (NULL == ompi_cmd_line_get_param(ompi_common_cmd_line, "tmpdir", 0, 0)) {
	    return(OMPI_ERROR);
	}
	tmpdir = strdup(ompi_cmd_line_get_param(ompi_common_cmd_line, "tmpdir", 0, 0));
    }

    /* get the session directory setup */
    if (OMPI_ERROR == ompi_session_dir_init(tmpdir, universe)) {
    /* this is a serious error, so return the error condition */
        return(OMPI_ERROR);
    }

    return(OMPI_SUCCESS);
}
