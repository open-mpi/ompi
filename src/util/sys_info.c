/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#include <stdlib.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <sys/stat.h>


#include "include/constants.h"
#include "util/output.h"

#include "util/sys_info.h"

orte_sys_info_t orte_system_info = {
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

int orte_sys_info(void)
{
    struct utsname sys_info;
    char *path_name;

#ifndef WIN32
	struct passwd *pwdent;
	char sep[2];
#else
    #define INFO_BUF_SIZE 32768
    TCHAR info_buf[INFO_BUF_SIZE];
    DWORD info_buf_length = INFO_BUF_SIZE;
	char *sep = "\\";
#endif

	if (orte_system_info.init) {
	return OMPI_SUCCESS;
    }

    if (0 > uname(&sys_info)) {  /* have an error - set utsname values to indicate */
        if (NULL != orte_system_info.sysname) {
            free(orte_system_info.sysname);
            orte_system_info.sysname = NULL;
        }
        if (NULL != orte_system_info.nodename) {
            free(orte_system_info.nodename);
            orte_system_info.nodename = NULL;
        }
        if (NULL != orte_system_info.release) {
            free(orte_system_info.release);
            orte_system_info.release = NULL;
        }
        if (NULL != orte_system_info.version) {
            free(orte_system_info.version);
            orte_system_info.version = NULL;
        }
        if (NULL != orte_system_info.machine) {
            free(orte_system_info.machine);
            orte_system_info.machine = NULL;
        }
        return OMPI_ERROR;
    } else {
        orte_system_info.sysname = strdup(sys_info.sysname);
        orte_system_info.nodename = strdup(sys_info.nodename);
        orte_system_info.release = strdup(sys_info.release);
        orte_system_info.version = strdup(sys_info.version);
        orte_system_info.machine = strdup(sys_info.machine);
    }

#ifndef WIN32
    if (NULL != (path_name = getcwd(NULL, 0))) {
        if (strlen(path_name) > 1) {
            sep[0] = path_name[strlen(path_name)-strlen(basename(path_name))-1];
        }
        else {
            sep[0] = path_name[0];
        }
        sep[1] = '\0';
        orte_system_info.path_sep = strdup(sep);
    }
#else
    /* we can hardcode windows path seperator to be "\" */
    orte_system_info.path_sep = strdup(sep);
#endif




	/* get the name of the user */
#ifndef WIN32
	if ((pwdent = getpwuid(getuid())) != 0) {
	    orte_system_info.user = strdup(pwdent->pw_name);
    } else {
	    orte_system_info.user = strdup("unknown");
    }
#else 
    if (!GetUserName(info_buf, &info_buf_length)) {
	    orte_system_info.user = strdup("unknown");
    } else {
	    orte_system_info.user = strdup(info_buf);
    }
#endif

    /* set the init flag */
    orte_system_info.init = true;  /* only indicates that we have been through here once - still have to test for NULL values */

    return(OMPI_SUCCESS);
}
