/*
 * $HEADER$
 *
 *  $Id: tmpdir.c $
 *
 *  Function: - 
 */

#include <stdio.h>
#include <pwd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
/*
#include <sys/socket.h>
#include <netdb.h>      MAXHOSTNAMELEN in Solaris
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
   
#include <ompi_debug.h>
#include <ompi_internal.h>
#include <terror.h>
#include <typical.h>
#include <etc_misc.h>
*/

#include "ompi_config.h"
#include "include/constants.h"

#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/os_path.h"
#include "util/os_create_dirpath.h"
#include "util/session_dir.h"

int ompi_check_dir(bool create, char *directory);

#define OMPI_DEFAULT_TMPDIR "tmp"

int ompi_check_dir(bool create, char *directory)
{
    struct stat buf;
    mode_t my_mode = S_IRWXU;  /* at the least, I need to be able to do anything */

    if (0 == stat(directory, &buf)) { /* exists - check access */
        if ((buf.st_mode & my_mode) == my_mode) { /* okay, I can work here */
            return(OMPI_SUCCESS);
        }
    }
    if (create) {
	return(ompi_os_create_dirpath(directory, my_mode)); /* try to create it with proper mode */
    }
    return(OMPI_ERROR);  /* couldn't find it, or don't have access rights, and not asked to create it */
}

char *ompi_session_dir(bool create, char *prefix, char *user, char *universe, char *job, char *proc)
{
    char *tmpprefix=NULL, *tmp=NULL;
    char *sessions;
    int length=0;

    if (NULL == user || NULL == universe) { /* error conditions - have to provide at least that much */
	return(NULL);
    }

    length = strlen("openmpi-sessions-") + strlen(user) + strlen(universe) + 2;

    if (NULL == job && NULL != proc) { /* can't give a proc without a job */
	return(NULL);
    }

    if (NULL != proc) {
	length = length + strlen(job) + strlen(proc) + 2;
	sessions = (char *)malloc(length*sizeof(char));
	sprintf(sessions, "openmpi-sessions-%s%s%s%s%s%s%s", user, ompi_system_info.path_sep, universe,
		ompi_system_info.path_sep, job, ompi_system_info.path_sep, proc);
    } else if (NULL != job) {
	length = length + strlen(job) + 1;
	sessions = (char *)malloc(length*sizeof(char));
	sprintf(sessions, "openmpi-sessions-%s%s%s%s%s", user, ompi_system_info.path_sep, universe,
		ompi_system_info.path_sep, job);
    } else {
	sessions = (char *)malloc(length*sizeof(char));
	sprintf(sessions, "openmpi-sessions-%s%s%s", user, ompi_system_info.path_sep, universe);
    }

    if (NULL != prefix) {
	tmpprefix = strdup(ompi_os_path(false, prefix, sessions, NULL)); /* make sure it's an absolute pathname */
	if (OMPI_SUCCESS == ompi_check_dir(create, tmpprefix)) { /* check for existence and access, or create it */
	    return(tmpprefix);
	}
	else {
	    return(NULL); /* user specified location, but we can't access it nor create it */
	}
    }
 
    if (NULL != getenv("OMPI_PREFIX_ENV")) {
	tmp = strdup(getenv("OMPI_PREFIX_ENV"));
	tmpprefix = strdup(ompi_os_path(false, tmp, sessions, NULL));
	if (OMPI_SUCCESS == ompi_check_dir(create, tmpprefix)) { /* check for existence and access, or create it */
	    free(tmp);
	    return(tmpprefix);
	}
    }
   if (tmp != NULL) {
        free(tmp);
    }
    if (tmpprefix != NULL) {
        free(tmpprefix);
    }

    if (NULL != getenv("TMPDIR")) {
	tmp = strdup(getenv("TMPDIR"));
	tmpprefix = strdup(ompi_os_path(false, tmp, sessions, NULL));
	if (OMPI_SUCCESS == ompi_check_dir(create, tmpprefix)) { /* check for existence and access, or create it */
	    free(tmp);
	    return(tmpprefix);
	}
    }
    if (tmp != NULL) {
        free(tmp);
    }
    if (tmpprefix != NULL) {
        free(tmpprefix);
    }

    if (NULL != getenv("TMP")) {
	tmp = strdup(getenv("TMP"));
	tmpprefix = strdup(ompi_os_path(false, tmp, sessions, NULL));
	if (OMPI_SUCCESS == ompi_check_dir(create, tmpprefix)) { /* check for existence and access, or create it */
	    free(tmp);
	    return(tmpprefix);
	}
    }
    if (tmp != NULL) {
        free(tmp);
    }
    if (tmpprefix != NULL) {
        free(tmpprefix);
    }

    tmp = strdup(OMPI_DEFAULT_TMPDIR);
    tmpprefix = strdup(ompi_os_path(false, tmp, sessions, NULL));
    if (OMPI_SUCCESS == ompi_check_dir(create, tmpprefix)) { /* check for existence and access, or create it */
	free(tmp);
	return(tmpprefix);
    }

    /* possibilities exhausted - time to surrender! */
    if (tmp != NULL) {
        free(tmp);
    }
    if (tmpprefix != NULL) {
        free(tmpprefix);
    }
    return(NULL);
}


