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
#include "util/os_path.h"
#include "util/os_create_dirpath.h"

static int ompi_check_dir(bool create, char *directory);

#define OMPI_DEFAULT_TMPDIR "tmp"

static int ompi_check_dir(bool create, char *directory)
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

char *ompi_find_session_dir(bool create, char *prefix)
{
    char *tmpprefix=NULL, *tmp=NULL;
    char *sessions=NULL;

    sprintf(sessions, "openmpi-sessions-%s", ompi_system_info.user);

    if (NULL != prefix) {
	tmpprefix = strdup(ompi_os_path(false, prefix, sessions, NULL)); /* make sure it's an absolute pathname */
	if (OMPI_SUCCESS == ompi_check_dir(create, tmpprefix)) { /* check for existence and access, or create it */
	    free(sessions);
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
	    free(sessions);
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
	    free(sessions);
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
	    free(sessions);
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
	free(sessions);
	return(tmpprefix);
    }

    /* possibilities exhausted - time to surrender! */
    if (tmp != NULL) {
        free(tmp);
    }
    if (tmpprefix != NULL) {
        free(tmpprefix);
    }
    if (sessions != NULL) {
        free(sessions);
    }
    return(NULL);
}


/*
 *  ompi_session_dir_init
*/

int ompi_session_dir_init(char *prefix, char *universe)
{

    char *tmpsuffix = NULL;
    char *tmpprefix = NULL;
    char *name;

    /* check if universe is specified - if not, use "default-universe" */
    if (NULL == universe) {
	universe = strdup("default-universe");
    }

    /* check to see if ompi_system_info populated - otherwise, error out */
    if (!ompi_system_info.init) {
	return(OMPI_ERROR);
    }

    /* locate the ompi-sessions directory - create it if it doesn't exist */
    if (NULL == (tmpprefix = ompi_find_session_dir(true, prefix))) { /* couldn't find nor create the sessions directory */
	return (OMPI_ERROR);
    }

    /* set up the name of the universe session directory, which is prefix/universe, and try to create it */
    name = ompi_os_path(false, tmpprefix, universe, NULL);
    if (OMPI_ERROR == ompi_os_create_dirpath(name, S_IRWXU)) { /* couldn't create the user directory */
	free(tmpprefix);
	free(name);
	return(OMPI_ERROR);
    }

    /* store the sessions directory information */
    ompi_system_info.session_dir = strdup(name);

    /* set up the prefix environment */
    /* after careful consideration, it was decided not to put this
     * back in the environment.  Processes that inherit from this
     * process will have the right things to get the same answer.  But
     * this became a major issue all around, because the session
     * prefix is not always the same across nodes and setting the env
     * caused MPIRUN to push the env variable out, which was just
     * causing major badness. 
     */

    /* may need to include the ability to detect if the system is automatically putting a suffix
     * on our files. Not sure what this means yet, so nothing is implemented at this time.
     */

    /* clean up */
    if (tmpprefix != NULL) {
	free(tmpprefix);
    }
    if (tmpsuffix != NULL) {
	free(tmpsuffix);
    }
    if (name != NULL) {
	free(name);
    }

    return(OMPI_SUCCESS);
}
