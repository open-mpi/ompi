/*
 * $HEADER$
 *
 *  $Id: tmpdir.c $
 *
 *  Function: - 
 */

#include "ompi_config.h"

#include <stdio.h>
#include <pwd.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>

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

#include "include/constants.h"

#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/output.h"
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

int ompi_session_dir(bool create, char *prefix, char *user, char *hostid, char *batchid, char *universe, char *job, char *proc)
{
    char *fulldirpath=NULL, *tmp=NULL, *hostname=NULL, *batchname=NULL;
    char *sessions=NULL, *frontend=NULL;
    int return_code;

    if (NULL == user || NULL == universe) { /* error conditions - have to provide at least that much */
	return OMPI_ERROR;
    }

    if (NULL == job && NULL != proc) { /* can't give a proc without a job */
	return OMPI_ERROR;
    }

    if (NULL == hostid) {  /* check if hostname set elsewhere */
	if (NULL == ompi_system_info.nodename) { /* don't have a hostname anywhere  - error */
	    return_code = OMPI_ERROR;
	    goto CLEANUP;
	} else {
	    hostname = strdup(ompi_system_info.nodename);
	}
    } else {
	hostname = strdup(hostid);
    }

    if (NULL == batchid) {
	batchname = strdup("0");
    } else {
	batchname = batchid;
    }

    if (0 > asprintf(&frontend, "openmpi-sessions-%s@%s:%s", user, hostname, batchname)) {
	return_code = OMPI_ERROR;
	goto CLEANUP;
    }

    if (NULL != proc) {
	if (0 > asprintf(&sessions, "%s%s%s%s%s%s%s", frontend,
			 ompi_system_info.path_sep, universe,
		         ompi_system_info.path_sep, job,
			 ompi_system_info.path_sep, proc)) {
	    return_code = OMPI_ERROR;
	    goto CLEANUP;
	}
    } else if (NULL != job) {
	if (0 > asprintf(&sessions, "%s%s%s%s%s", frontend,
			 ompi_system_info.path_sep, universe,
			 ompi_system_info.path_sep, job)) {
	    return_code = OMPI_ERROR;
	    goto CLEANUP;
	}
    } else {
	if (0 > asprintf(&sessions, "%s%s%s", frontend, ompi_system_info.path_sep, universe)) {
	    return_code = OMPI_ERROR;
	    goto CLEANUP;
	}
    }

    if (NULL != prefix) {  /* if a prefix is specified, start looking here */
	tmp = strdup(prefix);
	fulldirpath = strdup(ompi_os_path(false, tmp, sessions, NULL)); /* make sure it's an absolute pathname */
	if (OMPI_SUCCESS == ompi_check_dir(create, fulldirpath)) { /* check for existence and access, or create it */
	    return_code = OMPI_SUCCESS;
	    goto COMPLETE;
	}
   }
 
    /* no prefix was specified, so check other options in order */
    if (NULL != getenv("OMPI_PREFIX_ENV")) {  /* we have prefix enviro var - try that next */
	tmp = strdup(getenv("OMPI_PREFIX_ENV"));
	fulldirpath = strdup(ompi_os_path(false, tmp, sessions, NULL));
	if (OMPI_SUCCESS == ompi_check_dir(create, fulldirpath)) { /* check for existence and access, or create it */
	    return_code = OMPI_SUCCESS;
	    goto COMPLETE;
	}
    } else if (NULL != getenv("TMPDIR")) {
	tmp = strdup(getenv("TMPDIR"));
	fulldirpath = strdup(ompi_os_path(false, tmp, sessions, NULL));
	if (OMPI_SUCCESS == ompi_check_dir(create, fulldirpath)) { /* check for existence and access, or create it */
	    return_code = OMPI_SUCCESS;
	    goto COMPLETE;
	}
    } else if (NULL != getenv("TMP")) {
	tmp = strdup(getenv("TMP"));
	fulldirpath = strdup(ompi_os_path(false, tmp, sessions, NULL));
	if (OMPI_SUCCESS == ompi_check_dir(create, fulldirpath)) { /* check for existence and access, or create it */
	    return_code = OMPI_SUCCESS;
	    goto COMPLETE;
	}
    } else {
	tmp = strdup(OMPI_DEFAULT_TMPDIR);
	fulldirpath = strdup(ompi_os_path(false, tmp, sessions, NULL));
	if (OMPI_SUCCESS == ompi_check_dir(create, fulldirpath)) { /* check for existence and access, or create it */
	    return_code = OMPI_SUCCESS;
	    goto COMPLETE;
	}    }

    fulldirpath = strdup(ompi_os_path(false, tmp, sessions, NULL));
    if (OMPI_SUCCESS == ompi_check_dir(create, fulldirpath)) { /* check for existence and access, or create it */
	return_code = OMPI_SUCCESS;
	goto COMPLETE;
    } else {
	return_code = OMPI_ERROR;
	goto CLEANUP;
    }

 COMPLETE:
    if (create) {
	if (NULL != proc) {
            ompi_process_info.proc_session_dir = strdup(fulldirpath);
	    fulldirpath = dirname(fulldirpath);
        }

	if (NULL != job) {
            ompi_process_info.job_session_dir = strdup(fulldirpath);
	    fulldirpath = dirname(fulldirpath);
        }

        ompi_process_info.universe_session_dir = strdup(fulldirpath);
	ompi_process_info.tmpdir_base = strdup(tmp);
    }

 CLEANUP:
    if (tmp) {
        free(tmp);
    }
    if (fulldirpath) {
        free(fulldirpath);
    }
    if (frontend) {
	free(frontend);
    }
    if (batchname) {
	free(batchname);
    }
    if (hostname) {
	free(hostname);
    }
    if (sessions) {
	free(sessions);
    }

    return return_code;
}


int
ompi_session_dir_finalize()
{
    if (OMPI_SUCCESS != ompi_clean_dir(ompi_process_info.proc_session_dir)) {
	return OMPI_ERROR; /* should have been able to at least clear my own */
    }

    if (OMPI_SUCCESS != ompi_clean_dir(ompi_process_info.job_session_dir)) {
	return OMPI_SUCCESS; /* need new error condition - could be okay not to clear */
    }

    if (OMPI_SUCCESS != ompi_clean_dir(ompi_process_info.universe_session_dir)) {
	return OMPI_SUCCESS;
    }

    return OMPI_SUCCESS;
}

int
ompi_clean_dir(char *pathname)
{
    DIR *dp;
    struct dirent *ep;
    char *filenm;

    if (NULL != pathname) {  /* protect against error */
	dp = opendir(pathname);
	if (NULL != dp) {
	    while ((ep = readdir(dp))) {
		if ((0 != strcmp(ep->d_name, ".")) &&
		    (0 != strcmp(ep->d_name, "..")) &&
		    (DT_DIR != ep->d_type)) {
		    filenm = ompi_os_path(false, pathname, ep->d_name, NULL);
		    unlink(filenm);
		}
	    }
	    closedir(dp);
	}
	rmdir(pathname);
	return OMPI_SUCCESS;
    }
    return OMPI_ERROR;
}
