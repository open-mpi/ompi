/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"

#include <stdio.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_LIBGEN_H 
#include <libgen.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#include "orte/orte_constants.h"

#include "orte/util/univ_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/util/os_create_dirpath.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"

#include "orte/util/session_dir.h"

/*******************************
 * Local function Declarations
 *******************************/
static int orte_create_dir(char *directory);

static void orte_dir_empty(char *pathname);
static void orte_dir_empty_all(char *pathname);

static bool orte_is_empty(char *pathname);

#ifdef __WINDOWS__
#define OMPI_DEFAULT_TMPDIR "C:\\TEMP"
#else
#define OMPI_DEFAULT_TMPDIR "/tmp"
#endif

#define OMPI_PRINTF_FIX_STRING(a) ((NULL == a) ? "(null)" : a)

/****************************
 * Funcationality
 ****************************/
/*
 * Check and create the directory requested
 */
static int orte_create_dir(char *directory)
{
#ifndef __WINDOWS__
    mode_t my_mode = S_IRWXU;  /* at the least, I need to be able to do anything */
#else
    mode_t my_mode = _S_IREAD | _S_IWRITE | _S_IEXEC;
#endif
    int ret;

    /* Sanity check before creating the directory with the proper mode,
     * Make sure it doesn't exist already */
    if( ORTE_ERR_NOT_FOUND != (ret = orte_session_dir_check_dir(directory)) ) {
        /* Failure because orte_session_dir_check_dir() indicated that either:
         * - The directory exists and we can access it (no need to create it again), 
         *    return ORTE_SUCCESS, or
         * - don't have access rights, return ORTE_ERROR
         */
        return(ret);
    }
    /* The directory doesn't exist so create it */
    else {
	    return(opal_os_create_dirpath(directory, my_mode));
    }
}

/*
 * Check that the directory:
 *  - exists
 *  - if exists, then we have permission to interact with it
 */
int orte_session_dir_check_dir(char *directory)
{
#ifndef __WINDOWS__
    struct stat buf;
    mode_t my_mode = S_IRWXU;  /* at the least, I need to be able to do anything */
#else
    struct __stat64 buf;
    mode_t my_mode = _S_IREAD | _S_IWRITE | _S_IEXEC;
#endif

#ifndef __WINDOWS__
    if (0 == stat(directory, &buf)) { /* exists - check access */
#else
    if (0 == _stat64(directory, &buf)) { /* exist -- check */
#endif
        if ((buf.st_mode & my_mode) == my_mode) { /* okay, I can work here */
            return(ORTE_SUCCESS);
        }
        else {
            /* Don't have access rights to the existing directory */
            return(ORTE_ERROR);
        }
    }
    else {
        /* We could not find the directory */
        return( ORTE_ERR_NOT_FOUND );
    }
}

/*
 * Construct the fullpath to the session directory
 */
int
orte_session_dir_get_name(char **fulldirpath,
                          char **prefix,  /* This will come back as the valid tmp dir */
                          char **frontend,
                          char *usr, char *hostid,
                          char *batchid, char *univ, 
                          char *job, char *proc) {
    char *hostname  = NULL, 
        *batchname = NULL,
        *sessions  = NULL, 
        *user      = NULL, 
        *universe  = NULL;
    int exit_status = ORTE_SUCCESS;
    
    /* Ensure that system info is set */
    orte_sys_info();

    /*
     * set the 'user' value
     */
    if( NULL != usr) { /* User specified version */
        user = strdup(usr);
    }
    else {             /* check if it is set elsewhere */
        if( NULL != orte_system_info.user)
            user = strdup(orte_system_info.user);
        else {
            /* Couldn't find it, so fail */
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }

    /*
     * set the 'hostname'
     */
    if( NULL != hostid) { /* User specified version */
        hostname = strdup(hostid);
    }
    else {            /* check if it is set elsewhere */
        if( NULL != orte_system_info.nodename)
            hostname = strdup(orte_system_info.nodename);
        else {
            /* Couldn't find it, so fail */
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }
    
    /*
     * set the 'batchid'
     */
    if (NULL != batchid)
        batchname = strdup(batchid);
    else 
        batchname = strdup("0");

    /*
     * set the 'universe'
     */
    if( NULL != univ) { /* User specified version */
         universe = strdup(univ);
     }
     else {            /* check if it is set elsewhere */
         if( NULL != orte_universe_info.name)
             universe = strdup(orte_universe_info.name);
         else {
             /* Couldn't find it, so fail */
             exit_status = ORTE_ERROR;
             goto cleanup;
         }
     }

    /*
     * Check: Can't give a proc without a job
     */
    if( NULL == job &&
        NULL != proc) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /*
     * get the front part of the session directory
     * Will look something like:
     *    openmpi-sessions-USERNAME@HOSTNAME_BATCHID
     */
    if (NULL != orte_process_info.top_session_dir) {
        *frontend = strdup(orte_process_info.top_session_dir);
    }
    else { /* If not set then construct it */
        if (0 > asprintf(frontend, "openmpi-sessions-%s@%s_%s", user, hostname, batchname)) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }

    /*
     * Construct the session directory
     */
    /* If we were given a 'proc' then we can construct it fully into:
     *   openmpi-sessions-USERNAME@HOSTNAME_BATCHID/UNIVERSE/JOBID/PROC
     */
    if( NULL != proc) {
        if (0 > asprintf(&sessions, "%s%s%s%s%s%s%s", 
                         *frontend,
                         orte_system_info.path_sep, universe,
                         orte_system_info.path_sep, job,
                         orte_system_info.path_sep, proc)) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }
    /* If we were given a 'job' then we can construct it partially into:
     *   openmpi-sessions-USERNAME@HOSTNAME_BATCHID/UNIVERSE/JOBID
     */
    else if(NULL != job) {
        if (0 > asprintf(&sessions, "%s%s%s%s%s",
                         *frontend,
                         orte_system_info.path_sep, universe,
                         orte_system_info.path_sep, job)) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }
    /* If we were given neither then we can construct it partially into:
     *   openmpi-sessions-USERNAME@HOSTNAME_BATCHID/UNIVERSE
     */
    else {
        if (0 > asprintf(&sessions, "%s%s%s",
                         *frontend,
                         orte_system_info.path_sep, universe )) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }
    
    /*
     * If the user specified an invalid prefix, or no prefix at all
     * we need to keep looking
     */
    if( NULL != *fulldirpath) {
        free(*fulldirpath);
        *fulldirpath = NULL;
    }

    if( NULL != *prefix) { /* use the user specified one, if available */
        ;
    }
    /* Try to find a proper alternative prefix */
    else if (NULL != orte_process_info.tmpdir_base) { /* stored value */
        *prefix = strdup(orte_process_info.tmpdir_base);
    }
    else if( NULL != getenv("OMPI_PREFIX_ENV") ) { /* OMPI Environment var */
        *prefix = strdup(getenv("OMPI_PREFIX_ENV"));
    }
    else if( NULL != getenv("TMPDIR") ) { /* General Environment var */
        *prefix = strdup(getenv("TMPDIR"));
    }
    else if( NULL != getenv("TMP") ) { /* Another general environment var */
        *prefix = strdup(getenv("TMP"));
    }
    else { /* ow. just use the default tmp directory */
        *prefix = strdup(OMPI_DEFAULT_TMPDIR);
    }
    
    /*
     * Construct the absolute final path
     */
    *fulldirpath = strdup(opal_os_path(false, *prefix, sessions, NULL));

    
 cleanup:
    if(NULL != hostname)
        free(hostname);
    if(NULL != batchname)
        free(batchname);
    if(NULL != sessions)
        free(sessions);
    if(NULL != user)
        free(user);
    if(NULL != universe)
        free(universe);
    
    return exit_status;
}

/*
 * Construct the session directory and create it if necessary
 */
int orte_session_dir(bool create, 
                     char *prefix, char *usr, char *hostid,
                     char *batchid, char *univ, char *job, char *proc)
{
    char *fulldirpath = NULL,
        *frontend     = NULL,
        *sav          = NULL;
    int return_code = ORTE_SUCCESS, rtn;
    /* This indicates if the prefix was set, and so if it fails then we
     * should try with the default prefixes.*/
    bool dbl_check_prefix = false;
    
    if( NULL != prefix)
        dbl_check_prefix = true;

 try_again:
    /*
     * If the first attempt at the path creation failed, try with a null
     * prefix. unless the original prefix was null, then we fail.
     */
    if(!dbl_check_prefix && /* an indicator that we are trying a second time */
       NULL != prefix) {
        free(prefix);
        prefix = NULL;
    }
    
    /*
     * Get the session directory full name
     * First try it with the specified prefix.
     */
    if( ORTE_SUCCESS != ( rtn = orte_session_dir_get_name(&fulldirpath, 
                                                          &prefix,
                                                          &frontend,
                                                          usr, hostid, 
                                                          batchid, univ, job,
                                                          proc) ) ) {
        return_code = rtn;
        /*
         * If the first attempt at the path creation failed, try with a null
         * prefix. unless the original prefix was null, then we fail :(
         */
        if(dbl_check_prefix) {
            dbl_check_prefix = false;
            goto try_again;
        }
        else {
            goto cleanup;
        }
    }

    /*
     * Now that we have the full path, go ahead and create it if necessary
     */
    if( create ) {
        if( ORTE_SUCCESS != (rtn = orte_create_dir(fulldirpath) ) ) {
            return_code = rtn;
            
            if(dbl_check_prefix) {
                dbl_check_prefix = false;
                goto try_again;
            }
            else {
                goto cleanup;
            }
        }
    }
    /*
     * if we are not creating, then just verify that the path is OK
     */
    else {
        if( ORTE_SUCCESS != (rtn = orte_session_dir_check_dir(fulldirpath) )) {
            /* It is not valid so we give up and return an error */
            return_code = rtn;
            
            if(dbl_check_prefix) {
                dbl_check_prefix = false;
                goto try_again;
            }
            else {
                goto cleanup;
            }
        }
    }

    return_code = ORTE_SUCCESS;

    /*
     * If we are creating the directory tree, the overwrite the
     * global structure fields
     */
    if (create) {
    	if (NULL != orte_process_info.tmpdir_base) {
    	    free(orte_process_info.tmpdir_base);
    	    orte_process_info.tmpdir_base = NULL;
    	}
    	if (NULL != orte_process_info.top_session_dir) {
    	    free(orte_process_info.top_session_dir);
    	    orte_process_info.top_session_dir = NULL;
    	}
    }

    /* 
     * Update some of the global structures if they are empty
     */
    if (NULL == orte_process_info.tmpdir_base)
        orte_process_info.tmpdir_base = strdup(prefix);

    if (NULL == orte_process_info.top_session_dir)
        orte_process_info.top_session_dir = strdup(frontend);
    

    /*
     * Set the process session directory
     */
    if (NULL != proc) {
    	if (create) { /* overwrite if creating */
    	    if (NULL != orte_process_info.proc_session_dir) {
                free(orte_process_info.proc_session_dir);
                orte_process_info.proc_session_dir = NULL;
    	    }
    	}
    	if (NULL == orte_process_info.proc_session_dir) {
    	    orte_process_info.proc_session_dir = strdup(fulldirpath);
    	}

        /* Strip off last part of directory structure */
        sav = strdup(fulldirpath);
        free(fulldirpath);
        fulldirpath = strdup(dirname(sav));
        free(sav);
        sav = NULL;
    }

    /*
     * Set the job session directory
     */
    if (NULL != job) {
    	if (create) { /* overwrite if creating */
    	    if (NULL != orte_process_info.job_session_dir) {
                free(orte_process_info.job_session_dir);
                orte_process_info.job_session_dir = NULL;
    	    }
    	}
    	if (NULL == orte_process_info.job_session_dir) {
    	    orte_process_info.job_session_dir = strdup(fulldirpath);
    	}

        /* Strip off last part of directory structure */
        sav = strdup(fulldirpath);
        free(fulldirpath);
        fulldirpath = strdup(dirname(sav));
        free(sav); 
        sav = NULL;
    }

    /*
     * Set the universe session directory
     */
    if (create) { /* overwrite if creating */
    	if (NULL != orte_process_info.universe_session_dir) {
    	    free(orte_process_info.universe_session_dir);
    	    orte_process_info.universe_session_dir = NULL;
    	}
    }
    if (NULL == orte_process_info.universe_session_dir) {
        orte_process_info.universe_session_dir = strdup(fulldirpath);
    }

    if (orte_debug_flag) {
    	opal_output(0, "procdir: %s", 
                    OMPI_PRINTF_FIX_STRING(orte_process_info.proc_session_dir));
    	opal_output(0, "jobdir: %s", 
                    OMPI_PRINTF_FIX_STRING(orte_process_info.job_session_dir));
    	opal_output(0, "unidir: %s", 
                    OMPI_PRINTF_FIX_STRING(orte_process_info.universe_session_dir));
    	opal_output(0, "top: %s", 
                    OMPI_PRINTF_FIX_STRING(orte_process_info.top_session_dir));
    	opal_output(0, "tmp: %s", 
                    OMPI_PRINTF_FIX_STRING(orte_process_info.tmpdir_base));
    }

 cleanup:
    if(NULL != fulldirpath)
        free(fulldirpath);
    if(NULL != frontend)
        free(frontend);
    if(NULL != sav)
        free(sav);

    return return_code;
}

/*
 * A job has aborted - so force cleanup of the session directory
 */
int
orte_session_dir_cleanup(orte_jobid_t jobid)
{
    int rc;
    char *tmp;
    char *job, *job_session_dir;

    /* need to setup the top_session_dir with the prefix */
    tmp = opal_os_path(false,
                       orte_process_info.tmpdir_base,
                       orte_process_info.top_session_dir, NULL);
    
    /* define the proc and job session directories for this process */
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&job, jobid))) {
        ORTE_ERROR_LOG(rc);
        free(tmp);
        return rc;
    }
    if (0 > asprintf(&job_session_dir, "%s%s%s",
                        orte_process_info.universe_session_dir,
                        orte_system_info.path_sep, job)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(tmp);
        free(job);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    orte_dir_empty_all(job_session_dir);
    orte_dir_empty(orte_process_info.universe_session_dir);
    orte_dir_empty(tmp);

    if (orte_is_empty(job_session_dir)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found job session dir empty - deleting");
    	}
    	rmdir(job_session_dir);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: job session dir not empty - leaving");
    	}
        goto CLEANUP;
    }

    if (orte_is_empty(orte_process_info.universe_session_dir)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found univ session dir empty - deleting");
    	}
    	rmdir(orte_process_info.universe_session_dir);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: univ session dir not empty - leaving");
    	}
    	goto CLEANUP;
    }

    if (orte_is_empty(tmp)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found top session dir empty - deleting");
    	}
    	rmdir(tmp);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: top session dir not empty - leaving");
    	}
    }

CLEANUP:
    free(tmp);
    free(job);
    free(job_session_dir);
    return ORTE_SUCCESS;
}


int
orte_session_dir_finalize(orte_process_name_t *proc)
{
    int rc;
    char *tmp;
    char *job, *job_session_dir, *vpid, *proc_session_dir;

    /* need to setup the top_session_dir with the prefix */
    tmp = opal_os_path(false,
                       orte_process_info.tmpdir_base,
                       orte_process_info.top_session_dir, NULL);
    
    /* define the proc and job session directories for this process */
    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid_string(&job, proc))) {
        ORTE_ERROR_LOG(rc);
        free(tmp);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_string(&vpid, proc))) {
        ORTE_ERROR_LOG(rc);
        free(tmp);
        free(job);
        return rc;
    }
    
    if (0 > asprintf(&job_session_dir, "%s%s%s",
                        orte_process_info.universe_session_dir,
                        orte_system_info.path_sep, job)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(tmp);
        free(job);
        free(vpid);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (0 > asprintf(&proc_session_dir, "%s%s%s",
                        job_session_dir,
                        orte_system_info.path_sep, vpid)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(tmp);
        free(job);
        free(vpid);
        free(job_session_dir);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    orte_dir_empty(proc_session_dir);
    orte_dir_empty(job_session_dir);
    orte_dir_empty(orte_process_info.universe_session_dir);
    orte_dir_empty(tmp);

    if (orte_is_empty(proc_session_dir)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found proc session dir empty - deleting");
    	}
    	rmdir(proc_session_dir);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: proc session dir not empty - leaving");
    	}
        goto CLEANUP;
    }

    if (orte_is_empty(job_session_dir)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found job session dir empty - deleting");
    	}
    	rmdir(job_session_dir);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: job session dir not empty - leaving");
    	}
        goto CLEANUP;
    }

    if (orte_is_empty(orte_process_info.universe_session_dir)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found univ session dir empty - deleting");
    	}
    	rmdir(orte_process_info.universe_session_dir);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: univ session dir not empty - leaving");
    	}
    	goto CLEANUP;
    }

    if (orte_is_empty(tmp)) {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: found top session dir empty - deleting");
    	}
    	rmdir(tmp);
    } else {
    	if (orte_debug_flag) {
    	    opal_output(0, "sess_dir_finalize: top session dir not empty - leaving");
    	}
    }

CLEANUP:
    free(tmp);
    free(job);
    free(vpid);
    free(job_session_dir);
    free(proc_session_dir);
    return ORTE_SUCCESS;
}

static void
orte_dir_empty(char *pathname)
{
#ifndef __WINDOWS__
    DIR *dp;
    struct dirent *ep;
    char *filenm;
#ifndef HAVE_STRUCT_DIRENT_D_TYPE 
    int ret;
    struct stat buf;
#endif

    if (NULL == pathname) {  /* protect against error */
        return;
    }

    dp = opendir(pathname);
    if (NULL == dp) {
        return;
    }

    while (NULL != (ep = readdir(dp)) ) {
        /* skip:
         *  - . and ..
         *  - directories
         *  - files starting with "output-"
         *  - universe contact (universe-setup.txt)
         */
        if ((0 != strcmp(ep->d_name, ".")) &&
            (0 != strcmp(ep->d_name, "..")) &&
            (0 != strncmp(ep->d_name, "output-", strlen("output-"))) &&
            (0 != strcmp(ep->d_name, "universe-setup.txt"))) {

            filenm = opal_os_path(false, pathname, ep->d_name, NULL);

            /* make sure it's not a directory */
#ifdef HAVE_STRUCT_DIRENT_D_TYPE
            if (DT_DIR == ep->d_type) {
                free(filenm);
                continue;
            }
#else /* have dirent.d_type */
            ret = stat(filenm, &buf);
            if (ret < 0 || S_ISDIR(buf.st_mode)) {
                free(filenm);
                continue;
            }
#endif /* have dirent.d_type */
            unlink(filenm);
            free(filenm);
        }
    }
    closedir(dp);
#else
    bool empty = false;
    char search_path[MAX_PATH];
    HANDLE file;
    WIN32_FIND_DATA file_data;
    TCHAR *file_name;
                        
    if (NULL != pathname) {
        strncpy(search_path, pathname, strlen(pathname)+1);
        strncat (search_path, "\\*", 3);
        file = FindFirstFile(search_path, &file_data);

        if (INVALID_HANDLE_VALUE == file) {
            FindClose(&file_data);
            return;
        } 
        
        do {
            if ((0 != strcmp(file_data.cFileName, ".")) &&
                (0 != strcmp(file_data.cFileName, "..")) &&
                (!(file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) &&
                (0 != strncmp(file_data.cFileName,"output-", strlen("output-"))) &&
                (0 != strcmp(file_data.cFileName,"universe-setup.txt-"))) {
                
		            file_name = opal_os_path(false, pathname, file_data.cFileName, NULL);
                    DeleteFile(file_name);

            }
            if (0 == FindNextFile(file, &file_data)) {
                    empty = true;
            }
        } while(!empty);
        FindClose(&file_data);
    }
#endif
}


static void
orte_dir_empty_all(char *pathname)
{
#ifndef WIN32
    DIR *dp;
    struct dirent *ep;
    char *filenm;
#ifndef HAVE_STRUCT_DIRENT_D_TYPE 
    int ret;
    struct stat buf;
#endif
    int rc;

    if (NULL == pathname) {  /* protect against error */
        return;
    }

    dp = opendir(pathname);
    if (NULL == dp) {
        return;
    }

    while (NULL != (ep = readdir(dp)) ) {
        /* skip:
         *  - . and ..
         *  - directories
         *  - files starting with "output-"
         *  - universe contact (universe-setup.txt)
         */
        if ((0 != strcmp(ep->d_name, ".")) &&
            (0 != strcmp(ep->d_name, "..")) &&
            (0 != strncmp(ep->d_name, "output-", strlen("output-"))) &&
            (0 != strcmp(ep->d_name, "universe-setup.txt"))) {

            filenm = opal_os_path(false, pathname, ep->d_name, NULL);

            /* is it a directory */
#ifdef HAVE_STRUCT_DIRENT_D_TYPE
            if (DT_DIR == ep->d_type) {
                orte_dir_empty_all(filenm);
                rmdir(filenm);
                free(filenm);
                continue;
            }
#else /* have dirent.d_type */
            ret = stat(filenm, &buf);
            if (ret < 0 || S_ISDIR(buf.st_mode)) {
                orte_dir_empty_all(filenm);
                rmdir(filenm);
                free(filenm);
                continue;
            }
#endif /* have dirent.d_type */
            rc = unlink(filenm);
            free(filenm);
        }
    }
    closedir(dp);
#else
    bool empty = false;
    char search_path[MAX_PATH];
    HANDLE file;
    WIN32_FIND_DATA file_data;
    TCHAR *file_name;
                        
    if (NULL != pathname) {
        strncpy(search_path, pathname, strlen(pathname)+1);
        strncat (search_path, "\\*", 3);
        file = FindFirstFile(search_path, &file_data);

        if (INVALID_HANDLE_VALUE == file) {
            FindClose(&file_data);
            return;
        } 
        
        do {
            if(file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
                orte_dir_empty_all(file_name);
            } 
            if ((0 != strcmp(file_data.cFileName, ".")) &&
                (0 != strcmp(file_data.cFileName, "..")) &&
                (0 != strncmp(file_data.cFileName,"output-", strlen("output-"))) &&
                (0 != strcmp(file_data.cFileName,"universe-setup.txt-"))) {
                
		            file_name = opal_os_path(false, pathname, file_data.cFileName, NULL);
                    DeleteFile(file_name);

            }
            if (0 == FindNextFile(file, &file_data)) {
                    empty = true;
            }
        } while(!empty);
        FindClose(&file_data);
    }
#endif
}


/* tests if the directory is empty */
static bool orte_is_empty(char *pathname)
{
#ifndef __WINDOWS__
    DIR *dp;
    struct dirent *ep;
    if (NULL != pathname) {  /* protect against error */
    	dp = opendir(pathname);
    	if (NULL != dp) {
    	    while ((ep = readdir(dp))) {
        		if ((0 != strcmp(ep->d_name, ".")) &&
        		    (0 != strcmp(ep->d_name, ".."))) {
                            closedir(dp);
        		    return false;
        		}
    	    }
    	    closedir(dp);
    	    return true;
    	}
    	return false;
    }
    return true;
#else 
    char search_path[MAX_PATH];
    HANDLE file;
    WIN32_FIND_DATA file_data;

    if (NULL != pathname) {
        strncpy(search_path, pathname, strlen(pathname)+1);
        strncat (search_path, "\\*", 3);

        file = FindFirstFile(search_path, &file_data);
        if (INVALID_HANDLE_VALUE == file) {
            FindClose(&file_data);
            return true;
        }

        if (0 != strcmp(file_data.cFileName, ".") || 0 != strcmp(file_data.cFileName, "..")) {
            FindClose(&file_data);
            return false;
        }

        while (0 != FindNextFile(file, &file_data)) {
            if (0 != strcmp(file_data.cFileName, ".") || 0 != strcmp(file_data.cFileName, "..")) {
                FindClose(&file_data);
                return false;
            }
        }
    }

    FindClose(&file_data);
    return true;
#endif /* ifndef __WINDOWS__ */
}
