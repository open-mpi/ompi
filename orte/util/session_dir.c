/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
#include "opal/util/os_dirpath.h"
#include "opal/util/basename.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"

#include "orte/util/session_dir.h"

/*******************************
 * Local function Declarations
 *******************************/
static int orte_create_dir(char *directory);

static bool orte_dir_check_file(const char *root, const char *path);

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
    if( OPAL_ERR_NOT_FOUND != (ret = opal_os_dirpath_access(directory, my_mode)) ) {
        /* Failure because opal_os_dirpath_access() indicated that either:
         * - The directory exists and we can access it (no need to create it again), 
         *    return OPAL_SUCCESS, or
         * - don't have access rights, return OPAL_ERROR
         */
        return(ret);
    }
    /* The directory doesn't exist so create it */
    else {
	    return(opal_os_dirpath_create(directory, my_mode));
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
         if( NULL != orte_universe_info.name) {
             universe = strdup(orte_universe_info.name);
         }
         else {
             ;/* Couldn't find it, so continue with caution */
         }
     }

    /*
     * Check: Can't give proc or job without universe
     */
    if( NULL == universe &&
        (NULL != proc ||
         NULL != job) ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * Check: Can't give a proc without a job and universe
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
        sessions = opal_os_path( false, *frontend, universe, job, proc, NULL );
        if( NULL == sessions ) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }
    /* If we were given a 'job' then we can construct it partially into:
     *   openmpi-sessions-USERNAME@HOSTNAME_BATCHID/UNIVERSE/JOBID
     */
    else if(NULL != job) {
        sessions = opal_os_path( false, *frontend, universe, job, NULL );
        if( NULL == sessions ) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }
    /* If we were given neither then we can construct it partially into:
     *   openmpi-sessions-USERNAME@HOSTNAME_BATCHID/UNIVERSE
     */
    else if(NULL != universe) {
        sessions = opal_os_path( false, *frontend, universe, NULL );
        if( NULL == sessions ) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }
    /* If we were not given 'universe' then we can construct it into:
     *   openmpi-sessions-USERNAME@HOSTNAME_BATCHID
     */
    else {
        if (0 > asprintf(&sessions, "%s",
                         *frontend) ) {
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
    *fulldirpath = opal_os_path(false, *prefix, sessions, NULL);

    
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
        if( ORTE_SUCCESS != (rtn = opal_os_dirpath_access(fulldirpath, 0) )) {
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
        sav = opal_dirname(fulldirpath);
        free(fulldirpath);
        fulldirpath = sav;
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
        sav = opal_dirname(fulldirpath);
        free(fulldirpath);
        fulldirpath = sav;
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
    job_session_dir = opal_os_path( false, orte_process_info.universe_session_dir,
                                    job, NULL );
    if( NULL == job_session_dir ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(tmp);
        free(job);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    opal_os_dirpath_destroy(job_session_dir,
                            true, orte_dir_check_file);
    opal_os_dirpath_destroy(orte_process_info.universe_session_dir,
                            false, orte_dir_check_file);
    opal_os_dirpath_destroy(tmp,
                            false, orte_dir_check_file);

    if (opal_os_dirpath_is_empty(job_session_dir)) {
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

    if (opal_os_dirpath_is_empty(orte_process_info.universe_session_dir)) {
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

    if (opal_os_dirpath_is_empty(tmp)) {
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
    job_session_dir = opal_os_path( false, orte_process_info.universe_session_dir,
                                    job, NULL );
    if( NULL == job_session_dir ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(tmp);
        free(job);
        free(vpid);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    proc_session_dir = opal_os_path( false, job_session_dir, vpid, NULL );
    if( NULL == proc_session_dir ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(tmp);
        free(job);
        free(vpid);
        free(job_session_dir);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    opal_os_dirpath_destroy(proc_session_dir,
                            false, orte_dir_check_file);
    opal_os_dirpath_destroy(job_session_dir,
                            false, orte_dir_check_file);
    opal_os_dirpath_destroy(orte_process_info.universe_session_dir,
                            false, orte_dir_check_file);
    opal_os_dirpath_destroy(tmp,
                            false, orte_dir_check_file);

    if (opal_os_dirpath_is_empty(proc_session_dir)) {
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

    if (opal_os_dirpath_is_empty(job_session_dir)) {
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

    if (opal_os_dirpath_is_empty(orte_process_info.universe_session_dir)) {
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

    if (opal_os_dirpath_is_empty(tmp)) {
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

static bool 
orte_dir_check_file(const char *root, const char *path) {

    /*
     * Keep:
     *  - files starting with "output-"
     *  - universe contact (universe-setup.txt)
     */
    if( (0 == strncmp(path, "output-", strlen("output-"))) ||
        (0 == strcmp(path,  "universe-setup.txt"))) {
        return false;
    }

    return true;
}
