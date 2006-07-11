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
 */

/**
 * @file
 *
 * Setup command line options for the Open MPI Run Time Environment
 */


#include "orte_config.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <sys/types.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif  /* HAVE_LIBGEN_H */

#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "orte/util/univ_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "opal/util/os_path.h"
#include "opal/util/os_dirpath.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#ifdef _WINDOWS_
#include "opal/win32/ompi_util.h"
#endif  /* _WINDOWS_ */

static struct timeval ompi_rte_ping_wait = {2, 0};

int orte_universe_search(opal_list_t *universe_list) {
    int ret, exit_status = ORTE_SUCCESS;
#ifndef __WINDOWS__
    DIR *cur_dirp = NULL;
    struct dirent * dir_entry;
#else
    HANDLE hFind = INVALID_HANDLE_VALUE;
    WIN32_FIND_DATA file_data;
#endif  /* __WINDOWS__ */
    char *univ_setup_filename = NULL;
    char *fulldirpath = NULL;
    char *prefix = NULL;
    char *frontend = NULL;
    char *frontend_abs = NULL;

    /*
     * Get the session directory
     */
    if( ORTE_SUCCESS != (ret = orte_session_dir_get_name(&fulldirpath,
                                                         &prefix,
                                                         &frontend,
                                                         orte_system_info.user,
                                                         orte_system_info.nodename,
                                                         NULL, /* batch ID -- Not used */
                                                         NULL, /* Universe Name -- NONE */
                                                         NULL, /* jobid */
                                                         NULL  /* vpid */
                                                         ) ) ) {
        exit_status = ret;
        goto cleanup;
    }
    
    asprintf(&frontend_abs, "%s/%s", prefix, frontend);

    /*
     * Check to make sure we have access to this directory
     */
    if( ORTE_SUCCESS != (ret = opal_os_dirpath_access(frontend_abs, 0) )) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Open up the base directory so we can get a listing
     */
#ifndef __WINDOWS__
    if( NULL == (cur_dirp = opendir(frontend_abs)) ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
#else
    hFind = FindFirstFile( frontend_abs, &file_data );
#endif  /* __WINDOWS__ */

    /*
     * For each directory/universe
     */
#ifndef __WINDOWS__
    while( NULL != (dir_entry = readdir(cur_dirp)) ) {
        orte_universe_t *univ = NULL;
        char * tmp_str = NULL;

        /*
         * Skip non-universe directories
         */
        if( 0 == strncmp(dir_entry->d_name, ".", strlen(".")) ||
            0 == strncmp(dir_entry->d_name, ".", strlen("..")) ) {
            continue;
        }

        /*
         * Read the setup file
         */
        tmp_str = strdup(dir_entry->d_name);
        asprintf(&univ_setup_filename, "%s/%s/%s", 
                 frontend_abs,
                 tmp_str,
                 "universe-setup.txt");
        
        univ = OBJ_NEW(orte_universe_t);
        OBJ_RETAIN(univ);
        if(ORTE_SUCCESS != (ret = orte_read_universe_setup_file(univ_setup_filename, univ) ) ){
            printf("orte_ps: Unable to read the file (%s)\n", univ_setup_filename);
            exit_status = ret;
            goto cleanup;
        }

        opal_list_append(universe_list, &(univ->super));

        if( NULL != tmp_str)
            free(tmp_str);
    }
#else
    do {
        orte_universe_t *univ = NULL;
        char * tmp_str = NULL;

        /*
         * Skip non-universe directories
         */
        if( 0 == strncmp(file_data.cFileName, ".", strlen(".")) ||
            0 == strncmp(file_data.cFileName, ".", strlen("..")) ) {
            continue;
        }

        /*
         * Read the setup file
         */
        tmp_str = strdup(file_data.cFileName);
        asprintf(&univ_setup_filename, "%s/%s/%s", 
                 frontend_abs,
                 tmp_str,
                 "universe-setup.txt");
        
        univ = OBJ_NEW(orte_universe_t);
        OBJ_RETAIN(univ);
        if(ORTE_SUCCESS != (ret = orte_read_universe_setup_file(univ_setup_filename, univ) ) ){
            printf("orte_ps: Unable to read the file (%s)\n", univ_setup_filename);
            exit_status = ret;
            goto cleanup;
        }

        opal_list_append(universe_list, &(univ->super));

        if( NULL != tmp_str)
            free(tmp_str);
    } while( 0 != FindNextFile( hFind, &file_data ) );
#endif  /* __WINDOWS__ */
    
 cleanup:
#ifndef __WINDOWS__
    if( NULL != cur_dirp )
        closedir(cur_dirp);
#else
    FindClose(hFind);
#endif  /* __WINDOWS__ */
    if( NULL != univ_setup_filename)
        free(univ_setup_filename);
    if( NULL != fulldirpath)
        free(fulldirpath);
    if( NULL != prefix)
        free(prefix);
    if( NULL != frontend)
        free(frontend);
    if( NULL != frontend_abs)
        free(frontend_abs);

    return exit_status;
}

int orte_universe_exists(orte_universe_t *univ)
{
    char *contact_file;
    int ret;

    /* check to see if local universe session directory already exists */
    if (ORTE_SUCCESS != orte_session_dir(false,
					 orte_process_info.tmpdir_base,
					 orte_system_info.user,
					 orte_system_info.nodename,
					 NULL,
					 orte_universe_info.name,
					 NULL,
					 NULL)) { /* not found */
        /* NOTE: NOT FINDING THE DIRECTORY IS NOT AN ERROR - DON'T ERROR_LOG IT */
        return ORTE_ERR_NOT_FOUND;
    }

	/* check for "contact-info" file. if present, read it in. */
	if (NULL == (contact_file = opal_os_path(false, orte_process_info.universe_session_dir,
				    "universe-setup.txt", NULL))) {
        /* NOTE: NOT FINDING THE FILE IS NOT AN ERROR - DON'T ERROR_LOG IT */
        return ORTE_ERR_NOT_FOUND;
    }

	if (ORTE_SUCCESS != (ret = orte_read_universe_setup_file(contact_file, univ))) {
        /* NOTE: THIS IS NOT AN ERROR - DON'T ERROR_LOG IT */
        free(contact_file);
	    return ret;
	}

        /* don't need this string any more - free it */
        free(contact_file);

	if (orte_debug_flag) {
	    opal_output(0, "connect_uni: contact info read");
	}

	if (!orte_universe_info.console) {  /* if we aren't trying to connect a console */
	    if (!univ->persistence ||   /* if the target universe is not persistent... */
		(0 == strncmp(univ->scope, "exclusive", strlen("exclusive")))) {  /* ...or no connection allowed */
		/* also need to check "local" and that we did not specify the exact
		 * matching universe name
		 */
		if (orte_debug_flag) {
		    opal_output(0, "connect_uni: connection not allowed");
		}
        /* NOTE: THIS IS NOT AN ERROR - DON'T ERROR_LOG IT */
		return ORTE_ERR_NO_CONNECTION_ALLOWED;
	    }
	}

	if (orte_debug_flag) {
	    opal_output(0, "connect_uni: contact info to set: %s", univ->seed_uri);
	}


	/* if persistent, ping to verify it's alive */
	if (ORTE_SUCCESS != orte_rml.ping(univ->seed_uri, &ompi_rte_ping_wait)) {
        if (orte_debug_flag) {
            ORTE_ERROR_LOG(ORTE_ERR_CONNECTION_FAILED);
        }
	    return ORTE_ERR_CONNECTION_FAILED;
	}

	return ORTE_SUCCESS;
}
