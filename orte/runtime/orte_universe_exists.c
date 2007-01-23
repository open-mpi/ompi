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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
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

static struct timeval ompi_rte_ping_wait = {2, 0};

int orte_universe_search(opal_list_t *universe_list, bool report_broken_files, bool remove_broken_files)
{
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
    
#if !defined(__WINDOWS__)
    frontend_abs = opal_os_path(false, prefix, frontend, NULL);

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
    if( NULL == (cur_dirp = opendir(frontend_abs)) ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    /*
     * For each directory/universe
     */
    while( NULL != (dir_entry = readdir(cur_dirp)) ) {
        orte_universe_t *univ = NULL;

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
        univ_setup_filename = opal_os_path( false, frontend_abs,
                                            dir_entry->d_name, "universe-setup.txt", NULL );
        
        univ = OBJ_NEW(orte_universe_t);
        if(ORTE_SUCCESS != (ret = orte_read_universe_setup_file(univ_setup_filename, univ) ) ){
            if (report_broken_files) {
                printf("universe_search: Unable to read the file (%s)\n", univ_setup_filename);
                exit_status = ret;
            }

            /*
             * See if we want to remove any cases with broken
             * universe-setup.txt files.  If so, print out a message and
             * remove the directory.  This is used by the orte-clean
             * routine.  
             */
            if (remove_broken_files) {
                char *univ_directory;
                univ_directory = opal_os_path(false, frontend_abs,
                                              dir_entry->d_name, NULL);
                printf("universe_search: Removing defunct directory (%s)\n", univ_directory);
                opal_os_dirpath_destroy(univ_directory, true, NULL);
                free(univ_directory);
            }
            OBJ_RELEASE(univ);
        } else {
            OBJ_RETAIN(univ);
            opal_list_append(universe_list, &(univ->super));
        }
    }
#else
    /*
     * Open up the base directory so we can get a listing.
     *
     * On Windows if we want to parse the content of a directory the filename
     * should end with the "*". Otherwise we will only open the directory
     * structure (and not the content).
     */
    frontend_abs = opal_os_path(false, prefix, frontend, "*", NULL);
    hFind = FindFirstFile( frontend_abs, &file_data );
    if( INVALID_HANDLE_VALUE == hFind ) {
        exit_status = GetLastError();
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /*
     * For each directory/universe
     */
    do {
        orte_universe_t *univ = NULL;

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
        univ_setup_filename = opal_os_path( false, prefix, frontend,
                                            file_data.cFileName, "universe-setup.txt", NULL);
    
        univ = OBJ_NEW(orte_universe_t);
        if(ORTE_SUCCESS != (ret = orte_read_universe_setup_file(univ_setup_filename, univ) ) ){
            if (report_broken_files) {
                printf("universe_search: Unable to read the file (%s)\n", univ_setup_filename);
                exit_status = ret;
            }

            /*
             * See if we want to remove any cases with broken
             * universe-setup.txt files.  If so, print out a message and
             * remove the directory.  This is used by the orte-clean
             * routine.  
             */
            if (remove_broken_files) {
                char *univ_directory;
                univ_directory = opal_os_path(false, frontend_abs,
                                              file_data.cFileName, NULL);
                printf("universe_search: Removing defunct directory (%s)\n", univ_directory);
                opal_os_dirpath_destroy(univ_directory, true, NULL);
                free(univ_directory);
            }
            OBJ_RELEASE(univ);
        } else {
            OBJ_RETAIN(univ);
            opal_list_append(universe_list, &(univ->super));
        }
    } while( 0 != FindNextFile( hFind, &file_data ) );
#endif  /* !defined(__WINDOWS__) */
    
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

    return (opal_list_is_empty(universe_list) ? exit_status : ORTE_SUCCESS);
}

static int orte_universe_check_connect(orte_universe_t *uni)
{
	if (!orte_universe_info.console) {  /* if we aren't trying to connect a console */
	    if (!uni->persistence ||   /* if the target universe is not persistent... */
            (0 == strncmp(uni->scope, "exclusive", strlen("exclusive")))) {  /* ...or no connection allowed */
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
        opal_output(0, "connect_uni: contact info to set: %s", uni->seed_uri);
    }


    /* ping to verify it's alive */
    if (ORTE_SUCCESS != orte_rml.ping(uni->seed_uri, &ompi_rte_ping_wait)) {
        if (orte_debug_flag) {
            ORTE_ERROR_LOG(ORTE_ERR_CONNECTION_FAILED);
        }
        return ORTE_ERR_CONNECTION_FAILED;
    }

    return ORTE_SUCCESS;
}


int orte_universe_exists(orte_universe_t *univ)
{
    char *contact_file;
    opal_list_t universes;
    opal_list_item_t *item;
    orte_universe_t *uniptr;
    int ret;

    /* if the user didn't provide a name for our universe, then we have to check
     * for other universe names we could join. It is virtually impossible for
     * another universe to have our exact default universe name as they would
     * have to have the same PID - and that would be bad in so many ways!
     */
    if (orte_universe_info.default_name) {
        /* if we just have the default name - i.e., no name was specified -
         * then get a list of all universes known on the local system. All
         * we can do here is just loop through the session directory tree
         * for universes - we have no better discovery mechanism at this time
         */
        OBJ_CONSTRUCT(&universes, opal_list_t);
        if (ORTE_SUCCESS != (ret = orte_universe_search(&universes, false, false))) {
            /* if nothing was found, that's okay - report anything else */
            if (ORTE_ERR_NOT_FOUND != ret) {
                ORTE_ERROR_LOG(ret);
            }
            return ret;
        }
        /* if the list is empty, then we can just return */
        if (opal_list_is_empty(&universes)) return ORTE_ERR_NOT_FOUND;
        
        /* we have no real criteria for picking one over the other, so
         * we just loop through the returned objects and pick the first
         * one that will support connection
         */
        while (NULL != (item = opal_list_remove_first(&universes))) {
            uniptr = (orte_universe_t*)item;
            if (ORTE_SUCCESS == orte_universe_check_connect(uniptr)) {
                univ->name = strdup(uniptr->name);
                univ->host = strdup(uniptr->host);
                univ->uid = strdup(uniptr->uid);
                univ->persistence = uniptr->persistence;
                univ->scope = strdup(uniptr->scope);
                univ->seed_uri = strdup(uniptr->seed_uri);
                univ->console_connected = uniptr->console_connected;
                return ORTE_SUCCESS;
            }
        }
        
        /* if we get here, then we did not success in connecting to
         * anyone - report that situation
         */
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* if the user did provide a name, then see if we can join it */
    
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

    return orte_universe_check_connect(univ);
}
