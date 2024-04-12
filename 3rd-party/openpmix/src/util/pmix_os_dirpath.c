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
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include <errno.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <stdlib.h>
#if HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_DIRENT_H
#    include <dirent.h>
#endif /* HAVE_DIRENT_H */

#include "pmix_common.h"
#include "src/include/pmix_globals.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_show_help.h"

static const char path_sep[] = PMIX_PATH_SEP;

int pmix_os_dirpath_create(const char *path, const mode_t mode)
{
    char **parts, *tmp;
    int i, len;
    int ret;

    if (NULL == path) { /* protect ourselves from errors */
        return (PMIX_ERR_BAD_PARAM);
    }

    /* try to make directory */
    if (0 == mkdir(path, mode)) {
        return (PMIX_SUCCESS);
    }
    ret = errno; // preserve the error

    /* check the error */
    if (EEXIST == ret) {
        // already exists - try to set the mode
        chmod(path, mode);
    } else if (ENOENT != ret) {
        // cannot create it
        pmix_show_help("help-pmix-util.txt", "mkdir-failed", true,
                       path, strerror(ret));
        return PMIX_ERR_SILENT;
    }

    /* didn't work, so now have to build our way down the tree */
    /* Split the requested path up into its individual parts */

    parts = PMIx_Argv_split(path, path_sep[0]);

    /* Ensure to allocate enough space for tmp: the strlen of the
       incoming path + 1 (for \0) */

    tmp = (char *) malloc(strlen(path) + 1);
    tmp[0] = '\0';

    /* Iterate through all the subdirectory names in the path,
       building up a directory name.  Check to see if that dirname
       exists.  If it doesn't, create it. */

    len = PMIx_Argv_count(parts);
    for (i = 0; i < len; ++i) {
        if (i == 0) {
            /* If in POSIX-land, ensure that we never end a directory
               name with path_sep */

            if ('/' == path[0]) {
                strcat(tmp, path_sep);
            }
            strcat(tmp, parts[i]);
        }

        /* If it's not the first part, ensure that there's a
           preceding path_sep and then append this part */

        else {
            if (path_sep[0] != tmp[strlen(tmp) - 1]) {
                strcat(tmp, path_sep);
            }
            strcat(tmp, parts[i]);
        }

        /* Now that we have the name, try to create it */
        ret = mkdir(tmp, mode);
        if (0 != ret && EEXIST != errno) {
            // true error
            pmix_show_help("help-pmix-util.txt", "mkdir-failed", true,
                           tmp, strerror(errno));
            PMIx_Argv_free(parts);
            free(tmp);
            return PMIX_ERR_SILENT;
        }
    }

    /* All done */

    PMIx_Argv_free(parts);
    free(tmp);
    return PMIX_SUCCESS;
}

/**
 * This function attempts to remove a directory along with all the
 * files in it.  If the recursive variable is non-zero, then it will
 * try to recursively remove all directories.  If provided, the
 * callback function is executed prior to the directory or file being
 * removed.  If the callback returns non-zero, then no removal is
 * done.
 */
int pmix_os_dirpath_destroy(const char *path, bool recursive,
                            pmix_os_dirpath_destroy_callback_fn_t cbfunc)
{
    int rc, exit_status = PMIX_SUCCESS;
    DIR *dp;
    struct dirent *ep;
    char *filenm;

    if (NULL == path) { /* protect against error */
        return PMIX_ERROR;
    }

    /* Open up the directory */
    dp = opendir(path);
    if (NULL == dp) {
        return PMIX_ERROR;
    }

    while (NULL != (ep = readdir(dp))) {
        /* skip:
         *  - . and ..
         */
        if ((0 == strcmp(ep->d_name, ".")) || (0 == strcmp(ep->d_name, ".."))) {
            continue;
        }

        /* Will the caller allow us to remove this file/directory? */
        if (NULL != cbfunc) {
            /*
             * Caller does not wish to remove this file/directory,
             * continue with the rest of the entries
             */
            if (!(cbfunc(path, ep->d_name))) {
                continue;
            }
        }

        /* Create a pathname.  This is not always needed, but it makes
         * for cleaner code just to create it here.  Note that we are
         * allocating memory here, so we need to free it later on.
         */
        filenm = pmix_os_path(false, path, ep->d_name, NULL);

        // attempt to unlink it
        rc = unlink(filenm);
        if (0 > rc) {
            // we failed to unlink it - save the error
            rc = errno;
            if (EPERM == rc || EISDIR == rc) {
                // it's a directory - attempt to remove it
                rc = rmdir(filenm);
                if (0 == rc) {
                    // success
                    continue;
                }
                /* if it wasn't empty and we are recursively removing
                 * paths, then proceed downwards */
                if (ENOTEMPTY == errno && recursive) {
                    rc = pmix_os_dirpath_destroy(filenm, recursive, cbfunc);
                    free(filenm);
                    if (PMIX_SUCCESS != rc) {
                        exit_status = rc;
                        closedir(dp);
                        goto cleanup;
                    }
                }
            } else if (EBUSY == rc) {
                /* file system mount point or another process
                 * is using it */
                exit_status = PMIX_ERROR;
                continue;
            } else {
                // uncorrectable error
                pmix_show_help("help-pmix-util.txt", "unlink-error", true,
                               filenm,  strerror(rc));
                free(filenm);
                exit_status = PMIX_ERROR;
                break;
            }
        }
    }

    /* Done with this directory */
    closedir(dp);

cleanup:

    /*
     * If the directory is empty, then remove it - but
     * leave the system tmpdir alone!
     */
    if (NULL == pmix_server_globals.system_tmpdir ||
        0 != strcmp(path, pmix_server_globals.system_tmpdir)) {
        rmdir(path);
    }

    return exit_status;
}

bool pmix_os_dirpath_is_empty(const char *path)
{
    DIR *dp;
    struct dirent *ep;

    if (NULL != path) { /* protect against error */
        dp = opendir(path);
        if (NULL != dp) {
            while ((ep = readdir(dp))) {
                if ((0 != strcmp(ep->d_name, ".")) && (0 != strcmp(ep->d_name, ".."))) {
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
}

/**
 * Stale function left for PRRTE backward compatility
 */
int pmix_os_dirpath_access(const char *path, const mode_t mode)
{
    PMIX_HIDE_UNUSED_PARAMS(path, mode);
    return PMIX_SUCCESS;
}
