/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "pmix_config.h"
#include "pmix_common.h"

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#include <errno.h>

#include "src/util/pmix_basename.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_environ.h"

#include "src/util/pmix_context_fns.h"

pmix_status_t pmix_util_check_context_cwd(char **incwd,
                                          bool want_chdir,
                                          bool user_cwd)
{
    bool good = true;
    const char *tmp;
    char *cwd = NULL;

    if (NULL == incwd) {
        return PMIX_ERR_BAD_PARAM;
    }
    if (NULL == *incwd) {
        return PMIX_SUCCESS;
    }
    cwd = *incwd;

    /* If we want to chdir and the chdir fails (for any reason -- such
       as if the dir doesn't exist, it isn't a dir, we don't have
       permissions, etc.), then return error. */
    if (want_chdir && 0 != chdir(cwd)) {
        good = false;
    }

    /* If either of the above failed, go into this block */
    if (!good) {
        /* See if the directory was a user-specified directory.  If it
         was, barf because they specifically asked for something we
         can't provide. */
        if (user_cwd) {
            /* does not exist, or we must lack permissions */
            return PMIX_ERR_JOB_WDIR_NOT_ACCESSIBLE;
        }

        /* If the user didn't specifically ask for it, then it
         was a system-supplied default directory, so it's ok
         to not go there.  Try to go to the $HOME directory
         instead. */
        tmp = pmix_home_directory(-1);
        if (NULL != tmp) {
            /* Try $HOME.  Same test as above. */
            if (want_chdir && 0 != chdir(tmp)) {
                /* does not exist, or we must lack permissions */
                return PMIX_ERR_JOB_WDIR_NOT_ACCESSIBLE;
            }

            /* Reset the pwd in this local copy of the
             context */
            if (NULL != cwd) {
                free(cwd);
            }
            *incwd = strdup(tmp);
        }

        /* If we couldn't find $HOME, then just take whatever
         the default directory is -- assumedly there *is*
         one, or we wouldn't be running... */
    }
    /* All happy */
    return PMIX_SUCCESS;
}

pmix_status_t pmix_util_check_context_app(char **incmd,
                                          char *cwd,
                                          char **env)
{
    char *tmp;
    char *cmd = *incmd;

    /* Here's the possibilities:

        1. The caller specified an absolute pathname for the executable.
        We simply need to verify that it exists and we can run it.

        2. The caller specified a relative pathname for the executable.
        Ditto with #1 -- based on the cwd, we need to verify that it
        exists and we can run it.

        3. The caller specified a naked filename.  We need to search the
        path, find a match, and verify that we can run it.
    */

    tmp = pmix_basename(cmd);
    if (strlen(tmp) == strlen(cmd)) {
        /* If this is a naked executable -- no relative or absolute
        pathname -- then search the PATH for it */
        free(tmp);
        tmp = pmix_path_findv(cmd, X_OK, env, cwd);
        if (NULL == tmp) {
            return PMIX_ERR_JOB_EXE_NOT_FOUND;
        }
        free(cmd);
        *incmd = tmp;
    } else {
        free(tmp);
        if (0 != access(cmd, X_OK)) {
            return PMIX_ERR_EXE_NOT_ACCESSIBLE;
        }
    }

    /* All was good */
    return PMIX_SUCCESS;
}
