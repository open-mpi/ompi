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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "pmix_config.h"
#include "include/pmix_common.h"

#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <errno.h>

#include "src/util/basename.h"
#include "src/util/path.h"
#include "src/util/pmix_environ.h"

#include "src/util/context_fns.h"

int pmix_util_check_context_cwd(pmix_app_t *app)
{
    /* If we want to chdir and the chdir fails (for any reason -- such
       as if the dir doesn't exist, it isn't a dir, we don't have
       permissions, etc.), then return error. */
    if (NULL != app->cwd && 0 != chdir(app->cwd)) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* All happy */
    return PMIX_SUCCESS;
}

int pmix_util_check_context_app(pmix_app_t *app, char **env)
{
    char *tmp;

    /* Here's the possibilities:

        1. The caller specified an absolute pathname for the executable.
        We simply need to verify that it exists and we can run it.

        2. The caller specified a relative pathname for the executable.
        Ditto with #1 -- based on the cwd, we need to verify that it
        exists and we can run it.

        3. The caller specified a naked filename.  We need to search the
        path, find a match, and verify that we can run it.
    */

    tmp = pmix_basename(app->cmd);
    if (strlen(tmp) == strlen(app->cmd)) {
        /* If this is a naked executable -- no relative or absolute
        pathname -- then search the PATH for it */
        free(tmp);
        tmp = pmix_path_findv(app->cmd, X_OK, env, app->cwd);
        if (NULL == tmp) {
            return PMIX_ERR_NOT_FOUND;
        }
        free(app->cmd);
        app->cmd = tmp;
    } else {
        free(tmp);
        if (0 != access(app->cmd, X_OK)) {
            return PMIX_ERR_NO_PERMISSIONS;
        }
    }

    /* All was good */
    return PMIX_SUCCESS;
}
