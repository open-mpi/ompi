/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

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

#include "opal/util/show_help.h"
#include "opal/util/basename.h"
#include "opal/util/path.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

int orte_rmgr_base_check_context_cwd(orte_app_context_t *context,
                                     bool want_chdir)
{
    bool good = true;
    char *tmp;
    char hostname[MAXHOSTNAMELEN];
    struct stat buf;

    /* Use hostname in a few messages below */
    gethostname(hostname, sizeof(hostname));

    /* If the directory does not exist, or stat() otherwise fails to
       get info about it, then set good = false. */
    if (!(0 == stat(context->cwd, &buf) && S_ISDIR(buf.st_mode))) {
        good = false;
    }

    /* If the directory does exist, and we want to chdir, and the
       chdir fails, then set good = false. */
    if (good && want_chdir && 0 != chdir(context->cwd)) {
        good = false;
    }

    /* If either of the above failed, go into this block */
    if (!good) {
        /* See if the directory was a user-specified directory.  If it
           was, barf because they specifically asked for something we
           can't provide. */
        if (context->user_specified_cwd) {
            opal_show_help("help-rmgr-base.txt", "chdir-error",
                           true, hostname, context->cwd, strerror(errno));
            return ORTE_ERR_NOT_FOUND;
        }
        
        /* If the user didn't specifically ask for it, then it
           was a system-supplied default directory, so it's ok
           to not go there.  Try to go to the $HOME directory
           instead. */
        tmp = getenv("HOME");
        if (NULL != tmp) {
            /* Try $HOME.  Same 2 tests as above. */
            good = true;
            if (!(0 == stat(tmp, &buf) && S_ISDIR(buf.st_mode))) {
                good = false;
            }
            if (good && want_chdir && 0 != chdir(tmp)) {
                good = false;
            }
            if (!good) {
                opal_show_help("help-rmgr-base.txt", "chdir-error",
                               true, hostname, tmp, strerror(errno));
                return ORTE_ERR_NOT_FOUND;
            }
            
            /* Reset the pwd in this local copy of the
               context */
            free(context->cwd);
            context->cwd = strdup(tmp);
        }
        
        /* If we couldn't find $HOME, then just take whatever
           the default directory is -- assumedly there *is*
           one, or we wouldn't be running... */
    }

    /* All happy */
    return ORTE_SUCCESS;
}

int orte_rmgr_base_check_context_app(orte_app_context_t *context)
{
    char *tmp;
    char hostname[MAXHOSTNAMELEN];

    /* Use hostname in a few messages below */
    gethostname(hostname, sizeof(hostname));

    /* If the app is a naked filename, we need to do a path search for
       it.  orterun will send in whatever the user specified (e.g.,
       "orterun -np 2 uptime"), so in some cases, we need to search
       the path to verify that we can find it.  Here's the
       possibilities:
           
       1. The user specified an absolute pathname for the executable.
          We simply need to verify that it exists and we can run it.
           
       2. The user specified a relative pathname for the executable.
          Ditto with #1 -- based on the cwd, we need to verify that it
          exists and we can run it.
           
       3. The user specified a naked filename.  We need to search the
          path, find a match, and verify that we can run it.

       Note that in some cases, we won't be doing this work here --
       bproc, for example, does not use the fork pls for launching, so
       it does this same work over there. */

    tmp = opal_basename(context->argv[0]);
    if (strlen(tmp) == strlen(context->argv[0])) {
        /* If this is a naked executable -- no relative or absolute
           pathname -- then search the PATH for it */
        free(tmp);
        tmp = opal_path_findv(context->argv[0], X_OK, environ, context->cwd);
        if (NULL == tmp) {
            opal_show_help("help-rmgr-base.txt",
                           "argv0-not-found",
                           true, hostname, context->argv[0]);
            return ORTE_ERR_NOT_FOUND;
        }
        free(context->app);
        context->app = tmp;
    } else {
        if (0 != access(context->app, X_OK)) {
            opal_show_help("help-rmgr-base.txt",
                           "argv0-not-accessible",
                           true, hostname, context->argv[0]);
            return ORTE_ERR_NOT_FOUND;
        }
    }
    
    /* All was good */
    return ORTE_SUCCESS;
}

void orte_rmgr_base_purge_mca_params(char ***env)
{
    char *param;

    param = mca_base_param_environ_variable("rds",NULL,NULL);
    opal_setenv(param, "proxy", true, env);
    free(param);
    
    param = mca_base_param_environ_variable("ras",NULL,NULL);
    opal_setenv(param, "proxy", true, env);
    free(param);
    
    param = mca_base_param_environ_variable("rmaps",NULL,NULL);
    opal_setenv(param, "proxy", true, env);
    free(param);
    
    param = mca_base_param_environ_variable("pls",NULL,NULL);
    opal_setenv(param, "proxy", true, env);
    free(param);
    
    param = mca_base_param_environ_variable("rmgr",NULL,NULL);
    opal_setenv(param, "proxy", true, env);
    free(param);
}
