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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * This file is only here because some platforms have a broken strncpy
 * (e.g., Itanium with RedHat Advanced Server glibc).
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include "opal/constants.h"
#include "opal/runtime/opal_params.h"

#include "opal/util/sys_limits.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

/*
 * Create and initialize storage for the system limits
 */
OPAL_DECLSPEC opal_sys_limits_t opal_sys_limits = {
    /* initialized = */     false,
    /* num_files   = */     -1,
    /* num_procs   = */     -1,
    /* file_size   = */      0
};

static rlim_t opal_setlimit(int resource, char *value)
{
    struct rlimit rlim, rlim_set;
    rlim_t maxlim;

    rlim.rlim_cur = 0;

    if (0 == strcmp(value, "max")) {
            maxlim = -1;
    } else if (0 == strncmp(value, "unlimited", strlen(value))) {
            maxlim = RLIM_INFINITY;
    } else {
        maxlim = strtol(value, NULL, 10);
    }

    if (0 <= getrlimit(resource, &rlim)) {
        if (maxlim < 0 || rlim.rlim_max < maxlim) {
            rlim_set.rlim_cur = rlim.rlim_cur;
            rlim_set.rlim_max = rlim.rlim_max;
        } else {
            rlim_set.rlim_cur = maxlim;
            rlim_set.rlim_max = maxlim;
        }
        if (0 <= setrlimit(resource, &rlim_set)) {
            rlim.rlim_cur = rlim_set.rlim_cur;
        } else if (RLIM_INFINITY == maxlim) {
            /* if unlimited wasn't allowed, try to set
             * to max allowed
             */
            rlim_set.rlim_cur = rlim.rlim_max;
            rlim_set.rlim_max = rlim.rlim_max;
            if (0 <= setrlimit(resource, &rlim_set)) {
                rlim.rlim_cur = rlim_set.rlim_cur;
            } else {
                return -1;
            }
        } else {
            return -1;
        }
    } else {
        return -1;
    }
    return rlim.rlim_cur;
}

int opal_util_init_sys_limits(char **errmsg)
{
    char **lims, **lim, *setlim;
    int i;

    /* if limits were not given, then nothing to do */
    if (NULL == opal_set_max_sys_limits) {
        return OPAL_SUCCESS;
    }

    /* parse the requested limits to set */
    lims = opal_argv_split(opal_set_max_sys_limits, ',');

    /* each limit is expressed as a "param:value" pair */
    for (i=0; NULL != lims[i]; i++) {
        lim = opal_argv_split(lims[i], ':');
        if (1 == opal_argv_count(lim)) {
            setlim = "max";
        } else {
            setlim = lim[1];
        }

        /* for historical reasons, a value of "1" means
         * that we set the limits on #files, #children,
         * and max file size
         */
        if (0 == strcmp(lim[0], "1")) {
            if (0 > (opal_sys_limits.num_files = opal_setlimit(RLIMIT_NOFILE, "max"))) {
                *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-failed", true, "openfiles", "max");
                return OPAL_ERROR;
            }
#if HAVE_DECL_RLIMIT_NPROC
            if (0 > (opal_sys_limits.num_procs = opal_setlimit(RLIMIT_NPROC, "max"))) {
                *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-failed", true, "maxchildren", "max");
                return OPAL_ERROR;
            }
#endif
            if (0 > (opal_sys_limits.file_size = opal_setlimit(RLIMIT_FSIZE, "max"))) {
                *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-failed", true, "filesize", "max");
                return OPAL_ERROR;
            }
            break;
        } else if (0 == strcmp(lim[0], "0")) {
            /* user didn't want anything set */
            goto cleanup;
        }
 
        /* process them separately */
        if (0 == strcmp(lim[0], "core")) {
            if (0 > opal_setlimit(RLIMIT_CORE, setlim)) {
                *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-failed", true, "openfiles", setlim);
                return OPAL_ERROR;
            }
        } else if (0 == strcmp(lim[0], "filesize")) {
            if (0 > (opal_sys_limits.file_size = opal_setlimit(RLIMIT_FSIZE, setlim))) {
                *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-failed", true, "filesize", setlim);
                return OPAL_ERROR;
            }
        } else if (0 == strcmp(lim[0], "maxmem")) {
            if (0 > opal_setlimit(RLIMIT_AS, setlim)) {
                *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-failed", true, "maxmem", setlim);
                return OPAL_ERROR;
            }
        } else if (0 == strcmp(lim[0], "openfiles")) {
            if (0 > (opal_sys_limits.num_files = opal_setlimit(RLIMIT_NOFILE, setlim))) {
                *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-failed", true, "openfiles", setlim);
                return OPAL_ERROR;
            }
        } else if (0 == strcmp(lim[0], "stacksize")) {
            if (0 > opal_setlimit(RLIMIT_STACK, setlim)) {
                *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-failed", true, "stacksize", setlim);
                return OPAL_ERROR;
            }
#if HAVE_DECL_RLIMIT_NPROC
        } else if (0 == strcmp(lim[0], "maxchildren")) {
            if (0 > (opal_sys_limits.num_procs = opal_setlimit(RLIMIT_NPROC, setlim))) {
                *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-failed", true, "maxchildren", setlim);
                return OPAL_ERROR;
            }
#endif
        } else {
            *errmsg = opal_show_help_string("help-opal-util.txt", "sys-limit-unrecognized", true, lim[0], setlim);
            return OPAL_ERROR;
        }
    }

 cleanup:
    if (NULL != lim) {
        opal_argv_free(lim);
    }
    if (NULL != lims) {
        opal_argv_free(lims);
    }

    /* indicate we initialized the limits structure */
    opal_sys_limits.initialized = true;

    return OPAL_SUCCESS;
}
