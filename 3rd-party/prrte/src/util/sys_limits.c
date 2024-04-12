/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <string.h>

#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#    include <sys/resource.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "constants.h"
#include "src/runtime/prte_globals.h"

#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_show_help.h"
#include "src/util/sys_limits.h"

/*
 * Create and initialize storage for the system limits
 */
PRTE_EXPORT prte_sys_limits_t prte_sys_limits = {
    /* initialized = */ false,
    /* num_files   = */ -1,
    /* num_procs   = */ -1,
    /* file_size   = */ 0};

static int prte_setlimit(int resource, char *value, rlim_t *out)
{
    struct rlimit rlim, rlim_set;
    rlim_t maxlim;

    rlim.rlim_cur = 0;

    if (0 == strcmp(value, "max")) {
        maxlim = (rlim_t) -1;
    } else if (0 == strncmp(value, "unlimited", strlen(value))) {
        maxlim = RLIM_INFINITY;
    } else {
        maxlim = strtol(value, NULL, 10);
    }

    if (0 <= getrlimit(resource, &rlim)) {
        if (rlim.rlim_max < maxlim) {
            rlim_set.rlim_cur = rlim.rlim_max;
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
                return PRTE_ERROR;
            }
        } else {
            return PRTE_ERROR;
        }
    } else {
        return PRTE_ERROR;
    }
    *out = rlim.rlim_cur;
    return PRTE_SUCCESS;
}

int prte_util_init_sys_limits(char **errmsg)
{
    char **lims, **lim = NULL, *setlim;
    int i, rc = PRTE_ERROR;
    rlim_t value;

    /* if limits were not given, then nothing to do */
    if (NULL == prte_set_max_sys_limits) {
        return PRTE_SUCCESS;
    }

    /* parse the requested limits to set */
    lims = PMIX_ARGV_SPLIT_COMPAT(prte_set_max_sys_limits, ',');
    if (NULL == lims) {
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    /* each limit is expressed as a "param:value" pair */
    for (i = 0; NULL != lims[i]; i++) {
        lim = PMIX_ARGV_SPLIT_COMPAT(lims[i], ':');
        if (1 == PMIX_ARGV_COUNT_COMPAT(lim)) {
            setlim = "max";
        } else {
            setlim = lim[1];
        }

        /* for historical reasons, a value of "1" means
         * that we set the limits on #files, #children,
         * and max file size
         */
        if (0 == strcmp(lim[0], "1")) {
#if HAVE_DECL_RLIMIT_NOFILE
            if (PRTE_SUCCESS != prte_setlimit(RLIMIT_NOFILE, "max", &value)) {
                *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-failed", true,
                                                "openfiles", "max");
                goto out;
            }
            prte_sys_limits.num_files = value;
#endif
#if HAVE_DECL_RLIMIT_NPROC
            if (PRTE_SUCCESS != prte_setlimit(RLIMIT_NPROC, "max", &value)) {
                *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-failed", true,
                                                "maxchildren", "max");
                goto out;
            }
            prte_sys_limits.num_procs = value;
#endif
#if HAVE_DECL_RLIMIT_FSIZE
            if (PRTE_SUCCESS != prte_setlimit(RLIMIT_FSIZE, "max", &value)) {
                *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-failed", true,
                                                "filesize", "max");
                goto out;
            }
            prte_sys_limits.file_size = value;
#endif
            break;
        } else if (0 == strcmp(lim[0], "0")) {
            /* user didn't want anything set */
            break;
        }

        /* process them separately */
        if (0 == strcmp(lim[0], "core")) {
#if HAVE_DECL_RLIMIT_CORE
            if (PRTE_SUCCESS != prte_setlimit(RLIMIT_CORE, setlim, &value)) {
                *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-failed", true,
                                                "openfiles", setlim);
                goto out;
            }
#endif
        } else if (0 == strcmp(lim[0], "filesize")) {
#if HAVE_DECL_RLIMIT_FSIZE
            if (PRTE_SUCCESS != prte_setlimit(RLIMIT_FSIZE, setlim, &value)) {
                *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-failed", true,
                                                "filesize", setlim);
                goto out;
            }
            prte_sys_limits.file_size = value;
#endif
        } else if (0 == strcmp(lim[0], "maxmem")) {
#if HAVE_DECL_RLIMIT_AS
            if (PRTE_SUCCESS != prte_setlimit(RLIMIT_AS, setlim, &value)) {
                *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-failed", true,
                                                "maxmem", setlim);
                goto out;
            }
#endif
        } else if (0 == strcmp(lim[0], "openfiles")) {
#if HAVE_DECL_RLIMIT_NOFILE
            if (PRTE_SUCCESS != prte_setlimit(RLIMIT_NOFILE, setlim, &value)) {
                *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-failed", true,
                                                "openfiles", setlim);
                goto out;
            }
            prte_sys_limits.num_files = value;
#endif
        } else if (0 == strcmp(lim[0], "stacksize")) {
#if HAVE_DECL_RLIMIT_STACK
            if (PRTE_SUCCESS != prte_setlimit(RLIMIT_STACK, setlim, &value)) {
                *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-failed", true,
                                                "stacksize", setlim);
                goto out;
            }
#endif
        } else if (0 == strcmp(lim[0], "maxchildren")) {
#if HAVE_DECL_RLIMIT_NPROC
            if (PRTE_SUCCESS != prte_setlimit(RLIMIT_NPROC, setlim, &value)) {
                *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-failed", true,
                                                "maxchildren", setlim);
                goto out;
            }
            prte_sys_limits.num_procs = value;
#endif
        } else {
            *errmsg = pmix_show_help_string("help-prte-util.txt", "sys-limit-unrecognized", true,
                                            lim[0], setlim);
            goto out;
        }
        PMIX_ARGV_FREE_COMPAT(lim);
        lim = NULL;
    }

    /* indicate we initialized the limits structure */
    prte_sys_limits.initialized = true;

    rc = PRTE_SUCCESS;

out:
    PMIX_ARGV_FREE_COMPAT(lims);
    if (NULL != lim) {
        PMIX_ARGV_FREE_COMPAT(lim);
    }

    return rc;
}

int prte_getpagesize(void)
{
    static int page_size = -1;

    if (page_size != -1) {
        // testing in a loop showed sysconf() took ~5 usec vs ~0.3 usec with it cached
        return page_size;
    }

#ifdef HAVE_GETPAGESIZE
    return page_size = getpagesize();
#elif defined(_SC_PAGESIZE)
    return page_size = sysconf(_SC_PAGESIZE);
#elif defined(_SC_PAGE_SIZE)
    return page_size = sysconf(_SC_PAGE_SIZE);
#else
    return page_size = 65536; /* safer to overestimate than under */
#endif
}
