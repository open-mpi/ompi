/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <ctype.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/util/pmix_argv.h"
#include "src/util/attr.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_output.h"
#include "src/util/proc_info.h"

#include "src/util/proc_info.h"

/* provide a connection to a reqd variable */
extern bool prte_keep_fqdn_hostnames;

PRTE_EXPORT prte_process_info_t prte_process_info = {
    .myproc = PMIX_PROC_STATIC_INIT,
    .my_hnp = PMIX_PROC_STATIC_INIT,
    .my_hnp_uri = NULL,
    .my_parent = PMIX_PROC_STATIC_INIT,
    .hnp_pid = 0,
    .num_daemons = 1,
    .num_nodes = 1,
    .nodename = NULL,
    .aliases = NULL,
    .pid = 0,
    .proc_type = PRTE_PROC_TYPE_NONE,
    .my_port = 0,
    .tmpdir_base = NULL,
    .top_session_dir = NULL,
    .cpuset = NULL,
    .shared_fs = false
};

static bool init = false;
static char *prte_strip_prefix;

void prte_setup_hostname(void)
{
    char *ptr;
    char hostname[PRTE_MAXHOSTNAMELEN];
    char **prefixes;
    bool match;
    int i, idx;

    /* whether or not to keep FQDN hostnames */
    prte_keep_fqdn_hostnames = false;
    (void) pmix_mca_base_var_register("prte", "prte", NULL, "keep_fqdn_hostnames",
                                      "Whether or not to keep FQDN hostnames [default: no]",
                                      PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                      &prte_keep_fqdn_hostnames);

    /* get the nodename */
    gethostname(hostname, sizeof(hostname));

    prte_strip_prefix = NULL;
    (void) pmix_mca_base_var_register(
        "prte", "prte", NULL, "strip_prefix",
        "Prefix(es) to match when deciding whether to strip leading characters and zeroes from "
        "node names returned by daemons",
        PMIX_MCA_BASE_VAR_TYPE_STRING, &prte_strip_prefix);

    /* we have to strip node names here, if user directs, to ensure that
     * the names exchanged in the modex match the names found locally
     */
    if (NULL != prte_strip_prefix && !pmix_net_isaddr(hostname)) {
        prefixes = PMIX_ARGV_SPLIT_COMPAT(prte_strip_prefix, ',');
        match = false;
        for (i = 0; NULL != prefixes[i]; i++) {
            if (0 == strncmp(hostname, prefixes[i], strlen(prefixes[i]))) {
                /* remove the prefix and leading zeroes */
                idx = strlen(prefixes[i]);
                while (idx < (int) strlen(hostname)
                       && (hostname[idx] <= '0' || '9' < hostname[idx])) {
                    idx++;
                }
                if ((int) strlen(hostname) <= idx) {
                    /* there were no non-zero numbers in the name */
                    prte_process_info.nodename = strdup(&hostname[strlen(prefixes[i])]);
                } else {
                    prte_process_info.nodename = strdup(&hostname[idx]);
                }
                /* add this to our list of aliases */
                PMIX_ARGV_APPEND_UNIQUE_COMPAT(&prte_process_info.aliases, prte_process_info.nodename);
                match = true;
                break;
            }
        }
        /* if we didn't find a match, then just use the hostname as-is */
        if (!match) {
            prte_process_info.nodename = strdup(hostname);
        }
        PMIX_ARGV_FREE_COMPAT(prefixes);
    } else {
        prte_process_info.nodename = strdup(hostname);
    }

    // if we are not keeping FQDN, then strip it off if not an IP address
    if (!pmix_net_isaddr(prte_process_info.nodename) &&
        NULL != (ptr = strchr(prte_process_info.nodename, '.'))) {
        if (prte_keep_fqdn_hostnames) {
            /* retain the non-fqdn name as an alias */
            *ptr = '\0';
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&prte_process_info.aliases, prte_process_info.nodename);
            *ptr = '.';
        } else {
            /* add the fqdn name as an alias */
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&prte_process_info.aliases, prte_process_info.nodename);
            /* retain the non-fqdn name as the node's name */
            *ptr = '\0';
        }
    }

}

bool prte_check_host_is_local(const char *name)
{
    int i;

    if (0 == strcmp(name, prte_process_info.nodename) ||
        0 == strcmp(name, "localhost") ||
        0 == strcmp(name, "127.0.0.1")) {
        return true;
    }

    for (i = 0; NULL != prte_process_info.aliases[i]; i++) {
        if (0 == strcmp(name, prte_process_info.aliases[i])) {
            return true;
        }
    }
    /* if it wasn't one of those and we are allowed
     * to resolve addresses, then try that too */
    if (!prte_do_not_resolve) {
        if (pmix_ifislocal(name)) {
            /* add to our aliases */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_process_info.aliases, name);
            return true;
        }
    }
    return false;
}

int prte_proc_info(void)
{

    char *ptr;

    if (init) {
        return PRTE_SUCCESS;
    }

    init = true;

    prte_process_info.my_hnp_uri = NULL;
    pmix_mca_base_var_register("prte", "prte", NULL, "hnp_uri", "HNP contact info",
                               PMIX_MCA_BASE_VAR_TYPE_STRING,
                               &prte_process_info.my_hnp_uri);

    if (NULL != prte_process_info.my_hnp_uri) {
        ptr = prte_process_info.my_hnp_uri;
        /* the uri value passed to us will have quote marks around it to protect
         * the value if passed on the command line. We must remove those
         * to have a correct uri string
         */
        if ('"' == ptr[0]) {
            /* if the first char is a quote, then so will the last one be */
            ptr[strlen(ptr) - 1] = '\0';
            memmove(ptr, ptr + 1, strlen(ptr));
        }
    }

    /* get the process id */
    prte_process_info.pid = getpid();

    /* get the number of nodes in the job */
    prte_process_info.num_nodes = 1;
    (void) pmix_mca_base_var_register("prte", "prte", NULL, "num_nodes",
                                      "Number of nodes in the job",
                                      PMIX_MCA_BASE_VAR_TYPE_INT,
                                      &prte_process_info.num_nodes);

    return PRTE_SUCCESS;
}

int prte_proc_info_finalize(void)
{
    if (!init) {
        return PRTE_SUCCESS;
    }

    if (NULL != prte_process_info.tmpdir_base) {
        free(prte_process_info.tmpdir_base);
        prte_process_info.tmpdir_base = NULL;
    }

    if (NULL != prte_process_info.top_session_dir) {
        free(prte_process_info.top_session_dir);
        prte_process_info.top_session_dir = NULL;
    }

    if (NULL != prte_process_info.nodename) {
        free(prte_process_info.nodename);
        prte_process_info.nodename = NULL;
    }

    if (NULL != prte_process_info.cpuset) {
        free(prte_process_info.cpuset);
        prte_process_info.cpuset = NULL;
    }

    prte_process_info.proc_type = PRTE_PROC_TYPE_NONE;

    PMIX_ARGV_FREE_COMPAT(prte_process_info.aliases);

    init = false;
    return PRTE_SUCCESS;
}
