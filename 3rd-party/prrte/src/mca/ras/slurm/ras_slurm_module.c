/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include <ctype.h>
#include <netdb.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#include "src/include/prte_socket_errno.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"

#include "ras_slurm.h"
#include "src/mca/ras/base/ras_private.h"

#define PRTE_SLURM_DYN_MAX_SIZE 256

/*
 * API functions
 */
static int init(void);
static int prte_ras_slurm_allocate(prte_job_t *jdata, pmix_list_t *nodes);
static void deallocate(prte_job_t *jdata, prte_app_context_t *app);
static int prte_ras_slurm_finalize(void);

/*
 * RAS slurm module
 */
prte_ras_base_module_t prte_ras_slurm_module = {init, prte_ras_slurm_allocate, deallocate,
                                                prte_ras_slurm_finalize};

/* Local functions */
static int prte_ras_slurm_discover(char *regexp, char *tasks_per_node, pmix_list_t *nodelist);
static int prte_ras_slurm_parse_ranges(char *base, char *ranges, char ***nodelist);
static int prte_ras_slurm_parse_range(char *base, char *range, char ***nodelist);

static int dyn_allocate(prte_job_t *jdata);
static char *get_node_list(prte_app_context_t *app);
static int parse_alloc_msg(char *msg, int *idx, int *sjob, char **nodelist, char **tpn);

static void recv_data(int fd, short args, void *cbdata);
static void timeout(int fd, short args, void *cbdata);
static int read_ip_port(char *filename, char **ip, uint16_t *port);

/* define structs for tracking dynamic allocations */
typedef struct {
    pmix_object_t super;
    int sjob;
} local_apptracker_t;
PMIX_CLASS_INSTANCE(local_apptracker_t, pmix_object_t, NULL, NULL);

typedef struct {
    pmix_list_item_t super;
    char *cmd;
    prte_event_t timeout_ev;
    pmix_nspace_t nspace;
    pmix_pointer_array_t apps;
    int napps;
} local_jobtracker_t;
static void jtrk_cons(local_jobtracker_t *ptr)
{
    ptr->cmd = NULL;
    PMIX_CONSTRUCT(&ptr->apps, pmix_pointer_array_t);
    pmix_pointer_array_init(&ptr->apps, 1, INT_MAX, 1);
    ptr->napps = 0;
}
static void jtrk_des(local_jobtracker_t *ptr)
{
    int i;
    local_apptracker_t *ap;

    if (NULL != ptr->cmd) {
        free(ptr->cmd);
    }
    for (i = 0; i < ptr->apps.size; i++) {
        if (NULL != (ap = (local_apptracker_t *) pmix_pointer_array_get_item(&ptr->apps, i))) {
            PMIX_RELEASE(ap);
        }
    }
    PMIX_DESTRUCT(&ptr->apps);
}
PMIX_CLASS_INSTANCE(local_jobtracker_t, pmix_list_item_t, jtrk_cons, jtrk_des);

/* local vars */
static int socket_fd;
static pmix_list_t jobs;
static prte_event_t recv_ev;

/* init the module */
static int init(void)
{
    char *slurm_host = NULL;
    uint16_t port = 0;
    struct sockaddr_in address;
    int flags;
    struct hostent *h;

    if (prte_mca_ras_slurm_component.dyn_alloc_enabled) {
        if (NULL == prte_mca_ras_slurm_component.config_file) {
            pmix_show_help("help-ras-slurm.txt", "dyn-alloc-no-config", true);
            return PRTE_ERR_SILENT;
        }
        /* setup the socket */
        if (PRTE_SUCCESS != read_ip_port(prte_mca_ras_slurm_component.config_file, &slurm_host, &port)
            || NULL == slurm_host || 0 == port) {
            if (NULL != slurm_host) {
                free(slurm_host);
            }
            return PRTE_ERR_SILENT;
        }
        PMIX_OUTPUT_VERBOSE((2, prte_ras_base_framework.framework_output,
                             "ras:slurm got [ ip = %s, port = %u ] from %s\n", slurm_host, port,
                             prte_mca_ras_slurm_component.config_file));

        /* obtain a socket for our use */
        if ((socket_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            free(slurm_host);
            return PRTE_ERR_OUT_OF_RESOURCE;
        }

        /* connect to the Slurm dynamic allocation port */
        bzero(&address, sizeof(address));
        address.sin_family = AF_INET;
        if (!pmix_net_isaddr(slurm_host)) {
            /* if the ControlMachine was not specified as an IP address,
             * we need to resolve it here
             */
            if (NULL == (h = gethostbyname(slurm_host))) {
                /* could not resolve it */
                pmix_show_help("help-ras-slurm.txt", "host-not-resolved", true, slurm_host);
                free(slurm_host);
                return PRTE_ERR_SILENT;
            }
            free(slurm_host);
            slurm_host = strdup(inet_ntoa(*(struct in_addr *) h->h_addr_list[0]));
        }
        address.sin_addr.s_addr = inet_addr(slurm_host);
        address.sin_port = htons(port);
        if (connect(socket_fd, (struct sockaddr *) &address, sizeof(address)) < 0) {
            pmix_show_help("help-ras-slurm.txt", "connection-failed", true, slurm_host, (int) port);
            free(slurm_host);
            return PRTE_ERR_SILENT;
        }
        free(slurm_host);

        /* set socket up to be non-blocking */
        if ((flags = fcntl(socket_fd, F_GETFL, 0)) < 0) {
            pmix_output(0, "ras:slurm:dyn: fcntl(F_GETFL) failed: %s (%d)",
                        strerror(prte_socket_errno), prte_socket_errno);
            return PRTE_ERROR;
        } else {
            flags |= O_NONBLOCK;
            if (fcntl(socket_fd, F_SETFL, flags) < 0) {
                pmix_output(0, "ras:slurm:dyn: fcntl(F_SETFL) failed: %s (%d)",
                            strerror(prte_socket_errno), prte_socket_errno);
                return PRTE_ERROR;
            }
        }

        /* setup to recv data */
        prte_event_set(prte_event_base, &recv_ev, socket_fd, PRTE_EV_READ, recv_data, NULL);
        prte_event_add(&recv_ev, 0);

        /* initialize the list of jobs for tracking dynamic allocations */
        PMIX_CONSTRUCT(&jobs, pmix_list_t);
    }
    return PRTE_SUCCESS;
}

/**
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 *
 */
static int prte_ras_slurm_allocate(prte_job_t *jdata, pmix_list_t *nodes)
{
    int ret, cpus_per_task;
    char *slurm_node_str, *regexp;
    char *tasks_per_node, *node_tasks;
    char *tmp;
    char *slurm_jobid;

    if (NULL == (slurm_jobid = getenv("SLURM_JOBID"))) {
        /* we are not in a slurm allocation - see if dyn alloc
         * is enabled
         */
        if (!prte_mca_ras_slurm_component.dyn_alloc_enabled) {
            /* nope - nothing we can do */
            pmix_output_verbose(2, prte_ras_base_framework.framework_output,
                                "%s ras:slurm: no prior allocation and dynamic alloc disabled",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
            return PRTE_ERR_TAKE_NEXT_OPTION;
        }
    } else {
        /* save this value in the global job ident string for
         * later use in any error reporting
         */
        prte_job_ident = strdup(slurm_jobid);
    }

    slurm_node_str = getenv("SLURM_NODELIST");
    if (NULL == slurm_node_str) {
        /* see if dynamic allocation is enabled */
        if (prte_mca_ras_slurm_component.dyn_alloc_enabled) {
            /* attempt to get the allocation - the function
             * dyn_allocate will return as PRTE_ERR_ALLOCATION_PENDING
             * if it succeeds in sending the allocation request
             */
            ret = dyn_allocate(jdata);
            /* return to the above layer in ras/base/ras_base_allocate.c
             * to wait for event (libevent) happening
             */
            return ret;
        }
        pmix_show_help("help-ras-slurm.txt", "slurm-env-var-not-found", 1, "SLURM_NODELIST");
        return PRTE_ERR_NOT_FOUND;
    }
    regexp = strdup(slurm_node_str);
    if (NULL == regexp) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    if (prte_mca_ras_slurm_component.use_all) {
        /* this is an oddball case required for debug situations where
         * a tool is started that will then call mpirun. In this case,
         * Slurm will assign only 1 tasks/per node to the tool, but
         * we want mpirun to use the entire allocation. They don't give
         * us a specific variable for this purpose, so we have to fudge
         * a bit - but this is a special edge case, and we'll live with it */
        tasks_per_node = getenv("SLURM_JOB_CPUS_PER_NODE");
        if (NULL == tasks_per_node) {
            /* couldn't find any version - abort */
            pmix_show_help("help-ras-slurm.txt", "slurm-env-var-not-found", 1,
                           "SLURM_JOB_CPUS_PER_NODE");
            free(regexp);
            return PRTE_ERR_NOT_FOUND;
        }
        node_tasks = strdup(tasks_per_node);
        if (NULL == node_tasks) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            free(regexp);
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        cpus_per_task = 1;
    } else {
        /* get the number of process slots we were assigned on each node */
        tasks_per_node = getenv("SLURM_TASKS_PER_NODE");
        if (NULL == tasks_per_node) {
            /* couldn't find any version - abort */
            pmix_show_help("help-ras-slurm.txt", "slurm-env-var-not-found", 1,
                           "SLURM_TASKS_PER_NODE");
            free(regexp);
            return PRTE_ERR_NOT_FOUND;
        }
        node_tasks = strdup(tasks_per_node);
        if (NULL == node_tasks) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            free(regexp);
            return PRTE_ERR_OUT_OF_RESOURCE;
        }

        /* get the number of CPUs per task that the user provided to slurm */
        tmp = getenv("SLURM_CPUS_PER_TASK");
        if (NULL != tmp) {
            cpus_per_task = atoi(tmp);
            if (0 >= cpus_per_task) {
                pmix_output(0,
                            "ras:slurm:allocate: Got bad value from SLURM_CPUS_PER_TASK. "
                            "Variable was: %s\n",
                            tmp);
                PRTE_ERROR_LOG(PRTE_ERROR);
                free(node_tasks);
                free(regexp);
                return PRTE_ERROR;
            }
        } else {
            cpus_per_task = 1;
        }
    }

    ret = prte_ras_slurm_discover(regexp, node_tasks, nodes);
    free(regexp);
    free(node_tasks);
    if (PRTE_SUCCESS != ret) {
        PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                             "%s ras:slurm:allocate: discover failed!",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        return ret;
    }
    /* record the number of allocated nodes */
    prte_num_allocated_nodes = pmix_list_get_size(nodes);

    /* All done */

    PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                         "%s ras:slurm:allocate: success", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
    return PRTE_SUCCESS;
}

static void deallocate(prte_job_t *jdata, prte_app_context_t *app)
{
    PRTE_HIDE_UNUSED_PARAMS(jdata, app);
    return;
}

static int prte_ras_slurm_finalize(void)
{
    pmix_list_item_t *item;

    if (prte_mca_ras_slurm_component.dyn_alloc_enabled) {
        /* delete the recv event */
        prte_event_del(&recv_ev);
        while (NULL != (item = pmix_list_remove_first(&jobs))) {
            PMIX_RELEASE(item);
        }
        PMIX_DESTRUCT(&jobs);
        /* close the socket */
        shutdown(socket_fd, 2);
        close(socket_fd);
    }
    return PRTE_SUCCESS;
}

/**
 * Discover the available resources.
 *
 * In order to fully support slurm, we need to be able to handle
 * node regexp/task_per_node strings such as:
 * foo,bar    5,3
 * foo        5
 * foo[2-10,12,99-105],bar,foobar[3-11] 2(x10),5,100(x16)
 *
 * @param *regexp A node regular expression from SLURM (i.e. SLURM_NODELIST)
 * @param *tasks_per_node A tasks per node expression from SLURM
 *                        (i.e. SLURM_TASKS_PER_NODE)
 * @param *nodelist A list which has already been constucted to return
 *                  the found nodes in
 */
static int prte_ras_slurm_discover(char *regexp, char *tasks_per_node, pmix_list_t *nodelist)
{
    int i, j, len, ret, count, reps, num_nodes;
    char *base, **names = NULL;
    char *begptr, *endptr, *orig;
    int *slots;
    bool found_range = false;
    bool more_to_come = false;

    orig = base = strdup(regexp);
    if (NULL == base) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                         "%s ras:slurm:allocate:discover: checking nodelist: %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), regexp));

    do {
        /* Find the base */
        len = strlen(base);
        for (i = 0; i <= len; ++i) {
            if (base[i] == '[') {
                /* we found a range. this gets dealt with below */
                base[i] = '\0';
                found_range = true;
                break;
            }
            if (base[i] == ',') {
                /* we found a singleton node, and there are more to come */
                base[i] = '\0';
                found_range = false;
                more_to_come = true;
                break;
            }
            if (base[i] == '\0') {
                /* we found a singleton node */
                found_range = false;
                more_to_come = false;
                break;
            }
        }
        if (i == 0) {
            /* we found a special character at the beginning of the string */
            pmix_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value", 1, regexp,
                           tasks_per_node, "SLURM_NODELIST");
            PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
            free(orig);
            return PRTE_ERR_BAD_PARAM;
        }

        if (found_range) {
            /* If we found a range, now find the end of the range */
            for (j = i; j < len; ++j) {
                if (base[j] == ']') {
                    base[j] = '\0';
                    break;
                }
            }
            if (j >= len) {
                /* we didn't find the end of the range */
                pmix_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value", 1, regexp,
                               tasks_per_node, "SLURM_NODELIST");
                PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
                free(orig);
                return PRTE_ERR_BAD_PARAM;
            }

            ret = prte_ras_slurm_parse_ranges(base, base + i + 1, &names);
            if (PRTE_SUCCESS != ret) {
                pmix_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value", 1, regexp,
                               tasks_per_node, "SLURM_NODELIST");
                PRTE_ERROR_LOG(ret);
                free(orig);
                return ret;
            }
            if (base[j + 1] == ',') {
                more_to_come = true;
                base = &base[j + 2];
            } else {
                more_to_come = false;
            }
        } else {
            /* If we didn't find a range, just add the node */

            PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                                 "%s ras:slurm:allocate:discover: found node %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), base));

            if (PRTE_SUCCESS != (ret = PMIX_ARGV_APPEND_NOSIZE_COMPAT(&names, base))) {
                PRTE_ERROR_LOG(ret);
                free(orig);
                return ret;
            }
            /* set base equal to the (possible) next base to look at */
            base = &base[i + 1];
        }
    } while (more_to_come);

    free(orig);

    num_nodes = PMIX_ARGV_COUNT_COMPAT(names);

    /* Find the number of slots per node */

    slots = malloc(sizeof(int) * num_nodes);
    if (NULL == slots) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }
    memset(slots, 0, sizeof(int) * num_nodes);

    orig = begptr = strdup(tasks_per_node);
    if (NULL == begptr) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        free(slots);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    j = 0;
    while (begptr) {
        count = strtol(begptr, &endptr, 10);
        if ((endptr[0] == '(') && (endptr[1] == 'x')) {
            reps = strtol((endptr + 2), &endptr, 10);
            if (endptr[0] == ')') {
                endptr++;
            }
        } else {
            reps = 1;
        }

        /**
         * TBP: it seems like it would be an error to have more slot
         * descriptions than nodes. Turns out that this valid, and SLURM will
         * return such a thing. For instance, if I did:
         * srun -A -N 30 -w odin001
         * I would get SLURM_NODELIST=odin001 SLURM_TASKS_PER_NODE=4(x30)
         * That is, I am allocated 30 nodes, but since I only requested
         * one specific node, that's what is in the nodelist.
         * I'm not sure this is what users would expect, but I think it is
         * more of a SLURM issue than a prte issue, since SLURM is OK with it,
         * I'm ok with it
         */
        for (i = 0; i < reps && j < num_nodes; i++) {
            slots[j++] = count;
        }

        if (*endptr == ',') {
            begptr = endptr + 1;
        } else if (*endptr == '\0' || j >= num_nodes) {
            break;
        } else {
            pmix_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value", 1, regexp,
                           tasks_per_node, "SLURM_TASKS_PER_NODE");
            PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
            free(slots);
            free(orig);
            return PRTE_ERR_BAD_PARAM;
        }
    }

    free(orig);

    /* Convert the argv of node names to a list of node_t's */

    for (i = 0; NULL != names && NULL != names[i]; ++i) {
        prte_node_t *node;

        PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                             "%s ras:slurm:allocate:discover: adding node %s (%d slot%s)",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), names[i], slots[i],
                             (1 == slots[i]) ? "" : "s"));

        node = PMIX_NEW(prte_node_t);
        if (NULL == node) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            free(slots);
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        node->name = strdup(names[i]);
        node->state = PRTE_NODE_STATE_UP;
        node->slots_inuse = 0;
        node->slots_max = 0;
        node->slots = slots[i];
        pmix_list_append(nodelist, &node->super);
    }
    free(slots);
    PMIX_ARGV_FREE_COMPAT(names);

    /* All done */
    return ret;
}

/*
 * Parse one or more ranges in a set
 *
 * @param base     The base text of the node name
 * @param *ranges  A pointer to a range. This can contain multiple ranges
 *                 (i.e. "1-3,10" or "5" or "9,0100-0130,250")
 * @param ***names An argv array to add the newly discovered nodes to
 */
static int prte_ras_slurm_parse_ranges(char *base, char *ranges, char ***names)
{
    int i, len, ret;
    char *start, *orig;

    /* Look for commas, the separator between ranges */

    len = strlen(ranges);
    for (orig = start = ranges, i = 0; i < len; ++i) {
        if (',' == ranges[i]) {
            ranges[i] = '\0';
            ret = prte_ras_slurm_parse_range(base, start, names);
            if (PRTE_SUCCESS != ret) {
                PRTE_ERROR_LOG(ret);
                return ret;
            }
            start = ranges + i + 1;
        }
    }

    /* Pick up the last range, if it exists */

    if (start < orig + len) {

        PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                             "%s ras:slurm:allocate:discover: parse range %s (2)",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), start));

        ret = prte_ras_slurm_parse_range(base, start, names);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* All done */
    return PRTE_SUCCESS;
}

/*
 * Parse a single range in a set and add the full names of the nodes
 * found to the names argv
 *
 * @param base     The base text of the node name
 * @param *ranges  A pointer to a single range. (i.e. "1-3" or "5")
 * @param ***names An argv array to add the newly discovered nodes to
 */
static int prte_ras_slurm_parse_range(char *base, char *range, char ***names)
{
    char *str, temp1[BUFSIZ];
    size_t i, j, start, end;
    size_t base_len, len, num_len;
    size_t num_str_len;
    bool found;
    int ret;

    len = strlen(range);
    base_len = strlen(base);
    /* Silence compiler warnings; start and end are always assigned
       properly, below */
    start = end = 0;

    /* Look for the beginning of the first number */

    for (found = false, i = 0; i < len; ++i) {
        if (isdigit((int) range[i])) {
            if (!found) {
                start = atoi(range + i);
                found = true;
                break;
            }
        }
    }
    if (!found) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }

    /* Look for the end of the first number */

    for (found = false, num_str_len = 0; i < len; ++i, ++num_str_len) {
        if (!isdigit((int) range[i])) {
            break;
        }
    }

    /* Was there no range, just a single number? */

    if (i >= len) {
        end = start;
        found = true;
    }

    /* Nope, there was a range.  Look for the beginning of the second
       number */

    else {
        for (; i < len; ++i) {
            if (isdigit((int) range[i])) {
                end = atoi(range + i);
                found = true;
                break;
            }
        }
    }
    if (!found) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }

    /* Make strings for all values in the range */

    len = base_len + num_str_len + 32;
    str = malloc(len);
    if (NULL == str) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }
    strcpy(str, base);
    for (i = start; i <= end; ++i) {
        str[base_len] = '\0';
        snprintf(temp1, BUFSIZ - 1, "%lu", (long) i);

        /* Do we need zero pading? */

        if ((num_len = strlen(temp1)) < num_str_len) {
            for (j = base_len; j < base_len + (num_str_len - num_len); ++j) {
                str[j] = '0';
            }
            str[j] = '\0';
        }
        strcat(str, temp1);
        ret = PMIX_ARGV_APPEND_NOSIZE_COMPAT(names, str);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            free(str);
            return ret;
        }
    }
    free(str);

    /* All done */
    return PRTE_SUCCESS;
}

static void timeout(int fd, short args, void *cbdata)
{
    local_jobtracker_t *jtrk = (local_jobtracker_t *) cbdata;
    prte_job_t *jdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    pmix_show_help("help-ras-slurm.txt", "slurm-dyn-alloc-timeout", true);
    pmix_output_verbose(2, prte_ras_base_framework.framework_output,
                        "%s Timed out on dynamic allocation", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
    /* indicate that we failed to receive an allocation */
    jdata = prte_get_job_data_object(jtrk->nspace);
    PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_ALLOC_FAILED);
}

static void recv_data(int fd, short args, void *cbdata)
{
    bool found;
    int i, rc;
    prte_node_t *nd, *nd2;
    pmix_list_t nds, ndtmp;
    pmix_list_item_t *item, *itm;
    char recv_msg[8192];
    int nbytes, idx, sjob;
    char **alloc, *nodelist, *tpn;
    local_jobtracker_t *ptr, *jtrk;
    local_apptracker_t *aptrk;
    prte_app_context_t *app;
    pmix_nspace_t jobid;
    prte_job_t *jdata;
    char **dash_host = NULL;
    PRTE_HIDE_UNUSED_PARAMS(args, cbdata);

    pmix_output_verbose(2, prte_ras_base_framework.framework_output,
                        "%s ras:slurm: dynamic allocation - data recvd",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    /* read the data from the socket and put it in the
     * nodes field of op
     */
    memset(recv_msg, 0, sizeof(recv_msg));
    nbytes = read(fd, recv_msg, sizeof(recv_msg) - 1);

    pmix_output_verbose(2, prte_ras_base_framework.framework_output,
                        "%s ras:slurm: dynamic allocation msg: %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), recv_msg);

    /* check if we got something */
    if (0 == nbytes || 0 == strlen(recv_msg) || strstr(recv_msg, "failure") != NULL) {
        /* show an error here - basically, a "nothing was available"
         * message
         */
        pmix_show_help("help-ras-slurm.txt", "slurm-dyn-alloc-failed", true,
                       (0 == strlen(recv_msg)) ? "NO MSG" : recv_msg);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_ALLOC_FAILED);
        return;
    }

    /* break the message into its component parts, separated by colons */
    alloc = PMIX_ARGV_SPLIT_COMPAT(recv_msg, ':');

    /* the first section contains the PRTE jobid for this allocation */
    tpn = strchr(alloc[0], '=');
    PMIX_LOAD_NSPACE(jobid, tpn + 1);
    /* get the corresponding job object */
    jdata = prte_get_job_data_object(jobid);
    PMIX_LOAD_NSPACE(jdata->nspace, jobid);
    jtrk = NULL;
    /* find the associated tracking object */
    for (item = pmix_list_get_first(&jobs); item != pmix_list_get_end(&jobs);
         item = pmix_list_get_next(item)) {
        ptr = (local_jobtracker_t *) item;
        if (PMIX_CHECK_NSPACE(ptr->nspace, jobid)) {
            jtrk = ptr;
            break;
        }
    }
    if (NULL == jtrk) {
        pmix_show_help("help-ras-slurm.txt", "slurm-dyn-alloc-failed", true, "NO JOB TRACKER");
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_ALLOC_FAILED);
        PMIX_ARGV_FREE_COMPAT(alloc);
        return;
    }

    /* stop the timeout event */
    prte_event_del(&jtrk->timeout_ev);

    /* cycle across all the remaining parts - each is the allocation for
     * an app in this job
     */
    PMIX_CONSTRUCT(&nds, pmix_list_t);
    PMIX_CONSTRUCT(&ndtmp, pmix_list_t);
    idx = -1;
    sjob = -1;
    nodelist = NULL;
    tpn = NULL;
    for (i = 1; NULL != alloc[i]; i++) {
        if (PRTE_SUCCESS != parse_alloc_msg(alloc[i], &idx, &sjob, &nodelist, &tpn)) {
            pmix_show_help("help-ras-slurm.txt", "slurm-dyn-alloc-failed", true, jtrk->cmd);
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_ALLOC_FAILED);
            PMIX_ARGV_FREE_COMPAT(alloc);
            if (NULL != nodelist) {
                free(nodelist);
            }
            if (NULL != tpn) {
                free(tpn);
            }
            return;
        }
        if (idx < 0) {
            pmix_show_help("help-ras-slurm.txt", "slurm-dyn-alloc-failed", true, jtrk->cmd);
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_ALLOC_FAILED);
            PMIX_ARGV_FREE_COMPAT(alloc);
            free(nodelist);
            free(tpn);
            return;
        }
        if (NULL == (app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, idx))) {
            pmix_show_help("help-ras-slurm.txt", "slurm-dyn-alloc-failed", true, jtrk->cmd);
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_ALLOC_FAILED);
            PMIX_ARGV_FREE_COMPAT(alloc);
            free(nodelist);
            free(tpn);
            return;
        }
        /* release the current dash_host as that contained the *desired* allocation */
        prte_remove_attribute(&app->attributes, PRTE_APP_DASH_HOST);
        /* track the Slurm jobid */
        if (NULL
            == (aptrk = (local_apptracker_t *) pmix_pointer_array_get_item(&jtrk->apps, idx))) {
            aptrk = PMIX_NEW(local_apptracker_t);
            pmix_pointer_array_set_item(&jtrk->apps, idx, aptrk);
        }
        aptrk->sjob = sjob;
        /* since the nodelist/tpn may contain regular expressions, parse them */
        if (PRTE_SUCCESS != (rc = prte_ras_slurm_discover(nodelist, tpn, &ndtmp))) {
            PRTE_ERROR_LOG(rc);
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_ALLOC_FAILED);
            PMIX_ARGV_FREE_COMPAT(alloc);
            free(nodelist);
            free(tpn);
            return;
        }
        /* transfer the discovered nodes to our node list, and construct
         * the new dash_host entry to match what was allocated
         */
        while (NULL != (item = pmix_list_remove_first(&ndtmp))) {
            nd = (prte_node_t *) item;
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&dash_host, nd->name);
            /* check for duplicates */
            found = false;
            for (itm = pmix_list_get_first(&nds); itm != pmix_list_get_end(&nds);
                 itm = pmix_list_get_next(itm)) {
                nd2 = (prte_node_t *) itm;
                if (0 == strcmp(nd->name, nd2->name)) {
                    found = true;
                    nd2->slots += nd->slots;
                    PMIX_RELEASE(item);
                    break;
                }
            }
            if (!found) {
                /* append the new node to our list */
                pmix_list_append(&nds, item);
            }
        }
        /* cleanup */
        free(nodelist);
        free(tpn);
    }
    /* cleanup */
    PMIX_ARGV_FREE_COMPAT(alloc);
    PMIX_DESTRUCT(&ndtmp);
    if (NULL != dash_host) {
        tpn = PMIX_ARGV_JOIN_COMPAT(dash_host, ',');
        for (idx = 0; idx < jdata->apps->size; idx++) {
            if (NULL
                == (app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, idx))) {
                pmix_show_help("help-ras-slurm.txt", "slurm-dyn-alloc-failed", true, jtrk->cmd);
                PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_ALLOC_FAILED);
                PMIX_ARGV_FREE_COMPAT(dash_host);
                free(tpn);
                return;
            }
            prte_set_attribute(&app->attributes, PRTE_APP_DASH_HOST, PRTE_ATTR_LOCAL, (void *) tpn,
                               PMIX_STRING);
        }
        PMIX_ARGV_FREE_COMPAT(dash_host);
        free(tpn);
    }

    if (pmix_list_is_empty(&nds)) {
        /* if we get here, then we were able to contact slurm,
         * which means we are in an actively managed cluster.
         * However, slurm indicated that nothing is currently
         * available that meets our requirements. This is a fatal
         * situation - we do NOT have the option of running on
         * user-specified hosts as the cluster is managed.
         */
        PMIX_DESTRUCT(&nds);
        pmix_show_help("help-ras-base.txt", "ras-base:no-allocation", true);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_ALLOC_FAILED);
    }

    /* store the found nodes */
    if (PRTE_SUCCESS != (rc = prte_ras_base_node_insert(&nds, jdata))) {
        PRTE_ERROR_LOG(rc);
        PMIX_DESTRUCT(&nds);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_ALLOC_FAILED);
        return;
    }
    PMIX_DESTRUCT(&nds);

    /* default to no-oversubscribe-allowed for managed systems */
    if (!(PRTE_MAPPING_SUBSCRIBE_GIVEN & PRTE_GET_MAPPING_DIRECTIVE(prte_rmaps_base.mapping))) {
        PRTE_SET_MAPPING_DIRECTIVE(prte_rmaps_base.mapping, PRTE_MAPPING_NO_OVERSUBSCRIBE);
    }
    /* flag that the allocation is managed */
    prte_managed_allocation = true;
    /* move the job along */
    PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_ALLOCATION_COMPLETE);
    /* all done */
    return;
}

/* we cannot use the RML to communicate with SLURM as it doesn't
 * understand our internal protocol, so we have to do a bare-bones
 * exchange based on sockets
 */
static int dyn_allocate(prte_job_t *jdata)
{
    char *cmd_str, **cmd = NULL, *tmp;
    char *node_list;
    prte_app_context_t *app;
    int i;
    struct timeval tv;
    local_jobtracker_t *jtrk;
    int64_t i64, *i64ptr;

    if (NULL == prte_mca_ras_slurm_component.config_file) {
        pmix_output(0, "Cannot perform dynamic allocation as no Slurm configuration file provided");
        return PRTE_ERR_NOT_FOUND;
    }

    /* track this request */
    jtrk = PMIX_NEW(local_jobtracker_t);
    PMIX_LOAD_NSPACE(jtrk->nspace, jdata->nspace);
    pmix_list_append(&jobs, &jtrk->super);

    /* construct the command - note that the jdata structure contains
     * a field for the minimum number of nodes required for the job.
     * The node list can be constructed from the union of all the nodes
     * contained in the dash_host field of the app_contexts. So you'll
     * need to do a little work to build the command. We don't currently
     * have a field in the jdata structure for "mandatory" vs "optional"
     * allocations, so we'll have to add that someday. Likewise, you may
     * want to provide a param to adjust the timeout value
     */
    /* construct the cmd string */
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, "allocate");
    /* add the jobid */
    pmix_asprintf(&tmp, "jobid=%s", jdata->nspace);
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, tmp);
    free(tmp);
    /* if we want the allocation for all apps in one shot,
     * then tell slurm
     *
     * RHC: we don't currently have the ability to handle
     * rolling allocations in the rest of the code base
     */
#if 0
    if (!prte_mca_ras_slurm_component.rolling_alloc) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, "return=all");
    }
#else
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, "return=all");
#endif

    /* pass the timeout */
    pmix_asprintf(&tmp, "timeout=%d", prte_mca_ras_slurm_component.timeout);
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, tmp);
    free(tmp);

    /* for each app, add its allocation request info */
    i64ptr = &i64;
    for (i = 0; i < jdata->apps->size; i++) {
        if (NULL == (app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        /* add the app id, preceded by a colon separator */
        pmix_asprintf(&tmp, ": app=%d", (int) app->idx);
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, tmp);
        free(tmp);
        /* add the number of process "slots" we need */
        pmix_asprintf(&tmp, "np=%d", app->num_procs);
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, tmp);
        free(tmp);
        /* if we were given a minimum number of nodes, pass it along */
        if (prte_get_attribute(&app->attributes, PRTE_APP_MIN_NODES, (void **) &i64ptr,
                               PMIX_INT64)) {
            pmix_asprintf(&tmp, "N=%ld", (long int) i64);
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, tmp);
            free(tmp);
        }
        /* add the list of nodes, if one was given, ensuring
         * that each node only appears once
         */
        node_list = get_node_list(app);
        if (NULL != node_list) {
            pmix_asprintf(&tmp, "node_list=%s", node_list);
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, tmp);
            free(node_list);
            free(tmp);
        }
        /* add the mandatory/optional flag */
        if (prte_get_attribute(&app->attributes, PRTE_APP_MANDATORY, NULL, PMIX_BOOL)) {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, "flag=mandatory");
        } else {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&cmd, "flag=optional");
        }
    }

    /* assemble it into the final cmd to be sent */
    cmd_str = PMIX_ARGV_JOIN_COMPAT(cmd, ' ');
    PMIX_ARGV_FREE_COMPAT(cmd);

    /* start a timer - if the response to our request doesn't appear
     * in the defined time, then we will error out as Slurm isn't
     * responding to us
     */
    prte_event_evtimer_set(prte_event_base, &jtrk->timeout_ev, timeout, jtrk);
    tv.tv_sec = prte_mca_ras_slurm_component.timeout * 2;
    tv.tv_usec = 0;
    prte_event_evtimer_add(&jtrk->timeout_ev, &tv);

    pmix_output_verbose(2, prte_ras_base_framework.framework_output,
                        "%s slurm:dynalloc cmd_str = %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        cmd_str);

    if (send(socket_fd, cmd_str, strlen(cmd_str) + 1, 0) < 0) {
        PRTE_ERROR_LOG(PRTE_ERR_COMM_FAILURE);
    }
    free(cmd_str);

    /* we cannot wait here for a response as we
     * are already in an event. So return a value
     * that indicates we are waiting for an
     * allocation so the base functions know
     * that they shouldn't progress the job
     */
    return PRTE_ERR_ALLOCATION_PENDING;
}

static int parse_alloc_msg(char *msg, int *idx, int *sjob, char **nodelist, char **tpn)
{
    char *tmp;
    char *p_str;
    char *pos;
    int found = 0;

    if (msg == NULL || strlen(msg) == 0) {
        return PRTE_ERR_BAD_PARAM;
    }

    tmp = strdup(msg);
    p_str = strtok(tmp, " ");
    while (p_str) {
        if (NULL != strstr(p_str, "slurm_jobid")) {
            pos = strchr(p_str, '=');
            *sjob = strtol(pos + 1, NULL, 10);
            found++;
        } else if (NULL != strstr(p_str, "allocated_node_list")) {
            pos = strchr(p_str, '=');
            *nodelist = strdup(pos + 1);
            found++;
        } else if (NULL != strstr(p_str, "tasks_per_node")) {
            pos = strchr(p_str, '=');
            *tpn = strdup(pos + 1);
            found++;
        } else if (NULL != strstr(p_str, "app")) {
            pos = strchr(p_str, '=');
            *idx = strtol(pos + 1, NULL, 10);
            found++;
        }
        p_str = strtok(NULL, " ");
    }
    free(tmp);

    if (4 != found) {
        return PRTE_ERR_NOT_FOUND;
    }
    return PRTE_SUCCESS;
}

static char *get_node_list(prte_app_context_t *app)
{
    int j;
    char **total_host = NULL;
    char *nodes;
    char **dash_host, *dh;

    if (!prte_get_attribute(&app->attributes, PRTE_APP_DASH_HOST, (void **) &dh, PMIX_STRING)) {
        return NULL;
    }
    dash_host = PMIX_ARGV_SPLIT_COMPAT(dh, ',');
    free(dh);
    for (j = 0; NULL != dash_host[j]; j++) {
        PMIX_ARGV_APPEND_UNIQUE_COMPAT(&total_host, dash_host[j]);
    }
    PMIX_ARGV_FREE_COMPAT(dash_host);
    if (NULL == total_host) {
        return NULL;
    }

    nodes = PMIX_ARGV_JOIN_COMPAT(total_host, ',');
    PMIX_ARGV_FREE_COMPAT(total_host);
    return nodes;
}

static int read_ip_port(char *filename, char **ip, uint16_t *port)
{
    FILE *fp;
    char line[PRTE_SLURM_DYN_MAX_SIZE];
    char *pos;
    bool found_port = false;
    bool found_ip = false;

    if (NULL == (fp = fopen(filename, "r"))) {
        pmix_show_help("help-ras-slurm.txt", "config-file-not-found", true, filename);
        return PRTE_ERR_SILENT;
    }

    memset(line, 0, PRTE_SLURM_DYN_MAX_SIZE);
    while (NULL != fgets(line, PRTE_SLURM_DYN_MAX_SIZE, fp) && (!found_ip || !found_port)) {
        if (0 == strlen(line)) {
            continue;
        }
        line[strlen(line) - 1] = '\0';
        if (0 == strncmp(line, "JobSubmitDynAllocPort", strlen("JobSubmitDynAllocPort"))) {
            pos = strstr(line, "=") + 1;
            *port = strtol(pos, NULL, 10);
            found_port = true;
        } else if (0 == strncmp(line, "ControlMachine", strlen("ControlMachine"))) {
            pos = strstr(line, "=") + 1;
            *ip = strdup(pos);
            found_ip = true;
        }
        memset(line, 0, PRTE_SLURM_DYN_MAX_SIZE);
    }

    fclose(fp);
    if (!found_ip) {
        pmix_output(0, "The IP address or name of the Slurm control machine was not provided");
        return PRTE_ERR_NOT_FOUND;
    }
    if (!found_port) {
        pmix_output(0, "The IP port of the Slurm dynamic allocation service was not provided");
        return PRTE_ERR_NOT_FOUND;
    }

    return PRTE_SUCCESS;
}
