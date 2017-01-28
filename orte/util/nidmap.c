/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2012-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/types.h"
#include "orte/constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#include "opal/dss/dss.h"
#include "opal/runtime/opal.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/net.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/datatype/opal_datatype.h"

#include "orte/mca/dfs/dfs.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/state/state.h"

#include "orte/util/nidmap.h"

int orte_util_build_daemon_nidmap(char **nodes)
{
    int i, num_nodes;
    int rc;
    struct hostent *h;
    opal_buffer_t buf;
    opal_process_name_t proc;
    char *uri, *addr;
    char *proc_name;
    opal_value_t kv;

    num_nodes = opal_argv_count(nodes);

    if (0 == num_nodes) {
        /* nothing to do */
        return ORTE_SUCCESS;
    }

    /* install the entry for the HNP */
    proc.jobid = ORTE_PROC_MY_NAME->jobid;
    proc.vpid = 0;
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(ORTE_DB_DAEMON_VPID);
    kv.data.uint32 = proc.vpid;
    kv.type = OPAL_UINT32;
    if (OPAL_SUCCESS != (rc = opal_pmix.store_local(&proc, &kv))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);

    /* the daemon vpids will be assigned in order,
     * starting with vpid=0 for the HNP */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    for (i=0; i < num_nodes; i++) {
        /* define the vpid for this daemon */
        proc.vpid = i;
        /* store the hostname for the proc */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(OPAL_PMIX_HOSTNAME);
        kv.data.string = strdup(nodes[i]);
        kv.type = OPAL_STRING;
        if (OPAL_SUCCESS != (rc = opal_pmix.store_local(&proc, &kv))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);

        /* the arch defaults to our arch so that non-hetero
         * case will yield correct behavior
         */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(OPAL_PMIX_ARCH);
        kv.data.uint32 = opal_local_arch;
        kv.type = OPAL_UINT32;
        if (OPAL_SUCCESS != (rc = opal_pmix.store_local(&proc, &kv))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&kv);
            return rc;
        }
        OBJ_DESTRUCT(&kv);

        /* lookup the address of this node */
        if (NULL == (h = gethostbyname(nodes[i]))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        addr = inet_ntoa(*(struct in_addr*)h->h_addr_list[0]);

        /* since we are using static ports, all my fellow daemons will be on my
         * port. Setup the contact info for each daemon in my hash tables. Note
         * that this will -not- open a port to those daemons, but will only
         * define the info necessary for opening such a port if/when I communicate
         * to them
         */

        /* construct the URI */
        orte_util_convert_process_name_to_string(&proc_name, &proc);
        asprintf(&uri, "%s;tcp://%s:%d", proc_name, addr, (int)orte_process_info.my_port);
        OPAL_OUTPUT_VERBOSE((2, orte_debug_verbosity,
                             "%s orte:util:build:daemon:nidmap node %s daemon %d addr %s uri %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             nodes[i], i+1, addr, uri));
        opal_dss.pack(&buf, &uri, 1, OPAL_STRING);
        free(proc_name);
        free(uri);
    }

    /* load the hash tables */
    if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(&buf))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&buf);

    return rc;
}

int orte_util_encode_nodemap(opal_buffer_t *buffer)
{
    char *node;
    char prefix[ORTE_MAX_NODE_PREFIX];
    int i, j, n, len, startnum, nodenum, numdigits;
    bool found, fullname;
    char *suffix, *sfx;
    orte_regex_node_t *ndreg;
    orte_regex_range_t *range, *rng, *idrng;
    opal_list_t nodeids, nodenms, dvpids;
    opal_list_item_t *item, *itm2;
    char **regexargs = NULL, *tmp, *tmp2;
    orte_node_t *nptr;
    int rc;

    /* setup the list of results */
    OBJ_CONSTRUCT(&nodeids, opal_list_t);
    OBJ_CONSTRUCT(&nodenms, opal_list_t);
    OBJ_CONSTRUCT(&dvpids, opal_list_t);

    rng = NULL;
    idrng = NULL;
    for (n=0; n < orte_node_pool->size; n++) {
        if (NULL == (nptr = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, n))) {
            continue;
        }
        /* if no daemon has been assigned, then this node is not being used */
        if (NULL == nptr->daemon) {
            continue;
        }
        /* deal with the nodeids - these are just the index into
         * the node array. We pass these to ensure coherence across
         * the virtual machine */
        if (NULL == idrng) {
            /* just starting */
            idrng = OBJ_NEW(orte_regex_range_t);
            idrng->start = n;
            idrng->cnt = 1;
            opal_list_append(&nodeids, &idrng->super);
        } else {
            /* is this the next in line */
            if (n == (idrng->start + idrng->cnt)) {
                idrng->cnt++;
            } else {
                /* need to start another range */
                idrng = OBJ_NEW(orte_regex_range_t);
                idrng->start = n;
                opal_list_append(&nodeids, &idrng->super);
            }
        }
        /* deal with the daemon vpid - see if it is next in the
         * current range */
        if (NULL == rng) {
            /* just starting */
            rng = OBJ_NEW(orte_regex_range_t);
            rng->start = nptr->daemon->name.vpid;
            rng->cnt = 1;
            opal_list_append(&dvpids, &rng->super);
        } else {
            /* is this the next in line */
            if (nptr->daemon->name.vpid == (orte_vpid_t)(rng->start + rng->cnt)) {
                rng->cnt++;
            } else {
                /* need to start another range */
            rng = OBJ_NEW(orte_regex_range_t);
            rng->start = nptr->daemon->name.vpid;
            opal_list_append(&dvpids, &rng->super);
            }
        }
        node = nptr->name;
        /* determine this node's prefix by looking for first non-alpha char */
        fullname = false;
        len = strlen(node);
        startnum = -1;
        memset(prefix, 0, ORTE_MAX_NODE_PREFIX);
        numdigits = 0;
        for (i=0, j=0; i < len; i++) {
            if (!isalpha(node[i])) {
                /* found a non-alpha char */
                if (!isdigit(node[i])) {
                    /* if it is anything but a digit, we just use
                     * the entire name
                     */
                    fullname = true;
                    break;
                }
                /* count the size of the numeric field - but don't
                 * add the digits to the prefix
                 */
                numdigits++;
                if (startnum < 0) {
                    /* okay, this defines end of the prefix */
                    startnum = i;
                }
                continue;
            }
            if (startnum < 0) {
                prefix[j++] = node[i];
            }
        }
        if (fullname || startnum < 0) {
            /* can't compress this name - just add it to the list */
            ndreg = OBJ_NEW(orte_regex_node_t);
            ndreg->prefix = strdup(node);
            opal_list_append(&nodenms, &ndreg->super);
            continue;
        }
        /* convert the digits and get any suffix */
        nodenum = strtol(&node[startnum], &sfx, 10);
        if (NULL != sfx) {
            suffix = strdup(sfx);
        } else {
            suffix = NULL;
        }
        /* is this node name already on our list? */
        found = false;
        for (item = opal_list_get_first(&nodenms);
             !found && item != opal_list_get_end(&nodenms);
             item = opal_list_get_next(item)) {
            ndreg = (orte_regex_node_t*)item;
            if (0 < strlen(prefix) && NULL == ndreg->prefix) {
                continue;
            }
            if (0 == strlen(prefix) && NULL != ndreg->prefix) {
                continue;
            }
            if (0 < strlen(prefix) && NULL != ndreg->prefix
                && 0 != strcmp(prefix, ndreg->prefix)) {
                continue;
            }
            if (NULL == suffix && NULL != ndreg->suffix) {
                continue;
            }
            if (NULL != suffix && NULL == ndreg->suffix) {
                continue;
            }
            if (NULL != suffix && NULL != ndreg->suffix &&
                0 != strcmp(suffix, ndreg->suffix)) {
                continue;
            }
            if (numdigits != ndreg->num_digits) {
                continue;
            }
            /* found a match - flag it */
            found = true;
            /* get the last range on this nodeid - we do this
             * to preserve order
             */
            range = (orte_regex_range_t*)opal_list_get_last(&ndreg->ranges);
            if (NULL == range) {
                /* first range for this nodeid */
                range = OBJ_NEW(orte_regex_range_t);
                range->start = nodenum;
                range->cnt = 1;
                opal_list_append(&ndreg->ranges, &range->super);
                break;
            }
            /* see if the node number is out of sequence */
            if (nodenum != (range->start + range->cnt)) {
                /* start a new range */
                range = OBJ_NEW(orte_regex_range_t);
                range->start = nodenum;
                range->cnt = 1;
                opal_list_append(&ndreg->ranges, &range->super);
                break;
            }
            /* everything matches - just increment the cnt */
            range->cnt++;
            break;
        }
        if (!found) {
            /* need to add it */
            ndreg = OBJ_NEW(orte_regex_node_t);
            if (0 < strlen(prefix)) {
                ndreg->prefix = strdup(prefix);
            }
            if (NULL != suffix) {
                ndreg->suffix = strdup(suffix);
            }
            ndreg->num_digits = numdigits;
            opal_list_append(&nodenms, &ndreg->super);
            /* record the first range for this nodeid - we took
             * care of names we can't compress above
             */
            range = OBJ_NEW(orte_regex_range_t);
            range->start = nodenum;
            range->cnt = 1;
            opal_list_append(&ndreg->ranges, &range->super);
        }
        if (NULL != suffix) {
            free(suffix);
        }
    }

    /* begin constructing the regular expression */
    while (NULL != (item = opal_list_remove_first(&nodenms))) {
        ndreg = (orte_regex_node_t*)item;

        /* if no ranges, then just add the name */
        if (0 == opal_list_get_size(&ndreg->ranges)) {
            if (NULL != ndreg->prefix) {
                /* solitary node */
                asprintf(&tmp, "%s", ndreg->prefix);
                opal_argv_append_nosize(&regexargs, tmp);
                free(tmp);
            }
            OBJ_RELEASE(ndreg);
            continue;
        }
        /* start the regex for this nodeid with the prefix */
        if (NULL != ndreg->prefix) {
            asprintf(&tmp, "%s[%d:", ndreg->prefix, ndreg->num_digits);
        } else {
            asprintf(&tmp, "[%d:", ndreg->num_digits);
        }
        /* add the ranges */
        while (NULL != (itm2 = opal_list_remove_first(&ndreg->ranges))) {
            range = (orte_regex_range_t*)itm2;
            if (1 == range->cnt) {
                asprintf(&tmp2, "%s%d,", tmp, range->start);
            } else {
                asprintf(&tmp2, "%s%d-%d,", tmp, range->start, range->start + range->cnt - 1);
            }
            free(tmp);
            tmp = tmp2;
            OBJ_RELEASE(range);
        }
        /* replace the final comma */
        tmp[strlen(tmp)-1] = ']';
        if (NULL != ndreg->suffix) {
            /* add in the suffix, if provided */
            asprintf(&tmp2, "%s%s", tmp, ndreg->suffix);
            free(tmp);
            tmp = tmp2;
        }
        opal_argv_append_nosize(&regexargs, tmp);
        free(tmp);
        OBJ_RELEASE(ndreg);
    }

    /* assemble final result */
    tmp = opal_argv_join(regexargs, ',');
    /* cleanup */
    opal_argv_free(regexargs);
    OBJ_DESTRUCT(&nodenms);

    /* pack the string */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tmp, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OPAL_LIST_DESTRUCT(&dvpids);
        return rc;
    }
    if (NULL != tmp) {
        free(tmp);
    }

    /* do the same for the indices */
    tmp = NULL;
    while (NULL != (item = opal_list_remove_first(&nodeids))) {
        rng = (orte_regex_range_t*)item;
        if (1 < rng->cnt) {
            if (NULL == tmp) {
                asprintf(&tmp, "%d-%d", rng->start, rng->start + rng->cnt - 1);
            } else {
                asprintf(&tmp2, "%s,%d-%d", tmp, rng->start, rng->start + rng->cnt - 1);
                free(tmp);
                tmp = tmp2;
            }
        } else {
            if (NULL == tmp) {
                asprintf(&tmp, "%d", rng->start);
            } else {
                asprintf(&tmp2, "%s,%d", tmp, rng->start);
                free(tmp);
                tmp = tmp2;
            }
        }
        OBJ_RELEASE(rng);
    }
    OPAL_LIST_DESTRUCT(&nodeids);

    /* pack the string */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tmp, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OPAL_LIST_DESTRUCT(&dvpids);
        return rc;
    }
    if (NULL != tmp) {
        free(tmp);
    }

    /* do the same for the vpids */
    tmp = NULL;
    while (NULL != (item = opal_list_remove_first(&dvpids))) {
        rng = (orte_regex_range_t*)item;
        if (1 < rng->cnt) {
            if (NULL == tmp) {
                asprintf(&tmp, "%d-%d", rng->start, rng->start + rng->cnt - 1);
            } else {
                asprintf(&tmp2, "%s,%d-%d", tmp, rng->start, rng->start + rng->cnt - 1);
                free(tmp);
                tmp = tmp2;
            }
        } else {
            if (NULL == tmp) {
                asprintf(&tmp, "%d", rng->start);
            } else {
                asprintf(&tmp2, "%s,%d", tmp, rng->start);
                free(tmp);
                tmp = tmp2;
            }
        }
        OBJ_RELEASE(rng);
    }
    OPAL_LIST_DESTRUCT(&dvpids);

    /* pack the string */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tmp, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OPAL_LIST_DESTRUCT(&dvpids);
        return rc;
    }
    if (NULL != tmp) {
        free(tmp);
    }

    return ORTE_SUCCESS;
}

/* decode a nodemap for a daemon */
int orte_util_decode_daemon_nodemap(opal_buffer_t *buffer)
{
    int m, n, rc;
    orte_node_t *node;
    size_t k, num_nodes, endpt;
    orte_job_t *daemons;
    orte_proc_t *dptr;
    char **nodes, *indices, *dvpids;
    char *ndnames, *rmndr, **tmp;
    int *nodeids, *dids;

    /* unpack the node regex */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ndnames, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* it is okay for this to be NULL */
    if (NULL == ndnames) {
        return ORTE_SUCCESS;
    }

    /* unpack the index regex */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &indices, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        free(ndnames);
        return rc;
    }
    /* this is not allowed to be NULL */
    if (NULL == indices) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        free(ndnames);
        return ORTE_ERR_BAD_PARAM;
    }

    /* unpack the daemon vpid regex */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &dvpids, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        free(ndnames);
        free(indices);
        return rc;
    }
    /* this is not allowed to be NULL */
    if (NULL == dvpids) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        free(ndnames);
        free(indices);
        return ORTE_ERR_BAD_PARAM;
    }

    /* if we are the HNP, then we just discard these strings as we already
     * have a complete picture - but we needed to unpack them in order to
     * maintain sync in the unpacking order */
    if (ORTE_PROC_IS_HNP) {
        free(ndnames);
        free(indices);
        free(dvpids);
        return ORTE_SUCCESS;
    }

    /* decompress the regex */
    nodes = NULL;
    if (ORTE_SUCCESS != (rc = orte_regex_extract_node_names(ndnames, &nodes))) {
        free(ndnames);
        free(indices);
        free(dvpids);
        return rc;
    }
    free(ndnames);

    if (NULL == nodes) {
        /* should not happen */
        free(indices);
        free(dvpids);
        return ORTE_ERR_NOT_FOUND;
    }

    /* decompress the index ranges */
    num_nodes = opal_argv_count(nodes);
    nodeids = (int*)malloc(num_nodes * sizeof(int));
    tmp = opal_argv_split(indices, ',');
    k = 0;
    for (n=0; NULL != tmp[n]; n++) {
        /* convert the number - since it might be a range,
         * save the remainder pointer */
        nodeids[k++] = strtoul(tmp[n], &rmndr, 10);
        if (NULL != rmndr) {
            /* it must be a range - find the endpoint */
            ++rmndr;
            m = nodeids[k-1] + 1;
            endpt = strtoul(rmndr, NULL, 10);
            while (k <= endpt && k < num_nodes) {
                nodeids[k++] = m++;
            }
            --k;  // step back to compensate for later increment
        }
        ++k;
    }
    opal_argv_free(tmp);
    free(indices);

    /* decompress the vpids */
    dids = (int*)malloc(num_nodes * sizeof(int));
    tmp = opal_argv_split(dvpids, ',');
    k = 0;
    for (n=0; NULL != tmp[n]; n++) {
        /* convert the number - since it might be a range,
         * save the remainder pointer */
        dids[k++] = strtoul(tmp[n], &rmndr, 10);
        if (NULL != rmndr) {
            /* it must be a range - find the endpoint */
            ++rmndr;
            endpt = strtoul(rmndr, NULL, 10);
            m = dids[k-1] + 1;
            while (k <= endpt && k < num_nodes) {
                dids[k++] = m++;
            }
            --k;  // step back to compensate for later increment
        }
        ++k;
    }
    opal_argv_free(tmp);
    free(dvpids);

    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    /* since this is a complete picture of all the nodes, reset the
     * counters and regenerate them */
    orte_process_info.num_daemons = 0;
    orte_process_info.num_nodes = 0;
    daemons->num_procs = 0;

    /* update the node array */
    for (n=0; NULL != nodes[n]; n++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, nodeids[n]))) {
            node = OBJ_NEW(orte_node_t);
            node->name = nodes[n];
            node->index = nodeids[n];
            opal_pointer_array_set_item(orte_node_pool, node->index, node);
        } else if (NULL != node->daemon) {
            OBJ_RELEASE(node->daemon);
            node->daemon = NULL;
        }
        ++orte_process_info.num_nodes;
        if (NULL == (dptr = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, dids[n]))) {
            /* create a daemon object for this node */
            dptr = OBJ_NEW(orte_proc_t);
            dptr->name.jobid = ORTE_PROC_MY_NAME->jobid;
            dptr->name.vpid = dids[n];
            ORTE_FLAG_SET(dptr, ORTE_PROC_FLAG_ALIVE);  // assume the daemon is alive until discovered otherwise
            opal_pointer_array_set_item(daemons->procs, dids[n], dptr);
        } else if (NULL != dptr->node) {
            OBJ_RELEASE(dptr->node);
            dptr->node = NULL;
        }
        ++daemons->num_procs;
        /* link the node to the daemon */
        OBJ_RETAIN(dptr);
        node->daemon = dptr;
        /* link the node to the daemon */
        OBJ_RETAIN(node);
        dptr->node = node;
    }
    /* we cannot use opal_argv_free here as this would release
     * all the node names themselves. Instead, we just free the
     * array of string pointers, leaving the strings alone */
    free(nodes);
    free(nodeids);
    free(dids);

    /* unpdate num procs */
    if (orte_process_info.num_procs != daemons->num_procs) {
        orte_process_info.num_procs = daemons->num_procs;
        /* need to update the routing plan */
        orte_routed.update_routing_plan(NULL);
    }

    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    /* update num_daemons */
    orte_process_info.num_daemons = daemons->num_procs;

    if (0 < opal_output_get_verbosity(orte_debug_verbosity)) {
        int i;
        for (i=0; i < orte_node_pool->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                continue;
            }
            opal_output(0, "%s node[%d].name %s daemon %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), i,
                        (NULL == node->name) ? "NULL" : node->name,
                        (NULL == node->daemon) ? "NONE" : ORTE_VPID_PRINT(node->daemon->name.vpid));
        }
    }

    return ORTE_SUCCESS;
}
