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
 * Copyright (c) 2014-2017 Research Organization for Information Science
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
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/state/state.h"

#include "orte/util/nidmap.h"

int orte_util_build_daemon_nidmap(void)
{
    int i;
    int rc;
    struct hostent *h;
    orte_node_t *node;
    opal_buffer_t buf;
    opal_process_name_t proc;
    char *uri, *addr;
    char *proc_name;
    opal_value_t kv;

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

    /* we must have already built the node pool, so cycle across it */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    for (i=0; i < orte_node_pool->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
            continue;
        }
        if (NULL == node->daemon) {
            /* this node isn't occupied */
            continue;
        }
        /* define the vpid for this daemon */
        proc.vpid = node->daemon->name.vpid;
        /* store the hostname for the proc */
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(OPAL_PMIX_HOSTNAME);
        kv.data.string = strdup(node->name);
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
        if (NULL == (h = gethostbyname(node->name))) {
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
                             node->name, i+1, addr, uri));
        /* if this is the HNP, then store it */
        if (!ORTE_PROC_IS_HNP && 0 == i) {
            orte_process_info.my_hnp_uri = strdup(uri);
        }
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

int orte_util_nidmap_create(char **regex)
{
    char *node;
    char prefix[ORTE_MAX_NODE_PREFIX];
    int i, j, n, len, startnum, nodenum, numdigits;
    bool found, fullname;
    char *suffix, *sfx, *nodenames;
    orte_regex_node_t *ndreg;
    orte_regex_range_t *range, *rng;
    opal_list_t nodenms, dvpids;
    opal_list_item_t *item, *itm2;
    char **regexargs = NULL, *tmp, *tmp2;
    orte_node_t *nptr;
    orte_vpid_t vpid;

    OBJ_CONSTRUCT(&nodenms, opal_list_t);
    OBJ_CONSTRUCT(&dvpids, opal_list_t);

    rng = NULL;
    for (n=0; n < orte_node_pool->size; n++) {
        if (NULL == (nptr = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, n))) {
            continue;
        }
        /* if no daemon has been assigned, then this node is not being used */
        if (NULL == nptr->daemon) {
            vpid = -1;  // indicates no daemon assigned
        } else {
            vpid = nptr->daemon->name.vpid;
        }
        /* deal with the daemon vpid - see if it is next in the
         * current range */
        if (NULL == rng) {
            /* just starting */
            rng = OBJ_NEW(orte_regex_range_t);
            rng->vpid = vpid;
            rng->cnt = 1;
            opal_list_append(&dvpids, &rng->super);
        } else if (UINT32_MAX == vpid) {
            if (-1 == rng->vpid) {
                rng->cnt++;
            } else {
                /* need to start another range */
                rng = OBJ_NEW(orte_regex_range_t);
                rng->vpid = vpid;
                rng->cnt = 1;
                opal_list_append(&dvpids, &rng->super);
            }
        } else if (-1 == rng->vpid) {
            /* need to start another range */
            rng = OBJ_NEW(orte_regex_range_t);
            rng->vpid = vpid;
            rng->cnt = 1;
            opal_list_append(&dvpids, &rng->super);
        } else {
            /* is this the next in line */
            if (vpid == (orte_vpid_t)(rng->vpid + rng->cnt)) {
                rng->cnt++;
            } else {
                /* need to start another range */
                rng = OBJ_NEW(orte_regex_range_t);
                rng->vpid = vpid;
                rng->cnt = 1;
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
                range->vpid = nodenum;
                range->cnt = 1;
                opal_list_append(&ndreg->ranges, &range->super);
                break;
            }
            /* see if the node number is out of sequence */
            if (nodenum != (range->vpid + range->cnt)) {
                /* start a new range */
                range = OBJ_NEW(orte_regex_range_t);
                range->vpid = nodenum;
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
            range->vpid = nodenum;
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
                asprintf(&tmp2, "%s%u,", tmp, range->vpid);
            } else {
                asprintf(&tmp2, "%s%u-%u,", tmp, range->vpid, range->vpid + range->cnt - 1);
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
    nodenames = opal_argv_join(regexargs, ',');
    /* cleanup */
    opal_argv_free(regexargs);
    OBJ_DESTRUCT(&nodenms);

    /* do the same for the vpids */
    tmp = NULL;
    while (NULL != (item = opal_list_remove_first(&dvpids))) {
        rng = (orte_regex_range_t*)item;
        if (1 < rng->cnt) {
            if (NULL == tmp) {
                asprintf(&tmp, "%u(%u)", rng->vpid, rng->cnt);
            } else {
                asprintf(&tmp2, "%s,%u(%u)", tmp, rng->vpid, rng->cnt);
                free(tmp);
                tmp = tmp2;
            }
        } else {
            if (NULL == tmp) {
                asprintf(&tmp, "%u", rng->vpid);
            } else {
                asprintf(&tmp2, "%s,%u", tmp, rng->vpid);
                free(tmp);
                tmp = tmp2;
            }
        }
        OBJ_RELEASE(rng);
    }
    OPAL_LIST_DESTRUCT(&dvpids);

    /* now concatenate the results into one string */
    asprintf(&tmp2, "%s@%s", nodenames, tmp);
    free(nodenames);
    free(tmp);
    *regex = tmp2;
    return ORTE_SUCCESS;
}

int orte_util_encode_nodemap(opal_buffer_t *buffer)
{
    int n;
    bool test;
    orte_regex_range_t *rng, *slt, *tp, *flg;
    opal_list_t slots, topos, flags;
    opal_list_item_t *item;
    char *tmp, *tmp2;
    orte_node_t *nptr;
    int rc;
    uint8_t ui8;

    /* setup the list of results */
    OBJ_CONSTRUCT(&slots, opal_list_t);
    OBJ_CONSTRUCT(&topos, opal_list_t);
    OBJ_CONSTRUCT(&flags, opal_list_t);

    slt = NULL;
    tp = NULL;
    flg = NULL;

    /* pack a flag indicating if the HNP was included in the allocation */
    if (orte_hnp_is_allocated) {
        ui8 = 1;
    } else {
        ui8 = 0;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &ui8, 1, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* pack a flag indicating if we are in a managed allocation */
    if (orte_managed_allocation) {
        ui8 = 1;
    } else {
        ui8 = 0;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &ui8, 1, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* there is always one topology - our own - so start with it */
    nptr = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
    tp = OBJ_NEW(orte_regex_range_t);
    tp->t = nptr->topology;
    tp->cnt = 1;
    opal_list_append(&topos, &tp->super);

    /* likewise, we have slots */
    slt = OBJ_NEW(orte_regex_range_t);
    slt->slots = nptr->slots;
    slt->cnt = 1;
    opal_list_append(&slots, &slt->super);

    /* and flags */
    flg = OBJ_NEW(orte_regex_range_t);
    if (ORTE_FLAG_TEST(nptr, ORTE_NODE_FLAG_SLOTS_GIVEN)) {
        flg->slots = 1;
    } else {
        flg->slots = 0;
    }
    flg->cnt = 1;
    opal_list_append(&flags, &flg->super);

    for (n=1; n < orte_node_pool->size; n++) {
        if (NULL == (nptr = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, n))) {
            continue;
        }
        /* check the #slots */
        /* is this the next in line */
        if (nptr->slots == slt->slots) {
            slt->cnt++;
        } else {
            /* need to start another range */
            slt = OBJ_NEW(orte_regex_range_t);
            slt->slots = nptr->slots;
            slt->cnt = 1;
            opal_list_append(&slots, &slt->super);
        }
        /* check the topologies */
        if (NULL == nptr->topology) {
            /* we don't know this topology, likely because
             * we don't have a daemon on the node */
            tp = OBJ_NEW(orte_regex_range_t);
            tp->t = NULL;
            tp->cnt = 1;
            opal_list_append(&topos, &tp->super);
        } else {
            /* is this the next in line */
            if (tp->t == nptr->topology) {
                tp->cnt++;
            } else {
                /* need to start another range */
                tp = OBJ_NEW(orte_regex_range_t);
                tp->t = nptr->topology;
                tp->cnt = 1;
                opal_list_append(&topos, &tp->super);
            }
        }
        /* check the flags */
        test = ORTE_FLAG_TEST(nptr, ORTE_NODE_FLAG_SLOTS_GIVEN);
        /* is this the next in line */
         if ((test && 1 == flg->slots) ||
             (!test && 0 == flg->slots)) {
            flg->cnt++;
        } else {
            /* need to start another range */
            flg = OBJ_NEW(orte_regex_range_t);
            if (test) {
                flg->slots = 1;
            } else {
                flg->slots = 0;
            }
            flg->cnt = 1;
            opal_list_append(&flags, &flg->super);
        }
    }

    /* pass #slots on each node */
    tmp = NULL;
    while (NULL != (item = opal_list_remove_first(&slots))) {
        rng = (orte_regex_range_t*)item;
        if (NULL == tmp) {
            asprintf(&tmp, "%d[%d]", rng->cnt, rng->slots);
        } else {
            asprintf(&tmp2, "%s,%d[%d]", tmp, rng->cnt, rng->slots);
            free(tmp);
            tmp = tmp2;
        }
        OBJ_RELEASE(rng);
    }
    OPAL_LIST_DESTRUCT(&slots);
    /* pack the string */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tmp, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (NULL != tmp) {
        free(tmp);
    }

    /* do the same to pass the flags for each node */
    tmp = NULL;
    while (NULL != (item = opal_list_remove_first(&flags))) {
        rng = (orte_regex_range_t*)item;
        if (NULL == tmp) {
            asprintf(&tmp, "%d[%d]", rng->cnt, rng->slots);
        } else {
            asprintf(&tmp2, "%s,%d[%d]", tmp, rng->cnt, rng->slots);
            free(tmp);
            tmp = tmp2;
        }
        OBJ_RELEASE(rng);
    }
    OPAL_LIST_DESTRUCT(&flags);

    /* pack the string */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tmp, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (NULL != tmp) {
        free(tmp);
    }

    /* handle the topologies - as the most common case by far
     * is to have homogeneous topologies, we only send them
     * if something is different. We know that the HNP is
     * the first topology, and that any differing topology
     * on the compute nodes must follow. So send the topologies
     * if and only if:
     *
     * (a) the HNP is being used to house application procs and
     *     there is more than one topology on our list; or
     *
     * (b) the HNP is not being used, but there are more than
     *     two topologies on our list, thus indicating that
     *     there are multiple topologies on the compute nodes
     */
    if (!orte_hnp_is_allocated || (ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping) & ORTE_MAPPING_NO_USE_LOCAL)) {
        /* remove the first topo on the list */
        item = opal_list_remove_first(&topos);
        OBJ_RELEASE(item);
    }
    tmp = NULL;
    if (1 < opal_list_get_size(&topos)) {
        opal_buffer_t bucket, *bptr;
        OBJ_CONSTRUCT(&bucket, opal_buffer_t);
        while (NULL != (item = opal_list_remove_first(&topos))) {
            rng = (orte_regex_range_t*)item;
            if (NULL == tmp) {
                asprintf(&tmp, "%d", rng->cnt);
            } else {
                asprintf(&tmp2, "%s,%d", tmp, rng->cnt);
                free(tmp);
                tmp = tmp2;
            }
            if (NULL == rng->t) {
                /* need to account for NULL topology */
                tmp2 = NULL;
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&bucket, &tmp2, 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(rng);
                    OPAL_LIST_DESTRUCT(&topos);
                    OBJ_DESTRUCT(&bucket);
                    free(tmp);
                    return rc;
                }
            } else {
                /* pack this topology string */
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&bucket, &rng->t->sig, 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(rng);
                    OPAL_LIST_DESTRUCT(&topos);
                    OBJ_DESTRUCT(&bucket);
                    free(tmp);
                    return rc;
                }
                /* pack the topology itself */
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&bucket, &rng->t->topo, 1, OPAL_HWLOC_TOPO))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(rng);
                    OPAL_LIST_DESTRUCT(&topos);
                    OBJ_DESTRUCT(&bucket);
                    free(tmp);
                    return rc;
                }
            }
            OBJ_RELEASE(rng);
        }
        OPAL_LIST_DESTRUCT(&topos);
        /* pack the string */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tmp, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&bucket);
            free(tmp);
            return rc;
        }
        free(tmp);

        /* now pack the topologies */
        bptr = &bucket;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &bptr, 1, OPAL_BUFFER))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&bucket);
            return rc;
        }
        OBJ_DESTRUCT(&bucket);
    } else {
        /* need to pack the NULL just to terminate the region */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tmp, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return ORTE_SUCCESS;
}

int orte_util_nidmap_parse(char *regex)
{
    char *nodelist, *vpids, *ptr;
    char **nodes, **dvpids;
    int rc, n, cnt;
    orte_regex_range_t *rng;
    opal_list_t dids;
    orte_job_t *daemons;
    orte_node_t *nd;
    orte_proc_t *proc;

    /* if we are the HNP, we don't need to parse this */
    if (ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    /* split the regex into its node and vpid parts */
    nodelist = regex;
    vpids = strchr(regex, '@');
    if (NULL == vpids) {
        /* indicates the regex got mangled somewhere */
        return ORTE_ERR_BAD_PARAM;
    }
    *vpids = '\0';  // terminate the nodelist string
    ++vpids;  // step over the separator
    if (NULL == vpids || '\0' == *vpids) {
        /* indicates the regex got mangled somewhere */
        return ORTE_ERR_BAD_PARAM;
    }

    /* decompress the nodes regex */
    nodes = NULL;
    if (ORTE_SUCCESS != (rc = orte_regex_extract_node_names(nodelist, &nodes))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (NULL == nodes) {
        /* should not happen */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    /* decompress the vpids */
    OBJ_CONSTRUCT(&dids, opal_list_t);
    dvpids = opal_argv_split(vpids, ',');
    for (n=0; NULL != dvpids[n]; n++) {
        rng = OBJ_NEW(orte_regex_range_t);
        opal_list_append(&dids, &rng->super);
        /* check for a count */
        if (NULL != (ptr = strchr(dvpids[n], '('))) {
            dvpids[n][strlen(dvpids[n])-1] = '\0';  // remove trailing paren
            *ptr = '\0';
            ++ptr;
            rng->cnt = strtoul(ptr, NULL, 10);
        } else {
            rng->cnt = 1;
        }
        /* convert the number */
        rng->vpid = strtoul(dvpids[n], NULL, 10);
    }
    opal_argv_free(dvpids);

    /* get the daemon job object */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);

    /* create the node pool array - this will include
     * _all_ nodes known to the allocation */
    rng = (orte_regex_range_t*)opal_list_get_first(&dids);
    cnt = 0;
    for (n=0; NULL != nodes[n]; n++) {
        nd = OBJ_NEW(orte_node_t);
        nd->name = nodes[n];
        opal_pointer_array_set_item(orte_node_pool, n, nd);
        /* see if it has a daemon on it */
        if (-1 != rng->vpid) {
            /* we have a daemon, so let's create the tracker for it */
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, rng->vpid+cnt))) {
                proc = OBJ_NEW(orte_proc_t);
                proc->name.jobid = ORTE_PROC_MY_NAME->jobid;
                proc->name.vpid = rng->vpid + cnt;
                proc->state = ORTE_PROC_STATE_RUNNING;
                ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_ALIVE);
                daemons->num_procs++;
                opal_pointer_array_set_item(daemons->procs, proc->name.vpid, proc);
            }
            nd->index = proc->name.vpid;
            OBJ_RETAIN(nd);
            proc->node = nd;
            OBJ_RETAIN(proc);
            nd->daemon = proc;
        }
        ++cnt;
        if (rng->cnt <= cnt) {
            rng = (orte_regex_range_t*)opal_list_get_next(&rng->super);
            if (NULL == rng) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                return ORTE_ERR_NOT_FOUND;
            }
            cnt = 0;
        }
    }

    /* update num procs */
    if (orte_process_info.num_procs != daemons->num_procs) {
        orte_process_info.num_procs = daemons->num_procs;
        /* need to update the routing plan */
        orte_routed.update_routing_plan(NULL);
    }

    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    if (0 < opal_output_get_verbosity(orte_debug_verbosity)) {
        int i;
        for (i=0; i < orte_node_pool->size; i++) {
            if (NULL == (nd = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                continue;
            }
            opal_output(0, "%s node[%d].name %s daemon %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), i,
                        (NULL == nd->name) ? "NULL" : nd->name,
                        (NULL == nd->daemon) ? "NONE" : ORTE_VPID_PRINT(nd->daemon->name.vpid));
        }
    }

    return ORTE_SUCCESS;
}

/* decode a nodemap for a daemon */
int orte_util_decode_daemon_nodemap(opal_buffer_t *buffer)
{
    int n, nn, rc, cnt, offset;
    orte_node_t *node;
    char *slots=NULL, *topos=NULL, *flags=NULL;
    char *rmndr, **tmp;
    opal_list_t slts, flgs;;
    opal_buffer_t *bptr=NULL;
    orte_topology_t *t2;
    orte_regex_range_t *rng, *srng, *frng;
    uint8_t ui8;

    OBJ_CONSTRUCT(&slts, opal_list_t);
    OBJ_CONSTRUCT(&flgs, opal_list_t);

    /* unpack the flag indicating if the HNP was allocated */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ui8, &n, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (0 == ui8) {
        orte_hnp_is_allocated = false;
    } else {
        orte_hnp_is_allocated = true;
    }

    /* unpack the flag indicating we are in a managed allocation */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ui8, &n, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (0 == ui8) {
        orte_managed_allocation = false;
    } else {
        orte_managed_allocation = true;
    }

    /* unpack the slots regex */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &slots, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    /* this is not allowed to be NULL */
    if (NULL == slots) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        rc = ORTE_ERR_BAD_PARAM;
        goto cleanup;
    }

    /* unpack the flags regex */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &flags, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    /* this is not allowed to be NULL */
    if (NULL == flags) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        rc = ORTE_ERR_BAD_PARAM;
        goto cleanup;
    }

    /* unpack the topos regex - this may not have been
     * provided (e.g., for a homogeneous machine) */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &topos, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (NULL != topos) {
        /* need to unpack the topologies */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &bptr, &n, OPAL_BUFFER))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* if we are the HNP, then we just discard these strings as we already
     * have a complete picture - but we needed to unpack them in order to
     * maintain sync in the unpacking order */
    if (ORTE_PROC_IS_HNP) {
        rc = ORTE_SUCCESS;
        goto cleanup;
    }

    /* decompress the slots */
    tmp = opal_argv_split(slots, ',');
    for (n=0; NULL != tmp[n]; n++) {
        rng = OBJ_NEW(orte_regex_range_t);
        opal_list_append(&slts, &rng->super);
        /* find the '[' as that delimits the value */
        rmndr = strchr(tmp[n], '[');
        if (NULL == rmndr) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            rc = ORTE_ERR_BAD_PARAM;
            opal_argv_free(tmp);
            goto cleanup;
        }
        *rmndr = '\0';
        ++rmndr;
        /* convert that number as this is the number of
         * slots for this range */
        rng->slots = strtoul(rmndr, NULL, 10);
        /* convert the initial number as that is the cnt */
        rng->cnt = strtoul(tmp[n], NULL, 10);
    }
    opal_argv_free(tmp);

    /* decompress the flags */
    tmp = opal_argv_split(flags, ',');
    for (n=0; NULL != tmp[n]; n++) {
        rng = OBJ_NEW(orte_regex_range_t);
        opal_list_append(&flgs, &rng->super);
        /* find the '[' as that delimits the value */
        rmndr = strchr(tmp[n], '[');
        if (NULL == rmndr) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            opal_argv_free(tmp);
            rc = ORTE_ERR_BAD_PARAM;
            goto cleanup;
        }
        *rmndr = '\0';
        ++rmndr;
        /* check the value - it is just one character */
        if ('1' == *rmndr) {
            rng->slots = 1;
        } else {
            rng->slots = 0;
        }
        /* convert the initial number as that is the cnt */
        rng->cnt = strtoul(tmp[n], NULL, 10);
    }
    opal_argv_free(tmp);
    free(flags);

    /* update the node array */
    srng = (orte_regex_range_t*)opal_list_get_first(&slts);
    frng = (orte_regex_range_t*)opal_list_get_first(&flgs);
    for (n=0; n < orte_node_pool->size; n++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, n))) {
            continue;
        }
        /* set the number of slots */
        node->slots = srng->slots;
        srng->cnt--;
        if (0 == srng->cnt) {
            srng = (orte_regex_range_t*)opal_list_get_next(&srng->super);
        }
        /* set the flags */
        if (0 == frng->slots) {
            ORTE_FLAG_UNSET(node, ORTE_NODE_FLAG_SLOTS_GIVEN);
        } else {
            ORTE_FLAG_SET(node, ORTE_NODE_FLAG_SLOTS_GIVEN);
        }
        frng->cnt--;
        if (0 == frng->cnt) {
            frng = (orte_regex_range_t*)opal_list_get_next(&frng->super);
        }
    }

    /* if no topology info was passed, then everyone shares our topology */
    if (NULL == bptr) {
        /* our topology is first in the array */
        t2 = (orte_topology_t*)opal_pointer_array_get_item(orte_node_topologies, 0);
        for (n=0; n < orte_node_pool->size; n++) {
            if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, n))) {
                if (NULL == node->topology) {
                    OBJ_RETAIN(t2);
                    node->topology = t2;
                }
            }
        }
    } else {
        char *sig;
        hwloc_topology_t topo;
        /* decompress the topology regex */
        tmp = opal_argv_split(topos, ',');
        /* there must be a topology definition for each range */
        offset = 0;
        for (nn=0; NULL != tmp[nn]; nn++) {
            cnt = strtoul(tmp[nn], NULL, 10);
            /* unpack the signature */
            n = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(bptr, &sig, &n, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(tmp);
                OBJ_RELEASE(bptr);
                goto cleanup;
            }
            if (NULL == sig) {
                /* the nodes in this range have not reported a topology,
                 * so skip them */
                offset += cnt;
                continue;
            }
            n = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(bptr, &topo, &n, OPAL_HWLOC_TOPO))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(tmp);
                OBJ_RELEASE(bptr);
                free(sig);
                goto cleanup;
            }
            /* see if we already have this topology - could be an update */
            t2 = NULL;
            for (n=0; n < orte_node_topologies->size; n++) {
                if (NULL == (t2 = (orte_topology_t*)opal_pointer_array_get_item(orte_node_topologies, n))) {
                    continue;
                }
                if (0 == strcmp(t2->sig, sig)) {
                    /* found a match */
                    free(sig);
                    opal_hwloc_base_free_topology(topo);
                    sig = NULL;
                    break;
                }
            }
            if (NULL != sig || NULL == t2) {
                /* new topology - record it */
                t2 = OBJ_NEW(orte_topology_t);
                t2->sig = sig;
                t2->topo = topo;
                opal_pointer_array_add(orte_node_topologies, t2);
            }
            /* point each of the nodes in this range to this topology */
            n=0;
            while (n < cnt && (n+offset) < orte_node_pool->size) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, n+offset))) {
                    continue;
                }
                if (NULL == node->topology) {
                    OBJ_RETAIN(t2);
                    node->topology = t2;
                }
                ++n;
            }
            offset += cnt;
        }
        OBJ_RELEASE(bptr);
        opal_argv_free(tmp);
    }

  cleanup:
    OPAL_LIST_DESTRUCT(&slts);
    OPAL_LIST_DESTRUCT(&flgs);
    return rc;
}
