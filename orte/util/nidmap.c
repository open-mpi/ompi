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
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
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
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/state/state.h"

#include "orte/util/nidmap.h"

#if ORTE_ENABLE_STATIC_PORTS
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

    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = strdup(OPAL_PMIX_HOSTNAME);
    kv.data.string = strdup("HNP");
    kv.type = OPAL_STRING;
    if (OPAL_SUCCESS != (rc = opal_pmix.store_local(&proc, &kv))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&kv);
        return rc;
    }
    OBJ_DESTRUCT(&kv);

    /* the daemon vpids will be assigned in order,
     * starting with vpid=1 for the first node in
     * the list
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    for (i=0; i < num_nodes; i++) {
        /* define the vpid for this daemon */
        proc.vpid = i+1;
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
#endif

int orte_util_encode_nodemap(opal_byte_object_t *boptr, bool update)
{
    orte_node_t *node;
    int32_t i;
    int rc;
    opal_buffer_t buf;
    orte_job_t *daemons;
    orte_proc_t *dmn;

    /* if the daemon job has not been updated, then there is
     * nothing to send
     */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    if (update && !ORTE_FLAG_TEST(daemons, ORTE_JOB_FLAG_UPDATED)) {
        boptr->bytes = NULL;
        boptr->size = 0;
        return ORTE_SUCCESS;
    }

    /* setup a buffer for tmp use */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    /* send the number of nodes */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &daemons->num_procs, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    for (i=0; i < daemons->procs->size; i++) {
        if (NULL == (dmn = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, i))) {
            continue;
        }
        /* if the daemon doesn't have a node, that's an error */
        if (NULL == (node = dmn->node)) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &dmn->name.vpid, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* pack the node */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &node, 1, ORTE_NODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* transfer the payload to the byte object */
    opal_dss.unload(&buf, (void**)&boptr->bytes, &boptr->size);
    OBJ_DESTRUCT(&buf);

    return ORTE_SUCCESS;
}

/* decode a nodemap for a daemon */
int orte_util_decode_daemon_nodemap(opal_byte_object_t *bo)
{
    int n;
    orte_vpid_t vpid;
    orte_node_t *node, *nptr;
    opal_buffer_t buf;
    int rc=ORTE_SUCCESS;
    orte_job_t *daemons;
    orte_proc_t *dptr;
    orte_vpid_t num_daemons;

    if (NULL == bo->bytes || 0 == bo->size) {
        /* nothing to unpack */
        return ORTE_SUCCESS;
    }

    /* xfer the byte object to a buffer for unpacking */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, bo->bytes, bo->size);

    /* unpack the number of procs */
    n=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &num_daemons, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* transfer the data to the nodes */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    daemons->num_procs = num_daemons;
    n=1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &vpid, &n, ORTE_VPID))) {
        /* unpack and store the node */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &node, &n, ORTE_NODE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* do we already have this node? */
        nptr = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, vpid);
        /* set the new node object into the array */
        opal_pointer_array_set_item(orte_node_pool, vpid, node);
        if (NULL == (dptr = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, vpid))) {
            dptr = OBJ_NEW(orte_proc_t);
            dptr->name.jobid = ORTE_PROC_MY_NAME->jobid;
            dptr->name.vpid = vpid;
            opal_pointer_array_set_item(daemons->procs, vpid, dptr);
        }
        if (NULL != node->daemon) {
            OBJ_RELEASE(node->daemon);
        }
        OBJ_RETAIN(dptr);
        node->daemon = dptr;
        if (NULL != dptr->node) {
            OBJ_RELEASE(dptr->node);
        }
        OBJ_RETAIN(node);
        if (NULL != nptr) {
            OBJ_RELEASE(nptr);
        }
        dptr->node = node;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    rc = ORTE_SUCCESS;

    orte_process_info.num_procs = daemons->num_procs;

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

    OBJ_DESTRUCT(&buf);
    return rc;
}
